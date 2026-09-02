# =============================================================================
# SIMULACIÓN MONTE CARLO + REGRESIONES: acceso a CeSAC por villa  (v4)
# =============================================================================
#
# CAMBIOS RESPECTO DE v3:
#
#   [1] NUEVA LÓGICA DE VILLA (III.6):
#       - Villa (=1)     ⇔  tiene `asentamiento` (después de normalizar)
#       - No villa (=0)  ⇔  no tiene `asentamiento` (usa radio, sin importar
#                            si el radio solapa parcialmente con barrio popular)
#       Esto recupera ~886 casos que antes se excluían por "solapamiento
#       parcial" y ahora se tratan como no-villa según su comuna.
#
#   [2] NORMALIZACIÓN DE ASENTAMIENTO (II.3.5, nuevo):
#       - Lookup manual: variantes de escritura ("Villa 20", "VILLA 20",
#         "villa20") mapean al canónico ("Barrio 20") antes del join.
#       - Lista negra: strings que NO son villa (Villa Lugano, Barracas,
#         genéricos) se setean a NA → tratados como no-villa.
#       Recupera ~800 de los 852 casos que antes tenían asentamiento pero
#       no matcheaban con id_barrio.
#
#   [3] FALLBACK PARA comuna_regresion (III.6):
#       - Si comuna_ok es NA, usa comuna cruda de data_revisada.
#       Recupera los 774 casos sin comuna_regresion.
#
# Pipeline:
#   PARTE I     Preparación de individuos
#   PARTE II    Simulación Monte Carlo (incluye II.3.5 normalización)
#   PARTE III   Construcción de variables (nueva lógica en III.6)
#   PARTE IV    Preparación para regresiones
#   PARTE V     Ejecución de 7 especificaciones × 3 vcov
#   PARTE VI    Tablas estilo paper
#   PARTE VII   Gráficos
# =============================================================================


library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(broom)
library(fixest)
library(flextable)
library(officer)
library(ggplot2)
library(patchwork)

set.seed(1234)

N_SIM                  <- 1000
PONDERAR_POR_POBLACION <- TRUE

RUTA_DATOS <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/ESW version 2/data/processed"

setFixest_notes(FALSE)


# =============================================================================
# A) FUNCIONES AUXILIARES
# =============================================================================

# A.1 Sortear radios ponderado -----------------------------------------------

sortear_radios <- function(radios_pool, pesos_pool, n_sim, ponderar) {
  if (is.null(radios_pool) || length(radios_pool) == 0) {
    return(rep(NA_integer_, n_sim))
  }
  usar_pesos <- ponderar && !is.null(pesos_pool) && sum(pesos_pool, na.rm = TRUE) > 0
  if (usar_pesos) {
    sample(radios_pool, size = n_sim, replace = TRUE, prob = pesos_pool)
  } else {
    sample(radios_pool, size = n_sim, replace = TRUE)
  }
}

# A.2 Renombrado defensivo de columnas .x / .y -------------------------------

renombrar_si_existe <- function(df, viejo_x, viejo_y, nuevo_x, nuevo_y) {
  if (viejo_x %in% names(df)) names(df)[names(df) == viejo_x] <- nuevo_x
  if (viejo_y %in% names(df)) names(df)[names(df) == viejo_y] <- nuevo_y
  df
}

# A.3 Resolver de dónde sacar cada pct_<nbi> a nivel de radio ---------------

resolver_pct_radio <- function(df, var) {
  candidatos_pct <- c(paste0("pct_", var), paste0("pct_", var, ".x"), paste0("pct_", var, "_radio"))
  candidatos_n   <- c(paste0("n_", var), paste0("n_", var, ".x"))
  
  encontrado_pct <- candidatos_pct[candidatos_pct %in% names(df)]
  if (length(encontrado_pct) > 0) return(list(fuente = "pct", columna = encontrado_pct[1]))
  
  encontrado_n <- candidatos_n[candidatos_n %in% names(df)]
  if (length(encontrado_n) > 0) return(list(fuente = "n", columna = encontrado_n[1]))
  
  NULL
}

# A.4 Diagnóstico de NAs -----------------------------------------------------

diagnosticar_missingness <- function(data, vars, agrupar_por = NULL) {
  if (!is.null(agrupar_por)) {
    data <- data |> group_by(across(all_of(agrupar_por)))
  }
  data |>
    summarise(
      n = n(),
      across(all_of(vars), ~ sum(is.na(.)), .names = "na_{.col}"),
      .groups = "drop"
    )
}

# A.5 Diagnóstico: qué asentamientos no matchearon con id_barrio ------------

diagnosticar_matches_pendientes <- function(base_unida) {
  pendientes <- base_unida |>
    filter(iteracion == 1, !is.na(asentamiento), is.na(id_barrio)) |>
    distinct(asentamiento) |>
    arrange(asentamiento) |>
    pull(asentamiento)
  
  if (length(pendientes) == 0) {
    cat("   ✓ Todos los asentamientos matchearon con id_barrio.\n")
  } else {
    cat(sprintf("   ⚠ %d asentamiento(s) todavía sin match — agregarlos al lookup:\n",
                length(pendientes)))
    print(pendientes)
  }
}

# A.6 Chequeo previo de fórmulas --------------------------------------------

verificar_formula <- function(formula_str, datos, nombre_regresion = "") {
  vars_formula <- all.vars(as.formula(formula_str))
  faltantes <- setdiff(vars_formula, names(datos))
  if (length(faltantes) > 0) {
    stop(sprintf(
      "%s usa variables que no existen en los datos: %s\n   Fórmula: %s",
      nombre_regresion, paste(faltantes, collapse = ", "), formula_str
    ))
  }
  invisible(TRUE)
}

# A.7 Ajustar una spec sobre una base ---------------------------------------

ajustar_spec <- function(data, spec, vcov_type = "iid") {
  
  vcov_arg <- if (identical(vcov_type, "iid")) {
    "iid"
  } else {
    as.formula(paste0("~", vcov_type))
  }
  
  fit <- feols(as.formula(spec$formula), data = data, vcov = vcov_arg)
  
  coincide_patron <- function(term, patrones) {
    if (length(patrones) == 0) rep(FALSE, length(term))
    else str_detect(term, paste(patrones, collapse = "|"))
  }
  
  coef_rows <- as.data.frame(coeftable(fit)) |>
    rownames_to_column("term") |>
    rename(
      estimate  = Estimate,
      std.error = `Std. Error`,
      statistic = `t value`,
      p.value   = `Pr(>|t|)`
    ) |>
    filter(term %in% spec$coefs | coincide_patron(term, spec$extra_patterns))
  
  coef_rows |>
    mutate(
      nobs   = nobs(fit),
      adj_r2 = fixest::r2(fit, "ar2")
    )
}

# A.8 Correr todas las specs × todas las iteraciones ------------------------

ejecutar_todas <- function(especificaciones, datos_por_iteracion, vcov_type) {
  map_dfr(names(especificaciones), function(spec_id) {
    spec <- especificaciones[[spec_id]]
    verificar_formula(spec$formula, datos_por_iteracion[[1]], spec$label)
    
    imap_dfr(
      datos_por_iteracion,
      ~ ajustar_spec(.x, spec, vcov_type = vcov_type) |> mutate(iteracion = .y)
    ) |>
      mutate(regresion_id = spec_id, regresion_label = spec$label)
  })
}

# A.9 Resúmenes -------------------------------------------------------------

calcular_resumen <- function(resultados) {
  resultados |>
    group_by(regresion_id, term) |>
    summarise(
      n_iter_validas  = sum(!is.na(estimate)),
      media_coef      = round(mean(estimate, na.rm = TRUE), 4),
      de_coef         = round(sd(estimate, na.rm = TRUE), 4),
      se_prom         = round(mean(std.error, na.rm = TRUE), 4),
      p2_5            = round(quantile(estimate, 0.025, na.rm = TRUE), 4),
      p97_5           = round(quantile(estimate, 0.975, na.rm = TRUE), 4),
      pct_signif_5pct = round(mean(p.value < 0.05, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )
}

calcular_modelo_stats <- function(resultados) {
  resultados |>
    distinct(regresion_id, iteracion, nobs, adj_r2) |>
    group_by(regresion_id) |>
    summarise(
      n_obs_prom       = round(mean(nobs), 0),
      n_obs_rango      = paste0(min(nobs), "\u2013", max(nobs)),
      r2_ajustado_prom = mean(adj_r2),
      .groups = "drop"
    )
}

# A.10 Etiquetado de términos y armado de flextable -------------------------

etiquetar_termino <- function(term) {
  case_when(
    term == "factor(villa)1"                                ~ "Villa (dummy)",
    term == "poly(edad, 2, raw = TRUE)1"                    ~ "Edad",
    term == "poly(edad, 2, raw = TRUE)2"                    ~ "Edad\u00b2",
    term == "log(tiempo_transporte_cesac_t)"                ~ "Tiempo transporte (log)",
    term == "tiempo_transporte_cesac_t"                     ~ "Tiempo transporte",
    term == "pct_hacinamiento_t"                            ~ "Hacinamiento (%)",
    term == "pct_NBI_t"                                     ~ "NBI (%)",
    term == "log(tiempo_transporte_cesac_t):factor(villa)1" ~ "Tiempo transporte (log) \u00d7 Villa",
    term == "tiempo_transporte_cesac_t:factor(villa)1"      ~ "Tiempo transporte \u00d7 Villa",
    str_detect(term, "^factor\\(genero\\)") ~ paste0("G\u00e9nero: ", str_remove(term, "^factor\\(genero\\)")),
    TRUE ~ term
  )
}

orden_variables <- c(
  "Villa (dummy)", "Edad", "Edad\u00b2",
  "Tiempo transporte (log)", "Tiempo transporte",
  "Tiempo transporte (log) \u00d7 Villa", "Tiempo transporte \u00d7 Villa",
  "Hacinamiento (%)", "NBI (%)"
)

armar_tabla_flextable <- function(resumen, modelo_stats, especificaciones, nota_pie) {
  
  orden_regresiones <- names(especificaciones)
  nombres_columnas  <- map_chr(especificaciones, "label")
  
  tabla_larga <- resumen |>
    mutate(
      t_prom = media_coef / se_prom,
      estrellas = case_when(
        abs(t_prom) > 2.576 ~ "***",
        abs(t_prom) > 1.960 ~ "**",
        abs(t_prom) > 1.645 ~ "*",
        TRUE ~ ""
      ),
      celda = paste0(
        formatC(media_coef, format = "f", digits = 3), estrellas,
        "\n(", formatC(se_prom, format = "f", digits = 3), ")"
      ),
      Variable = etiquetar_termino(term)
    )
  
  tabla_ancha <- tabla_larga |>
    select(regresion_id, Variable, celda) |>
    pivot_wider(names_from = regresion_id, values_from = celda) |>
    select(Variable, all_of(orden_regresiones))
  
  niveles_finales <- c(orden_variables, setdiff(unique(tabla_ancha$Variable), orden_variables))
  tabla_ancha <- tabla_ancha |>
    mutate(Variable = factor(Variable, levels = niveles_finales)) |>
    arrange(Variable) |>
    mutate(Variable = as.character(Variable))
  
  meta <- tibble(
    regresion_id = names(especificaciones),
    fe_comuna    = map_chr(especificaciones, ~ if_else(.x$tiene_fe_comuna, "S\u00ed", "No"))
  ) |>
    left_join(modelo_stats, by = "regresion_id")
  
  filas_extra <- meta |>
    transmute(
      regresion_id,
      `FE Comuna`        = fe_comuna,
      `Observaciones`    = as.character(n_obs_prom),
      "R\u00b2 ajustado" = formatC(r2_ajustado_prom, format = "f", digits = 3)
    ) |>
    pivot_longer(-regresion_id, names_to = "Variable", values_to = "valor") |>
    pivot_wider(names_from = regresion_id, values_from = valor) |>
    select(Variable, all_of(orden_regresiones))
  
  tabla_final  <- bind_rows(tabla_ancha, filas_extra)
  n_filas_coef <- nrow(tabla_ancha)
  
  flextable(tabla_final) |>
    set_header_labels(values = c(Variable = "Variable", nombres_columnas)) |>
    theme_booktabs() |>
    align(align = "center", part = "all") |>
    align(j = "Variable", align = "left", part = "all") |>
    fontsize(size = 9, part = "body") |>
    fontsize(size = 8, part = "header") |>
    bold(part = "header") |>
    hline(i = n_filas_coef, border = fp_border(width = 1), part = "body") |>
    autofit() |>
    add_footer_lines(nota_pie) |>
    fontsize(size = 7, part = "footer")
}

# A.11 Gráfico de distribución de un coeficiente ----------------------------

plot_distribucion_coef <- function(datos_coef, regresion_id_sel, term_sel, titulo,
                                   color = "#1B998B") {
  datos_plot <- datos_coef |>
    filter(regresion_id == regresion_id_sel, term == term_sel, !is.na(estimate))
  
  if (nrow(datos_plot) == 0) {
    warning(sprintf("Sin datos para '%s' en %s — se omite el plot.", term_sel, regresion_id_sel))
    return(NULL)
  }
  
  media <- mean(datos_plot$estimate)
  
  ggplot(datos_plot, aes(x = estimate)) +
    geom_histogram(bins = 40, fill = color, alpha = 0.75, color = "white") +
    geom_vline(xintercept = 0,     linetype = "dashed", color = "grey40") +
    geom_vline(xintercept = media, linetype = "solid",  color = "black", linewidth = 0.6) +
    labs(
      title    = titulo,
      subtitle = sprintf("Media entre %d iteraciones: %.3f", nrow(datos_plot), media),
      x = "Coeficiente estimado", y = "Frecuencia"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 10))
}


# =============================================================================
# PARTE I — PREPARACIÓN DE INDIVIDUOS
# =============================================================================

cat("── I.1 Importando data_revisada_ok.xlsx...\n")

data_revisada <- read_excel(file.path(RUTA_DATOS, "data_revisada_ok.xlsx"))

cat(sprintf("   %d filas, %d columnas\n", nrow(data_revisada), ncol(data_revisada)))
stopifnot("La columna 'comuna' no está en data_revisada_ok" = "comuna" %in% names(data_revisada))

# I.2 Normalizar "comuna" al formato de nom_depto en base_radios1 ----------

cat("── I.2 Normalizando columna comuna...\n")

data_revisada <- data_revisada |>
  mutate(
    comuna_num  = as.integer(str_extract(as.character(comuna), "\\d+")),
    comuna_norm = paste0("Comuna ", comuna_num)
  )

# I.3 Chequeo de correspondencia con nom_depto ------------------------------

comunas_datos  <- sort(unique(data_revisada$comuna_norm))
comunas_radios <- sort(unique(base_radios1$nom_depto))

sin_match_datos  <- setdiff(comunas_datos, comunas_radios)
sin_match_radios <- setdiff(comunas_radios, comunas_datos)

if (length(sin_match_datos) > 0) {
  cat("   ⚠ comuna_norm SIN correspondencia en nom_depto:\n"); print(sin_match_datos)
}
if (length(sin_match_radios) > 0) {
  cat("   ⚠ nom_depto que no aparecen en los datos:\n"); print(sin_match_radios)
}

n_sin_comuna <- sum(is.na(data_revisada$comuna_num))
if (n_sin_comuna > 0) {
  warning(sprintf("%d individuos sin comuna identificable.", n_sin_comuna))
}

# I.4 Asegurar id_individuo -------------------------------------------------

if (!"id_individuo" %in% names(data_revisada)) {
  data_revisada <- data_revisada |>
    mutate(id_individuo = row_number(), .before = 1)
}


# =============================================================================
# PARTE II — SIMULACIÓN MONTE CARLO
# =============================================================================

# II.1 Pool de radios por comuna -------------------------------------------

cat("── II.1 Armando pool de radios por comuna...\n")

pool_por_comuna <- base_radios1 |>
  filter(!is.na(tiempo_transp_min_cesac)) |>
  group_by(nom_depto) |>
  summarise(
    n_radios_pool = n(),
    radios_pool   = list(codigo_redatam),
    pesos         = list(replace_na(total_viv, 0)),
    .groups = "drop"
  )

cat("   Radios por comuna:\n"); print(pool_por_comuna |> select(nom_depto, n_radios_pool))

# II.2 Simulación: N_SIM sorteos por individuo -----------------------------

cat(sprintf("── II.2 Sorteando %d radios por individuo (ponderado: %s)...\n",
            N_SIM, PONDERAR_POR_POBLACION))

individuos_pool <- data_revisada |>
  select(id_individuo, comuna_norm) |>
  left_join(pool_por_comuna, by = c("comuna_norm" = "nom_depto"))

n_sin_pool <- sum(is.na(individuos_pool$n_radios_pool))
if (n_sin_pool > 0) {
  warning(sprintf("%d individuos con comuna sin radios válidos.", n_sin_pool))
}

simulacion_larga <- individuos_pool |>
  mutate(sim = map2(radios_pool, pesos, ~ sortear_radios(.x, .y, N_SIM, PONDERAR_POR_POBLACION))) |>
  select(id_individuo, comuna_norm, sim) |>
  unnest_longer(sim, values_to = "id_radio_sim", indices_to = "iteracion")

cat(sprintf("   simulacion_larga: %d filas (%d individuos × %d iteraciones)\n",
            nrow(simulacion_larga), n_distinct(simulacion_larga$id_individuo), N_SIM))

# II.3 Joins con variables de individuo y radio ----------------------------

cat("── II.3 Pegando variables de individuo y de radio...\n")

data_revisada_covariables <- data_revisada |>
  select(-comuna_num, -comuna_norm)

base_radios1_para_join <- base_radios1 |>
  select(-nom_depto)

simulacion_completa <- simulacion_larga |>
  left_join(data_revisada_covariables, by = "id_individuo") |>
  left_join(base_radios1_para_join,     by = c("id_radio_sim" = "codigo_redatam"))

cat(sprintf("   simulacion_completa: %d filas × %d columnas\n",
            nrow(simulacion_completa), ncol(simulacion_completa)))


# =============================================================================
# II.3.5 — NORMALIZACIÓN DE ASENTAMIENTO ANTES DEL JOIN CON analisis_barrios
# =============================================================================
# Dos pasos:
#   (a) Lookup manual: variantes de escritura → nombre canónico de
#       analisis_barrios$nombre_barrio.
#   (b) Lista negra: strings que NO son villa (barrios formales de CABA,
#       GBA, genéricos) → NA. Con la lógica nueva, estos pasan a no-villa
#       y usan su radio.
# =============================================================================

cat("── II.3.5 Normalizando asentamiento...\n")

lookup_asentamiento <- tribble(
  ~asentamiento_raw,                                    ~nombre_barrio_canonico,
  
  # Villa 20
  "VILLA 20",                                           "Barrio 20",
  "Villa 20",                                           "Barrio 20",
  "villa 20",                                           "Barrio 20",
  "villa20",                                            "Barrio 20",
  "CMV ex-Villa 20",                                    "Barrio 20",
  
  # Villa 21-24
  "21-24 ZABALETA",                                     "Barrio 21-24",
  "21.24",                                              "Barrio 21-24",
  "21/24",                                              "Barrio 21-24",
  "VILLA 21-24",                                        "Barrio 21-24",
  "Villa 21-24",                                        "Barrio 21-24",
  
  # Villa 31 y 31 bis → Barrio Padre Mugica
  "BARRIO 31",                                          "Barrio Padre Mugica",
  "Barrio Padre Carlos Mugica (Villa 31)",              "Barrio Padre Mugica",
  "Barrio Padre Carlos Mugica (Villa 31 bis)",          "Barrio Padre Mugica",
  "PADRE MUJICA",                                       "Barrio Padre Mugica",
  "VILLA 31 BIS BARRIO FERROVIARIO",                    "Barrio Padre Mugica",
  "Villa 31",                                           "Barrio Padre Mugica",
  "Villa 31 bis",                                       "Barrio Padre Mugica",
  "villa 31 bis",                                       "Barrio Padre Mugica",
  
  # Villa 3 - Fátima
  "BARRIO FATIMA",                                      "Barrio Fatima (Villa 3)",
  "Barrio Fatima",                                      "Barrio Fatima (Villa 3)",
  "Barrio Fátima (Villa 3)",                            "Barrio Fatima (Villa 3)",
  "Villa 3 - Barrio Fátima",                            "Barrio Fatima (Villa 3)",
  "Villa 3 - Bo. Fátima",                               "Barrio Fatima (Villa 3)",
  "barrio fatima",                                      "Barrio Fatima (Villa 3)",
  
  # Villa 1-11-14 y sub-áreas
  "Villa 1-11-14 - Barrio Padre Ricciardelli",          "Barrio Padre Ricciardelli (ex 1-11-14)",
  "Villa 1-11-14 - Bo. Padre Ricciardelli",             "Barrio Padre Ricciardelli (ex 1-11-14)",
  "Polideportivo ex Villa 1-11-14",                     "Polideportivo (ex 1-11-14)",
  "Sector Bonorino ex Villa 1-11-14",                   "Sector Bonorino (ex 1-11-14)",
  "Barrio Illia (ex 1-11-14)",                          "Barrio Illia",
  "Bo. Illia",                                          "Barrio Illia",
  "Barrio Rivadavia I (ex 1-11-14)",                    "Barrio Rivadavia I",
  "Bo. Rivadavia I",                                    "Barrio Rivadavia I",
  "Bo. Rivadavia II",                                   "Barrio Rivadavia II",
  
  # Villa 6 - Cildáñez (ojo: canónico tiene DOBLE espacio antes del 6)
  "Barrio Cildáñez (Villa 6)",                          "Barrio Cildañez (Villa  6)",
  "Villa 6 - Barrio Cildáñez",                          "Barrio Cildañez (Villa  6)",
  
  # Villa 15 - Ciudad Oculta
  "Villa 15 - Ciudad Oculta",                           "Barrio 15 (Ciudad Oculta)",
  
  # Villa 19 → INTA
  "BARRIO INTA",                                        "Barrio INTA (ex Villa 19)",
  
  # Varios (con acentos / mayúsculas / abreviaciones)
  "BERMEJO",                                            "Asentamiento Bermejo",
  "Asentamiento María Auxiliadora",                     "Asentamiento Maria Auxiliadora",
  "Barrio Playón de Chacarita",                         "Barrio Playon Chacarita",
  "Barrio Ramón Carrillo",                              "Barrio Ramon Carrillo",
  "Barrio Ramón Carrillo 1",                            "Barrio Ramon Carrillo",
  "Bo. Ramón Carrillo 1",                               "Barrio Ramon Carrillo",
  "ramon carrillo",                                     "Barrio Ramon Carrillo",
  "Bo. Los Perales (ex Dorrego)",                       "Barrio Los Perales (ex Dorrego)",
  "Bo. Soldati",                                        "Barrio Soldati",
  "SOLDATI",                                            "Barrio Soldati",
  "scarpino",                                           "Asentamiento Scapino",
  "Asentamiento Fraga",                                 "Urbanizacion Barrio de Playon Chacarita (ex Fraga)",
  "Urbanización Barrio de Playón Chacarita (ex Fraga)", "Urbanizacion Barrio de Playon Chacarita (ex Fraga)",
  "los piletones",                                      "Villa Piletones"
)

# Strings que NO son villa/asentamiento (pasan a NA → no-villa)
asentamientos_no_villa <- c(
  "VILLA LUGANO", "villa lugano", "v lugano", "LUGANO",   # barrio formal CABA
  "barracas",                                              # barrio formal CABA
  "VILLA CELINA",                                          # partido La Matanza (GBA)
  "LOMA ALEGRE",                                           # no identificable
  "barrio guemes",                                         # no identificable
  "darsena f puerto nuevo",                                # dársena portuaria
  "barra", "villa",                                        # genéricos
  "Barrio Los Pinos"                                       # ambiguo: 2 canónicos posibles
)

# Aplicar normalización -----------------------------------------------------

n_asent_antes <- sum(!is.na(simulacion_completa$asentamiento))

simulacion_completa <- simulacion_completa |>
  mutate(
    # (b) primero, los "no villa" pasan a NA
    asentamiento = if_else(asentamiento %in% asentamientos_no_villa,
                           NA_character_, asentamiento)
  ) |>
  # (a) después, reemplazar variantes por canónicos
  left_join(lookup_asentamiento, by = c("asentamiento" = "asentamiento_raw")) |>
  mutate(asentamiento = coalesce(nombre_barrio_canonico, asentamiento)) |>
  select(-nombre_barrio_canonico)

n_asent_despues <- sum(!is.na(simulacion_completa$asentamiento))

cat(sprintf("   Con asentamiento antes:   %d\n", n_asent_antes))
cat(sprintf("   Con asentamiento después: %d (pasados a NA: %d)\n",
            n_asent_despues, n_asent_antes - n_asent_despues))


# II.4 Join con analisis_barrios (barrios populares) ----------------------

cat("── II.4 Uniendo con analisis_barrios...\n")

base_unida <- simulacion_completa |>
  left_join(
    select(analisis_barrios,
           nombre_barrio, id_barrio, tipo_barrio,
           area_km2_opA, area_km2_opB,
           dist_eucl_km_cesac, dist_eucl_km_hosp,
           dist_red_km_cesac,  dist_red_km_hosp,
           tiempo_transp_min_cesac, tiempo_transp_min_hosp),
    by = c("asentamiento" = "nombre_barrio")
  )

diagnosticar_matches_pendientes(base_unida)


# =============================================================================
# PARTE III — CONSTRUCCIÓN DE VARIABLES DE ANÁLISIS
# =============================================================================

# III.1 Chequeo de estructura ----------------------------------------------

cols_esperadas <- c("tiempo_transp_min_cesac.x", "tiempo_transp_min_cesac.y",
                    "id_barrio", "pct_area_villa", "comuna_norm", "iteracion")
faltantes <- setdiff(cols_esperadas, names(base_unida))
if (length(faltantes) > 0) {
  stop("Faltan columnas esperadas en base_unida: ", paste(faltantes, collapse = ", "))
}

if (!exists("indicadores_por_barrio")) {
  stop("No se encontró 'indicadores_por_barrio' — es de 05_radios_en_barrios_populares.R.")
}

# III.2 Renombrar columnas .x (radio) / .y (barrio) -----------------------

base_unida <- base_unida |>
  rename(
    tiempo_transp_min_cesac_radio  = tiempo_transp_min_cesac.x,
    tiempo_transp_min_cesac_barrio = tiempo_transp_min_cesac.y
  ) |>
  renombrar_si_existe("tiempo_transp_min_hosp.x", "tiempo_transp_min_hosp.y",
                      "tiempo_transp_min_hosp_radio", "tiempo_transp_min_hosp_barrio") |>
  renombrar_si_existe("dist_eucl_km_cesac.x", "dist_eucl_km_cesac.y",
                      "dist_eucl_km_cesac_radio", "dist_eucl_km_cesac_barrio") |>
  renombrar_si_existe("dist_eucl_km_hosp.x", "dist_eucl_km_hosp.y",
                      "dist_eucl_km_hosp_radio", "dist_eucl_km_hosp_barrio") |>
  renombrar_si_existe("dist_red_km_cesac.x", "dist_red_km_cesac.y",
                      "dist_red_km_cesac_radio", "dist_red_km_cesac_barrio") |>
  renombrar_si_existe("dist_red_km_hosp.x", "dist_red_km_hosp.y",
                      "dist_red_km_hosp_radio", "dist_red_km_hosp_barrio")

# III.3 NBI a nivel de barrio popular -------------------------------------

vars_nbi        <- c("hacinamiento", "clima_educ_bajo", "calidad_mat", "NBI")
vars_opcionales <- c("calidad_mat")

cols_pct_barrio_todas <- paste0("pct_", vars_nbi)
faltantes_barrio      <- setdiff(cols_pct_barrio_todas, names(indicadores_por_barrio))
if (length(faltantes_barrio) > 0) {
  cat(sprintf("   ⚠ indicadores_por_barrio no tiene: %s\n",
              paste(faltantes_barrio, collapse = ", ")))
}
cols_pct_barrio_disp <- intersect(cols_pct_barrio_todas, names(indicadores_por_barrio))

nbi_barrio <- indicadores_por_barrio |>
  select(id_barrio, all_of(cols_pct_barrio_disp)) |>
  rename_with(~ paste0(., "_barrio"), all_of(cols_pct_barrio_disp))

base_unida <- base_unida |>
  left_join(nbi_barrio, by = "id_barrio")

# III.4 NBI a nivel de radio ----------------------------------------------

cat("── III.4 Resolviendo columnas NBI a nivel de radio:\n")
for (v in vars_nbi) {
  resultado <- resolver_pct_radio(base_unida, v)
  col_out   <- paste0("pct_", v, "_radio")
  
  if (is.null(resultado)) {
    msg <- sprintf("   '%s': no se encontró columna en base_unida.", v)
    if (v %in% vars_opcionales) {
      cat(msg, "— se omite.\n"); base_unida[[col_out]] <- NA_real_; next
    } else {
      stop(paste0(msg, " Revisar names(base_unida)."))
    }
  }
  
  if (resultado$fuente == "pct") {
    cat(sprintf("   '%s': usando '%s' directamente.\n", v, resultado$columna))
    base_unida[[col_out]] <- base_unida[[resultado$columna]]
  } else {
    cat(sprintf("   '%s': calculando desde '%s' / total_viv * 100.\n", v, resultado$columna))
    base_unida[[col_out]] <- if_else(
      base_unida$total_viv > 0,
      base_unida[[resultado$columna]] / base_unida$total_viv * 100,
      NA_real_
    )
  }
}

# III.5 Chequear comuna_ok -------------------------------------------------

if (!"comuna_ok" %in% names(base_unida)) {
  stop("comuna_ok no está en base_unida — verificar que exista en data_revisada.")
}

# =========================================================================
# III.6 VARIABLES UNIFICADAS — NUEVA LÓGICA
# =========================================================================
#
# LÓGICA v4 (cambio respecto de v3):
#
#   villa:
#     - Si tiene `asentamiento` (después de normalizar) → 1
#     - Si no tiene `asentamiento`                       → 0
#     Nota: pct_area_villa NO se usa para decidir. Un radio que solapa
#     parcialmente con barrio popular pero cuyo individuo NO reportó
#     asentamiento se trata como no-villa según su comuna.
#
#   tiempo_transporte_cesac_t:
#     - Villa con id_barrio matcheado → tiempo del barrio
#     - No villa                        → tiempo del radio sorteado
#     - Villa SIN id_barrio             → NA (nombre no matcheable ni con
#                                              lookup — casos residuales)
#
#   comuna_regresion:
#     - Villa → "BP"
#     - No villa: comuna_ok, con fallback a comuna cruda (recupera 774
#       casos donde comuna_ok era NA pero comuna existía)
# =========================================================================

base_unida <- base_unida |>
  mutate(
    villa = case_when(
      !is.na(asentamiento) ~ 1,
      is.na(asentamiento)  ~ 0,
      TRUE                 ~ NA_real_   # no debería ocurrir
    ),
    
    tiempo_transporte_cesac_t = case_when(
      !is.na(asentamiento) & !is.na(id_barrio) ~ tiempo_transp_min_cesac_barrio,
      is.na(asentamiento)                       ~ tiempo_transp_min_cesac_radio,
      TRUE                                       ~ NA_real_   # villa sin id_barrio
    ),
    
    comuna_regresion = case_when(
      !is.na(asentamiento)  ~ "BP",
      !is.na(comuna_ok)     ~ comuna_ok,
      # Fallback: recupera 774 casos con comuna cruda pero sin comuna_ok
      !is.na(comuna)        ~ paste0("Comuna ",
                                     as.integer(str_extract(as.character(comuna), "\\d+"))),
      TRUE                  ~ NA_character_
    )
  )

# NBI unificados: misma lógica que tiempo (barrio para villa, radio para no-villa)
for (v in vars_nbi) {
  col_radio  <- paste0("pct_", v, "_radio")
  col_barrio <- paste0("pct_", v, "_barrio")
  col_t      <- paste0("pct_", v, "_t")
  
  if (!(col_barrio %in% names(base_unida))) {
    cat(sprintf("   ⚠ '%s' sin dato de barrio — %s queda NA para villa.\n", v, col_t))
    base_unida[[col_barrio]] <- NA_real_
  }
  
  base_unida[[col_t]] <- case_when(
    !is.na(base_unida$asentamiento) & !is.na(base_unida$id_barrio) ~ base_unida[[col_barrio]],
    is.na(base_unida$asentamiento)                                  ~ base_unida[[col_radio]],
    TRUE                                                             ~ NA_real_
  )
}

# III.7 Resumen de exclusiones ---------------------------------------------

n_total    <- nrow(base_unida)
n_villa    <- sum(base_unida$villa == 1, na.rm = TRUE)
n_no_villa <- sum(base_unida$villa == 0, na.rm = TRUE)
n_sin_tpo  <- sum(is.na(base_unida$tiempo_transporte_cesac_t))
n_sin_com  <- sum(is.na(base_unida$comuna_regresion))

cat(sprintf("\n   Filas totales:                     %d\n", n_total))
cat(sprintf("   Villa (=1, con asentamiento):        %d\n", n_villa))
cat(sprintf("   No villa (=0, sin asentamiento):     %d\n", n_no_villa))
cat(sprintf("   Sin tiempo_transporte_cesac_t:       %d (villa con id_barrio sin match)\n", n_sin_tpo))
cat(sprintf("   Sin comuna_regresion:                 %d\n", n_sin_com))


# =============================================================================
# PARTE IV — PREPARACIÓN PARA REGRESIONES
# =============================================================================

# IV.1 Imputación puntual: individuo 2698 ----------------------------------
# Con la lógica nueva, villa ya no queda en NA (case_when cubre ambos casos),
# así que solo hay que imputar genero.

base_unida <- base_unida |>
  mutate(
    genero = if_else(id_individuo == 2698 & is.na(genero), "Masculino", genero)
  )

# IV.2 Split en lista por iteración ----------------------------------------

datos_por_iteracion <- split(base_unida, base_unida$iteracion)

cat(sprintf("\n   Iteraciones: %d | Filas por iteración: %d\n",
            length(datos_por_iteracion), nrow(datos_por_iteracion[[1]])))

# IV.3 Diagnóstico de missingness ------------------------------------------

vars_clave <- c(
  "id_individuo", "iteracion",
  "dias_ult_cons", "villa", "edad", "genero",
  "comuna_regresion", "comuna_ok",
  "tiempo_transporte_cesac_t", "pct_hacinamiento_t", "pct_NBI_t"
)

cat("\n   NAs por variable (iteración 1):\n")
diagnosticar_missingness(datos_por_iteracion[[1]], vars_clave) |>
  pivot_longer(starts_with("na_"), names_to = "variable", values_to = "n_na") |>
  mutate(variable = str_remove(variable, "^na_")) |>
  arrange(desc(n_na)) |>
  print()


# =============================================================================
# PARTE V — REGRESIONES
# =============================================================================
#
# ⚠ reg7 usa log(tiempo_transporte_cesac_t) en la fórmula pero pide
# "tiempo_transporte_cesac_t" (sin log) en coefs. Esos coeficientes van a
# salir vacíos. Homologar cuando decidas.

especificaciones <- list(
  
  reg1 = list(
    label            = "Reg 1: Villa (sin controles)",
    formula          = "log(dias_ult_cons) ~ factor(villa)",
    coefs            = c("factor(villa)1"),
    extra_patterns   = character(0),
    tiene_fe_comuna  = FALSE
  ),
  
  reg2 = list(
    label            = "Reg 2: + sociodemográfico",
    formula          = "log(dias_ult_cons) ~ factor(villa) + poly(edad, 2, raw = TRUE) + factor(genero)",
    coefs            = c("factor(villa)1"),
    extra_patterns   = c("^poly\\(edad", "^factor\\(genero\\)"),
    tiene_fe_comuna  = FALSE
  ),
  
  reg3 = list(
    label            = "Reg 3: + FE comuna",
    formula          = "log(dias_ult_cons) ~ factor(villa) + poly(edad, 2, raw = TRUE) + factor(genero) + factor(comuna_regresion)",
    coefs            = c("factor(villa)1"),
    extra_patterns   = c("^poly\\(edad", "^factor\\(genero\\)"),
    tiene_fe_comuna  = TRUE
  ),
  
  reg4 = list(
    label            = "Reg 4: + tiempo de transporte",
    formula          = "log(dias_ult_cons) ~ log(tiempo_transporte_cesac_t) + factor(villa) + poly(edad, 2, raw = TRUE) + factor(genero) + factor(comuna_regresion)",
    coefs            = c("factor(villa)1", "log(tiempo_transporte_cesac_t)"),
    extra_patterns   = c("^poly\\(edad", "^factor\\(genero\\)"),
    tiene_fe_comuna  = TRUE
  ),
  
  reg5 = list(
    label            = "Reg 5: + hacinamiento y NBI",
    formula          = "log(dias_ult_cons) ~ tiempo_transporte_cesac_t + factor(villa) + poly(edad, 2, raw = TRUE) + factor(genero) + factor(comuna_regresion) + pct_hacinamiento_t + pct_NBI_t",
    coefs            = c("factor(villa)1", "pct_hacinamiento_t", "pct_NBI_t"),
    extra_patterns   = c("^poly\\(edad", "^factor\\(genero\\)"),
    tiene_fe_comuna  = TRUE
  ),
  
  reg6 = list(
    label            = "Reg 6: interacción tiempo (log) × villa",
    formula          = "log(dias_ult_cons) ~ log(tiempo_transporte_cesac_t) * factor(villa) + poly(edad, 2, raw = TRUE) + factor(genero) + factor(comuna_regresion)",
    coefs            = c(
      "log(tiempo_transporte_cesac_t):factor(villa)1",
      "factor(villa)1",
      "log(tiempo_transporte_cesac_t)"
    ),
    extra_patterns   = c("^poly\\(edad", "^factor\\(genero\\)"),
    tiene_fe_comuna  = TRUE
  ),
  
  reg7 = list(
    label            = "Reg 7: interacción tiempo × villa + NBI",
    formula          = "log(dias_ult_cons) ~ log(tiempo_transporte_cesac_t) * factor(villa) + poly(edad, 2, raw = TRUE) + factor(genero) + factor(comuna_regresion) + pct_hacinamiento_t + pct_NBI_t",
    coefs            = c(
      "tiempo_transporte_cesac_t:factor(villa)1",
      "factor(villa)1",
      "tiempo_transporte_cesac_t",
      "pct_hacinamiento_t",
      "pct_NBI_t"
    ),
    extra_patterns   = c("^poly\\(edad", "^factor\\(genero\\)"),
    tiene_fe_comuna  = TRUE
  )
)

# V.1 EE clásicos -----------------------------------------------------------

cat("── V.1 Corriendo regresiones con EE clásicos...\n")
resultados_todas <- ejecutar_todas(especificaciones, datos_por_iteracion, vcov_type = "iid")
resumen_mc       <- calcular_resumen(resultados_todas)
modelo_stats     <- calcular_modelo_stats(resultados_todas)

cat("\n   N por especificación (EE clásicos):\n"); print(modelo_stats)

# V.2 EE clusterizados por comuna_regresion --------------------------------

cat("── V.2 Corriendo regresiones con EE clusterizados por comuna_regresion...\n")
resultados_todas_cluster <- ejecutar_todas(especificaciones, datos_por_iteracion, vcov_type = "comuna_regresion")
resumen_mc_cluster       <- calcular_resumen(resultados_todas_cluster)
modelo_stats_cluster     <- calcular_modelo_stats(resultados_todas_cluster)

# V.3 FE alternativo con comuna_ok -----------------------------------------

cat("── V.3 Corriendo regresiones con FE = comuna_ok...\n")

especificaciones_comuna_ok <- map(especificaciones, function(spec) {
  spec$formula <- str_replace(spec$formula, "factor\\(comuna_regresion\\)", "factor(comuna_ok)")
  spec
})

resultados_todas_comuna_ok <- ejecutar_todas(especificaciones_comuna_ok, datos_por_iteracion, vcov_type = "iid")
resumen_mc_comuna_ok       <- calcular_resumen(resultados_todas_comuna_ok)
modelo_stats_comuna_ok     <- calcular_modelo_stats(resultados_todas_comuna_ok)


# =============================================================================
# PARTE VI — TABLAS Y EXPORTACIÓN
# =============================================================================

ft_estandar <- armar_tabla_flextable(
  resumen_mc, modelo_stats, especificaciones,
  nota_pie = paste(
    "Nota: coeficiente promedio entre iteraciones; EE promedio del modelo entre iteraciones entre par\u00e9ntesis.",
    "* p<0.10, ** p<0.05, *** p<0.01 (t = coeficiente / EE).",
    "Errores est\u00e1ndar cl\u00e1sicos. FE de comuna: comuna_regresion."
  )
)

ft_cluster <- armar_tabla_flextable(
  resumen_mc_cluster, modelo_stats_cluster, especificaciones,
  nota_pie = paste(
    "Nota: coeficiente promedio entre iteraciones; EE promedio del modelo entre iteraciones entre par\u00e9ntesis.",
    "* p<0.10, ** p<0.05, *** p<0.01 (t = coeficiente / EE).",
    "Errores est\u00e1ndar clusterizados por comuna_regresion. FE de comuna: comuna_regresion."
  )
)

ft_comuna_ok <- armar_tabla_flextable(
  resumen_mc_comuna_ok, modelo_stats_comuna_ok, especificaciones_comuna_ok,
  nota_pie = paste(
    "Nota: coeficiente promedio entre iteraciones; EE promedio del modelo entre iteraciones entre par\u00e9ntesis.",
    "* p<0.10, ** p<0.05, *** p<0.01 (t = coeficiente / EE).",
    "Errores est\u00e1ndar cl\u00e1sicos. FE de comuna: comuna_ok."
  )
)

ft_estandar
ft_cluster
ft_comuna_ok

save_as_docx(ft_estandar,  path = "tabla_regresiones_villa_EEclasicos.docx")
save_as_docx(ft_cluster,   path = "tabla_regresiones_villa_EEcluster.docx")
save_as_docx(ft_comuna_ok, path = "tabla_regresiones_villa_FEcomunaOK.docx")


# =============================================================================
# PARTE VII — GRÁFICOS DE DISTRIBUCIÓN DE COEFICIENTES
# =============================================================================

cat("── VII. Armando gráficos...\n")

p_villa_reg3  <- plot_distribucion_coef(resultados_todas, "reg3", "factor(villa)1",
                                        "Villa (Reg 3: + FE comuna)", color = "#D7263D")
p_tiempo_reg4 <- plot_distribucion_coef(resultados_todas, "reg4", "log(tiempo_transporte_cesac_t)",
                                        "log(Tiempo al CeSAC) (Reg 4)", color = "#E09F3E")
p_villa_reg5  <- plot_distribucion_coef(resultados_todas, "reg5", "factor(villa)1",
                                        "Villa (Reg 5: modelo más rico sin interacción)", color = "#D7263D")
p_nbi_reg5    <- plot_distribucion_coef(resultados_todas, "reg5", "pct_NBI_t",
                                        "NBI % (Reg 5)", color = "#5C4D7D")
p_hac_reg5    <- plot_distribucion_coef(resultados_todas, "reg5", "pct_hacinamiento_t",
                                        "Hacinamiento % (Reg 5)", color = "#5C4D7D")
p_inter_reg6  <- plot_distribucion_coef(resultados_todas, "reg6",
                                        "log(tiempo_transporte_cesac_t):factor(villa)1",
                                        "log(Tiempo) × Villa (Reg 6)", color = "#1B998B")

panel_coeficientes <- (p_villa_reg3 + p_tiempo_reg4 + p_villa_reg5) /
  (p_nbi_reg5   + p_hac_reg5   + p_inter_reg6) +
  plot_annotation(
    title = "Distribución de coeficientes — 1000 simulaciones Monte Carlo",
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
  )

panel_coeficientes


# =============================================================================
# NOTAS METODOLÓGICAS
# =============================================================================
#
# CAMBIO CENTRAL v4: `asentamiento` (reportado por el individuo, normalizado)
# es lo que define villa=1, no el solapamiento geográfico. Es una decisión de
# medición: creemos más al dato de campo que al cruce geográfico cuando ambos
# discrepan.
#
# Consecuencias:
#   - Los ~886 casos con solapamiento parcial de radio pero SIN asentamiento
#     ahora se cuentan como no-villa (antes se excluían).
#   - Los ~800 casos con asentamiento no matcheado se recuperan vía lookup.
#   - Los casos residuales sin match ni asentamiento canónico quedan con
#     tiempo_transporte_cesac_t = NA (villa=1 pero sin barrio identificado),
#     y se pierden en reg4-7.
#
# ALTERNATIVA CONSERVADORA: si preferís excluir los casos con solapamiento
# parcial no reportado por el individuo (por ser "geográficamente ambiguos"),
# reemplazar en III.6 el case_when de villa por:
#   villa = case_when(
#     !is.na(asentamiento)                            ~ 1,
#     is.na(asentamiento) & pct_area_villa == 0        ~ 0,
#     TRUE                                              ~ NA_real_
#   )
#
# ⚠ SEMÁNTICA de "villa" v4: mide "auto-reporte de vivir en villa/BP", no
# necesariamente "vive en villa según cartografía". Documentar esto en el
# paper — es diferente al v3 anterior.
# =============================================================================