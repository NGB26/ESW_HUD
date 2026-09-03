# =============================================================================
# SIMULACIÓN MONTE CARLO + REGRESIONES: acceso a CeSAC por villa
# =============================================================================
#
# Pipeline completo (script 09), reorganizado:
#
#   PARTE I    Preparación de individuos (data_revisada)
#   PARTE II   Simulación Monte Carlo: sortear radio por individuo
#   PARTE III  Construcción de variables de análisis (villa, tiempo,
#              NBI, comuna_regresion)
#   PARTE IV   Preparación para regresiones (imputación puntual, split)
#   PARTE V    Ejecución de 7 especificaciones con 3 vcov distintos
#   PARTE VI   Tablas estilo paper (.docx)
#   PARTE VII  Gráficos de distribución de coeficientes
#
# Todas las funciones auxiliares están al comienzo (bloque A), así se pueden
# leer las partes sustantivas de arriba abajo sin ir y volver.
#
# Requiere en el entorno:
#   - base_radios1          : nom_depto, codigo_redatam, tiempo_transp_min_cesac,
#                              total_viv y variables NBI (pct_ o n_ + total_viv)
#   - analisis_barrios       : nombre_barrio, id_barrio, tipo_barrio,
#                               tiempo_transp_min_cesac, etc.
#   - indicadores_por_barrio : id_barrio + pct_<var> a nivel de barrio popular
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

set.seed(1234)   # reproducibilidad — documentar esta semilla si esto va al paper

# Constantes globales -------------------------------------------------------

N_SIM                  <- 1000
PONDERAR_POR_POBLACION <- TRUE   # TRUE: pondera por total_viv | FALSE: uniforme

RUTA_DATOS <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/ESW version 2/data/processed"

# Silenciar "NOTE: N observations removed..." que feols imprime por ajuste
setFixest_notes(FALSE)


# =============================================================================
# A) FUNCIONES AUXILIARES
# =============================================================================

# A.1 Sorteo de radios ponderado ---------------------------------------------

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

# A.2 Renombrado defensivo de columnas .x/.y ---------------------------------

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

# A.5 Chequeo previo de fórmulas ---------------------------------------------

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

# A.6 Ajustar una spec sobre una base ----------------------------------------
#     vcov_type = "iid"              -> EE clásicos
#     vcov_type = "<nombre_var>"      -> EE clusterizados por esa variable

ajustar_spec <- function(data, spec, vcov_type = "iid") {
  
  vcov_arg <- if (identical(vcov_type, "iid")) {
    "iid"
  } else {
    as.formula(paste0("~", vcov_type))
  }
  
  fit <- feols(as.formula(spec$formula), data = data, vcov = vcov_arg)
  
  # & en R no hace short-circuit: si `patrones` es NA, str_detect igual se
  # evalúa y tira error. Este helper chequea el largo antes de llamar.
  coincide_patron <- function(term, patrones) {
    if (length(patrones) == 0) {
      rep(FALSE, length(term))
    } else {
      str_detect(term, paste(patrones, collapse = "|"))
    }
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

# A.7 Correr todas las specs × todas las iteraciones ------------------------

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

# A.8 Resúmenes: coeficientes y estadísticas de modelo ----------------------

# Regla de combinación de Rubin (1987) para inferencia bajo imputación
# múltiple, aplicada al Monte Carlo geográfico (cada iteración = una
# "imputación" del radio de residencia). Detalle:
#
#   beta_bar = (1/m) Σ beta_k                         (estimador combinado)
#   Ubar     = (1/m) Σ SE_k^2                          (varianza WITHIN)
#   B        = (1/(m-1)) Σ (beta_k - beta_bar)^2       (varianza BETWEEN)
#   T        = Ubar + (1 + 1/m) B                      (varianza TOTAL de Rubin)
#   SE_Rubin = sqrt(T)
#   lambda   = (1 + 1/m) B / T                         (fracción info faltante)
#   r        = (1 + 1/m) B / Ubar                      (aumento relativo de var.)
#
# Grados de libertad:
#   nu_old = (m-1) (1 + 1/r)^2                          Rubin (1987)
#   nu_obs = (nu_com+1)/(nu_com+3) * nu_com * (1-lambda)   Barnard & Rubin (1999)
#   nu_adj = 1 / (1/nu_old + 1/nu_obs)                  df ajustados (usados aquí)
#
# nu_com = df del análisis completo por iteración. Para EE clásicos ≈ n - p.
# Para EE clusterizados, los df relevantes son ~ (nº clusters - 1); como el
# nº de parámetros p y de clusters no viaja en `resultados`, se aproxima
# nu_com con la mediana de nobs menos un p nominal (configurable) — sólo
# afecta la corrección de segundo orden de Barnard-Rubin, no T ni SE_Rubin.
#
# Referencias: Rubin (1987) "Multiple Imputation for Nonresponse in Surveys";
# Barnard & Rubin (1999, Biometrika 86(4)); Schafer (1997) "Analysis of
# Incomplete Multivariate Data". Salvedad: el sorteo ponderado por población
# aproxima —no reproduce exactamente— una imputación bayesiana "proper"
# (Robins & Wang 2000, Biometrika); por eso T se reporta como incorporación
# de la incertidumbre de agregación geográfica, no como el SE "verdadero".

calcular_resumen <- function(resultados, p_nominal = 30) {
  resultados |>
    group_by(regresion_id, term) |>
    summarise(
      n_iter_validas  = sum(!is.na(estimate)),
      m               = sum(!is.na(estimate) & !is.na(std.error)),
      
      # --- Estimador puntual combinado (promedio) ---
      media_coef_raw  = mean(estimate, na.rm = TRUE),
      
      # --- Componentes de varianza de Rubin ---
      U_bar           = mean(std.error^2, na.rm = TRUE),          # within
      B_between       = var(estimate,     na.rm = TRUE),          # between
      nobs_med        = median(nobs,      na.rm = TRUE),
      
      # --- Antiguo se_prom (promedio simple de EE): se conserva para comparar ---
      se_prom_raw     = mean(std.error, na.rm = TRUE),
      
      # --- Dispersión empírica de coeficientes e IC percentil MC ---
      de_coef         = round(sd(estimate, na.rm = TRUE), 4),
      p2_5            = round(quantile(estimate, 0.025, na.rm = TRUE), 4),
      p97_5           = round(quantile(estimate, 0.975, na.rm = TRUE), 4),
      pct_signif_5pct = round(mean(p.value < 0.05, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) |>
    mutate(
      # Varianza total de Rubin y SE corregido
      # B puede ser NA si m<2; se trata como 0 (sin varianza entre imputaciones).
      B_between  = if_else(is.na(B_between), 0, B_between),
      T_total    = U_bar + (1 + 1 / m) * B_between,
      se_rubin_raw = sqrt(T_total),
      
      # Aumento relativo de varianza y fracción de información faltante.
      # Si U_bar = 0 (EE degenerados) r_incr -> Inf; se acota para df finitos.
      r_incr     = if_else(U_bar > 0, (1 + 1 / m) * B_between / U_bar, Inf),
      lambda_fmi = if_else(T_total > 0, (1 + 1 / m) * B_between / T_total, 0),
      
      # Grados de libertad (Rubin 1987 + Barnard-Rubin 1999).
      # Si B=0 (r_incr=0) nu_old -> Inf: sin info faltante, df del análisis completo.
      nu_com     = pmax(nobs_med - p_nominal, 1),
      nu_old     = if_else(r_incr > 0 & is.finite(r_incr),
                           (m - 1) * (1 + 1 / r_incr)^2, Inf),
      nu_obs     = ((nu_com + 1) / (nu_com + 3)) * nu_com * (1 - lambda_fmi),
      nu_adj     = 1 / (1 / nu_old + 1 / nu_obs),
      nu_adj     = if_else(is.finite(nu_adj) & nu_adj > 0, nu_adj, nu_com),
      
      # Estadístico t con SE de Rubin y p-valor con df ajustados
      t_rubin    = if_else(se_rubin_raw > 0, media_coef_raw / se_rubin_raw, NA_real_),
      p_rubin    = 2 * pt(-abs(t_rubin), df = nu_adj),
      
      # IC 95% de Rubin (basado en t con nu_adj)
      ic_low     = media_coef_raw - qt(0.975, df = nu_adj) * se_rubin_raw,
      ic_high    = media_coef_raw + qt(0.975, df = nu_adj) * se_rubin_raw,
      
      # --- Versiones redondeadas para mostrar ---
      media_coef = round(media_coef_raw, 4),
      se_prom    = round(se_prom_raw,    4),   # EE viejo (promedio simple)
      se_rubin   = round(se_rubin_raw,   4),   # EE corregido de Rubin
      fmi        = round(lambda_fmi,     3),
      ic_low_r   = round(ic_low,  4),
      ic_high_r  = round(ic_high, 4)
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

# A.9 Etiquetado de términos y armado de flextable --------------------------

etiquetar_termino <- function(term) {
  case_when(
    term == "factor(villa)1"                                ~ "Villa (dummy)",
    term == "factor(edad_grupo)45-64"                       ~ "Edad 45-64",
    term == "factor(edad_grupo)65+"                         ~ "Edad 65+",
    term == "log(tiempo_transporte_cesac_t)"                ~ "Tiempo transporte (log)",
    term == "tiempo_transporte_cesac_t"                     ~ "Tiempo transporte",
    term == "pct_hacinamiento_t"                            ~ "Hacinamiento (%)",
    term == "pct_NBI_t"                                     ~ "NBI (%)",
    term == "log(tiempo_transporte_cesac_t):factor(villa)1" ~ "Tiempo transporte (log) \u00d7 Villa",
    term == "tiempo_transporte_cesac_t:factor(villa)1"      ~ "Tiempo transporte \u00d7 Villa",
    str_detect(term, "^factor\\(edad_grupo\\)") ~ paste0("Edad ", str_remove(term, "^factor\\(edad_grupo\\)")),
    str_detect(term, "^factor\\(genero\\)")     ~ paste0("G\u00e9nero: ", str_remove(term, "^factor\\(genero\\)")),
    TRUE ~ term
  )
}

# Orden solicitado: villa, genero, edad_grupo, hacinamiento, NBI,
# tiempo_transporte, interacción. Las filas concretas de Género y de
# Edad (según niveles observados) se ubican por prefijo; lo que no esté
# listado se agrega al final automáticamente en armar_tabla_flextable.
orden_variables <- c(
  "Villa (dummy)",
  "G\u00e9nero: Masculino", "G\u00e9nero: Femenino",
  "Edad 45-64", "Edad 65+",
  "Hacinamiento (%)", "NBI (%)",
  "Tiempo transporte (log)", "Tiempo transporte",
  "Tiempo transporte (log) \u00d7 Villa", "Tiempo transporte \u00d7 Villa"
)

armar_tabla_flextable <- function(resumen, modelo_stats, especificaciones, nota_pie,
                                  se_a_usar = "rubin") {
  
  orden_regresiones <- names(especificaciones)
  nombres_columnas  <- map_chr(especificaciones, "label")
  
  # --- coeficientes ---
  # se_a_usar: EE que va entre paréntesis y define las estrellas.
  #   "rubin" (default) -> SE de Rubin (within + between) con p-valor de df
  #                        ajustados (Barnard-Rubin). Recomendado para el paper.
  #   "prom"            -> promedio simple de EE (comportamiento viejo), útil
  #                        para comparar cuánto agranda Rubin los EE.
  tabla_larga <- resumen |>
    mutate(
      se_cell = if (identical(se_a_usar, "rubin")) se_rubin else se_prom,
      # Estrellas por p-valor de Rubin (df ajustados) si está disponible;
      # si se pide el EE viejo, se usan umbrales t normales sobre se_prom.
      p_usar = if (identical(se_a_usar, "rubin")) {
        p_rubin
      } else {
        2 * pnorm(-abs(media_coef_raw / se_prom_raw))
      },
      estrellas = case_when(
        p_usar < 0.01 ~ "***",
        p_usar < 0.05 ~ "**",
        p_usar < 0.10 ~ "*",
        TRUE ~ ""
      ),
      celda = paste0(
        formatC(media_coef, format = "f", digits = 3), estrellas,
        "\n(", formatC(se_cell, format = "f", digits = 3), ")"
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
  
  # --- metadatos: FE comuna, N, R² ajustado ---
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

# A.10 Gráfico de distribución de un coeficiente -----------------------------

plot_distribucion_coef <- function(datos_coef, regresion_id_sel, term_sel, titulo,
                                   color = "#1B998B") {
  datos_plot <- datos_coef |>
    filter(regresion_id == regresion_id_sel, term == term_sel, !is.na(estimate))
  
  if (nrow(datos_plot) == 0) {
    warning(sprintf("Sin datos para '%s' en %s — se omite el plot.",
                    term_sel, regresion_id_sel))
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

# I.2 Normalizar "comuna" al formato de nom_depto en base_radios1 -----------
# Extrae el número desde cualquier formato ("1", "01", "Comuna 1", "COMUNA  01")
# y reconstruye "Comuna N" sin cero a la izquierda. Si nom_depto usa cero a la
# izquierda ("Comuna 01"), reemplazar por sprintf("Comuna %02d", comuna_num).

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
  cat("   ⚠ comuna_norm SIN correspondencia en nom_depto:\n")
  print(sin_match_datos)
  cat("   Revisar formato (¿cero a la izquierda? ¿tildes? ¿comuna fuera de CABA?)\n")
}
if (length(sin_match_radios) > 0) {
  cat("   ⚠ nom_depto que no aparecen en los datos:\n")
  print(sin_match_radios)
}

n_sin_comuna <- sum(is.na(data_revisada$comuna_num))
if (n_sin_comuna > 0) {
  warning(sprintf("%d individuos sin comuna identificable tras la normalización.", n_sin_comuna))
}

# I.4 Asegurar id_individuo -------------------------------------------------

if (!"id_individuo" %in% names(data_revisada)) {
  data_revisada <- data_revisada |>
    mutate(id_individuo = row_number(), .before = 1)
}


# =============================================================================
# PARTE II — SIMULACIÓN MONTE CARLO
# =============================================================================
#
# Motivación: los individuos tienen comuna, pero tiempo_transp_min_cesac
# está a nivel de radio censal. Sorteamos radios dentro de cada comuna
# (ponderado por población) para propagar esa incertidumbre de agregación.
#
# Se sortea el ID del radio, no el tiempo — así podemos recuperar CUALQUIER
# variable del radio por join después (NBI/censo, etc.), no solo el tiempo.
# =============================================================================

# II.1 Pool de radios por comuna (solo radios con tiempo válido) ------------

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

cat("   Radios por comuna:\n")
print(pool_por_comuna |> select(nom_depto, n_radios_pool))

# II.2 Simulación: N_SIM sorteos por individuo ------------------------------

cat(sprintf("── II.2 Sorteando %d radios por individuo (ponderado: %s)...\n",
            N_SIM, PONDERAR_POR_POBLACION))

individuos_pool <- data_revisada |>
  select(id_individuo, comuna_norm) |>
  left_join(pool_por_comuna, by = c("comuna_norm" = "nom_depto"))

n_sin_pool <- sum(is.na(individuos_pool$n_radios_pool))
if (n_sin_pool > 0) {
  warning(sprintf(
    "%d individuos con comuna sin radios con tiempo válido — quedan en NA en las %d simulaciones.",
    n_sin_pool, N_SIM
  ))
}

# Genera n_individuos × N_SIM filas
simulacion_larga <- individuos_pool |>
  mutate(sim = map2(radios_pool, pesos, ~ sortear_radios(.x, .y, N_SIM, PONDERAR_POR_POBLACION))) |>
  select(id_individuo, comuna_norm, sim) |>
  unnest_longer(sim, values_to = "id_radio_sim", indices_to = "iteracion")

cat(sprintf("   simulacion_larga: %d filas (%d individuos × %d iteraciones)\n",
            nrow(simulacion_larga), n_distinct(simulacion_larga$id_individuo), N_SIM))

# II.3 Joins con variables de individuo y radio -----------------------------

cat("── II.3 Pegando variables de individuo y de radio...\n")

data_revisada_covariables <- data_revisada |>
  select(-comuna_num, -comuna_norm)  # generadas en I.2, no duplicar

base_radios1_para_join <- base_radios1 |>
  select(-nom_depto)  # ya tenemos comuna_norm del lado del individuo

simulacion_completa <- simulacion_larga |>
  left_join(data_revisada_covariables, by = "id_individuo") |>
  left_join(base_radios1_para_join,     by = c("id_radio_sim" = "codigo_redatam"))

cat(sprintf("   simulacion_completa: %d filas × %d columnas\n",
            nrow(simulacion_completa), ncol(simulacion_completa)))

n_sin_radio_sim <- sum(is.na(simulacion_completa$id_radio_sim))
cat(sprintf("   Filas sin radio sorteado: %d de %d\n",
            n_sin_radio_sim, nrow(simulacion_completa)))

# II.4 Join con analisis_barrios (barrios populares) ------------------------

cat("── II.4 Uniendo con analisis_barrios (barrios populares)...\n")

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


# =============================================================================
# PARTE III — CONSTRUCCIÓN DE VARIABLES DE ANÁLISIS
# =============================================================================

# III.1 Chequeo de estructura ------------------------------------------------

cols_esperadas <- c("tiempo_transp_min_cesac.x", "tiempo_transp_min_cesac.y",
                    "id_barrio", "pct_area_villa", "comuna_norm", "iteracion")
faltantes <- setdiff(cols_esperadas, names(base_unida))
if (length(faltantes) > 0) {
  stop("Faltan columnas esperadas en base_unida: ", paste(faltantes, collapse = ", "))
}

if (!exists("indicadores_por_barrio")) {
  stop("No se encontró 'indicadores_por_barrio' — es de 05_radios_en_barrios_populares.R.")
}

# III.2 Renombrar columnas duplicadas .x (radio) / .y (barrio) -------------

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

# III.3 NBI a nivel de barrio popular ---------------------------------------

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

# III.4 NBI a nivel de radio (resuelve nombres candidatos) ------------------

cat("── III.4 Resolviendo columnas NBI a nivel de radio:\n")
for (v in vars_nbi) {
  resultado <- resolver_pct_radio(base_unida, v)
  col_out   <- paste0("pct_", v, "_radio")
  
  if (is.null(resultado)) {
    msg <- sprintf("   '%s': no se encontró pct_%s / pct_%s.x / pct_%s_radio / n_%s / n_%s.x.",
                   v, v, v, v, v, v)
    if (v %in% vars_opcionales) {
      cat(msg, "— se omite (opcional).\n")
      base_unida[[col_out]] <- NA_real_
      next
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

# III.5 Chequear comuna_ok ---------------------------------------------------
# comuna_ok viene de data_revisada, así que ya está en base_unida desde el
# join del paso II.3. NOTA: en el script original acá había un left_join
# adicional para re-traer comuna y comuna_ok, seguido de un select() con
# sintaxis mixta positiva/negativa que probablemente fallaba en silencio.
# Al no existir la colisión, no hace falta ni el re-join ni el select.

if (!"comuna_ok" %in% names(base_unida)) {
  stop("comuna_ok no está en base_unida — verificar que exista en data_revisada.")
}
# =============================================================================
# II.3.5 — NORMALIZACIÓN DE ASENTAMIENTO ANTES DEL JOIN CON analisis_barrios
# =============================================================================
# Los strings de `asentamiento` en data_revisada tienen muchas grafías
# distintas para el mismo barrio popular. Sin normalizar, el left_join contra
# analisis_barrios$nombre_barrio falla en ~852 casos (villa=1 pero id_barrio=NA).
#
# Se resuelve en dos pasos:
#   (a) Lookup manual: mapeo explícito de las variantes que aparecen en los
#       datos a los nombres canónicos de analisis_barrios.
#   (b) Casos que NO son villa/asentamiento (Villa Lugano ≠ villa,
#       Barracas ≠ villa, etc.) se setean a NA para que caigan en la rama
#       "no villa" del case_when de III.6.
#
# IMPORTANTE: este bloque debe correrse ANTES del left_join con
# analisis_barrios. Modifica simulacion_completa$asentamiento in-place.
# =============================================================================

# --- (a) Lookup manual de variantes -> canónico -----------------------------

lookup_asentamiento <- tribble(
  ~asentamiento_raw,                                    ~nombre_barrio_canonico,
  
  # Villa 20 (varias grafías)
  "VILLA 20",                                           "Barrio 20",
  "Villa 20",                                           "Barrio 20",
  "villa 20",                                           "Barrio 20",
  "villa20",                                            "Barrio 20",
  "Villa20",                                            "Barrio 20",
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
  
  # Villa 6 - Cildáñez (ojo: canónico tiene doble espacio antes del 6)
  "Barrio Cildáñez (Villa 6)",                          "Barrio Cildañez (Villa  6)",
  "Villa 6 - Barrio Cildáñez",                          "Barrio Cildañez (Villa  6)",
  
  # Villa 15 - Ciudad Oculta
  "Villa 15 - Ciudad Oculta",                           "Barrio 15 (Ciudad Oculta)",
  
  # Villa 19 → INTA
  "BARRIO INTA",                                        "Barrio INTA (ex Villa 19)",
  
  # Barrios / asentamientos varios (con acentos/mayúsculas)
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

# --- (b) Casos que NO son villa/asentamiento (setear a NA) ------------------
# Estos strings aparecen en `asentamiento` pero se refieren a barrios formales
# de CABA (no villas), a partidos del GBA (fuera de CABA), o son textos
# genéricos/no interpretables. Al setear a NA, el individuo se trata como
# no-villa (y el modelo usa su radio sorteado, si tiene comuna).

asentamientos_no_villa <- c(
  "VILLA LUGANO",       # barrio formal de CABA, no villa
  "villa lugano",
  "v lugano",
  "LUGANO",             # ambiguo, probablemente Villa Lugano barrio
  "barracas",           # barrio formal de CABA
  "VILLA CELINA",       # partido de La Matanza, fuera de CABA
  "LOMA ALEGRE",        # no identificable en catálogo
  "barrio guemes",      # no identificable
  "darsena f puerto nuevo",  # dársena portuaria, no barrio
  "barra",              # genérico
  "villa",              # genérico
  "Barrio Los Pinos"    # ambiguo: dos canónicos posibles (26 de junio o Las Palomas)
  # ← si tenés forma de desambiguar cuál es, quitarlo de esta
  # lista y agregarlo al lookup_asentamiento con el canónico.
)

# --- Aplicar la normalización -----------------------------------------------
#
# ROBUSTEZ DEL MATCH: el left_join por string exacto falla silenciosamente ante
# diferencias invisibles (espacios sobrantes, tildes en forma Unicode NFD vs
# NFC, mayúsculas). Por eso el emparejamiento se hace por una CLAVE NORMALIZADA:
# minúsculas, sin acentos (translitera a ASCII), colapsando espacios múltiples
# y recortando extremos. Así "VILLA 20", "Villa 20", " villa  20 " y "villa20"
# (esta última no, ver nota) se comparan de forma consistente.
#
# stringi::stri_trans_general(..., "Latin-ASCII") saca acentos; si stringi no
# estuviera disponible, se cae a un gsub manual de los acentos más comunes.

clave_norm <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- tolower(x)
  # Quitar acentos/diacríticos
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  } else {
    x <- chartr("áéíóúàèìòùäëïöüâêîôûñç", "aeiouaeiouaeiouaeiounc", x)
  }
  # Colapsar espacios internos múltiples a uno
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

n_antes <- sum(!is.na(simulacion_completa$asentamiento))

# Claves normalizadas de los "no villa" y del lookup (lado izquierdo)
claves_no_villa <- clave_norm(asentamientos_no_villa)

lookup_norm <- lookup_asentamiento |>
  mutate(clave = clave_norm(asentamiento_raw)) |>
  # Si dos variantes normalizan a la misma clave, quedarse con la primera
  distinct(clave, .keep_all = TRUE) |>
  select(clave, nombre_barrio_canonico)

simulacion_completa <- simulacion_completa |>
  mutate(clave_asent = clave_norm(asentamiento)) |>
  mutate(
    # (b) los "no villa" (por clave) pasan a NA
    asentamiento = if_else(clave_asent %in% claves_no_villa,
                           NA_character_, asentamiento),
    clave_asent  = if_else(clave_asent %in% claves_no_villa,
                           NA_character_, clave_asent)
  ) |>
  # (a) emparejar por clave normalizada y reemplazar por canónico
  left_join(lookup_norm, by = c("clave_asent" = "clave")) |>
  mutate(
    asentamiento = coalesce(nombre_barrio_canonico, asentamiento)
  ) |>
  select(-nombre_barrio_canonico, -clave_asent)

# --- Chequeo ----------------------------------------------------------------

n_despues <- sum(!is.na(simulacion_completa$asentamiento))

cat(sprintf("── II.3.5 Normalización de asentamiento:\n"))
cat(sprintf("   Con asentamiento antes:  %d\n", n_antes))
cat(sprintf("   Con asentamiento después: %d (diff = %d, pasados a NA)\n",
            n_despues, n_antes - n_despues))

# Ver qué asentamientos SIGUEN sin matchear con el canónico
# (esto se corre después de que armes base_unida en II.4)
diagnosticar_matches_pendientes <- function(base_unida) {
  pendientes <- base_unida |>
    filter(iteracion == 1, !is.na(asentamiento), is.na(id_barrio)) |>
    distinct(asentamiento) |>
    arrange(asentamiento) |>
    pull(asentamiento)
  
  if (length(pendientes) == 0) {
    cat("   ✓ Todos los asentamientos con nombre válido matchearon con id_barrio.\n")
  } else {
    cat(sprintf("   ⚠ %d asentamiento(s) todavía sin match — agregarlos al lookup:\n",
                length(pendientes)))
    print(pendientes)
  }
}

# III.6 Variables unificadas de análisis ------------------------------------
#
# CRITERIO (v3, actualizado): la pertenencia a villa/barrio popular se define
# ÚNICAMENTE por tener un valor válido en `asentamiento`. Se ELIMINA el criterio
# anterior basado en pct_area_villa == 0.
#
#   - Villa (asentamiento no NA) : valor del BARRIO (constante entre las 1000
#                                  iteraciones).
#   - No villa (asentamiento NA) : valor del RADIO sorteado, SIEMPRE (ya no se
#                                  exige solapamiento 0% con barrio popular; el
#                                  solapamiento parcial deja de excluir filas).
#
# Con esto, la única razón para que tiempo/NBI queden NA en no-villa es que el
# RADIO sorteado no tenga el dato (p. ej. radio sin tiempo válido). Y en villa,
# que el `asentamiento` no haya matcheado un id_barrio (ver II.3.5 / lookup).
#
# Comuna para el FE: si es de villa -> "BP"; si no -> comuna_ok, con fallback a
# comuna_norm cuando comuna_ok es NA. Esto recupera los ~774 no-villa que tenían
# comuna_ok = NA pero comuna_norm válida (derivada del campo `comuna` original),
# que antes se perdían al entrar el FE de comuna. comuna_norm y comuna_ok
# codifican la misma comuna geográfica; el fallback no introduce información nueva.

base_unida <- base_unida |>
  mutate(
    comuna_ok_fb     = coalesce(comuna_ok, comuna_norm),
    comuna_regresion = if_else(!is.na(asentamiento), "BP", comuna_ok_fb),
    
    tiempo_transporte_cesac_t = if_else(
      !is.na(asentamiento),
      tiempo_transp_min_cesac_barrio,   # villa -> barrio
      tiempo_transp_min_cesac_radio     # no villa -> radio sorteado (siempre)
    ),
    villa = if_else(!is.na(asentamiento), 1, 0)
  )

for (v in vars_nbi) {
  col_radio  <- paste0("pct_", v, "_radio")
  col_barrio <- paste0("pct_", v, "_barrio")
  col_t      <- paste0("pct_", v, "_t")
  
  if (!(col_barrio %in% names(base_unida))) {
    cat(sprintf("   ⚠ '%s' sin dato a nivel de barrio — %s queda NA para villa.\n", v, col_t))
    base_unida[[col_barrio]] <- NA_real_
  }
  
  # Villa -> valor de barrio; no-villa -> valor del radio sorteado (siempre).
  base_unida[[col_t]] <- if_else(
    !is.na(base_unida$id_barrio),
    base_unida[[col_barrio]],
    base_unida[[col_radio]]
  )
}

# III.7 Resumen de exclusiones y diagnóstico de missing --------------------
# Bajo el criterio nuevo, tiempo/NBI SOLO quedan NA por dos causas:
#   (i)  villa=1 pero id_barrio NA -> asentamiento no matcheó el canónico
#        (se soluciona ampliando lookup_asentamiento / normalización II.3.5).
#   (ii) no-villa cuyo RADIO sorteado no tiene el dato (tiempo/NBI del radio NA).

n_total    <- nrow(base_unida)
n_excluido <- sum(is.na(base_unida$tiempo_transporte_cesac_t))
n_villa    <- sum(base_unida$villa == 1, na.rm = TRUE)
n_no_villa <- sum(base_unida$villa == 0, na.rm = TRUE)

cat(sprintf("\n   Filas totales:                          %d\n", n_total))
cat(sprintf("   Villa (asentamiento válido):            %d\n", n_villa))
cat(sprintf("   No villa (asentamiento NA):             %d\n", n_no_villa))
cat(sprintf("   NA en tiempo_transporte_cesac_t:        %d (%.1f%%)\n",
            n_excluido, 100 * n_excluido / n_total))
cat(sprintf("   Variables NBI unificadas: %s\n", paste0("pct_", vars_nbi, "_t", collapse = ", ")))

# Descomposición del NA de tiempo por causa (iteración 1)
cat("\n   ── Origen de los NA en tiempo (iteración 1) ──\n")
base_unida |>
  filter(iteracion == 1, is.na(tiempo_transporte_cesac_t)) |>
  mutate(
    causa = case_when(
      villa == 1 & is.na(id_barrio) ~ "villa sin match de barrio (ampliar lookup)",
      villa == 1 & !is.na(id_barrio) ~ "villa con barrio pero tiempo_barrio NA",
      villa == 0                     ~ "no-villa: radio sorteado sin tiempo",
      TRUE                           ~ "otro"
    )
  ) |>
  count(causa, sort = TRUE) |>
  print()

# Villa sin match: qué strings de asentamiento faltan en el lookup
villa_sin_match <- base_unida |>
  filter(iteracion == 1, villa == 1, is.na(id_barrio)) |>
  distinct(asentamiento) |>
  arrange(asentamiento) |>
  pull(asentamiento)

if (length(villa_sin_match) > 0) {
  cat(sprintf("\n   ⚠ %d asentamiento(s) villa=1 sin id_barrio (agregar al lookup):\n",
              length(villa_sin_match)))
  print(villa_sin_match)
} else {
  cat("\n   ✓ Todos los villa=1 tienen id_barrio (match completo).\n")
}

if (n_villa == 0) {
  cat("   ⚠ n_villa = 0 — revisar match de 'asentamiento' contra 'nombre_barrio'.\n")
}


# =============================================================================
# PARTE IV — PREPARACIÓN PARA REGRESIONES
# =============================================================================

# IV.0 Grupo etario categórico (reemplaza el efecto cuadrático de edad) -----
# Tres estratos: 18–44, 45–64, 65+. La categoría de referencia en las
# regresiones será "18-44" (primer nivel del factor). Se usa cut() con
# right = FALSE para que los cortes sean [18,45), [45,65), [65, Inf).
# Registro cuántos casos caen fuera de [18, Inf) o con edad NA.

cat("── IV.0 Construyendo edad_grupo (18-44 / 45-64 / 65+)...\n")

base_unida <- base_unida |>
  mutate(
    edad_grupo = cut(
      edad,
      breaks = c(18, 45, 65, Inf),
      labels = c("18-44", "45-64", "65+"),
      right  = FALSE,          # [18,45), [45,65), [65,Inf)
      include.lowest = TRUE
    ),
    edad_grupo = factor(edad_grupo, levels = c("18-44", "45-64", "65+"))
  )

n_edad_na    <- sum(is.na(base_unida$edad))
n_grupo_na   <- sum(is.na(base_unida$edad_grupo) & !is.na(base_unida$edad))
if (n_grupo_na > 0) {
  cat(sprintf("   ⚠ %d filas con edad no-NA quedaron fuera de [18, Inf) → edad_grupo NA (edad < 18?).\n",
              n_grupo_na))
}
cat(sprintf("   edad NA: %d | distribución de edad_grupo (iteración-invariante):\n", n_edad_na))
print(base_unida |> filter(iteracion == 1) |> count(edad_grupo))


# IV.1 Imputación puntual: individuo 2698 (genero + villa) ------------------
# Ambos NAs se repiten en las 1000 iteraciones. El id + is.na() como
# condición limita el cambio a esa fila.

base_unida <- base_unida |>
  mutate(
    genero = if_else(id_individuo == 2698 & is.na(genero), "Masculino", genero),
    villa  = if_else(id_individuo == 2698 & is.na(villa),  0,           villa)
  )

# IV.2 Split en lista por iteración -----------------------------------------
# split() preserva el orden natural de iteracion, así datos_por_iteracion[[1]]
# es la iteración 1. NO se aplica drop_na global: feols hace listwise por
# fórmula, así reg1 (solo villa) no pierde filas por NAs en tiempo/NBI que
# no le corresponden.

datos_por_iteracion <- split(base_unida, base_unida$iteracion)

cat(sprintf("\n   Iteraciones: %d | Filas por iteración: %d\n",
            length(datos_por_iteracion), nrow(datos_por_iteracion[[1]])))

# IV.3 Diagnóstico de missingness -------------------------------------------

vars_clave <- c(
  "id_individuo", "iteracion",
  "dias_ult_cons", "villa", "edad", "edad_grupo", "genero",
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
# CAMBIOS v3:
#   - Se reemplaza poly(edad, 2, raw = TRUE) por factor(edad_grupo)
#     (categorías 18-44 [ref] / 45-64 / 65+). Se elimina el término cuadrático.
#   - Orden de covariables solicitado: villa, genero, edad_grupo,
#     hacinamiento, NBI, tiempo_transporte, interacción.
#     (El orden en la fórmula no afecta las estimaciones OLS; sí define el
#      orden por defecto en coeftable, y `orden_variables` gobierna la tabla.)
#   - reg5, reg6 y reg7 usan log(tiempo_transporte_cesac_t) de forma
#     consistente (antes reg5/reg7 mezclaban nivel y log → coefs vacíos).
#
# NOTA de interpretación (Wooldridge 2002, Cap. 2; Angrist & Pischke MHE
# Cap. 3): con factor(edad_grupo), cada coeficiente es el diferencial de
# E[log(dias)] del grupo respecto de 18-44, condicional al resto. Ya no hay
# un "efecto edad" continuo; el perfil etario se captura de forma escalonada.

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
    formula          = "log(dias_ult_cons) ~ factor(villa) + factor(genero) + factor(edad_grupo)",
    coefs            = c("factor(villa)1"),
    extra_patterns   = c("^factor\\(genero\\)", "^factor\\(edad_grupo\\)"),
    tiene_fe_comuna  = FALSE
  ),
  
  reg3 = list(
    label            = "Reg 3: + FE comuna",
    formula          = "log(dias_ult_cons) ~ factor(villa) + factor(genero) + factor(edad_grupo) + factor(comuna_regresion)",
    coefs            = c("factor(villa)1"),
    extra_patterns   = c("^factor\\(genero\\)", "^factor\\(edad_grupo\\)"),
    tiene_fe_comuna  = TRUE
  ),
  
  reg4 = list(
    label            = "Reg 4: + tiempo de transporte",
    formula          = "log(dias_ult_cons) ~ factor(villa) + factor(genero) + factor(edad_grupo) + log(tiempo_transporte_cesac_t) + factor(comuna_regresion)",
    coefs            = c("factor(villa)1", "log(tiempo_transporte_cesac_t)"),
    extra_patterns   = c("^factor\\(genero\\)", "^factor\\(edad_grupo\\)"),
    tiene_fe_comuna  = TRUE
  ),
  
  reg5 = list(
    label            = "Reg 5: + hacinamiento y NBI",
    formula          = "log(dias_ult_cons) ~ factor(villa) + factor(genero) + factor(edad_grupo) + pct_hacinamiento_t + pct_NBI_t + log(tiempo_transporte_cesac_t) + factor(comuna_regresion)",
    coefs            = c("factor(villa)1", "pct_hacinamiento_t", "pct_NBI_t", "log(tiempo_transporte_cesac_t)"),
    extra_patterns   = c("^factor\\(genero\\)", "^factor\\(edad_grupo\\)"),
    tiene_fe_comuna  = TRUE
  ),
  
  reg6 = list(
    label            = "Reg 6: interacción tiempo (log) × villa",
    formula          = "log(dias_ult_cons) ~ factor(villa) + factor(genero) + factor(edad_grupo) + log(tiempo_transporte_cesac_t) * factor(villa) + factor(comuna_regresion)",
    coefs            = c(
      "factor(villa)1",
      "log(tiempo_transporte_cesac_t)",
      "log(tiempo_transporte_cesac_t):factor(villa)1"
    ),
    extra_patterns   = c("^factor\\(genero\\)", "^factor\\(edad_grupo\\)"),
    tiene_fe_comuna  = TRUE
  ),
  
  reg7 = list(
    label            = "Reg 7: interacción tiempo × villa + NBI",
    formula          = "log(dias_ult_cons) ~ factor(villa) + factor(genero) + factor(edad_grupo) + pct_hacinamiento_t + pct_NBI_t + log(tiempo_transporte_cesac_t) * factor(villa) + factor(comuna_regresion)",
    coefs            = c(
      "factor(villa)1",
      "pct_hacinamiento_t",
      "pct_NBI_t",
      "log(tiempo_transporte_cesac_t)",
      "log(tiempo_transporte_cesac_t):factor(villa)1"
    ),
    extra_patterns   = c("^factor\\(genero\\)", "^factor\\(edad_grupo\\)"),
    tiene_fe_comuna  = TRUE
  )
)

# V.1 Errores estándar clásicos ---------------------------------------------

cat("── V.1 Corriendo regresiones con EE clásicos...\n")
resultados_todas <- ejecutar_todas(especificaciones, datos_por_iteracion, vcov_type = "iid")
resumen_mc       <- calcular_resumen(resultados_todas)
modelo_stats     <- calcular_modelo_stats(resultados_todas)

cat("\n   N por especificación (EE clásicos):\n")
print(modelo_stats)

# V.2 Errores estándar clusterizados por comuna_regresion -------------------

cat("── V.2 Corriendo regresiones con EE clusterizados por comuna_regresion...\n")
resultados_todas_cluster <- ejecutar_todas(especificaciones, datos_por_iteracion, vcov_type = "comuna_regresion")
resumen_mc_cluster       <- calcular_resumen(resultados_todas_cluster)
modelo_stats_cluster     <- calcular_modelo_stats(resultados_todas_cluster)

# V.3 FE alternativo con comuna_ok en vez de comuna_regresion ---------------

cat("── V.3 Corriendo regresiones con FE = comuna_ok...\n")

especificaciones_comuna_ok <- map(especificaciones, function(spec) {
  spec$formula <- str_replace(spec$formula, "factor\\(comuna_regresion\\)", "factor(comuna_ok)")
  spec
})

# Chequeo: TRUE para reg3–reg7 (las que tienen FE comuna), FALSE para reg1–reg2
cat("   Reemplazo comuna_regresion → comuna_ok pegó en:\n")
print(map_lgl(especificaciones_comuna_ok, ~ str_detect(.x$formula, "comuna_ok")))

resultados_todas_comuna_ok <- ejecutar_todas(especificaciones_comuna_ok, datos_por_iteracion, vcov_type = "iid")
resumen_mc_comuna_ok       <- calcular_resumen(resultados_todas_comuna_ok)
modelo_stats_comuna_ok     <- calcular_modelo_stats(resultados_todas_comuna_ok)



# =============================================================================
# PARTE V-bis — CORRELACIÓN INTRA-CLASE (ICC) DEL OUTCODE POR CLUSTER
# =============================================================================
#
# Objetivo: cuantificar qué fracción de la varianza de log(dias_ult_cons) es
# ATRIBUIBLE a diferencias ENTRE clusters (comuna; radio censal) frente a la
# variación INTRA cluster. Es el insumo para justificar EE clusterizados y
# para dimensionar el "design effect".
#
#   ICC = rho = sigma^2_entre / (sigma^2_entre + sigma^2_intra)
#
# Fundamentación:
#   - Descomposición de varianza en modelos jerárquicos de una vía:
#     Snijders & Bosker (2012), "Multilevel Analysis", Cap. 3.
#   - Relación ICC ↔ clustering de EE y design effect
#     deff = 1 + (n_bar - 1) * rho:
#     Cameron & Miller (2015, J. of Human Resources 50(2)); Angrist & Pischke,
#     "Mostly Harmless Econometrics", Cap. 8 (Moulton factor).
#   - Correlación INTER-cluster (entre medias de cluster) se reporta de forma
#     complementaria: es 1 - algo solo en diseños balanceados; acá se informa
#     como correlación de las medias de cluster con el gran promedio, más el
#     rango/CV de las medias por cluster.
#
# Se estima el ICC con un modelo de intercepto aleatorio (lme4::lmer) si el
# paquete está disponible; si no, con el estimador ANOVA de una vía (momentos),
# que no requiere dependencias extra. Ambos coinciden en diseños balanceados.
# Se calcula sobre la iteración 1 (el outcome NO varía entre iteraciones; lo
# que varía es el radio sorteado, pero dias_ult_cons es del individuo).
# Para el ICC a nivel de RADIO, sí importa la iteración: se promedia el ICC
# sobre un subconjunto de iteraciones para propagar la incertidumbre del sorteo.

cat("\n── V-bis Estimando correlación intra-clase (ICC) del outcome...\n")

# --- Estimadores de ICC -----------------------------------------------------

# ICC por momentos (ANOVA de una vía). df: data.frame; y: nombre outcome;
# g: nombre de la variable de cluster. Devuelve componentes de varianza e ICC.
icc_anova <- function(df, y, g) {
  d <- df[!is.na(df[[y]]) & !is.na(df[[g]]), c(y, g)]
  names(d) <- c("y", "g")
  d$g <- as.factor(d$g)
  k   <- nlevels(droplevels(d$g))
  N   <- nrow(d)
  if (k < 2 || N <= k) {
    return(list(icc = NA_real_, sigma2_entre = NA_real_, sigma2_intra = NA_real_,
                k = k, N = N, n_bar = NA_real_))
  }
  # ANOVA de una vía: MSB (entre) y MSW (intra)
  aov_fit <- stats::aov(y ~ g, data = d)
  ms      <- summary(aov_fit)[[1]][, "Mean Sq"]
  MSB <- ms[1]; MSW <- ms[2]
  # tamaño de cluster "efectivo" para diseño no balanceado (Snijders & Bosker)
  n_j   <- as.numeric(table(d$g))
  n_bar <- (N - sum(n_j^2) / N) / (k - 1)
  sigma2_intra <- MSW
  sigma2_entre <- max((MSB - MSW) / n_bar, 0)   # truncado en 0 (varianza ≥ 0)
  icc <- sigma2_entre / (sigma2_entre + sigma2_intra)
  list(icc = icc, sigma2_entre = sigma2_entre, sigma2_intra = sigma2_intra,
       k = k, N = N, n_bar = n_bar)
}

# ICC por modelo de intercepto aleatorio (si lme4 está instalado)
icc_lmer <- function(df, y, g) {
  if (!requireNamespace("lme4", quietly = TRUE)) return(NULL)
  d <- df[!is.na(df[[y]]) & !is.na(df[[g]]), c(y, g)]
  names(d) <- c("y", "g")
  fit <- try(lme4::lmer(y ~ 1 + (1 | g), data = d, REML = TRUE), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  vc <- as.data.frame(lme4::VarCorr(fit))
  s2_entre <- vc$vcov[vc$grp == "g"]
  s2_intra <- vc$vcov[vc$grp == "Residual"]
  list(icc = s2_entre / (s2_entre + s2_intra),
       sigma2_entre = s2_entre, sigma2_intra = s2_intra)
}

# Correlación INTER-cluster complementaria: dispersión de las medias de cluster.
resumen_medias_cluster <- function(df, y, g) {
  d <- df[!is.na(df[[y]]) & !is.na(df[[g]]), c(y, g)]
  names(d) <- c("y", "g")
  medias <- tapply(d$y, d$g, mean)
  tibble(
    n_clusters   = length(medias),
    media_global = mean(d$y),
    de_medias    = sd(medias),
    cv_medias    = sd(medias) / abs(mean(medias)),
    min_media    = min(medias),
    max_media    = max(medias)
  )
}

# --- ICC a nivel de COMUNA (comuna_regresion y comuna_ok) -------------------
# El outcome no depende del sorteo → basta la iteración 1.

datos_icc_comuna <- datos_por_iteracion[[1]] |>
  mutate(log_dias = log(dias_ult_cons)) |>
  filter(is.finite(log_dias))

icc_comuna_reg <- icc_anova(datos_icc_comuna, "log_dias", "comuna_regresion")
icc_comuna_ok  <- icc_anova(datos_icc_comuna, "log_dias", "comuna_ok")

icc_comuna_reg_lmer <- icc_lmer(datos_icc_comuna, "log_dias", "comuna_regresion")
icc_comuna_ok_lmer  <- icc_lmer(datos_icc_comuna, "log_dias", "comuna_ok")

# --- ICC a nivel de RADIO censal (id_radio_sim) — depende del sorteo --------
# Promediamos el ICC sobre las primeras N_ICC_ITER iteraciones para propagar
# la incertidumbre de asignación individuo→radio.

N_ICC_ITER <- min(100L, length(datos_por_iteracion))
cat(sprintf("   ICC a nivel de radio: promediando sobre %d iteraciones...\n", N_ICC_ITER))

icc_radio_por_iter <- map_dfr(seq_len(N_ICC_ITER), function(i) {
  di <- datos_por_iteracion[[i]] |>
    mutate(log_dias = log(dias_ult_cons)) |>
    filter(is.finite(log_dias))
  r <- icc_anova(di, "log_dias", "id_radio_sim")
  tibble(iteracion = i, icc = r$icc,
         sigma2_entre = r$sigma2_entre, sigma2_intra = r$sigma2_intra,
         k_radios = r$k, n_bar = r$n_bar)
})

icc_radio_resumen <- icc_radio_por_iter |>
  summarise(
    icc_medio      = mean(icc, na.rm = TRUE),
    icc_de         = sd(icc,  na.rm = TRUE),
    icc_p2_5       = quantile(icc, 0.025, na.rm = TRUE),
    icc_p97_5      = quantile(icc, 0.975, na.rm = TRUE),
    n_bar_medio    = mean(n_bar, na.rm = TRUE),
    k_radios_medio = mean(k_radios, na.rm = TRUE)
  )

# --- Tabla resumen de ICC ---------------------------------------------------

deff <- function(rho, n_bar) 1 + (n_bar - 1) * rho   # Moulton / design effect

tabla_icc <- tibble(
  Cluster = c("Comuna (comuna_regresion)", "Comuna (comuna_ok)", "Radio censal (sorteado)"),
  `ICC (ANOVA)` = c(icc_comuna_reg$icc, icc_comuna_ok$icc, icc_radio_resumen$icc_medio),
  `ICC (lmer)`  = c(
    if (!is.null(icc_comuna_reg_lmer)) icc_comuna_reg_lmer$icc else NA_real_,
    if (!is.null(icc_comuna_ok_lmer))  icc_comuna_ok_lmer$icc  else NA_real_,
    NA_real_
  ),
  `sigma2 entre` = c(icc_comuna_reg$sigma2_entre, icc_comuna_ok$sigma2_entre, NA_real_),
  `sigma2 intra` = c(icc_comuna_reg$sigma2_intra, icc_comuna_ok$sigma2_intra, NA_real_),
  `n_bar`        = c(icc_comuna_reg$n_bar, icc_comuna_ok$n_bar, icc_radio_resumen$n_bar_medio),
  `k clusters`   = c(icc_comuna_reg$k, icc_comuna_ok$k, round(icc_radio_resumen$k_radios_medio))
) |>
  mutate(`Design effect` = deff(`ICC (ANOVA)`, n_bar))

cat("\n   ── Tabla ICC (correlación intra-clase del outcome) ──\n")
print(as.data.frame(tabla_icc), digits = 4)

cat("\n   Dispersión de medias por comuna (inter-cluster, comuna_regresion):\n")
print(resumen_medias_cluster(datos_icc_comuna, "log_dias", "comuna_regresion"))

cat(sprintf("\n   ICC radio (media entre iter): %.4f  [IC95%%: %.4f, %.4f]\n",
            icc_radio_resumen$icc_medio, icc_radio_resumen$icc_p2_5, icc_radio_resumen$icc_p97_5))

# Flextable de ICC para exportar
ft_icc <- flextable(tabla_icc |>
                      mutate(across(where(is.numeric), ~ round(.x, 4)))) |>
  theme_booktabs() |>
  align(align = "center", part = "all") |>
  align(j = "Cluster", align = "left", part = "all") |>
  fontsize(size = 9, part = "body") |>
  bold(part = "header") |>
  autofit() |>
  add_footer_lines(paste(
    "Nota: ICC = sigma2_entre / (sigma2_entre + sigma2_intra) sobre log(dias_ult_cons).",
    "ANOVA de una vía (estimador de momentos) y modelo de intercepto aleatorio (lme4).",
    "Design effect = 1 + (n_bar - 1)*ICC (factor de Moulton). ICC de radio promediado entre iteraciones.",
    "Ref.: Snijders & Bosker (2012) Cap. 3; Cameron & Miller (2015, JHR); Angrist & Pischke (MHE) Cap. 8."
  )) |>
  fontsize(size = 7, part = "footer")


# =============================================================================
# PARTE V-ter — ROBUSTEZ: SOLO COMUNAS CON BARRIOS POPULARES
# =============================================================================
#
# Chequeo de robustez: re-estimar TODAS las especificaciones restringiendo la
# muestra a comunas que efectivamente contienen barrios populares. Es decir,
# comunas que tienen tanto observaciones de asentamiento válido (villa=1) como
# de "no villa" (villa=0) dentro de la comuna. Se EXCLUYEN comunas sin ninguna
# villa (donde el contraste villa vs no-villa no existe).
#
# Motivación (Angrist & Pischke, MHE Cap. 3, "common support"; Wooldridge 2002
# Cap. 21): el contraste villa/no-villa solo está identificado localmente en
# comunas donde ambos grupos coexisten. Restringir a esas comunas aproxima una
# comparación con soporte común y evita que comunas 100% no-villa entren solo
# como controles de intercepto vía el FE.
#
# La pertenencia villa NO depende del sorteo de radio (asentamiento es del
# individuo). Por eso el conjunto de comunas-con-villa se fija UNA vez con la
# iteración 1 y se aplica a todas.

cat("\n── V-ter Robustez: comunas con barrios populares...\n")

# La comuna "real" del individuo es comuna_ok_fb (comuna_ok con fallback a
# comuna_norm). comuna_regresion pone "BP" a las villas, así que NO sirve para
# identificar la comuna geográfica de origen. Identificamos comunas que tienen
# al menos una villa=1 y al menos una villa=0.

comunas_con_villa <- datos_por_iteracion[[1]] |>
  filter(!is.na(comuna_ok_fb), !is.na(villa)) |>
  group_by(comuna_ok_fb) |>
  summarise(
    n_villa    = sum(villa == 1, na.rm = TRUE),
    n_no_villa = sum(villa == 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n_villa > 0 & n_no_villa > 0) |>
  pull(comuna_ok_fb)

cat(sprintf("   Comunas con villa (soporte común villa/no-villa): %d\n",
            length(comunas_con_villa)))
print(sort(comunas_con_villa))

# Submuestra por iteración: filtrar a comunas_con_villa
datos_por_iteracion_bp <- map(datos_por_iteracion, ~ filter(.x, comuna_ok_fb %in% comunas_con_villa))

cat(sprintf("   Filas por iteración (submuestra BP): %d (vs %d en muestra completa)\n",
            nrow(datos_por_iteracion_bp[[1]]), nrow(datos_por_iteracion[[1]])))

# Correr las mismas specs (EE clusterizados por comuna_regresion, coherente
# con la especificación principal). Se puede cambiar vcov_type a "iid".
resultados_bp     <- ejecutar_todas(especificaciones, datos_por_iteracion_bp, vcov_type = "comuna_regresion")
resumen_mc_bp     <- calcular_resumen(resultados_bp)
modelo_stats_bp   <- calcular_modelo_stats(resultados_bp)

cat("\n   N por especificación (submuestra BP):\n")
print(modelo_stats_bp)


# =============================================================================
# PARTE V-quater — ROBUSTEZ + REPONDERACIÓN IPW (villa | comuna)
# =============================================================================
#
# Igual que V-ter (submuestra de comunas con villa) pero reponderando cada
# observación por el inverso de la probabilidad de su condición villa DENTRO de
# su comuna. La idea la planteaste como "una suerte de IPW con variable de
# resultado villa y variable independiente i.comuna".
#
# QUÉ IDENTIFICA (importante para el paper):
#   villa NO es un tratamiento asignable, es una condición estructural. Por eso
#   este reponderado NO recupera un efecto causal de "vivir en villa" (no hay
#   unconfoundedness sobre un tratamiento manipulable — Imbens & Wooldridge
#   2009, JEL 47(1); Wooldridge 2002 Cap. 21). Es una ESTANDARIZACIÓN /
#   reweighting descriptivo: iguala la composición villa/no-villa entre comunas,
#   de modo que ninguna comuna con sobre-representación de villas domine el
#   contraste. Se reporta como robustez, con esa interpretación explícita.
#
# PESOS ESTABILIZADOS (SIPW) — preferidos sobre IPW crudo:
#   Se ajusta el propensity de villa sobre indicadores de comuna:
#       glm(villa ~ factor(comuna_ok), family = binomial)   [i.comuna]
#   con e_hat = P(villa=1 | comuna). Peso estabilizado (Robins, Hernán &
#   Brumback 2000, Epidemiology 11(5)):
#       villa=1 :  w = p_bar   / e_hat
#       villa=0 :  w = (1-p_bar)/(1-e_hat)
#   donde p_bar = P(villa=1) marginal (en la submuestra). El IPW crudo
#   (1/e_hat, 1/(1-e_hat)) queda comentado. La estabilización evita pesos
#   explosivos en comunas con muy pocas villas (SIPW ya usado en tu otro
#   workstream de RCT).
#
# NOTA sobre el propensity con i.comuna: como e_hat es exactamente la
# proporción de villa por comuna (modelo saturado en comuna), el SIPW iguala
# la fracción villa a p_bar en TODA comuna. Es el reweighting exacto pedido.
# Se estima sobre la iteración 1 (villa y comuna_ok no dependen del sorteo) y
# los pesos se reutilizan en todas las iteraciones vía id_individuo.

cat("\n── V-quater Robustez con reponderación IPW (villa | i.comuna)...\n")

# --- Ajuste del propensity y construcción de pesos (iteración 1) ------------

base_pesos <- datos_por_iteracion_bp[[1]] |>
  filter(!is.na(villa), !is.na(comuna_ok_fb)) |>
  select(id_individuo, villa, comuna_ok_fb) |>
  distinct(id_individuo, .keep_all = TRUE)

# propensity: villa ~ i.comuna  (modelo saturado en comuna => e_hat = share villa)
ps_fit <- glm(villa ~ factor(comuna_ok_fb), family = binomial, data = base_pesos)
base_pesos <- base_pesos |>
  mutate(
    e_hat = predict(ps_fit, type = "response"),
    p_bar = mean(villa == 1)
  )

# Recorte defensivo del propensity para evitar división por ~0 (common support;
# Imbens & Wooldridge 2009 recomiendan overlap; recorte a [0.01, 0.99]).
eps <- 0.01
base_pesos <- base_pesos |>
  mutate(
    e_hat_c = pmin(pmax(e_hat, eps), 1 - eps),
    # SIPW estabilizado (preferido)
    w_ipw = if_else(villa == 1, p_bar / e_hat_c, (1 - p_bar) / (1 - e_hat_c))
    # IPW crudo (Horvitz-Thompson) — alternativa, descomentar si se desea:
    # w_ipw = if_else(villa == 1, 1 / e_hat_c, 1 / (1 - e_hat_c))
  )

cat("   Distribución de pesos SIPW:\n")
print(summary(base_pesos$w_ipw))
cat(sprintf("   p_bar (share villa en submuestra BP): %.4f | comunas: %d\n",
            base_pesos$p_bar[1], n_distinct(base_pesos$comuna_ok_fb)))

# --- Pegar pesos a cada iteración -------------------------------------------

pesos_lookup <- base_pesos |> select(id_individuo, w_ipw)

datos_por_iteracion_bp_w <- map(datos_por_iteracion_bp, function(di) {
  di |> left_join(pesos_lookup, by = "id_individuo")
})

# --- Variante de ajuste con pesos (feols admite `weights`) ------------------
# Se replica ajustar_spec pero pasando weights = ~w_ipw. Se mantiene el mismo
# esquema de vcov clusterizado por comuna_regresion.

ajustar_spec_w <- function(data, spec, vcov_type = "comuna_regresion", peso = "w_ipw") {
  vcov_arg <- if (identical(vcov_type, "iid")) "iid" else as.formula(paste0("~", vcov_type))
  data_ok  <- data[!is.na(data[[peso]]), , drop = FALSE]
  
  fit <- feols(as.formula(spec$formula), data = data_ok,
               weights = as.formula(paste0("~", peso)), vcov = vcov_arg)
  
  coincide_patron <- function(term, patrones) {
    if (length(patrones) == 0) rep(FALSE, length(term))
    else str_detect(term, paste(patrones, collapse = "|"))
  }
  
  as.data.frame(coeftable(fit)) |>
    rownames_to_column("term") |>
    rename(estimate = Estimate, std.error = `Std. Error`,
           statistic = `t value`, p.value = `Pr(>|t|)`) |>
    filter(term %in% spec$coefs | coincide_patron(term, spec$extra_patterns)) |>
    mutate(nobs = nobs(fit), adj_r2 = fixest::r2(fit, "ar2"))
}

ejecutar_todas_w <- function(especificaciones, datos_por_iteracion, vcov_type, peso = "w_ipw") {
  map_dfr(names(especificaciones), function(spec_id) {
    spec <- especificaciones[[spec_id]]
    verificar_formula(spec$formula, datos_por_iteracion[[1]], spec$label)
    imap_dfr(
      datos_por_iteracion,
      ~ ajustar_spec_w(.x, spec, vcov_type = vcov_type, peso = peso) |> mutate(iteracion = .y)
    ) |>
      mutate(regresion_id = spec_id, regresion_label = spec$label)
  })
}

resultados_bp_w   <- ejecutar_todas_w(especificaciones, datos_por_iteracion_bp_w,
                                      vcov_type = "comuna_regresion", peso = "w_ipw")
resumen_mc_bp_w   <- calcular_resumen(resultados_bp_w)
modelo_stats_bp_w <- calcular_modelo_stats(resultados_bp_w)

cat("\n   N por especificación (submuestra BP + IPW):\n")
print(modelo_stats_bp_w)


# =============================================================================
# PARTE VI — TABLAS Y EXPORTACIÓN
# =============================================================================

nota_rubin <- paste(
  "EE de Rubin (1987) entre par\u00e9ntesis: T = Ubar + (1+1/m)B, combinando la varianza",
  "intra-iteraci\u00f3n (Ubar) y entre iteraciones (B) del Monte Carlo geogr\u00e1fico.",
  "p-valores con grados de libertad ajustados (Barnard-Rubin 1999)."
)

ft_estandar <- armar_tabla_flextable(
  resumen_mc, modelo_stats, especificaciones,
  nota_pie = paste(
    "Nota: coeficiente promedio entre iteraciones.", nota_rubin,
    "* p<0.10, ** p<0.05, *** p<0.01. EE base cl\u00e1sicos. FE de comuna: comuna_regresion."
  )
)

ft_cluster <- armar_tabla_flextable(
  resumen_mc_cluster, modelo_stats_cluster, especificaciones,
  nota_pie = paste(
    "Nota: coeficiente promedio entre iteraciones.", nota_rubin,
    "* p<0.10, ** p<0.05, *** p<0.01. EE base clusterizados por comuna_regresion. FE: comuna_regresion."
  )
)

ft_comuna_ok <- armar_tabla_flextable(
  resumen_mc_comuna_ok, modelo_stats_comuna_ok, especificaciones_comuna_ok,
  nota_pie = paste(
    "Nota: coeficiente promedio entre iteraciones.", nota_rubin,
    "* p<0.10, ** p<0.05, *** p<0.01. EE base cl\u00e1sicos. FE de comuna: comuna_ok."
  )
)

# Robustez 1: submuestra de comunas con barrios populares (soporte común)
ft_bp <- armar_tabla_flextable(
  resumen_mc_bp, modelo_stats_bp, especificaciones,
  nota_pie = paste(
    "Nota: robustez restringida a comunas con presencia de villa (soporte com\u00fan villa/no-villa).",
    nota_rubin,
    "* p<0.10, ** p<0.05, *** p<0.01. EE base clusterizados por comuna_regresion.",
    "Comunas sin ninguna villa quedan excluidas (el contraste no est\u00e1 identificado all\u00ed)."
  )
)

# Robustez 2: submuestra BP + reponderación SIPW (villa | i.comuna)
ft_bp_w <- armar_tabla_flextable(
  resumen_mc_bp_w, modelo_stats_bp_w, especificaciones,
  nota_pie = paste(
    "Nota: robustez en comunas con villa, reponderando por SIPW estabilizado del propensity villa~i.comuna.",
    "Pesos: villa=1 -> p_bar/e_hat; villa=0 -> (1-p_bar)/(1-e_hat), recorte de overlap a [0.01,0.99].",
    "Reweighting DESCRIPTIVO (estandariza composici\u00f3n villa/no-villa por comuna), NO efecto causal.",
    nota_rubin,
    "* p<0.10, ** p<0.05, *** p<0.01. EE base clusterizados por comuna_regresion.",
    "Ref.: Robins, Hern\u00e1n & Brumback (2000, Epidemiology); Imbens & Wooldridge (2009, JEL)."
  )
)

# --- Tabla diagnóstica de Rubin: descomposición de varianza por coeficiente ---
# Muestra, para los coeficientes de interés, cuánta de la incertidumbre total
# proviene de la incertidumbre de agregación geográfica (FMI). Insumo directo
# para reportar en el paper la robustez del resultado al problema de asignación.

coefs_interes_rubin <- c(
  "factor(villa)1", "log(tiempo_transporte_cesac_t)",
  "pct_NBI_t", "pct_hacinamiento_t",
  "log(tiempo_transporte_cesac_t):factor(villa)1"
)

tabla_diag_rubin <- resumen_mc_cluster |>
  filter(term %in% coefs_interes_rubin) |>
  transmute(
    `Regresión`   = regresion_id,
    Variable    = etiquetar_termino(term),
    `Coef.`     = round(media_coef_raw, 4),
    `EE prom (viejo)` = round(se_prom_raw, 4),
    `EE Rubin`  = round(se_rubin_raw, 4),
    `sqrt(Ubar)` = round(sqrt(U_bar), 4),
    `sqrt(B)`    = round(sqrt(B_between), 4),
    `Aumento EE (%)` = round(100 * (se_rubin_raw / se_prom_raw - 1), 1),
    `FMI`       = round(lambda_fmi, 3),
    `gl ajust.` = round(nu_adj, 0)
  ) |>
  arrange(`Regresión`, Variable)

cat("\n   ── Diagnóstico de Rubin (EE clusterizado por comuna) ──\n")
print(as.data.frame(tabla_diag_rubin))

ft_rubin_diag <- flextable(tabla_diag_rubin) |>
  theme_booktabs() |>
  align(align = "center", part = "all") |>
  align(j = "Variable", align = "left", part = "all") |>
  fontsize(size = 8, part = "body") |>
  bold(part = "header") |>
  autofit() |>
  add_footer_lines(paste(
    "Nota: EE Rubin = sqrt(Ubar + (1+1/m)B). 'Aumento EE (%)' = cu\u00e1nto agranda Rubin el EE respecto del promedio simple.",
    "FMI = fracci\u00f3n de informaci\u00f3n faltante = (1+1/m)B / T: proporci\u00f3n de la incertidumbre atribuible a la asignaci\u00f3n",
    "individuo->radio. gl ajustados seg\u00fan Barnard & Rubin (1999). m = n\u00famero de iteraciones Monte Carlo v\u00e1lidas."
  )) |>
  fontsize(size = 7, part = "footer")

# Ver en el Viewer de RStudio
ft_estandar
ft_cluster
ft_comuna_ok
ft_icc
ft_bp
ft_bp_w
ft_rubin_diag

# Exportar a .docx
save_as_docx(ft_estandar,   path = "tabla_regresiones_villa_EEclasicos.docx")
save_as_docx(ft_cluster,    path = "tabla_regresiones_villa_EEcluster.docx")
save_as_docx(ft_comuna_ok,  path = "tabla_regresiones_villa_FEcomunaOK.docx")
save_as_docx(ft_icc,        path = "tabla_ICC_outcome.docx")
save_as_docx(ft_bp,         path = "tabla_robustez_comunas_con_villa.docx")
save_as_docx(ft_bp_w,       path = "tabla_robustez_comunas_con_villa_IPW.docx")
save_as_docx(ft_rubin_diag, path = "tabla_diagnostico_Rubin.docx")


# =============================================================================
# PARTE VII — GRÁFICOS DE DISTRIBUCIÓN DE COEFICIENTES
# =============================================================================
# Los datos vienen de `resultados_todas` (iid) filtrando por regresion_id.
# Cambiar las specs elegidas según qué distribuciones interesen mostrar.

cat("── VII. Armando gráficos de distribución de coeficientes...\n")

p_villa_reg3 <- plot_distribucion_coef(
  resultados_todas, "reg3", "factor(villa)1",
  "Villa → log(Días desde última consulta)\n(Reg 3: + FE comuna)",
  color = "#D7263D"
)

p_tiempo_reg4 <- plot_distribucion_coef(
  resultados_todas, "reg4", "log(tiempo_transporte_cesac_t)",
  "log(Tiempo al CeSAC) → log(Días)\n(Reg 4)",
  color = "#E09F3E"
)

p_nbi_reg5 <- plot_distribucion_coef(
  resultados_todas, "reg5", "pct_NBI_t",
  "NBI (%)\n(Reg 5)",
  color = "#5C4D7D"
)

p_hac_reg5 <- plot_distribucion_coef(
  resultados_todas, "reg5", "pct_hacinamiento_t",
  "Hacinamiento (%)\n(Reg 5)",
  color = "#5C4D7D"
)

p_villa_reg5 <- plot_distribucion_coef(
  resultados_todas, "reg5", "factor(villa)1",
  "Villa → log(Días)\n(Reg 5: modelo más rico sin interacción)",
  color = "#D7263D"
)

p_inter_reg6 <- plot_distribucion_coef(
  resultados_todas, "reg6", "log(tiempo_transporte_cesac_t):factor(villa)1",
  "Interacción log(Tiempo) × Villa\n(Reg 6)",
  color = "#1B998B"
)

panel_coeficientes <- (p_villa_reg3 + p_tiempo_reg4 + p_villa_reg5) /
  (p_nbi_reg5  + p_hac_reg5   + p_inter_reg6) +
  plot_annotation(
    title = "Distribución de coeficientes — 1000 simulaciones Monte Carlo",
    theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5))
  )

panel_coeficientes







# ¿Qué grupos de casos tienen NA en tiempo_transporte_cesac_t?
# Cruce de las 3 dimensiones que gobiernan la construcción:
base_unida |>
  filter(iteracion == 1) |>
  mutate(
    tiene_asentamiento = !is.na(asentamiento),
    tiene_id_barrio    = !is.na(id_barrio),
    solapamiento_bin   = case_when(
      is.na(pct_area_villa) ~ "NA",
      pct_area_villa == 0   ~ "0% (radio limpio)",
      TRUE                  ~ ">0% (parcial)"
    )
  ) |>
  count(tiene_asentamiento, tiene_id_barrio, solapamiento_bin,
        na_tiempo = is.na(tiempo_transporte_cesac_t)) |>
  arrange(desc(n))

# Los 774 casos con NA en comuna_regresion: ¿quiénes son?
base_unida |>
  filter(iteracion == 1, is.na(comuna_regresion)) |>
  count(comuna, comuna_ok, sort = TRUE) |>
  head(20)

# ¿Los 479 villa=1 sin id_barrio son diferencias de escritura?
base_unida |>
  filter(iteracion == 1, villa == 1, is.na(id_barrio)) |>
  distinct(asentamiento) |>
  arrange(asentamiento) |>
  print(n = Inf)

# Comparar contra los nombres canónicos:
sort(unique(analisis_barrios$nombre_barrio))

# ggsave("panel_coeficientes_montecarlo.png", panel_coeficientes,
#        width = 12, height = 8, dpi = 300)


base_unida |>
  filter(iteracion == 1, is.na(comuna_regresion)) |>
  summarise(
    n_total       = n(),
    n_con_comuna  = sum(!is.na(comuna)),
    n_con_c_norm  = sum(!is.na(comuna_norm))   # esta no está en base_unida, mirá si falla
  )



sin_match <- base_unida |>
  filter(iteracion == 1, !is.na(asentamiento), is.na(id_barrio)) |>
  distinct(asentamiento) |>
  arrange(asentamiento) |>
  pull(asentamiento)

sin_match

# Y comparalo con los canónicos:
sort(unique(analisis_barrios$nombre_barrio))
# =============================================================================
# NOTAS METODOLÓGICAS
# =============================================================================
#
# 1) N POR ESPECIFICACIÓN: al no aplicar drop_na global sobre
#    datos_por_iteracion, cada spec pierde filas solo por las variables que
#    efectivamente usa. Esperable:
#      reg1 (solo villa) → N alto
#      reg2-3 (+ edad, genero, comuna) → similar o levemente menor
#      reg4-7 (+ tiempo/NBI) → N más chico, porque tiempo y NBI son NA
#                              en casos con solapamiento parcial villa/radio
#
# 2) INFERENCIA DE RUBIN (implementada): el promedio simple del EE del modelo
#    (se_prom) IGNORA la varianza entre iteraciones y subestima la incertidumbre.
#    Ahora se reporta el EE de Rubin (1987): T = Ubar + (1+1/m)B, con Ubar =
#    promedio de EE^2 (within) y B = varianza de los coeficientes entre
#    iteraciones (between). El p-valor usa gl ajustados (Barnard & Rubin 1999).
#    La tabla_diagnostico_Rubin reporta el FMI (fracción de información
#    faltante) por coeficiente: qué parte de la incertidumbre total proviene
#    del sorteo geográfico. se_prom se conserva en resumen_mc para comparar.
#    SALVEDAD: el sorteo ponderado por población aproxima —no reproduce
#    exactamente— una imputación bayesiana "proper" (Robins & Wang 2000,
#    Biometrika); T se interpreta como incorporación de la incertidumbre de
#    agregación geográfica, no como el EE "verdadero" en sentido estricto.
#
# 3) 479 CASOS villa=1 SIN tiempo: individuos con `asentamiento` reportado
#    que no matcheó contra ningún `nombre_barrio` en analisis_barrios. No
#    se pierden en reg1-3, pero sí en reg4-7. Para recuperarlos, revisar:
#
#      base_unida |>
#        filter(iteracion == 1, villa == 1, is.na(id_barrio)) |>
#        distinct(asentamiento) |>
#        arrange(asentamiento)
#
#    y normalizar los nombres antes del join si son diferencias de escritura.
# =============================================================================