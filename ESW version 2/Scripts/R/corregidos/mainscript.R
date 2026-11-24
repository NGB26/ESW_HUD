############################################################
# Análisis ensayo HTA – CABA / BID                (versión corregida)
# Autor: Nicolás García Balus (nicolasga@iadb.org // nicolas.gbalus@gmail.com)
# Fecha original: 17/12/2025
#
# ETAPAS
#   A. Construye dataset_unificado.csv (pacientes + barrio + comuna + indicadores)
#   -- Paso externo en Stata (rename_pct_unificar.do, etc.) → df_esw.csv --
#   C. Lee df_esw.csv, agrega CPE por comuna, arma mapas / ggpairs / tabla 1.
#
# ORDEN SUGERIDO: 03 → 03b → 04 → 05 → 07bis → 01 → 06 → 07 → mainscript (A)
#                 → Stata → mainscript (C)
#   (A necesita: comuna_unido.csv [03], indicadores_por_barrio.csv [05],
#                analisis_completo_barrios/comunas.csv [06, 07])
#   (C necesita: df_esw.csv [Stata], cpe_comuna.csv [03b],
#                comunas_proj.rds y todos_centros_sf.rds [01])
#
# CAMBIOS RESPECTO A LA VERSIÓN ANTERIOR
#   · Rutas desde 00_config.R (nada hardcodeado); todas las figuras en PDF.
#   · Faltaban library(readr), library(stringr), library(sf), library(patchwork)…
#   · `match_barrio` nunca se creaba (se usaba sum(NULL)): ahora se define.
#   · case_when(... ~ NA_real_) rompía con columnas de texto: reemplazado por
#     una asignación que conserva el tipo.
#   · Join por nombre de barrio con la misma normalización en ambos lados y
#     aviso de nombres sin match; checks de que los joins no multipliquen filas.
#   · `centroides_villa` no estaba definido en el script: ahora se construye.
#   · El padding de las etiquetas del mapa estaba en grados (±0,08) pero el
#     mapa está en metros (EPSG:22185): ahora es proporcional al bbox.
#   · Los polígonos de villas se dibujan ENCIMA de las comunas (orden de capas).
#   · make_map redefinida 2 veces y p1..p4 pisados por los paneles (A)(B)(C):
#     objetos renombrados. El panel (A)(B)(C) ahora muestra los tres (antes
#     solo p1|p2 y p3 quedaba sin usar).
#   · ggpairs: aes(color = df_plot$fuente) → columns= + aes(color = fuente).
#   · log(dias_ult_cons) con días <= 0 daba -Inf/NaN: ahora NA.
#   · edad_deteccion usaba /360; ahora /365.25 (años).
#   · Se agrega pct_cpe_c (CPE por comuna) y el peso de post-estratificación w_cpe.
############################################################

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)          # sf, dplyr, readr + rutas + guardar_pdf()

## 0. Paquetes ---------------------------------------------------------
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(purrr)
  library(GGally)
  library(gt)
  library(gtsummary)
})

sf_use_s2(FALSE)

# Normaliza nombres para joins (sin tildes, minúsculas, espacios simples)
norm_clave <- function(x) {
  x <- as.character(x)
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }
  str_to_lower(str_squish(x))
}

# Carpeta "vieja" (estructura anterior). Solo se usa como último recurso para
# archivos que no estén bajo RAIZ.
RUTA_LEGADO <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/ESW version 2/Data frames consolidados"
)

buscar_en_proyecto <- function(nombre) {
  tryCatch(
    buscar_archivo(nombre, RAIZ),
    error = function(e) {
      if (dir.exists(RUTA_LEGADO)) buscar_archivo(nombre, RUTA_LEGADO) else stop(e)
    }
  )
}

# =============================================================================
# ETAPA A.1 — Pacientes
# =============================================================================
cat("── A.1 Pacientes...\n")

df <- read_excel(buscar_en_proyecto("tabla_receptora_hta_bid_share.xlsx"))

df <- df |>
  mutate(
    turno = if_else(
      (!is.na(servicio) & servicio != "") | (!is.na(fecha_turno)),
      1L, 0L
    ),
    villa = if_else(!is.na(asentamiento), 1, 0),
    dbt   = replace_na(dbt, 0),
    fecha_corte    = as.Date(fecha_ultima_consulta) + dias_desde_ultima_consulta,
    dias_deteccion = as.numeric(fecha_corte - as.Date(fecha_deteccion)),
    dias_dispensa  = as.numeric(fecha_corte - as.Date(ultima_dispensa_hta_fecha)),
    edad_deteccion = edad - dias_deteccion / 365.25
  )

df |>
  group_by(villa, dbt) |>
  summarise(
    edad_media           = mean(edad, na.rm = TRUE),
    edad_deteccion_media = mean(edad_deteccion, na.rm = TRUE),
    n                    = n(),
    .groups = "drop"
  ) |>
  print()

cant_villas <- df |> count(comuna, asentamiento, villa)
df |> count(villa) |> print()

# =============================================================================
# ETAPA A.2 — Barrio normalizado + geometría
# =============================================================================
cat("── A.2 Barrio normalizado (id_persona)...\n")

df_barrios_normalizado <- read_excel(
  buscar_en_proyecto("barrios_populares_normalizados.xlsx")
)

if (anyDuplicated(df_barrios_normalizado$id_persona)) {
  warning("barrios_populares_normalizados.xlsx tiene id_persona repetidos: ",
          "se conserva la primera fila de cada uno.")
  df_barrios_normalizado <- df_barrios_normalizado |> distinct(id_persona, .keep_all = TRUE)
}

n0 <- nrow(df)
df <- df |>
  left_join(
    df_barrios_normalizado |>
      select(id_persona, comuna_barrio, barrio_norm, geom_barrio),
    by = "id_persona"
  )
stopifnot("El join de barrios cambió el número de filas" = nrow(df) == n0)

# =============================================================================
# ETAPA A.3 — Indicadores censales por comuna
# =============================================================================
cat("── A.3 Comuna (Censo 2022)...\n")

path_cu <- file.path(RUTA_PROC, "comuna_unido.csv")
if (!file.exists(path_cu)) stop("No existe ", path_cu, "\n  Ejecutar 03_unir_comuna.R")
comuna_unido <- read_csv(path_cu, show_col_types = FALSE)

df <- df |>
  mutate(nro_comuna = as.numeric(gsub("[^0-9]", "", comuna)))

df <- df |>
  left_join(comuna_unido, by = c("nro_comuna" = "numero_comuna"))
stopifnot("El join de comunas cambió el número de filas" = nrow(df) == n0)

# =============================================================================
# ETAPA A.4 — Indicadores censales por barrio popular (de 05)
# =============================================================================
cat("── A.4 Indicadores por barrio (Censo 2022)...\n")

indicadores_por_barrio <- read_csv(
  file.path(RUTA_PROC, "indicadores_por_barrio.csv"), show_col_types = FALSE
)

indicadores_por_barrio_v <- indicadores_por_barrio |>
  mutate(clave_barrio = norm_clave(nombre_barrio)) |>
  rename_with(~ paste0(.x, "_v"), -clave_barrio)

if (anyDuplicated(indicadores_por_barrio_v$clave_barrio)) {
  warning("Hay nombres de barrio repetidos en indicadores_por_barrio.csv: ",
          "se usa el primero de cada nombre.")
  indicadores_por_barrio_v <- indicadores_por_barrio_v |>
    distinct(clave_barrio, .keep_all = TRUE)
}

df <- df |>
  mutate(clave_barrio = norm_clave(barrio_norm)) |>
  left_join(indicadores_por_barrio_v, by = "clave_barrio") |>
  select(-clave_barrio)
stopifnot("El join de indicadores por barrio cambió el número de filas" = nrow(df) == n0)

sin_ind <- df |>
  filter(villa == 1, is.na(nombre_barrio_v)) |>
  distinct(barrio_norm) |> pull(barrio_norm)
if (length(sin_ind) > 0) {
  cat("   ⚠ villa==1 sin indicadores de barrio (nombre sin match):",
      paste(sin_ind, collapse = ", "), "\n")
}

df_backup <- df    # copia de seguridad

# =============================================================================
# ETAPA A.5 — Unir barrios / comunas con el análisis de acceso (06 y 07)
#
#   1. Join con analisis_barrios por nombre de barrio (solo se usa para villa==1).
#   2. Join con analisis_comunas por comuna (se usa para villa==0 o si el barrio
#      no trae dato).
#   3. De analisis_barrios se elimina la opción A de densidad y se renombra la
#      opción B sin el sufijo "_opB" (mismos nombres que analisis_comunas).
# =============================================================================
cat("── A.5 Unión con analisis_completo_barrios / comunas...\n")

df_orig <- df

COL_BARRIO <- "barrio_norm"
COL_COMUNA <- "nro_comuna"
COL_VILLA  <- "villa"

analisis_barrios_raw <- read_csv(
  file.path(RUTA_PROC, "analisis_completo_barrios.csv"), show_col_types = FALSE
)

analisis_barrios <- analisis_barrios_raw |>
  select(-matches("_opA_"), -any_of("area_km2_opA")) |>
  rename_with(~ str_replace(., "_opB_", "_"), matches("_opB_")) |>
  rename_with(~ str_replace(., "area_km2_opB", "area_km2"), any_of("area_km2_opB"))

analisis_comunas <- read_csv(
  file.path(RUTA_PROC, "analisis_completo_comunas.csv"), show_col_types = FALSE
)

cat(sprintf("   analisis_barrios: %d barrios x %d vars | analisis_comunas: %d comunas x %d vars\n",
            nrow(analisis_barrios), ncol(analisis_barrios),
            nrow(analisis_comunas), ncol(analisis_comunas)))

ids_barrio  <- c("id_barrio", "nombre_barrio", "tipo_barrio", "area_km2", "poblacion")
vars_analisis_barrio <- analisis_barrios |> select(-any_of(ids_barrio)) |> names()
vars_analisis_comuna <- analisis_comunas |>
  select(-any_of(c("numero_comuna", "barrios", "area_km2", "poblacion"))) |> names()

vars_solo_barrio <- setdiff(vars_analisis_barrio, vars_analisis_comuna)
vars_solo_comuna <- setdiff(vars_analisis_comuna, vars_analisis_barrio)
if (length(vars_solo_barrio) > 0)
  cat("   Variables solo en barrios:", paste(vars_solo_barrio, collapse = ", "), "\n")
if (length(vars_solo_comuna) > 0)
  cat("   Variables solo en comunas:", paste(vars_solo_comuna, collapse = ", "), "\n")

# Claves de join
analisis_barrios <- analisis_barrios |>
  mutate(join_key_barrio = norm_clave(nombre_barrio), match_barrio = TRUE)
if (anyDuplicated(analisis_barrios$join_key_barrio)) {
  warning("nombre_barrio repetido en analisis_completo_barrios.csv: ",
          "se usa el primero de cada nombre.")
  analisis_barrios <- analisis_barrios |> distinct(join_key_barrio, .keep_all = TRUE)
}
analisis_comunas <- analisis_comunas |>
  mutate(join_key_comuna = norm_clave(paste0("comuna ", numero_comuna)))

df_orig <- df_orig |>
  mutate(
    join_key_barrio = norm_clave(.data[[COL_BARRIO]]),
    join_key_comuna = if_else(
      is.na(.data[[COL_COMUNA]]),
      NA_character_,
      norm_clave(paste0("comuna ", .data[[COL_COMUNA]]))
    )
  )

# Columnas de df_orig que chocarían con las del join (se eliminan antes)
vars_traer_barrio <- analisis_barrios |>
  select(-any_of(c(ids_barrio, "join_key_barrio", "match_barrio"))) |> names()

cols_conflicto <- names(df_orig)[names(df_orig) %in% c(
  "area_km2", vars_traer_barrio,
  "codigo", "departamento",
  "n_agua_corriente", "pct_agua_corriente",
  "n_calidad_mat", "pct_calidad_mat",
  "n_clima_educ_bajo", "pct_clima_educ_bajo",
  "n_sec_completa", "pct_sec_completa",
  "n_hogares_hacinamiento", "pct_hogares_hacinamiento",
  "id_barrio_v", "tipo_barrio_v", "n_radios_v", "comunas_v", "total_viv_v",
  "n_agua_corriente_v", "n_calidad_mat_v", "n_clima_educ_bajo_v",
  "n_sec_completa_v", "n_edad_65_mas_v", "n_hacinamiento_v",
  "pct_agua_corriente_v", "pct_calidad_mat_v", "pct_clima_educ_bajo_v",
  "pct_sec_completa_v", "pct_edad_65_mas_v", "pct_hacinamiento_v"
)]
cat("   Columnas que se quitan de df_orig antes del join:", length(cols_conflicto), "\n")

df_con_barrio <- df_orig |>
  select(-any_of(cols_conflicto)) |>
  left_join(
    analisis_barrios |>
      select(join_key_barrio, match_barrio, area_km2_barrio = area_km2,
             all_of(vars_traer_barrio)),
    by = "join_key_barrio"
  ) |>
  mutate(match_barrio = coalesce(match_barrio, FALSE))
stopifnot("El join con analisis_barrios cambió el número de filas" = nrow(df_con_barrio) == n0)

n_villa_match   <- sum(df_con_barrio[[COL_VILLA]] == 1 &  df_con_barrio$match_barrio, na.rm = TRUE)
n_villa_nomatch <- sum(df_con_barrio[[COL_VILLA]] == 1 & !df_con_barrio$match_barrio, na.rm = TRUE)
cat(sprintf("   villa==1 con match en analisis_barrios: %d | sin match: %d\n",
            n_villa_match, n_villa_nomatch))
if (n_villa_nomatch > 0) {
  cat("   Barrios sin match:",
      paste(unique(df_con_barrio$barrio_norm[df_con_barrio[[COL_VILLA]] == 1 &
                                               !df_con_barrio$match_barrio]), collapse = ", "), "\n")
}

vars_traer_comuna <- analisis_comunas |>
  select(-any_of(c("numero_comuna", "barrios", "area_km2",
                   "poblacion", "join_key_comuna"))) |> names()

df_con_comuna <- df_con_barrio |>
  left_join(
    analisis_comunas |>
      select(join_key_comuna, area_km2_comuna = area_km2,
             poblacion_comuna = poblacion,
             all_of(vars_traer_comuna)),
    by     = "join_key_comuna",
    suffix = c("", "_com")
  )
stopifnot("El join con analisis_comunas cambió el número de filas" = nrow(df_con_comuna) == n0)

# villa==1 → valor del barrio; villa==0 → barrio si existe, si no comuna;
# villa NA → NA. La asignación conserva el tipo de cada columna.
es_villa <- which(df_con_comuna[[COL_VILLA]] == 1)
villa_na <- which(is.na(df_con_comuna[[COL_VILLA]]))
for (v in vars_traer_comuna) {
  v_com <- paste0(v, "_com")
  if (v_com %in% names(df_con_comuna)) {
    x_b <- df_con_comuna[[v]]
    x_c <- df_con_comuna[[v_com]]
    out <- dplyr::coalesce(x_b, x_c)
    out[es_villa] <- x_b[es_villa]
    out[villa_na] <- x_b[villa_na][NA_integer_]
    df_con_comuna[[v]] <- out
  }
}

df_final <- df_con_comuna |>
  mutate(
    area_km2 = if_else(.data[[COL_VILLA]] == 1, area_km2_barrio, area_km2_comuna),
    poblacion_src = case_when(
      .data[[COL_VILLA]] == 1 & !is.na(area_km2_barrio) ~ "barrio",
      .data[[COL_VILLA]] == 0 & !is.na(area_km2_comuna) ~ "comuna",
      TRUE                                              ~ NA_character_
    )
  ) |>
  select(-ends_with("_com"),
         -any_of(c("area_km2_barrio", "area_km2_comuna", "poblacion_comuna",
                   "join_key_barrio", "join_key_comuna", "match_barrio")))

cat(sprintf("   Datos de barrio: %d | de comuna: %d | sin datos: %d | filas: %d (input: %d)\n",
            sum(df_final$poblacion_src == "barrio", na.rm = TRUE),
            sum(df_final$poblacion_src == "comuna", na.rm = TRUE),
            sum(is.na(df_final$poblacion_src)), nrow(df_final), n0))

write_csv(df_final, file.path(RUTA_PROC, "dataset_unificado.csv"))
cat("✓ Exportado: dataset_unificado.csv en", RUTA_PROC, "\n")

# =============================================================================
# PASO EXTERNO (Stata) — del dataset_unificado.csv sale df_esw.csv
# =============================================================================
path_esw <- tryCatch(
  buscar_archivo("df_esw.csv", RAIZ),
  error = function(e) NA_character_
)
if (is.na(path_esw)) {
  stop("No encuentro df_esw.csv bajo ", RAIZ, ".\n",
       "  Correr el paso de Stata (rename_pct_unificar.do, etc.) sobre ",
       "dataset_unificado.csv y guardar df_esw.csv dentro del proyecto; luego ",
       "volver a correr este script (la etapa A es idempotente).")
}

# =============================================================================
# ETAPA C.1 — df_esw: geometrías, CPE y variables unificadas
# =============================================================================
cat("── C.1 df_esw desde:", path_esw, "\n")

df_esw <- read.csv(path_esw)

req_esw <- c("villa", "barrio_norm", "comuna_ok", "fuente_poblacion", "nro_com",
             "geom_barrio", "t_ors_p_cesac", "t_ors_p_hosp", "dias_ult_cons",
             "edad", "genero", "dbt", "dias_dispensa",
             "pct_nbi_v", "pct_hacinamiento_v", "pct_clima_educ_bajo_v", "pct_calidad_mat_v",
             "pct_clima_educ_bajo", "pct_calidad_mat")
falta_esw <- setdiff(req_esw, names(df_esw))
if (length(falta_esw) > 0) {
  stop("df_esw.csv no tiene: ", paste(falta_esw, collapse = ", "),
       "\n  Columnas parecidas: ",
       paste(grep("pct_|t_ors|dias|nro_com|geom", names(df_esw), value = TRUE), collapse = ", "))
}

df_esw <- df_esw |>
  mutate(barrio_ok = if_else(fuente_poblacion == "Barrio_Villa", barrio_norm, comuna_ok))

# NBI y hacinamiento de comuna: se reemplazan por los de comuna_unido (03)
df_esw <- df_esw |>
  select(-any_of(c("pct_hogares_nbi", "pct_hogares_NBI",
                   "pct_hogares_hacinamiento"))) |>
  left_join(
    comuna_unido |> select(numero_comuna, pct_hogares_NBI, pct_hogares_hacinamiento),
    by = c("nro_com" = "numero_comuna")
  )

# ── CPE por comuna (03b) ──────────────────────────────────────────────────────
path_cpe <- file.path(RUTA_PROC, "cpe_comuna.csv")
if (!file.exists(path_cpe)) stop("No existe ", path_cpe, "\n  Ejecutar 03b_cpe_por_comuna.R")
cpe_comuna <- read_csv(path_cpe, show_col_types = FALSE)

n_esw0 <- nrow(df_esw)
df_esw <- df_esw |>
  select(-any_of(c("pct_cpe_c", "n_cpe_c", "w_cpe"))) |>
  left_join(
    cpe_comuna |> select(numero_comuna, pct_cpe_c, n_cpe_c = n_cpe),
    by = c("nro_com" = "numero_comuna")
  )
stopifnot("El join de CPE cambió el número de filas" = nrow(df_esw) == n_esw0)

if (any(is.na(df_esw$pct_cpe_c) & !is.na(df_esw$nro_com))) {
  warning(sum(is.na(df_esw$pct_cpe_c) & !is.na(df_esw$nro_com)),
          " pacientes con nro_com válido y sin pct_cpe_c (¿comuna fuera de 1..15?).")
}

# Peso de post-estratificación por comuna: participación de la comuna en la
# población CPE de CABA / participación de la comuna en la muestra.
#   w_cpe = (N_CPE_c / sum N_CPE) / (n_c / n)      (promedio de w_cpe = 1)
# (El cociente evita que comunas muy sobre-representadas en la muestra pesen
#  más solo por tener más pacientes. Ver Solon, Haider & Wooldridge 2015,
#  J. Human Resources 50(2): 301-316, https://doi.org/10.3368/jhr.50.2.301)
# Requiere n_cpe en cpe_comuna.csv (solo está si 03b encontró el Excel de Redatam).
if (!all(is.na(df_esw$n_cpe_c))) {
  df_esw <- df_esw |>
    mutate(share_pob = n_cpe_c / sum(cpe_comuna$n_cpe, na.rm = TRUE)) |>
    group_by(nro_com) |>
    mutate(share_muestra = n() / n_esw0) |>
    ungroup() |>
    mutate(w_cpe = share_pob / share_muestra) |>
    select(-share_pob, -share_muestra)
  cat(sprintf("   w_cpe: media %.3f | min %.3f | max %.3f\n",
              mean(df_esw$w_cpe, na.rm = TRUE),
              min(df_esw$w_cpe, na.rm = TRUE), max(df_esw$w_cpe, na.rm = TRUE)))
} else {
  df_esw$w_cpe <- NA_real_
  message("cpe_comuna.csv no trae n_cpe (Excel de Redatam no encontrado): w_cpe = NA.")
}

# ── Geometrías ────────────────────────────────────────────────────────────────
path_cp <- file.path(RUTA_PROC, "comunas_proj.rds")
if (!file.exists(path_cp)) stop("No existe ", path_cp, "\n  Ejecutar 01_preparar_datos.R")
comunas_proj <- readRDS(path_cp)

df_esw <- df_esw |>
  left_join(comunas_proj |> select(numero_comuna, geometry),
            by = c("nro_com" = "numero_comuna"))

df_esw <- df_esw |>
  mutate(
    geom_barrio_sfc = st_as_sfc(
      ifelse(is.na(geom_barrio), "GEOMETRYCOLLECTION EMPTY", geom_barrio),
      crs = 4326                       # geom_barrio es WKT en WGS84
    ) |> st_transform(crs = CRS_PROY),
    geometry = st_transform(geometry, crs = CRS_PROY),
    geom_ok = if_else(villa == 1, geom_barrio_sfc, geometry)
  ) |>
  select(-geom_barrio_sfc)

# Exportar df_esw SIN columnas de geometría. Antes se pasaban a WKT, pero un
# polígono en WKT supera el límite de 32.767 caracteres por celda de Excel
# (writexl falla o trunca). Para el script 09 solo hacen falta los atributos;
# las geometrías viven en comunas_proj.rds / df_esw.csv.
df_esw |>
  sf::st_drop_geometry() |>
  select(-any_of(c("geometry", "geom_ok", "geom_barrio"))) |>
  select(where(~ !inherits(.x, c("sfc", "list")))) |>
  write_xlsx(file.path(RUTA_PROC, "data_revisada_ok.xlsx"))

# =============================================================================
# ETAPA C.2 — Mapa de casos por comuna y barrio
# =============================================================================
cat("── C.2 Mapas de casos...\n")

resumen_barrio <- df_esw |>
  group_by(barrio_ok) |>
  summarise(
    n_casos = n(),
    fuente  = first(fuente_poblacion),
    geom_ok = first(geom_ok),
    .groups = "drop"
  ) |>
  # comunas primero, villas al final: geom_sf dibuja en orden de filas y las
  # villas (chicas) quedarían tapadas por las comunas
  arrange(fuente == "Barrio_Villa") |>
  st_as_sf(sf_column_name = "geom_ok")

mapa_casos <- ggplot(resumen_barrio) +
  geom_sf(aes(fill = n_casos), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "N",
                       na.value = "grey85") +
  labs(title   = "Distribución de casos por barrio — CABA",
       caption = "Fuente: datos propios") +
  theme_void(base_size = 11) +
  theme(
    plot.title        = element_text(size = 12, face = "plain", margin = margin(b = 8)),
    plot.caption      = element_text(size = 8, color = "grey50", margin = margin(t = 6)),
    legend.position   = "right",
    legend.key.height = unit(1.2, "cm"),
    legend.key.width  = unit(0.35, "cm"),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    plot.margin       = margin(10, 10, 10, 10)
  )
print(mapa_casos)
guardar_pdf(mapa_casos, "mapa_casos_por_barrio", width = 8, height = 8)

# ── Con etiquetas "(n=…)" en villas con > 100 casos ──────────────────────────
UMBRAL_ETIQUETA <- 100

centroides_villa <- resumen_barrio |>
  filter(fuente == "Barrio_Villa", !st_is_empty(geom_ok)) |>
  st_make_valid() |>
  (\(x) suppressWarnings(st_centroid(x)))()

coords_villa <- centroides_villa |>
  mutate(
    x     = st_coordinates(geom_ok)[, 1],
    y     = st_coordinates(geom_ok)[, 2],
    label = paste0(barrio_ok, "\n(n=", n_casos, ")")
  ) |>
  st_drop_geometry()

# Mapa en metros (EPSG:22185): el padding es proporcional al tamaño del mapa
bbox  <- st_bbox(resumen_barrio)
pad_x <- 0.30 * (bbox["xmax"] - bbox["xmin"])
pad_y <- 0.15 * (bbox["ymax"] - bbox["ymin"])
lim_x <- unname(c(bbox["xmin"] - pad_x, bbox["xmax"] + pad_x))
lim_y <- unname(c(bbox["ymin"] - pad_y, bbox["ymax"] + pad_y))

mapa_casos_etiq <- ggplot(resumen_barrio) +
  geom_sf(aes(fill = n_casos), color = "white", linewidth = 0.2) +
  geom_label_repel(
    data               = coords_villa |> filter(n_casos > UMBRAL_ETIQUETA),
    aes(x = x, y = y, label = label),
    size               = 2.3,
    color              = "black",
    fill               = NA,
    label.size         = 0,
    label.padding      = unit(0.2, "lines"),
    segment.color      = "grey40",
    segment.size       = 0.35,
    segment.linetype   = "solid",
    min.segment.length = 0,
    box.padding        = 0.6,
    point.padding      = 0.3,
    force              = 8,
    force_pull         = 0.1,
    xlim               = lim_x,
    ylim               = lim_y,
    direction          = "both",
    seed               = 123
  ) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "",
                       na.value = "grey85") +
  coord_sf(xlim = lim_x, ylim = lim_y) +
  labs(
    title   = "Distribución de casos de la muestra por comuna y barrio",
    caption = paste0("Nota: etiquetas en villas con más de ", UMBRAL_ETIQUETA,
                     " casos. Fuente: datos propios")
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title        = element_text(size = 12, face = "bold", family = "serif",
                                     hjust = 0.5, margin = margin(b = 8)),
    plot.caption      = element_text(size = 8, family = "serif", color = "grey40",
                                     hjust = 0, margin = margin(t = 6)),
    legend.position   = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(0.4, "cm"),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    plot.margin       = margin(10, 10, 10, 10)
  )
print(mapa_casos_etiq)
guardar_pdf(mapa_casos_etiq, "mapa_casos_comuna_barrio_etiquetas", width = 10, height = 8)

# =============================================================================
# ETAPA C.3 — Variables unificadas (_t) y resumen por barrio / comuna
# =============================================================================
cat("── C.3 Resumen por barrio...\n")

vars_pct <- list(
  pct_nbi             = "pct_hogares_NBI",
  pct_hacinamiento    = "pct_hogares_hacinamiento",
  pct_clima_educ_bajo = "pct_clima_educ_bajo",
  pct_calidad_mat     = "pct_calidad_mat"
)

for (v in names(vars_pct)) {
  df_esw <- df_esw |> mutate(
    !!paste0(v, "_t") := if_else(
      fuente_poblacion == "Barrio_Villa",
      .data[[paste0(v, "_v")]],
      .data[[vars_pct[[v]]]]
    )
  )
}

resumen_barrios_amp <- df_esw |>
  group_by(barrio_ok) |>
  summarise(
    n_casos        = n(),
    nbi            = mean(pct_nbi_t,               na.rm = TRUE),
    calmat         = 100 - mean(pct_calidad_mat_t, na.rm = TRUE),
    climaedu       = mean(pct_clima_educ_bajo_t,   na.rm = TRUE),
    hacinamiento   = mean(pct_hacinamiento_t,      na.rm = TRUE),
    t_p_cesac      = mean(t_ors_p_cesac,           na.rm = TRUE),
    t_p_hosp       = mean(t_ors_p_hosp,            na.rm = TRUE),
    dias_ult_cons  = mean(dias_ult_cons,           na.rm = TRUE),
    l_dias_ultcons = mean(if_else(dias_ult_cons > 0, log(dias_ult_cons), NA_real_),
                          na.rm = TRUE),
    pct_cpe        = mean(pct_cpe_c,               na.rm = TRUE),
    fuente         = first(fuente_poblacion),
    geom_ok        = st_union(geom_ok),
    .groups        = "drop"
  ) |>
  st_as_sf(sf_column_name = "geom_ok")

resumen_barrios_amp <- resumen_barrios_amp |>
  filter(!st_is_empty(geom_ok)) |>
  st_cast("MULTIPOLYGON") |>
  arrange(fuente == "Barrio_Villa")          # villas encima de las comunas

# =============================================================================
# ETAPA C.4 — Cuatro mapas magma (calidad de materiales, NBI, hacinamiento, clima)
# =============================================================================
cat("── C.4 Mapas de indicadores...\n")

# (a) Versión facet
mapa_facet <- resumen_barrios_amp |>
  select(calmat, nbi, hacinamiento, climaedu, geom_ok) |>
  pivot_longer(cols = c(calmat, nbi, hacinamiento, climaedu),
               names_to = "variable", values_to = "valor") |>
  st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = valor), color = "white", linewidth = 0.2) +
  facet_wrap(~variable, ncol = 2) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 11),
        legend.position = "bottom", legend.key.width = unit(1.5, "cm")) +
  labs(fill = NULL)
guardar_pdf(mapa_facet, "mapas_indicadores_facet", width = 10, height = 10)

vars_labels <- c(
  calmat       = "Calidad de materiales (%)",
  nbi          = "NBI (%)",
  hacinamiento = "Hacinamiento (%)",
  climaedu     = "Clima educativo bajo (%)"
)

# (b) Un mapa por variable con wrap_plots
plots_ind <- imap(vars_labels, function(label, var) {
  resumen_barrios_amp |>
    select(all_of(var), geom_ok) |>
    st_as_sf(sf_column_name = "geom_ok") |>
    ggplot() +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    theme_void() +
    theme(
      plot.title       = element_text(face = "bold", size = 8, hjust = 0.5),
      legend.position  = "bottom",
      legend.key.width = unit(0.6, "cm"),
      legend.text      = element_text(size = 6)
    ) +
    labs(title = label, fill = NULL)
})
mapas_indicadores <- wrap_plots(plots_ind, ncol = 2)
print(mapas_indicadores)
guardar_pdf(mapas_indicadores, "mapas_indicadores_barrios", width = 9, height = 10)

# (c) ggpairs: Barrio Villa vs Comuna
df_plot <- resumen_barrios_amp |>
  st_drop_geometry() |>
  select(calmat, nbi, hacinamiento, climaedu, fuente) |>
  drop_na()

g_pairs <- ggpairs(
  data    = df_plot,
  columns = c("calmat", "nbi", "hacinamiento", "climaedu"),
  mapping = aes(color = fuente, alpha = 0.5),
  upper   = list(continuous = wrap("cor", size = 2)),
  lower   = list(continuous = wrap("points", size = 1.5)),
  diag    = list(continuous = "densityDiag")
)
print(g_pairs)
dev_pdf <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
dev_pdf(file.path(RUTA_FIG, "ggpairs_indicadores_fuente.pdf"), width = 9, height = 9)
print(g_pairs)
invisible(dev.off())
message("PDF guardado: ", file.path(RUTA_FIG, "ggpairs_indicadores_fuente.pdf"))

# =============================================================================
# ETAPA C.5 — Mapas por variable: barrios (comuna) + villas encima
# =============================================================================
villas  <- resumen_barrios_amp |> filter(fuente == "Barrio_Villa")
barrios <- resumen_barrios_amp |> filter(fuente != "Barrio_Villa")

make_map <- function(var, titulo, leyenda = NULL) {
  ggplot() +
    geom_sf(data = barrios, aes(fill = .data[[var]]), color = "white", linewidth = 0.2) +
    geom_sf(data = villas,  aes(fill = .data[[var]]), linewidth = 0.01) +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    theme_void() +
    theme(
      plot.title       = element_text(face = "bold", size = 8, hjust = 0.5),
      legend.position  = "bottom",
      legend.key.width = unit(0.6, "cm"),
      legend.text      = element_text(size = 6)
    ) +
    labs(title = titulo, fill = leyenda)
}

m_calmat <- make_map("calmat",       "Calidad de materiales (%)")
m_nbi    <- make_map("nbi",          "NBI (%)")
m_hacin  <- make_map("hacinamiento", "Hacinamiento (%)")
m_clima  <- make_map("climaedu",     "Clima educativo bajo (%)")

mapas_indicadores_villas <- (m_calmat | m_nbi) / (m_hacin | m_clima)
print(mapas_indicadores_villas)
guardar_pdf(mapas_indicadores_villas, "mapas_indicadores_villas_barrios", width = 9, height = 10)

# Tiempos de viaje y días desde la última consulta
m_cesac  <- make_map("t_p_cesac",      "Tiempo al CeSAC más cercano (min a pie)", "min")
m_hosp   <- make_map("t_p_hosp",       "Tiempo al Hospital más cercano (min a pie)", "min")
m_dias   <- make_map("dias_ult_cons",  "Días desde última consulta (promedio)", "días")
m_ldias  <- make_map("l_dias_ultcons", "Log días desde última consulta (promedio)", "log(días)")

mapas_tiempos <- m_cesac | m_hosp
print(mapas_tiempos)
guardar_pdf(mapas_tiempos, "mapas_tiempo_cesac_hospital", width = 11, height = 6)
guardar_pdf(m_dias,  "mapa_dias_ultima_consulta",     width = 7, height = 7)
guardar_pdf(m_ldias, "mapa_log_dias_ultima_consulta", width = 7, height = 7)

# CPE por comuna sobre el mapa (comunas; las villas quedan en gris)
m_cpe <- ggplot() +
  geom_sf(data = barrios, aes(fill = pct_cpe), color = "white", linewidth = 0.2) +
  geom_sf(data = villas, fill = "grey60", color = "grey30", linewidth = 0.05) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "% CPE") +
  labs(title = "Población con cobertura pública exclusiva por comuna (Censo 2022)") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 9, hjust = 0.5),
        legend.position = "bottom", legend.key.width = unit(1, "cm"))
guardar_pdf(m_cpe, "mapa_cpe_comunas_villas", width = 7, height = 7)

# =============================================================================
# ETAPA C.6 — Mapas de acceso: tiempo a CeSAC con capas de centros de salud
# =============================================================================
cat("── C.6 Mapas de acceso a centros de salud...\n")

path_tc <- file.path(RUTA_PROC, "todos_centros_sf.rds")
if (!file.exists(path_tc)) stop("No existe ", path_tc, "\n  Ejecutar 01_preparar_datos.R")
todos_centros_sf <- readRDS(path_tc)

cesacs <- todos_centros_sf |> filter(tipo_centro == "CeSAC")    |> st_transform(st_crs(barrios))
hosps  <- todos_centros_sf |> filter(tipo_centro == "Hospital") |> st_transform(st_crs(barrios))

lim_cesac <- range(villas$t_p_cesac, na.rm = TRUE)

base_mapa <- function(mostrar_leyenda = FALSE) {
  list(
    geom_sf(data = barrios, fill = "#F5F5F5", color = "black", linewidth = 0.1),
    geom_sf(data = villas, aes(fill = t_p_cesac), color = "black", linewidth = 0.05),
    scale_fill_gradientn(colours = c("yellow", "red"), na.value = "#CCCCCC",
                         limits = lim_cesac, name = "Minutos"),
    theme_void(base_size = 8),
    theme(
      legend.position  = if (mostrar_leyenda) "bottom" else "none",
      legend.key.width = unit(1, "cm"),
      legend.text      = element_text(size = 7),
      legend.title     = element_text(size = 7),
      plot.margin      = margin(4, 4, 4, 4)
    )
  )
}

pa <- ggplot() + base_mapa(FALSE) + labs(title = "(A)")
pb <- ggplot() + base_mapa(FALSE) + labs(title = "(B)") +
  geom_sf(data = cesacs, color = "#E67E22", shape = 21,
          fill = "white", size = 1.5, stroke = 0.6)
pc <- ggplot() + base_mapa(TRUE) + labs(title = "(C)") +
  geom_sf(data = cesacs, color = "#E67E22", shape = 21,
          fill = "white", size = 1.8, stroke = 0.6) +
  geom_sf(data = hosps, color = "#1A5276", shape = 23,
          fill = "#1A5276", size = 2, stroke = 0.5)

panel_acceso <- (pa | pb | pc) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title      = element_text(hjust = 0.5, size = 9, face = "bold"))
print(panel_acceso)
guardar_pdf(panel_acceso, "panel_acceso_cesac_hospital", width = 12, height = 5)

# =============================================================================
# ETAPA C.7 — Tabla descriptiva: Total | Villa==0 | Villa==1
# =============================================================================
cat("── C.7 Tabla descriptiva...\n")

tabla_gt <- df_esw |>
  mutate(
    villa_label = factor(villa, levels = c(0, 1), labels = c("No villa", "Villa")),
    genero      = factor(genero)
  ) |>
  select(
    villa_label, edad, genero, dbt,
    pct_nbi_t, pct_calidad_mat_t, pct_hacinamiento_t, pct_clima_educ_bajo_t,
    pct_cpe_c,
    t_ors_p_cesac, t_ors_p_hosp, dias_ult_cons, dias_dispensa
  ) |>
  tbl_summary(
    by = villa_label,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    label = list(
      edad                  ~ "Edad (años)",
      genero                ~ "Sexo",
      dbt                   ~ "Diabetes",
      pct_nbi_t             ~ "NBI (%)",
      pct_calidad_mat_t     ~ "Calidad de materiales (%)",
      pct_hacinamiento_t    ~ "Hacinamiento (%)",
      pct_clima_educ_bajo_t ~ "Clima educativo bajo (%)",
      pct_cpe_c             ~ "Cobertura pública exclusiva, comuna (%)",
      t_ors_p_cesac         ~ "Tiempo al CeSAC más cercano (min a pie)",
      t_ors_p_hosp          ~ "Tiempo al Hospital más cercano (min a pie)",
      dias_ult_cons         ~ "Días desde última consulta",
      dias_dispensa         ~ "Días desde última dispensa de medicación"
    ),
    missing = "no"
  ) |>
  add_overall(last = FALSE) |>
  modify_header(
    label  ~ "**Variable**",
    stat_0 ~ "**Total**",
    stat_1 ~ "**No villa**",
    stat_2 ~ "**Villa**"
  ) |>
  modify_spanning_header(c(stat_1, stat_2) ~ "**Residencia**") |>
  bold_labels() |>
  italicize_levels() |>
  modify_footnote(
    all_stat_cols() ~ "Media (DE) para variables continuas; N (%) para categóricas"
  ) |>
  as_gt() |>
  tab_options(
    table.font.names                  = "Times New Roman",
    table.font.size                   = px(12),
    table.border.top.style            = "solid",
    table.border.top.width            = px(2),
    table.border.top.color            = "black",
    table.border.bottom.style         = "solid",
    table.border.bottom.width         = px(2),
    table.border.bottom.color         = "black",
    column_labels.border.top.style    = "none",
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = "black",
    column_labels.border.lr.style     = "none",
    table.background.color            = "white",
    column_labels.background.color    = "white",
    row.striping.include_table_body   = FALSE,
    data_row.padding                  = px(4),
    column_labels.padding             = px(6)
  ) |>
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners())

tabla_gt

gtsave(tabla_gt, file.path(RUTA_FIG, "tabla_descriptiva.docx"))
gtsave(tabla_gt, file.path(RUTA_FIG, "tabla_descriptiva.html"))
# PDF (necesita webshot2 + Chrome/Edge); la versión PNG anterior se eliminó
tryCatch(
  gtsave(tabla_gt, file.path(RUTA_FIG, "tabla_descriptiva.pdf")),
  error = function(e) message("No se pudo generar tabla_descriptiva.pdf: ",
                              conditionMessage(e),
                              "\n  (install.packages('webshot2') y tener Chrome/Edge)")
)

cat("\n✓ mainscript terminado. Figuras en", RUTA_FIG, "\n")
