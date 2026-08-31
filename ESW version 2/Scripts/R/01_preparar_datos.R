# =============================================================================
# Script: preparar_datos.R
#
# Descripción: Construye los tres objetos que necesita analisis_de_datos.R:
#
#   1. datos_comuna_censo  — indicadores socioeconómicos por comuna (Censo 2022)
#                            Sale de unir_comuna_v2.R (ya debe estar ejecutado,
#                            o se lee desde el CSV exportado)
#
#   2. comunas_proj        — sf con polígonos de las 15 comunas de CABA
#
#   3. todos_centros_sf    — sf con todos los centros de salud de CABA
#                            (Hospitales, CeSACs, Centros Médicos Barriales,
#                             Estaciones Saludables)
#
#   4. df_final            — tabla de pacientes con HTA enriquecida con:
#                              · geometría del barrio popular (geom_barrio)
#                              · número de comuna (nro_comuna)
#                              · indicadores del Censo 2022 a nivel comuna
#
# PREREQUISITO: ejecutar unir_comuna_v2.R antes, o tener comuna_unido.csv
#               disponible en RUTA/files_censo22/
#
# Fuentes geoespaciales (archivos que debés tener en RUTA/geo/):
#   comunas_caba.geojson         — polígonos de comunas (GCBA datos abiertos)
#   barrios_populares_caba.geojson — polígonos de barrios populares / villas (GCBA)
#   centros_salud_caba.geojson   — puntos de centros de salud (GCBA datos abiertos)
#
#   Fuente sugerida: https://data.buenosaires.gob.ar/dataset/
#
# Dependencias: sf, dplyr, readr, readxl, janitor
# =============================================================================

library(sf)
library(dplyr)
library(readr)
library(readxl)
library(janitor)

sf_use_s2(FALSE)

# ── Rutas ─────────────────────────────────────────────────────────────────────

RUTA      <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2"
RUTA_GEO  <- file.path(RUTA, "geo")
RUTA_CENS <- file.path(RUTA, "files_censo22")

# =============================================================================
# BLOQUE 0 — Cargar datos del Censo 2022 por comuna
# =============================================================================
# Si unir_comuna_v2.R ya fue ejecutado, los datos están en el entorno como
# `datos_comuna_censo`. Si no, los levantamos desde el CSV exportado.

cat("-- 0. Cargando datos del Censo 2022 por comuna...\n")

if (!exists("datos_comuna_censo")) {
  datos_comuna_censo <- read_csv(
    file.path(RUTA_CENS, "comuna_unido.csv"),
    show_col_types = FALSE
  )
  cat("   Leído desde comuna_unido.csv\n")
} else {
  cat("   Objeto datos_comuna_censo ya disponible en el entorno\n")
}

cat(sprintf("   %d comunas | %d variables\n",
            nrow(datos_comuna_censo), ncol(datos_comuna_censo)))

# =============================================================================
# BLOQUE 1 — Polígonos de comunas  →  comunas_proj
# =============================================================================

cat("-- 1. Cargando polígonos de comunas...\n")

comunas_raw <- st_read(
  file.path(RUTA_GEO, "comunas_caba.geojson"),
  quiet = TRUE
) |>
  clean_names()

# Estandarizar: necesitamos numero_comuna (integer), barrios (texto), geometry
# Ajustá los nombres de columna según el archivo que uses de datos abiertos GCBA
comunas_proj <- comunas_raw |>
  rename_with(~ case_when(
    .x %in% c("comuna", "id_comuna", "nro_comuna") ~ "numero_comuna",
    .x %in% c("barrios", "barrio", "denominacion") ~ "barrios",
    TRUE ~ .x
  )) |>
  mutate(
    numero_comuna = as.integer(numero_comuna)
  ) |>
  # Unir indicadores del Censo 2022
  left_join(
    datos_comuna_censo |>
      select(numero_comuna, total,
             pct_agua_corriente, pct_calidad_mat,
             pct_clima_educ_bajo, pct_sec_completa,
             n_hogares_NBI,          pct_hogares_NBI,
             n_hogares_hacinamiento, pct_hogares_hacinamiento),
    by = "numero_comuna"
  ) |>
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6  # área en km²
  ) |>
  st_transform(4326)

cat(sprintf("   %d comunas cargadas | CRS: %s\n",
            nrow(comunas_proj), st_crs(comunas_proj)$input))

# =============================================================================
# BLOQUE 2 — Centros de salud  →  todos_centros_sf
# =============================================================================

cat("-- 2. Cargando centros de salud...\n")

# GCBA publica distintas capas por tipo de efector.
# Si tenés un solo archivo unificado, usá este bloque directo.
# Si tenés archivos separados, usá el bloque alternativo más abajo.

# ── Opción A: archivo único ────────────────────────────────────────────────
centros_raw <- st_read(
  file.path(RUTA_GEO, "centros_salud_caba.geojson"),
  quiet = TRUE
) |>
  clean_names()

# Estandarizar columnas según el GeoJSON del GCBA
# Ajustá los nombres si tu archivo tiene columnas distintas
todos_centros_sf <- centros_raw |>
  rename_with(~ case_when(
    .x %in% c("nombre", "establecimiento", "nombre_establecimiento") ~ "nombre_centro",
    .x %in% c("tipo",   "tipo_efector",    "clase")                 ~ "tipo_centro",
    .x %in% c("comuna", "nro_comuna",      "id_comuna")             ~ "nro_comuna",
    TRUE ~ .x
  )) |>
  mutate(
    # Normalizar nombres de tipo_centro para que coincidan con los iconos del mapa
    tipo_centro = case_when(
      grepl("hospital",              tipo_centro, ignore.case = TRUE) ~ "Hospital",
      grepl("cesac|centro de salud", tipo_centro, ignore.case = TRUE) ~ "CeSAC",
      grepl("barrial|cmb",           tipo_centro, ignore.case = TRUE) ~ "Centro Medico Barrial",
      grepl("saludable|estaci",      tipo_centro, ignore.case = TRUE) ~ "Estacion Saludable",
      TRUE ~ tipo_centro
    ),
    # Etiqueta de comuna para el popup
    comuna_texto = if_else(
      !is.na(nro_comuna),
      paste0("Comuna ", nro_comuna),
      NA_character_
    )
  ) |>
  st_transform(4326)

# ── Opción B: archivos separados por tipo (comentar/descomentar según corresponda)
# hospitales   <- st_read(file.path(RUTA_GEO, "hospitales.geojson"),   quiet = TRUE) |> mutate(tipo_centro = "Hospital")
# cesacs       <- st_read(file.path(RUTA_GEO, "cesacs.geojson"),        quiet = TRUE) |> mutate(tipo_centro = "CeSAC")
# cmb          <- st_read(file.path(RUTA_GEO, "cmb.geojson"),           quiet = TRUE) |> mutate(tipo_centro = "Centro Medico Barrial")
# estaciones   <- st_read(file.path(RUTA_GEO, "estaciones_saludables.geojson"), quiet = TRUE) |> mutate(tipo_centro = "Estacion Saludable")
# todos_centros_sf <- bind_rows(hospitales, cesacs, cmb, estaciones) |>
#   clean_names() |>
#   rename(nombre_centro = nombre) |>     # ajustar según columnas reales
#   mutate(comuna_texto = paste0("Comuna ", nro_comuna)) |>
#   st_transform(4326)

cat(sprintf("   %d centros de salud | tipos: %s\n",
            nrow(todos_centros_sf),
            paste(sort(unique(todos_centros_sf$tipo_centro)), collapse = ", ")))

# =============================================================================
# BLOQUE 3 — Construir df_final
# =============================================================================
# Fuente de pacientes: tabla_receptora_hta_bid_share.xlsx
#   Ruta: Re_ Informe alejandro macchia  (subcarpeta dentro de OneDrive IDB)
#
# Columnas que se usan de esta tabla:
#   villa                      — 0/1, residencia en barrio popular
#   dias_desde_ultima_consulta — días desde la última consulta registrada
#   barrio_norm                — nombre del barrio popular (solo villa==1)
#                                ⚠ Verificar el nombre exacto de esta columna
#                                  en el xlsx; renombrar abajo si difiere.
#   nro_comuna                 — número de comuna (1–15)
#                                ⚠ Ídem: verificar y renombrar si difiere.
#
# Lo que se agrega en este bloque:
#   geom_barrio  — WKT del polígono del barrio popular (join con barrios_sf)
#   indicadores del Censo 2022 a nivel comuna (join con datos_comuna_censo)
# =============================================================================

RUTA_PACIENTES <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Re_ Informe alejandro macchia"
)

cat("-- 3. Construyendo df_final...\n")

# ── 3a. Leer tabla de pacientes ───────────────────────────────────────────────

pacientes_raw <- read_excel(
  file.path(RUTA_PACIENTES, "tabla_receptora_hta_bid_share.xlsx")
) |>
  clean_names()   # convierte nombres a snake_case minúscula

cat(sprintf("   tabla_receptora_hta_bid_share.xlsx leída: %d filas | %d columnas\n",
            nrow(pacientes_raw), ncol(pacientes_raw)))

# Mostrar columnas disponibles para facilitar la verificación
cat("   Columnas disponibles:\n")
cat(paste0("     ", names(pacientes_raw), collapse = "\n"), "\n")

# ── 3b. Normalizar columnas clave ────────────────────────────────────────────
# ⚠ Si clean_names() cambió los nombres de 'barrio_norm' o 'nro_comuna',
#   ajustar los rename() de abajo con los nombres reales que aparecen arriba.

pacientes_norm <- pacientes_raw |>
  rename(
    # Descomentar y ajustar solo si el nombre en el xlsx es distinto:
    # barrio_norm = nombre_barrio,    # <- nombre real en el xlsx
    # nro_comuna  = comuna,           # <- nombre real en el xlsx
  ) |>
  mutate(
    villa                      = as.integer(villa),
    dias_desde_ultima_consulta = as.numeric(dias_desde_ultima_consulta),
    barrio_norm                = toupper(trimws(barrio_norm)),
    nro_comuna                 = as.integer(nro_comuna)
  )

# ── 3c. Polígonos de barrios populares ───────────────────────────────────────
# Fuente: GeoJSON de GCBA datos abiertos (barrios populares / villas CABA)

barrios_sf <- st_read(
  file.path(RUTA_GEO, "barrios_populares_caba.geojson"),
  quiet = TRUE
) |>
  clean_names() |>
  rename_with(~ case_when(
    .x %in% c("nombre", "nombre_barrio", "denominacion") ~ "barrio_norm",
    TRUE ~ .x
  )) |>
  mutate(
    barrio_norm = toupper(trimws(barrio_norm)),
    geom_barrio = st_as_text(geometry)   # WKT para unir al df de pacientes
  ) |>
  st_transform(4326)

cat(sprintf("   %d barrios populares cargados desde GeoJSON\n", nrow(barrios_sf)))

# ── 3d. Unir geometría del barrio (solo villa == 1) ──────────────────────────

barrios_geom <- barrios_sf |>
  st_drop_geometry() |>
  select(barrio_norm, geom_barrio)

pacientes_con_geom <- pacientes_norm |>
  left_join(barrios_geom, by = "barrio_norm")

# Diagnóstico de matches fallidos
n_sin_geom <- pacientes_con_geom |>
  filter(villa == 1, is.na(geom_barrio)) |>
  nrow()

if (n_sin_geom > 0) {
  cat(sprintf("   ⚠ %d pacientes villa==1 sin geometría (nombres sin match)\n",
              n_sin_geom))
  barrios_sin_match <- pacientes_con_geom |>
    filter(villa == 1, is.na(geom_barrio)) |>
    distinct(barrio_norm) |>
    pull(barrio_norm)
  cat("     Sin match:", paste(barrios_sin_match, collapse = ", "), "\n")
  cat("     Barrios en GeoJSON:", paste(sort(barrios_geom$barrio_norm), collapse = ", "), "\n")
} else {
  cat("   ✓ Todos los pacientes villa==1 tienen geometría asignada\n")
}

# ── 3e. Unir indicadores del Censo 2022 a nivel comuna ───────────────────────

df_final <- pacientes_con_geom |>
  left_join(
    datos_comuna_censo |>
      select(numero_comuna,
             pct_hogares_NBI,
             pct_hogares_hacinamiento,
             pct_agua_corriente,
             pct_calidad_mat,
             pct_clima_educ_bajo,
             pct_sec_completa),
    by = c("nro_comuna" = "numero_comuna")
  )

cat(sprintf("   df_final: %d filas | %d columnas\n",
            nrow(df_final), ncol(df_final)))
cat(sprintf("   Villa==1: %d | Villa==0: %d\n",
            sum(df_final$villa == 1, na.rm = TRUE),
            sum(df_final$villa == 0, na.rm = TRUE)))

# =============================================================================
# RESUMEN FINAL DE OBJETOS DISPONIBLES
# =============================================================================

cat("\n=== OBJETOS LISTOS PARA analisis_de_datos.R ===\n")
cat(sprintf("  datos_comuna_censo : %d comunas × %d variables\n",
            nrow(datos_comuna_censo), ncol(datos_comuna_censo)))
cat(sprintf("  comunas_proj       : %d comunas | sf | CRS %s\n",
            nrow(comunas_proj), st_crs(comunas_proj)$input))
cat(sprintf("  todos_centros_sf   : %d centros de salud\n",
            nrow(todos_centros_sf)))
cat(sprintf("  df_final           : %d pacientes\n",
            nrow(df_final)))
cat(sprintf("  barrios_sf         : %d barrios populares (disponible para mapas)\n",
            nrow(barrios_sf)))

# =============================================================================
# NOTA SOBRE piso (variable usada en analisis_de_datos.R, sección mapa 2)
# =============================================================================
# En analisis_de_datos.R aparece `piso` como el límite inferior de la paleta
# de color para días desde la última consulta. Se define aquí para que esté
# disponible cuando corras ese script.

piso <- 180   # valor operativo del estudio: ≥ 180 días sin consulta = perdido

cat(sprintf("\n  piso (días sin consulta) = %d\n", piso))
