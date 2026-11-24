# =============================================================================
# Script: 01_preparar_datos.R   (versión corregida de preparar_datos.R)
#
# Construye los objetos que necesita el análisis posterior:
#   1. datos_comuna_censo — indicadores por comuna (Censo 2022); sale de
#                           03_unir_comuna.R (lee data/processed/comuna_unido.csv)
#   2. comunas_proj       — sf con polígonos de las 15 comunas (CRS 22185)
#   3. todos_centros_sf   — sf con los centros de salud (CRS 22185)
#   4. barrios_sf         — sf con barrios populares
#   5. df_final           — tabla de pacientes enriquecida con geometría del
#                           barrio (WKT) e indicadores del Censo a nivel comuna
#
# ORDEN DE EJECUCIÓN: 03 → 01   (04 es independiente de 01)
#
# Correcciones respecto a la versión anterior:
#   · Rutas centralizadas (00_config.R). Ya no depende de geo/*.geojson ni de
#     `files_censo22` (no existen en la estructura actual): usa los CSV con WKT
#     de data/raw, igual que 04/05.
#   · datos_comuna_censo se lee SIEMPRE del CSV (antes `if (!exists())` tomaba
#     objetos viejos del .RData sin avisar).
#   · Centros de salud: se construyen con cargar_centros_salud() (hospitales en
#     el CRS correcto).
#   · comunas_proj: ahora está realmente proyectado a 22185 y el área se calcula
#     en metros (antes se calculaba en grados con s2 apagado).
#   · Pacientes: los nombres de columnas se resuelven por alias (p. ej.
#     dias_desde_ultima_consulta / dias_ult_cons) y, si falta una columna
#     esencial, el error lista las columnas disponibles.
#   · Join de barrios: se verifica que el nombre de barrio sea único; antes un
#     nombre repetido multiplicaba filas de pacientes sin avisar.
# =============================================================================

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

library(readxl)
library(janitor)

sf_use_s2(FALSE)

norm_txt <- function(x) {
  x <- as.character(x)
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }
  toupper(trimws(x))
}

# =============================================================================
# BLOQUE 0 — Censo 2022 por comuna (siempre desde el CSV de 03)
# =============================================================================
cat("-- 0. Cargando datos del Censo 2022 por comuna...\n")

path_comuna <- file.path(RUTA_PROC, "comuna_unido.csv")
if (!file.exists(path_comuna)) {
  stop("No existe ", path_comuna, "\n  Ejecutar primero 03_unir_comuna.R")
}
datos_comuna_censo <- read_csv(path_comuna, show_col_types = FALSE)
cat(sprintf("   %d comunas | %d variables\n",
            nrow(datos_comuna_censo), ncol(datos_comuna_censo)))
stopifnot("comuna_unido.csv debe tener 15 comunas" = nrow(datos_comuna_censo) == 15)

# =============================================================================
# BLOQUE 1 — Polígonos de comunas → comunas_proj
# =============================================================================
cat("-- 1. Cargando polígonos de comunas...\n")

comunas_raw <- leer_wkt_csv(buscar_archivo("comunas.csv"), 4326, delim = ";") |>
  clean_names()

if (!"comuna" %in% names(comunas_raw)) {
  stop("comunas.csv no tiene columna 'comuna'. Columnas: ",
       paste(names(comunas_raw), collapse = ", "))
}

comunas_proj <- comunas_raw |>
  mutate(
    numero_comuna = as.integer(readr::parse_number(as.character(comuna))),
    barrios       = if ("barrios" %in% names(comunas_raw)) barrios else NA_character_
  ) |>
  select(numero_comuna, barrios, geometry) |>
  st_transform(CRS_PROY) |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  left_join(
    datos_comuna_censo |>
      select(numero_comuna, total,
             pct_agua_corriente, pct_calidad_mat,
             pct_clima_educ_bajo, pct_sec_completa,
             n_hogares_NBI,          pct_hogares_NBI,
             n_hogares_hacinamiento, pct_hogares_hacinamiento),
    by = "numero_comuna"
  )

cat(sprintf("   %d comunas cargadas | CRS: EPSG:%s\n",
            nrow(comunas_proj), st_crs(comunas_proj)$epsg))

# =============================================================================
# BLOQUE 2 — Centros de salud → todos_centros_sf
# =============================================================================
cat("-- 2. Cargando centros de salud...\n")

todos_centros_sf <- cargar_centros_salud() |>
  mutate(nro_comuna = as.integer(readr::parse_number(comuna_texto)))

cat(sprintf("   %d centros de salud | tipos: %s\n",
            nrow(todos_centros_sf),
            paste(sort(unique(todos_centros_sf$tipo_centro)), collapse = ", ")))

# =============================================================================
# BLOQUE 3 — df_final
# =============================================================================
cat("-- 3. Construyendo df_final...\n")

# ── 3a. Tabla de pacientes ────────────────────────────────────────────────────
# Se busca tabla_receptora_hta_bid_share.xlsx en cualquier subcarpeta del
# proyecto; si no está, se usa data/processed/data_revisada_ok.xlsx.
path_pac <- tryCatch(
  buscar_archivo("tabla_receptora_hta_bid_share.xlsx", RAIZ),
  error = function(e) file.path(RUTA_PROC, "data_revisada_ok.xlsx")
)
cat("   Pacientes desde:", path_pac, "\n")

pacientes_raw <- read_excel(path_pac) |> clean_names()
cat(sprintf("   %d filas | %d columnas\n", nrow(pacientes_raw), ncol(pacientes_raw)))

# Renombra la primera columna que coincida con algún alias
renombrar_alias <- function(df, nuevo, aliases, obligatoria = TRUE) {
  hit <- intersect(aliases, names(df))
  if (length(hit) == 0) {
    if (obligatoria) {
      stop("Falta la columna '", nuevo, "' (alias probados: ",
           paste(aliases, collapse = ", "), ").\n  Columnas disponibles: ",
           paste(names(df), collapse = ", "))
    }
    warning("No hay columna para '", nuevo, "'; se completa con NA.")
    df[[nuevo]] <- NA
    return(df)
  }
  if (hit[1] != nuevo) names(df)[names(df) == hit[1]] <- nuevo
  df
}

# tabla_receptora_hta_bid_share.xlsx no trae `villa`: mainscript.R la construye
# como !is.na(asentamiento). Si falta, se deriva igual acá (antes el script se
# detenía con "Falta la columna 'villa'" salvo que se usara data_revisada_ok.xlsx).
if (!"villa" %in% names(pacientes_raw) && "asentamiento" %in% names(pacientes_raw)) {
  message("La tabla de pacientes no trae 'villa': se deriva de !is.na(asentamiento).")
  pacientes_raw$villa <- as.integer(!is.na(pacientes_raw$asentamiento) &
                                      trimws(as.character(pacientes_raw$asentamiento)) != "")
}

pacientes_norm <- pacientes_raw |>
  renombrar_alias("villa",                      c("villa")) |>
  renombrar_alias("dias_desde_ultima_consulta", c("dias_desde_ultima_consulta",
                                                  "dias_ult_cons")) |>
  renombrar_alias("barrio_norm",                c("barrio_norm", "asentamiento",
                                                  "nombre_barrio", "barrio"),
                  obligatoria = FALSE) |>
  renombrar_alias("nro_comuna",                 c("nro_comuna", "numero_comuna",
                                                  "comuna_num", "comuna"),
                  obligatoria = FALSE) |>
  mutate(
    villa                      = suppressWarnings(as.integer(villa)),
    dias_desde_ultima_consulta = suppressWarnings(as.numeric(dias_desde_ultima_consulta)),
    barrio_norm                = norm_txt(barrio_norm),
    # parse_number: acepta "1", "Comuna 1"; textos como "Inválido" → NA
    nro_comuna                 = suppressWarnings(as.integer(readr::parse_number(as.character(nro_comuna))))
  )

# ── 3b. Barrios populares (polígonos, CSV con WKT) ───────────────────────────
barrios_sf <- leer_wkt_csv(buscar_archivo("barrios_populares_poligono.csv"), 4326) |>
  clean_names()
if (!"nombre" %in% names(barrios_sf)) {
  stop("barrios_populares_poligono.csv no tiene 'nombre'. Columnas: ",
       paste(names(barrios_sf), collapse = ", "))
}
barrios_sf <- barrios_sf |>
  mutate(barrio_norm = norm_txt(nombre),
         geom_barrio = st_as_text(geometry))
cat(sprintf("   %d barrios populares cargados\n", nrow(barrios_sf)))

# ── 3c. Geometría del barrio (una fila por nombre → no multiplica pacientes) ─
if (!"id" %in% names(barrios_sf)) {
  stop("barrios_populares_poligono.csv no tiene 'id'. Columnas: ",
       paste(names(barrios_sf), collapse = ", "))
}

barrios_geom <- barrios_sf |>
  st_drop_geometry() |>
  mutate(id_barrio = as.integer(id)) |>
  distinct(barrio_norm, .keep_all = TRUE) |>
  select(barrio_norm, id_barrio, geom_barrio)   # id_barrio: clave con 07/08

n_dup <- nrow(barrios_sf) - nrow(barrios_geom)
if (n_dup > 0) {
  warning(n_dup, " barrios con nombre repetido en el CSV; se usa la 1ª geometría de cada nombre.")
}

# Si la tabla de pacientes YA trae id_barrio / geom_barrio, el left_join los
# renombraría a .x/.y y `geom_barrio` dejaría de existir. Se preservan con otro
# nombre para que las columnas nuevas queden con el nombre esperado.
previas <- intersect(c("id_barrio", "geom_barrio"), names(pacientes_norm))
if (length(previas) > 0) {
  message("La tabla de pacientes ya trae: ", paste(previas, collapse = ", "),
          " → se renombran a *_orig antes del join.")
  pacientes_norm <- pacientes_norm |>
    rename(any_of(c(id_barrio_orig = "id_barrio", geom_barrio_orig = "geom_barrio")))
}

n_antes <- nrow(pacientes_norm)
pacientes_con_geom <- pacientes_norm |>
  left_join(barrios_geom, by = "barrio_norm")
stopifnot("El join de barrios cambió el número de filas" = nrow(pacientes_con_geom) == n_antes)

if (all(is.na(pacientes_con_geom$geom_barrio))) {
  warning("Ningún paciente matcheó con un barrio popular. Revisar la columna de ",
          "barrio de la tabla de pacientes (¿todo NA o con otro formato de nombre?).\n",
          "  Ejemplos en pacientes: ",
          paste(head(unique(na.omit(pacientes_norm$barrio_norm)), 5), collapse = " | "),
          "\n  Ejemplos en barrios:   ",
          paste(head(barrios_geom$barrio_norm, 5), collapse = " | "))
}

n_sin_geom <- sum(pacientes_con_geom$villa == 1 & is.na(pacientes_con_geom$geom_barrio),
                  na.rm = TRUE)
if (n_sin_geom > 0) {
  cat(sprintf("   ⚠ %d pacientes villa==1 sin geometría (nombres sin match)\n", n_sin_geom))
  sin_match <- pacientes_con_geom |>
    filter(villa == 1, is.na(geom_barrio)) |>
    distinct(barrio_norm) |> pull(barrio_norm)
  cat("     Sin match:", paste(sin_match, collapse = ", "), "\n")
  cat("     Barrios en CSV:", paste(sort(barrios_geom$barrio_norm), collapse = ", "), "\n")
} else {
  cat("   ✓ Todos los pacientes villa==1 tienen geometría asignada\n")
}

# ── 3d. Indicadores del Censo a nivel comuna ─────────────────────────────────
df_final <- pacientes_con_geom |>
  left_join(
    datos_comuna_censo |>
      select(numero_comuna, pct_hogares_NBI, pct_hogares_hacinamiento,
             pct_agua_corriente, pct_calidad_mat,
             pct_clima_educ_bajo, pct_sec_completa),
    by = c("nro_comuna" = "numero_comuna")
  )

columnas_req <- c("villa", "dias_desde_ultima_consulta", "barrio_norm",
                  "nro_comuna", "id_barrio", "geom_barrio")
if (!all(columnas_req %in% names(df_final))) {
  stop("df_final quedó sin: ", paste(setdiff(columnas_req, names(df_final)), collapse = ", "),
       "\n  Columnas actuales: ", paste(names(df_final), collapse = ", "))
}

cat(sprintf("   df_final: %d filas | %d columnas\n", nrow(df_final), ncol(df_final)))
cat(sprintf("   Villa==1: %d | Villa==0: %d | Villa NA: %d\n",
            sum(df_final$villa == 1, na.rm = TRUE),
            sum(df_final$villa == 0, na.rm = TRUE),
            sum(is.na(df_final$villa))))
cat(sprintf("   Sin comuna válida (nro_comuna NA): %d\n", sum(is.na(df_final$nro_comuna))))

# ── Resumen ───────────────────────────────────────────────────────────────────
cat("\n=== OBJETOS DISPONIBLES ===\n")
cat(sprintf("  datos_comuna_censo : %d comunas x %d variables\n",
            nrow(datos_comuna_censo), ncol(datos_comuna_censo)))
cat(sprintf("  comunas_proj       : %d comunas | sf | EPSG:%s\n",
            nrow(comunas_proj), st_crs(comunas_proj)$epsg))
cat(sprintf("  todos_centros_sf   : %d centros | EPSG:%s\n",
            nrow(todos_centros_sf), st_crs(todos_centros_sf)$epsg))
cat(sprintf("  df_final           : %d pacientes\n", nrow(df_final)))
cat(sprintf("  barrios_sf         : %d barrios populares (EPSG:4326)\n", nrow(barrios_sf)))

# Umbral operativo: >= 180 días sin consulta = perdido (paleta de mapas)
piso <- 180
cat(sprintf("\n  piso (días sin consulta) = %d\n", piso))

# Guardar a disco para que 08 no dependa de objetos en memoria / .RData
saveRDS(df_final,          file.path(RUTA_PROC, "df_final.rds"))
saveRDS(comunas_proj,      file.path(RUTA_PROC, "comunas_proj.rds"))
saveRDS(todos_centros_sf,  file.path(RUTA_PROC, "todos_centros_sf.rds"))
saveRDS(barrios_sf,        file.path(RUTA_PROC, "barrios_sf.rds"))
cat("  ✓ df_final, comunas_proj, todos_centros_sf, barrios_sf guardados en", RUTA_PROC, "\n")
