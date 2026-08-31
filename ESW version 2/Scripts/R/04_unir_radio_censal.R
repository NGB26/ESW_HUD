# =============================================================================
# Script: unir_radio_censal.R
# Descripción: Lee y une todos los archivos de indicadores a nivel de
#              radio censal (Redatam / INDEC Censo 2022, CABA).
#
# Estructura de cada xlsx:
#   - Fila 12   : encabezados en columna B → "Código | Total | Seleccionado | Porcentaje"
#   - Filas 13+ : datos (col A vacía, datos desde col B)
#   - Últimas 3 : pie INDEC (texto libre, se descartan)
#   - Código    : 8 dígitos (ej. 20070101 = Departamento 2007, Radio 01, Sector 01)
#
# Variables unidas
#   agua_corriente      radio_agua_corriente.xlsx
#   calidad_materiales  radio_calidad_materiales.xlsx
#   clima_educ_bajo     radio_clima_educativo_bajo.xlsx
#   NBI                 radio_NBI.xlsx
#   sec_completa_mas    radio_secundaria_completa.xlsx
#   edad_65_mas         radio_edad_65_o_mas.xlsx
#   hacinamiento        radio_hacinamiento.xlsx
#   cond_sanitarias_A   radio_cond_sanitarias_A.xlsx
#   cond_sanitarias_B   radio_cond_sanitarias_B.xlsx
#
# Output:
#   radio_censal_unido.csv   — tabla larga con todas las variables
#   radio_censal_unido.xlsx  — mismo contenido en Excel
#
# Dependencias: readxl, dplyr, tidyr, writexl
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(sf)
library(readr)
library(tidyverse)

# ── Configuración ─────────────────────────────────────────────────────────────

# Carpeta donde están los xlsx (ajustar si es necesario)
RUTA <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/ESW version 2/data/raw/censo_2022"

# Definición de archivos: nombre_archivo -> nombre_variable de destino
# Cada fila tiene: archivo, variable_total, variable_n, variable_pct
#   variable_total = denominador (total viviendas del radio)
#   variable_n     = numerador  (viviendas con la característica)
#   variable_pct   = porcentaje calculado por Redatam

archivos_radio <- tribble(
  ~archivo,                           ~var_total,           ~var_n,                    ~var_pct,
  "radio_agua_corriente.xlsx",        "total_viv",          "n_agua_corriente",         "pct_agua_corriente",
  "radio_calidad_materiales.xlsx",    "total_viv",          "n_calidad_mat",            "pct_calidad_mat",
  "radio_clima_educativo_bajo.xlsx",  "total_viv",          "n_clima_educ_bajo",        "pct_clima_educ_bajo",
  "radio_NBI.xlsx",                   "total_viv",          "n_NBI",                    "pct_NBI",
  "radio_secundaria_completa.xlsx",   "total_viv",          "n_sec_completa",           "pct_sec_completa",
  "radio_edad_65_o_mas.xlsx",         "total_viv",          "n_edad_65_mas",            "pct_edad_65_mas",
  "radio_hacinamiento.xlsx",          "total_viv",          "n_hacinamiento",           "pct_hacinamiento",
  "radio_cond_sanitarias_A.xlsx",     "total_viv",          "n_cond_san_A",             "pct_cond_san_A",
  "radio_cond_sanitarias_B.xlsx",     "total_viv",          "n_cond_san_B",             "pct_cond_san_B"
)

# ── Función lectora ────────────────────────────────────────────────────────────

leer_radio <- function(archivo, var_total, var_n, var_pct) {

  ruta_completa <- file.path(RUTA, archivo)

  # Los xlsx de Redatam tienen:
  #   - col A vacía, datos desde col B
  #   - fila 12 = encabezados (Código | Total | Seleccionado | Porcentaje)
  #   - fila 13 en adelante = datos
  #   - últimas 3 filas = pie INDEC (texto, se filtran por tipo de Código)
  df_raw <- read_excel(
    ruta_completa,
    col_names = FALSE,     # leemos sin encabezado para controlar todo
    skip       = 11        # saltamos filas 1-11 (metadatos Redatam)
  )

  # Después del skip, fila 1 = encabezados, fila 2 en adelante = datos
  # Las columnas útiles son B, C, D, E → posiciones 2, 3, 4, 5 del df
  df <- df_raw |>
    slice(-1) |>                        # descartar fila de encabezados (ya sabemos qué son)
    select(codigo = 2,                  # col B: código radio (8 dígitos)
           total  = 3,                  # col C: total viviendas
           n      = 4,                  # col D: viviendas con característica
           pct    = 5) |>               # col E: porcentaje
    filter(
      !is.na(codigo),
      suppressWarnings(!is.na(as.numeric(codigo)))   # descarta filas INDEC (texto)
    ) |>
    mutate(
      codigo = as.integer(codigo),
      total  = as.numeric(total),
      n      = as.numeric(n),
      pct    = as.numeric(pct)
    )

  # Renombrar columnas con nombres de variable
  names(df)[names(df) == "total"] <- var_total
  names(df)[names(df) == "n"]     <- var_n
  names(df)[names(df) == "pct"]   <- var_pct

  df
}

# ── Lectura y merge ────────────────────────────────────────────────────────────

# Leer el primer archivo como base
cat("Leyendo archivos de radio censal...\n")

base <- leer_radio(
  archivos_radio$archivo[1],
  archivos_radio$var_total[1],
  archivos_radio$var_n[1],
  archivos_radio$var_pct[1]
)

cat(sprintf("  [1/9] %s — %d radios\n", archivos_radio$archivo[1], nrow(base)))

# Unir el resto con full_join para detectar eventuales radios ausentes
resultado <- base

for (i in seq(2, nrow(archivos_radio))) {

  df_nuevo <- leer_radio(
    archivos_radio$archivo[i],
    archivos_radio$var_total[i],
    archivos_radio$var_n[i],
    archivos_radio$var_pct[i]
  ) |>
    # El total de viviendas es el mismo en todos → lo eliminamos antes del join
    # para no duplicarlo (lo mantenemos solo del primer archivo)
    select(-matches("^total_viv$"))

  resultado <- full_join(resultado, df_nuevo, by = "codigo")

  cat(sprintf("  [%d/9] %s — %d radios\n",
              i, archivos_radio$archivo[i], nrow(df_nuevo)))
}

# ── Derivar identificadores geográficos desde el código ───────────────────────
# Código radio (8 dígitos): PPMMRRSS
#   PP = Provincia (02 = CABA, pero Redatam usa 2007 etc → ver abajo)
# Estructura real Redatam CABA: AAAABBCC
#   AAAA = código departamento/comuna (ej. 2007 = Comuna 1)
#   BB   = fracción censal
#   CC   = radio censal
# Extraemos:
#   cod_comuna   = primeros 4 dígitos  (ej. 2007)
#   cod_fraccion = dígitos 5-6         (ej. 01)
#   cod_radio    = dígitos 7-8         (ej. 01)

datos_radio_censo <- resultado |>
  mutate(
    cod_departamento = as.integer(substr(as.character(codigo), 1, 4)),
    cod_fraccion     = as.integer(substr(as.character(codigo), 5, 6)),
    cod_radio        = as.integer(substr(as.character(codigo), 7, 8)),
    # Número de comuna: Redatam asigna 2007=C1, 2014=C2, ... 2105=C15
    # Fórmula: (cod_departamento - 2007) / 7 + 1
    numero_comuna    = ((cod_departamento - 2007) / 7) + 1
  ) |>
  select(
    codigo,
    cod_departamento,
    numero_comuna,
    cod_fraccion,
    cod_radio,
    total_viv,
    everything()
  )

# ── Resumen ────────────────────────────────────────────────────────────────────

cat("\n=== RESUMEN DATASET RADIO CENSAL ===\n")
cat(sprintf("  Radios totales:    %d\n",   nrow(resultado)))
cat(sprintf("  Variables:         %d\n",   ncol(resultado)))
cat(sprintf("  Comunas cubiertas: %d\n",   n_distinct(resultado$numero_comuna)))
cat(sprintf("  Columnas:\n"))
cat(paste0("    ", names(resultado), collapse = "\n"), "\n")

# Verificar que todos los archivos aportaron el mismo conjunto de radios
cat(sprintf("\n  Radios sin NA en n_agua_corriente: %d\n",
            sum(!is.na(resultado$n_agua_corriente))))

# ── Exportar ───────────────────────────────────────────────────────────────────

write.csv(datos_radio_censo,
          file      = file.path(RUTA, "radio_censal_unido.csv"),
          row.names = FALSE,
          fileEncoding = "UTF-8")

write_xlsx(datos_radio_censo,
           path = file.path(RUTA, "radio_censal_unido.xlsx"))

cat("\n✓ Archivos exportados:\n")
cat("   radio_censal_unido.csv\n")
cat("   radio_censal_unido.xlsx\n")


#### Agrego sección de carga de polígonos shp de radios censales en esta instancia para luego calcular los centroides por radio censal, 
#calcular distancias por centroide de radio censal y finalmente promediar por comuna 


# =============================================================================
# 1. CARGAR SHAPEFILE Y FILTRAR A CABA
# =============================================================================



PATH_SHP_RADIOS  <- file.path("C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/ESW version 2/data/raw/radios_censales_2022/shapefile", "radios2022_v1_0.shp")

cat("── 1. Cargando shapefile de radios censales...\n")

Sys.setenv(SHAPE_RESTORE_SHX = "YES")
radios_nac <- st_read(PATH_SHP_RADIOS, quiet = TRUE)
cat(sprintf("   Total nacional: %d radios\n", nrow(radios_nac)))

# Filtrar CABA (PROV == "02") y construir el código Redatam desde los campos del DBF:
#   codigo_redatam = (2000 + DEPTO) * 10000 + FRAC * 100 + RADIO
radios_sf <- radios_nac |>
  filter(PROV == "02") |>
  mutate(
    codigo_redatam = (2000L + as.integer(DEPTO)) * 10000L +
      as.integer(FRAC) * 100L +
      as.integer(RADIO),
    numero_comuna  = as.integer(DEPTO)   # 7 = C1, 14 = C2, ..., 105 = C15
  ) |>
  select(
    codigo_redatam,
    numero_comuna,
    link       = LINK,
    nom_depto  = NOMDEPTO,
    fraccion   = FRAC,
    radio      = RADIO,
    tipo_radio = TIPO,
    geometry
  )

cat(sprintf("   CABA: %d radios\n", nrow(radios_sf)))


# =============================================================================
# 2. UNIR SHAPEFILE CON DATOS DEL CENSO (radio_censal_unido.csv)
# =============================================================================

cat("── 2. Uniendo shapefile con indicadores del Censo 2022...\n")

datos_radio <- datos_radio_censo 
# Join exacto por codigo_redatam (match perfecto 3820 = 3820)
radios_con_datos <- radios_sf |>
  left_join(datos_radio, by = c("codigo_redatam" = "codigo"))

n_matched <- sum(!is.na(radios_con_datos$total_viv))
cat(sprintf("   Radios con datos de Censo: %d / %d\n", n_matched, nrow(radios_sf)))

# =============================================================================
# 3. CALCULO LOS CENTROIDES DE LOS RADIOS CENSALES
# =============================================================================


# Reproyectar a POSGAR 2007 (metros) para cálculos de distancia y área
radios_proj <- radios_con_datos |> st_transform(22185)

# Área en km² (polígono original)
radios_proj <- radios_proj |>
  mutate(area_km2_opA = as.numeric(st_area(geometry)) / 1e6)


centroides_proj <- radios_proj |>
  st_centroid() |>
  select(codigo_redatam, numero_comuna.x)

cat(sprintf("   %d centroides calculados\n", nrow(centroides_proj)))


# =============================================================================
# 4. CARGAR HOSPITALES Y CENTROS DE SALUD
# =============================================================================

# CRS oficial catastro CABA (reemplaza "0 de Flores", vigente desde 2020)
# https://epsg.io/9498
crs_caba2019 <- "+proj=tmerc +lat_0=-34.6292666666667 +lon_0=-58.4633083333333 +k=1 +x_0=20000 +y_0=70000 +ellps=WGS84 +units=m +no_defs"
# alternativa si tu PROJ ya tiene el código EPSG registrado: crs = 9498

hospitales_sf <- read_csv(
  file.path(RUTA_hosp, "hospitales.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
) |>
  mutate(
    tipo_centro   = "Hospital",
    nombre_centro = fna,
    comuna_texto  = paste0("Comuna ", com)
  ) |>
  st_as_sf(wkt = "geometry", crs = crs_caba2019) |>
  select(tipo_centro, nombre_centro, comuna_texto) |>
  st_transform(22185)

# =============================================================================
# 4.1 CARGAR CENTROS WGS84
# =============================================================================

cat("── 4. Cargando CMB, CeSAC y estaciones saludables...\n")

leer_wgs84 <- function(path, tipo, col_nombre = "nombre", col_comuna = "comuna") {
  df <- read_csv(path, show_col_types = FALSE,
                 locale = locale(encoding = "UTF-8"))
  sf_obj <- df |>
    mutate(geometry = st_as_sfc(geometry, crs = 4326)) |>
    st_sf(crs = 4326) |>
    mutate(
      tipo_centro   = tipo,
      nombre_centro = if (col_nombre %in% names(df)) .data[[col_nombre]] else NA_character_,
      comuna_texto  = if (col_comuna  %in% names(df)) as.character(.data[[col_comuna]]) else NA_character_
    ) |>
    select(tipo_centro, nombre_centro, comuna_texto) |>
    st_transform(22185)
  if (any(!st_is_valid(sf_obj))) sf_obj <- st_make_valid(sf_obj)
  sf_obj
}

cmb_sf <- leer_wgs84(file.path(RUTA_hosp, "centros_medicos_barriales.csv"),
                     tipo = "Centro Medico Barrial")
cesac_sf <- leer_wgs84(file.path(RUTA_hosp, "centros_salud_nivel_1_cesac.csv"),
                       tipo = "CeSAC")
estaciones_sf <- leer_wgs84(file.path(RUTA_hosp, "estaciones_saludables.csv"),
                            tipo = "Estacion Saludable")

todos_centros_sf <- bind_rows(hospitales_sf, cmb_sf, cesac_sf, estaciones_sf)

cat(sprintf("   Total centros: %d (Hosp: %d | CMB: %d | CeSAC: %d | Est: %d)\n",
            nrow(todos_centros_sf),
            nrow(hospitales_sf), nrow(cmb_sf),
            nrow(cesac_sf), nrow(estaciones_sf)))

tipos <- c("Hospital", "Centro Medico Barrial", "CeSAC", "Estacion Saludable")

# =============================================================================
# PASO 1 — CENTROS DENTRO O MÁS CERCANOS A CADA BARRIO
# =============================================================================
#
# Para cada centro: asignar el barrio popular más cercano (sjoin por distancia).
# Esto es informativo — el análisis principal de densidad usa el polígono/buffer.

cat("── 5. Asignando centros a barrios más cercanos...\n")

centros_con_radio <- st_join(
  todos_centros_sf,
  radios_proj |> select(codigo_redatam, numero_comuna.x),
  join   = st_nearest_feature   # cada centro al barrio más cercano
)

centros_por_radio <- centros_con_radio |>
  st_drop_geometry() |>
  select(codigo_redatam, numero_comuna.x, tipo_centro, nombre_centro, comuna_texto) |>
  arrange(codigo_redatam, tipo_centro, nombre_centro)

cat(sprintf("   Centros asignados: %d\n", nrow(centros_por_radio)))

# =============================================================================
# PASO 2 y 3 — DISTANCIA Y TIEMPO AL CENTRO MÁS CERCANO POR TIPO
# =============================================================================

cat("── 6. Calculando distancias y tiempos al centro más cercano por tipo...\n")

calcular_acceso <- function(tipo_sel) {
  centros_tipo <- todos_centros_sf |> filter(tipo_centro == tipo_sel)
  
  if (nrow(centros_tipo) == 0) {
    return(tibble(
      id_radio = radios_proj$codigo_redatam, tipo_centro = tipo_sel,
      dist_eucl_m = NA_real_, dist_eucl_km = NA_real_,
      dist_red_km = NA_real_, tiempo_auto_min = NA_real_,
      tiempo_transp_min = NA_real_
    ))
  }
  
  
  
  
  # ── Parámetros ────────────────────────────────────────────────────────────────
  VEL_AUTO_KMH   <- 20
  VEL_TRANSP_KMH <- 12
  FACTOR_TORT    <- 1.3
  BUFFER_M       <- 500    # metros de expansión para opción B
  
  
  # Matriz de distancias: filas = centroides de barrios, cols = centros del tipo
  dist_matrix <- st_distance(centroides_proj, centros_tipo)
  
  tibble(
    id_radio         = centroides_proj$codigo_redatam,
    tipo_centro       = tipo_sel,
    dist_eucl_m       = as.numeric(apply(dist_matrix, 1, min, na.rm = TRUE)),
    dist_eucl_km      = round(dist_eucl_m / 1000, 3),
    dist_red_km       = round(dist_eucl_km * FACTOR_TORT, 3),
    tiempo_auto_min   = round(dist_red_km / VEL_AUTO_KMH   * 60, 1),
    tiempo_transp_min = round(dist_red_km / VEL_TRANSP_KMH * 60, 1)
  )
}

distancias_tiempos <- bind_rows(lapply(tipos, calcular_acceso))

cat("\n   Resumen de acceso por tipo (promedio entre barrios):\n")
print(
  distancias_tiempos |>
    group_by(tipo_centro) |>
    summarise(
      dist_prom_km       = round(mean(dist_eucl_km,     na.rm = TRUE), 2),
      dist_max_km        = round(max(dist_eucl_km,      na.rm = TRUE), 2),
      tiempo_auto_prom   = round(mean(tiempo_auto_min,  na.rm = TRUE), 1),
      tiempo_transp_prom = round(mean(tiempo_transp_min,na.rm = TRUE), 1)
    )
)





























