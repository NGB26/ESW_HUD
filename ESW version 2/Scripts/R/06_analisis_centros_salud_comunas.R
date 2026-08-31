
library(sf)
library(dplyr)
library(tidyr)
library(readr)

# ── Configuración ─────────────────────────────────────────────────────────────

# =============================================================================
# Script: analisis_centros_salud_comunas.R
#
# Objetivo:
#   1. Consolidar todos los centros de salud por tipo asociados a cada comuna
#   2. Calcular la distancia desde el centroide de cada comuna al centro
#      de salud más cercano, por tipo
#   3. Estimar el tiempo de viaje en transporte público y privado al centro
#      más cercano, por tipo
#   4. Calcular densidad de centros de salud por km² y por habitante
#
# ── NOTA TÉCNICA SOBRE CRS DE HOSPITALES ────────────────────────────────────
#   El archivo hospitales.csv usa un sistema de coordenadas LOCAL de la Ciudad
#   de Buenos Aires (no es un EPSG estándar). Es una proyección equirectangular
#   simple con origen en (-58.6599°, -35.2641°):
#     lon = -58.6599 + x / (111320 × cos(-35.2641°))
#     lat = -35.2641 + y / 111320
#   Este origen fue determinado empíricamente a partir de hospitales con
#   coordenadas conocidas (ej. Hospital Garrahan en -58.3678°, -34.6360°).
#   Todos los demás archivos usan WGS84 (EPSG:4326) estándar.
#
# ── NOTA SOBRE TIEMPO DE VIAJE ───────────────────────────────────────────────
#   El cálculo de tiempos reales requiere una API de routing. Este script usa
#   velocidades promedio empíricas para CABA:
#     - Auto:              20 km/h (TomTom Traffic Index Buenos Aires 2023)
#     - Transporte público: 12 km/h (velocidad comercial colectivos + espera)
#   La distancia de red se estima como: dist_euclidiana × 1.3 (factor de
#   tortuosidad estándar para ciudades en cuadrícula, Boscoe et al. 2012).
#   Ver sección OPCIONAL al final para routing real con OpenRouteService o r5r.
#
# Entradas:
#   hospitales.csv, centros_medicos_barriales.csv,
#   centros_salud_nivel_1_cesac.csv, estaciones_saludables.csv, comunas.csv
#
# Outputs:
#   centros_por_comuna.csv          — todos los centros con tipo y comuna
#   distancias_tiempos_por_tipo.csv — distancia y tiempo al centro más cercano
#   densidad_por_tipo.csv           — densidad por km² y por 100k hab.
#   analisis_completo_comunas.csv   — dataset wide unido por comuna
#
# Dependencias: sf, dplyr, tidyr, readr, stringr
# =============================================================================

library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

sf_use_s2(FALSE)   # usar GEOS en lugar de S2 para evitar errores en geometrías complejas

# ── Rutas ─────────────────────────────────────────────────────────────────────
RUTA <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2"

# ── Parámetros de estimación de tiempos ──────────────────────────────────────
VEL_AUTO_KMH   <- 20    # velocidad promedio auto en CABA con tráfico
VEL_TRANSP_KMH <- 12    # velocidad promedio transporte público (incluye espera)
FACTOR_TORT    <- 1.3   # factor de tortuosidad red vial (cuadrícula urbana)

# ── Origen del sistema local de hospitales (determinado empíricamente) ────────
HOSP_LON0 <- -58.6599   # longitud del origen del sistema local
HOSP_LAT0 <- -35.2641   # latitud del origen del sistema local

# ── Población por comuna — Censo 2022 (INDEC) ────────────────────────────────

pob_comuna<-read_excel("C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2/files_censo22/pob_comuna.xlsx")
pob_comuna<-mutate(pob_comuna, numero_comuna=c(1:15))
pob_comuna<-rename(pob_comuna, poblacion="Total")
poblacion_comunas<-pob_comuna

# =============================================================================
# 1. CARGAR COMUNAS
# =============================================================================

cat("── 1. Cargando comunas...\n")

comunas_raw <- read_delim(
  file.path(RUTA, "comunas.csv"),
  delim = ";", show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

comunas_sf <- comunas_raw |>
  mutate(geometry = st_as_sfc(geometry, crs = 4326)) |>
  st_sf(crs = 4326) |>
  mutate(
    numero_comuna = as.integer(comuna),
    area_km2      = as.numeric(area) / 1e6
  ) |>
  select(numero_comuna, barrios, area_km2, geometry)

if (any(!st_is_valid(comunas_sf)))
  comunas_sf <- st_make_valid(comunas_sf)

# Reproyectar a POSGAR 2007 (metros) para distancias y áreas correctas
comunas_proj <- comunas_sf |> st_transform(22185)

# Centroides de comunas (en CRS proyectado)
centroides_proj <- comunas_proj |>
  st_centroid() |>
  select(numero_comuna)

cat(sprintf("   %d comunas | área total: %.1f km²\n",
            nrow(comunas_sf), sum(comunas_sf$area_km2)))

# =============================================================================
# 2. CARGAR HOSPITALES (sistema de coordenadas local → WGS84)
# =============================================================================

cat("── 2. Cargando hospitales (convirtiendo sistema local BA → WGS84)...\n")

hosp_raw <- read_csv(
  file.path(RUTA, "hospitales.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

# Extraer coordenadas X e Y del campo WKT "POINT (x y)"

hosp_coords <- hosp_raw |>
  mutate(
    m = str_match(geometry, "^POINT\\s*\\(\\s*([-0-9.]+)\\s+([-0-9.]+)\\s*\\)$"),
    x_local = as.numeric(m[, 2]),
    y_local = as.numeric(m[, 3])
  ) |>
  select(-m)


# Convertir sistema local → WGS84 usando el origen empírico
# lon = lon0 + x / (111320 * cos(lat0))
# lat = lat0 + y / 111320
cos_lat0 <- cos(HOSP_LAT0 * pi / 180)

hospitales_sf <- hosp_coords |>
  mutate(
    lon = HOSP_LON0 + x_local / (111320 * cos_lat0),
    lat = HOSP_LAT0 + y_local / 111320,
    tipo_centro   = "Hospital",
    nombre_centro = fna,
    # numero_comuna desde campo 'com' (número directo)
    comuna_texto  = paste0("Comuna ", com)
  ) |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  select(tipo_centro, nombre_centro, comuna_texto) |>
  st_transform(22185)

cat(sprintf("   %d hospitales cargados\n", nrow(hospitales_sf)))

# Verificar rango de coordenadas (debe estar dentro de CABA)
bbox_hosp <- st_bbox(hospitales_sf |> st_transform(4326))
cat(sprintf("   Bbox hospitales: lon [%.3f, %.3f], lat [%.3f, %.3f]\n",
            bbox_hosp["xmin"], bbox_hosp["xmax"],
            bbox_hosp["ymin"], bbox_hosp["ymax"]))

# =============================================================================
# 3. FUNCIÓN GENÉRICA PARA CENTROS EN WGS84
# =============================================================================

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
  
  if (any(!st_is_valid(sf_obj)))
    sf_obj <- st_make_valid(sf_obj)
  
  sf_obj
}

# =============================================================================
# 4. CARGAR CENTROS WGS84
# =============================================================================

cat("── 3. Cargando centros médicos barriales, CeSAC y estaciones...\n")

cmb_sf <- leer_wgs84(
  file.path(RUTA, "centros_medicos_barriales.csv"),
  tipo = "Centro Medico Barrial"
)

cesac_sf <- leer_wgs84(
  file.path(RUTA, "centros_salud_nivel_1_cesac.csv"),
  tipo = "CeSAC"
)

estaciones_sf <- leer_wgs84(
  file.path(RUTA, "estaciones_saludables.csv"),
  tipo = "Estacion Saludable"
)

cat(sprintf("   CMB: %d | CeSAC: %d | Estaciones: %d\n",
            nrow(cmb_sf), nrow(cesac_sf), nrow(estaciones_sf)))
# =============================================================================
# PASO 1 — CONSOLIDAR Y ASIGNAR COMUNA
# =============================================================================
#
# Lógica de asignación (en orden de prioridad):
#   1. Variable original del CSV (campo comuna_texto), si no está vacía
#   2. Fallback: sjoin espacial centroide-en-polígono de comunas
#   3. Fallback final: comuna del polígono más cercano (para centros en borde)

cat("── 4. Consolidando y asignando comunas...\n")

todos_centros_sf <- bind_rows(hospitales_sf, cmb_sf, cesac_sf, estaciones_sf)

# Tabla de correspondencia: "Comuna N" → numero_comuna (entero)
# Cubre los formatos "Comuna 1", "comuna 1", "COMUNA 1"
comuna_lookup <- tibble(
  comuna_texto  = paste0("Comuna ", 1:15),
  numero_comuna = 1:15
)

# Paso 1: extraer número de comuna desde el campo original
todos_centros_sf <- todos_centros_sf |>
  mutate(
    # Normalizar: extraer el número de "Comuna N" independientemente de mayúsculas
    comuna_num_orig = as.integer(
      str_extract(str_to_title(comuna_texto), "(?<=Comuna )\\d+")
    )
  )

n_con_original <- sum(!is.na(todos_centros_sf$comuna_num_orig))
n_sin_original <- sum(is.na(todos_centros_sf$comuna_num_orig))
cat(sprintf("   Con comuna en campo original:  %d\n", n_con_original))
cat(sprintf("   Sin comuna (requiere sjoin):   %d\n", n_sin_original))

# Paso 2: sjoin espacial solo para los que no tienen comuna original
if (n_sin_original > 0) {
  idx_sin <- which(is.na(todos_centros_sf$comuna_num_orig))
  
  sjoin_result <- st_join(
    todos_centros_sf[idx_sin, ],
    comunas_proj |> select(numero_comuna),
    join = st_within,
    left = TRUE
  )
  
  todos_centros_sf$comuna_num_orig[idx_sin] <- sjoin_result$numero_comuna
  
  # Paso 3: los que siguen en NA tras el sjoin (borde de comunas) → vecino más cercano
  idx_na <- which(is.na(todos_centros_sf$comuna_num_orig))
  if (length(idx_na) > 0) {
    nearest <- st_nearest_feature(
      todos_centros_sf[idx_na, ],
      comunas_proj |> select(numero_comuna)
    )
    todos_centros_sf$comuna_num_orig[idx_na] <-
      comunas_proj$numero_comuna[nearest]
    cat(sprintf("   %d centros en borde asignados por vecino más cercano\n",
                length(idx_na)))
  }
}

centros_con_comuna <- todos_centros_sf |>
  rename(numero_comuna = comuna_num_orig)

cat(sprintf("   Total centros: %d | Con comuna asignada: %d\n",
            nrow(centros_con_comuna),
            sum(!is.na(centros_con_comuna$numero_comuna))))

# Dataset tabular de centros por comuna
centros_por_comuna <- centros_con_comuna |>
  st_drop_geometry() |>
  left_join(comunas_sf |> st_drop_geometry() |> select(numero_comuna, barrios),
            by = "numero_comuna") |>
  select(numero_comuna, barrios_comuna = barrios,
         tipo_centro, nombre_centro, comuna_original = comuna_texto) |>
  arrange(numero_comuna, tipo_centro, nombre_centro)

cat("\n   Conteo de centros por tipo y comuna:\n")
print(
  centros_por_comuna |>
    count(numero_comuna, tipo_centro) |>
    pivot_wider(names_from = tipo_centro, values_from = n, values_fill = 0) |>
    arrange(numero_comuna)
)

# =============================================================================
# PASO 2 y 3 — DISTANCIA Y TIEMPO AL CENTRO MÁS CERCANO POR TIPO
# =============================================================================

cat("── 5. Calculando distancias y tiempos al centro más cercano por tipo...\n")

tipos <- c("Hospital", "Centro Medico Barrial", "CeSAC", "Estacion Saludable")

calcular_acceso <- function(tipo_sel) {
  centros_tipo <- centros_con_comuna |> filter(tipo_centro == tipo_sel)
  
  if (nrow(centros_tipo) == 0) {
    warning(sprintf("No hay centros del tipo '%s'", tipo_sel))
    return(tibble(numero_comuna = centroides_proj$numero_comuna,
                  tipo_centro = tipo_sel,
                  dist_eucl_m = NA_real_,
                  dist_eucl_km = NA_real_,
                  dist_red_km = NA_real_,
                  tiempo_auto_min = NA_real_,
                  tiempo_transp_min = NA_real_))
  }
  
  # Matriz de distancias euclidiana (en metros, CRS proyectado)
  dist_matrix <- st_distance(centroides_proj, centros_tipo)
  
  tibble(
    numero_comuna     = centroides_proj$numero_comuna,
    tipo_centro       = tipo_sel,
    dist_eucl_m       = as.numeric(apply(dist_matrix, 1, min)),
    dist_eucl_km      = round(dist_eucl_m / 1000, 3),
    # Distancia por red vial ≈ euclidiana × factor de tortuosidad
    dist_red_km       = round(dist_eucl_km * FACTOR_TORT, 3),
    # Tiempo estimado: dist_red / velocidad × 60 minutos
    tiempo_auto_min   = round(dist_red_km / VEL_AUTO_KMH   * 60, 1),
    tiempo_transp_min = round(dist_red_km / VEL_TRANSP_KMH * 60, 1)
  )
}

distancias_tiempos <- bind_rows(lapply(tipos, calcular_acceso))

cat("\n   Resumen de acceso por tipo de centro:\n")
print(
  distancias_tiempos |>
    group_by(tipo_centro) |>
    summarise(
      dist_prom_km       = round(mean(dist_eucl_km, na.rm = TRUE), 2),
      dist_max_km        = round(max(dist_eucl_km,  na.rm = TRUE), 2),
      tiempo_auto_prom   = round(mean(tiempo_auto_min,   na.rm = TRUE), 1),
      tiempo_transp_prom = round(mean(tiempo_transp_min, na.rm = TRUE), 1)
    )
)

# =============================================================================
# PASO 4 — DENSIDAD DE CENTROS POR TIPO
# =============================================================================

cat("── 6. Calculando densidades...\n")

conteo_centros <- centros_con_comuna |>
  st_drop_geometry() |>
  count(numero_comuna, tipo_centro, name = "n_centros")

densidad_por_tipo <- expand_grid(
  numero_comuna = 1:15,
  tipo_centro   = tipos
) |>
  left_join(conteo_centros, by = c("numero_comuna", "tipo_centro")) |>
  mutate(n_centros = replace_na(n_centros, 0L)) |>
  left_join(comunas_sf |> st_drop_geometry() |> select(numero_comuna, area_km2),
            by = "numero_comuna") |>
  left_join(poblacion_comunas, by = "numero_comuna") |>
  mutate(
    densidad_por_km2      = round(n_centros / area_km2, 4),
    densidad_por_100k_hab = round(n_centros / poblacion * 100000, 3)
  )

cat("\n   Densidad promedio por tipo:\n")
print(
  densidad_por_tipo |>
    group_by(tipo_centro) |>
    summarise(
      total          = sum(n_centros),
      dens_km2_prom  = round(mean(densidad_por_km2), 4),
      dens_100k_prom = round(mean(densidad_por_100k_hab), 2)
    )
)

# =============================================================================
# DATASET FINAL UNIDO POR COMUNA (formato wide)
# =============================================================================

cat("── 7. Armando dataset final por comuna...\n")

slug <- c("Hospital"              = "hosp",
          "Centro Medico Barrial" = "cmb",
          "CeSAC"                 = "cesac",
          "Estacion Saludable"    = "est_sal")

dist_wide <- distancias_tiempos |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(numero_comuna, tipo_slug,
         dist_eucl_km, dist_red_km,
         tiempo_auto_min, tiempo_transp_min) |>
  pivot_wider(names_from  = tipo_slug,
              values_from = c(dist_eucl_km, dist_red_km,
                              tiempo_auto_min, tiempo_transp_min),
              names_glue  = "{.value}_{tipo_slug}")

dens_wide <- densidad_por_tipo |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(numero_comuna, tipo_slug,
         n_centros, densidad_por_km2, densidad_por_100k_hab) |>
  pivot_wider(names_from  = tipo_slug,
              values_from = c(n_centros, densidad_por_km2,
                              densidad_por_100k_hab),
              names_glue  = "{.value}_{tipo_slug}")

analisis_comunas <- comunas_sf |>
  st_drop_geometry() |>
  select(numero_comuna, barrios, area_km2) |>
  left_join(poblacion_comunas, by = "numero_comuna") |>
  left_join(dist_wide,         by = "numero_comuna") |>
  left_join(dens_wide,         by = "numero_comuna") |>
  arrange(numero_comuna)

cat(sprintf("   Dataset final: %d comunas × %d variables\n",
            nrow(analisis_comunas), ncol(analisis_comunas)))

# =============================================================================
# EXPORTAR
# =============================================================================

cat("\n── 8. Exportando resultados...\n")

write_csv(centros_por_comuna,
          file.path(RUTA, "centros_por_comuna.csv"))

write_csv(distancias_tiempos,
          file.path(RUTA, "distancias_tiempos_por_tipo.csv"))

write_csv(densidad_por_tipo,
          file.path(RUTA, "densidad_por_tipo.csv"))

write_csv(analisis_comunas,
          file.path(RUTA, "analisis_completo_comunas.csv"))

cat("✓ Archivos exportados:\n")
cat("   centros_por_comuna.csv             — todos los centros con tipo y comuna\n")
cat("   distancias_tiempos_por_tipo.csv    — distancia eucl., red y tiempos estimados\n")
cat("   densidad_por_tipo.csv              — densidad por km² y por 100k habitantes\n")
cat("   analisis_completo_comunas.csv      — dataset wide por comuna\n")

# =============================================================================
# OPCIONAL: ROUTING REAL PARA TIEMPOS DE VIAJE
# =============================================================================
#
# =============================================================================
# ROUTING REAL CON OPENROUTESERVICE
# =============================================================================
#
# Reemplaza las estimaciones por velocidad con distancias y tiempos reales
# calculados sobre la red vial (auto) y peatonal (como proxy de transporte
# público, dado que ORS free no tiene GTFS).
#
# Requisitos:
#   - Cuenta gratuita en https://openrouteservice.org/dev/#/home → TOKENS
#   - install.packages("openrouteservice")
#
# Límites del plan gratuito:
#   - Matrix API: máx. 3.500 pares origen × destino por request
#   - Con 15 comunas y ≤ 50 centros por tipo: 15 × 50 = 750 pares → dentro del límite
#
# Perfiles disponibles:
#   "driving-car"   → auto
#   "foot-walking"  → caminata (proxy de transporte público a falta de GTFS)
#   "cycling-*"     → bicicleta
#
# Para transport público real se recomienda r5r con datos GTFS de CABA:
#   https://data.buenosaires.gob.ar/dataset/gtfs
# =============================================================================

# ── Configuración ORS ─────────────────────────────────────────────────────────

ORS_API_KEY  <- "eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6IjM3OTNiYWM0NzUxNzQwZTc4MWE2YWI0YjQ3MmRiNTdkIiwiaCI6Im11cm11cjY0In0="   # <-- pegar tu clave aquí
EJECUTAR_ORS <- T              # <-- cambiar a TRUE para ejecutar

# ─────────────────────────────────────────────────────────────────────────────

if (EJECUTAR_ORS) {
  
  if (!requireNamespace("openrouteservice", quietly = TRUE))
    stop("Instalá el paquete: install.packages('openrouteservice')")
  
  library(openrouteservice)
  ors_api_key(ORS_API_KEY)
  
  # ── Preparar coordenadas en WGS84 ─────────────────────────────────────────
  
  # Centroides de comunas (WGS84, formato lon/lat)
  centroides_wgs84 <- centroides_proj |>
    st_transform(4326)
  
  coords_comunas <- st_coordinates(centroides_wgs84)   # matriz [lon, lat]
  
  # Función: extraer coordenadas WGS84 de un sf de centros
  coords_sf <- function(sf_obj) {
    pts <- sf_obj |> st_transform(4326) |> st_geometry()
    coords <- st_coordinates(pts)
    # ORS espera lista de vectores c(lon, lat)
    lapply(seq_len(nrow(coords)), function(i) c(coords[i, 1], coords[i, 2]))
  }
  
  # Coordenadas de centroides como lista
  lista_comunas <- lapply(
    seq_len(nrow(coords_comunas)),
    function(i) c(coords_comunas[i, 1], coords_comunas[i, 2])
  )
  
  # ── Función principal: matrix ORS para un tipo de centro ──────────────────
  #
  # Lógica:
  #   - Orígenes: 15 centroides de comunas
  #   - Destinos: todos los centros del tipo seleccionado
  #   - ORS devuelve matrices de duración (seg) y distancia (m) de tamaño 15 × N
  #   - Tomamos el mínimo de cada fila → distancia/tiempo al más cercano
  
  calcular_ors <- function(tipo_sel, perfil, etiqueta) {
    
    cat(sprintf("   [ORS %s] %s...\n", etiqueta, tipo_sel))
    
    centros_tipo <- centros_con_comuna |> filter(tipo_centro == tipo_sel)
    
    if (nrow(centros_tipo) == 0) {
      warning(sprintf("Sin centros del tipo '%s'", tipo_sel))
      return(tibble(numero_comuna = 1:15, tipo_centro = tipo_sel,
                    dist_ors_km = NA_real_, tiempo_ors_min = NA_real_))
    }
    
    lista_destinos <- coords_sf(centros_tipo)
    
    # Verificar límite de la API: máx 3500 pares
    n_pares <- length(lista_comunas) * length(lista_destinos)
    if (n_pares > 3500)
      warning(sprintf("Se superan los 3500 pares (%.0f). Considerar chunking.", n_pares))
    
    # Llamada a la Matrix API
    # sources: índices 0-based de los orígenes (comunas)
    # destinations: índices 0-based de los destinos (centros)
    res <- tryCatch(
      ors_matrix(
        locations    = c(lista_comunas, lista_destinos),
        profile      = perfil,
        sources      = seq(0, length(lista_comunas) - 1),
        destinations = seq(length(lista_comunas),
                           length(lista_comunas) + length(lista_destinos) - 1),
        metrics      = c("duration", "distance"),
        units        = "km"
      ),
      error = function(e) {
        message(sprintf("   ✗ Error ORS [%s - %s]: %s", tipo_sel, etiqueta, e$message))
        return(NULL)
      }
    )
    
    if (is.null(res)) {
      return(tibble(numero_comuna = centroides_wgs84$numero_comuna,
                    tipo_centro = tipo_sel,
                    dist_ors_km = NA_real_,
                    tiempo_ors_min = NA_real_))
    }
    
    # Matrices: filas = comunas, columnas = centros
    mat_dur  <- res$durations   # en segundos
    mat_dist <- res$distances   # en km
    
    # Mínimo por fila = al centro más cercano (en tiempo)
    tibble(
      numero_comuna  = centroides_wgs84$numero_comuna,
      tipo_centro    = tipo_sel,
      dist_ors_km    = round(apply(mat_dist, 1, min, na.rm = TRUE), 3),
      tiempo_ors_min = round(apply(mat_dur,  1, min, na.rm = TRUE) / 60, 1)
    )
  }
  
  # ── Ejecutar para cada tipo × perfil ──────────────────────────────────────
  #
  # Nota: ORS free no tiene routing de transporte público.
  # Usamos:
  #   driving-car   → tiempo en auto real (sobre red vial)
  #   foot-walking  → caminata (límite de distancia: 6 km, suficiente para CABA)
  #                   Como aproximación de transporte público es conservadora
  #                   (subestima velocidad). Ver r5r para tiempos GTFS reales.
  
  perfiles_ors <- list(
    list(perfil = "driving-car",  sufijo = "auto"),
    list(perfil = "foot-walking", sufijo = "pie")
  )
  
  cat("── 9. Calculando tiempos reales con OpenRouteService...\n")
  cat("   (Esto puede tardar ~30 segundos por las llamadas a la API)\n\n")
  
  resultados_ors <- list()
  
  for (p in perfiles_ors) {
    cat(sprintf("   Perfil: %s\n", p$perfil))
    
    res_tipo <- bind_rows(lapply(tipos, function(t) {
      Sys.sleep(1)   # respetar rate limit: 40 req/min en plan free
      calcular_ors(t, p$perfil, p$sufijo)
    }))
    
    res_tipo <- res_tipo |>
      rename_with(~ paste0(., "_", p$sufijo), c("dist_ors_km", "tiempo_ors_min"))
    
    resultados_ors[[p$sufijo]] <- res_tipo
  }
  
  # ── Unir resultados ORS ───────────────────────────────────────────────────
  # Join por numero_comuna Y tipo_centro para evitar multiplicación de filas
  
  ors_final <- resultados_ors[["auto"]] |>
    left_join(
      resultados_ors[["pie"]] |>
        select(numero_comuna, tipo_centro, dist_ors_km_pie, tiempo_ors_min_pie),
      by = c("numero_comuna", "tipo_centro")
    )
  
  cat("\n   Resultados ORS — resumen por tipo:\n")
  print(
    ors_final |>
      group_by(tipo_centro) |>
      summarise(
        dist_auto_prom_km   = round(mean(dist_ors_km_auto,    na.rm = TRUE), 2),
        tiempo_auto_prom    = round(mean(tiempo_ors_min_auto, na.rm = TRUE), 1),
        tiempo_pie_prom     = round(mean(tiempo_ors_min_pie,  na.rm = TRUE), 1)
      )
  )
  
  # ── Agregar al dataset final y exportar ──────────────────────────────────
  
  # Formato wide: una fila por comuna, columnas por tipo × métrica
  # values_fn = first es un seguro extra por si quedara algún duplicado
  ors_wide <- ors_final |>
    mutate(tipo_slug = slug[tipo_centro]) |>
    select(numero_comuna, tipo_slug,
           dist_ors_km_auto, tiempo_ors_min_auto,
           dist_ors_km_pie,  tiempo_ors_min_pie) |>
    pivot_wider(
      names_from  = tipo_slug,
      values_from = c(dist_ors_km_auto, tiempo_ors_min_auto,
                      dist_ors_km_pie,  tiempo_ors_min_pie),
      names_glue  = "{.value}_{tipo_slug}",
      values_fn   = first     # garantiza escalar aunque haya duplicados
    )
  
  analisis_comunas <- analisis_comunas |>
    left_join(ors_wide, by = "numero_comuna")
  
  # Exportar tabla ORS standalone
  write_csv(ors_final,
            file.path(RUTA, "distancias_tiempos_ors.csv"))
  
  # Sobreescribir dataset wide con las nuevas columnas ORS incluidas
  write_csv(analisis_comunas,
            file.path(RUTA, "analisis_completo_comunas.csv"))
  
  cat("\n✓ Archivos ORS exportados:\n")
  cat("   distancias_tiempos_ors.csv       — dist. y tiempo ORS por tipo y modo\n")
  cat("   analisis_completo_comunas.csv    — actualizado con columnas ORS\n")
  
} else {
  cat("\n── ORS desactivado (EJECUTAR_ORS = FALSE)\n")
  cat("   Para activar: setear ORS_API_KEY y cambiar EJECUTAR_ORS <- TRUE\n")
  cat("   Registro gratuito: https://openrouteservice.org/dev/#/home → TOKENS\n")
}

