# =============================================================================
# Script: analisis_centros_salud_barrios_populares.R
#
# Objetivo: Replicar el análisis de accesibilidad a centros de salud del
#           script de comunas, pero a nivel de barrio popular.
#
#   1. Calcular el centroide de cada barrio popular
#   2. Distancia euclidiana al centro de salud más cercano por tipo
#   3. Tiempo estimado (velocidad promedio) y real (ORS) al más cercano
#   4. Densidad de centros por tipo:
#        Opción A — usando el polígono original del barrio popular
#        Opción B — expandiendo el polígono 500 m en todas las direcciones
#                   (buffer), para capturar centros en el entorno inmediato
#
# ── NOTA SOBRE POBLACIÓN ─────────────────────────────────────────────────────
#   El archivo barrios_populares_poligono.csv no contiene población.
#   La densidad por habitante queda como NA en este script.
#   Para completarla: unir la tabla externa de población por barrio al
#   dataframe `barrios_sf` antes de la sección de densidad, usando
#   `nombre` o `id` como clave:
#
#     poblacion_barrios <- read_csv("mi_tabla_poblacion.csv")
#     barrios_sf <- barrios_sf |>
#       left_join(poblacion_barrios, by = c("id_barrio" = "id"))
#
#   Luego descomentar las líneas marcadas con # [POBLACION]
#
# ── NOTA TÉCNICA SOBRE CRS ───────────────────────────────────────────────────
#   hospitales.csv usa un sistema local de CABA (no EPSG estándar):
#     lon = -58.6599 + x / (111320 × cos(-35.2641°))
#     lat = -35.2641 + y / 111320
#
# ── NOTA SOBRE TIEMPO DE VIAJE ───────────────────────────────────────────────
#   Estimación por velocidad promedio:
#     Auto: 20 km/h | Transporte público: 12 km/h
#     Factor de tortuosidad: 1.3 (Boscoe et al. 2012)
#   Para tiempos reales: ver sección ORS al final del script.
#
# Entradas:
#   barrios_populares_poligono.csv
#   hospitales.csv, centros_medicos_barriales.csv,
#   centros_salud_nivel_1_cesac.csv, estaciones_saludables.csv
#
# Outputs:
#   centros_por_barrio.csv              — centros asignados a cada barrio
#   distancias_tiempos_barrios.csv      — distancia y tiempo al más cercano
#   densidad_barrios_opA.csv            — densidad usando polígono original
#   densidad_barrios_opB.csv            — densidad usando buffer de 500 m
#   analisis_completo_barrios.csv       — dataset wide unido por barrio
#
# Dependencias: sf, dplyr, tidyr, readr, stringr
# =============================================================================

library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

sf_use_s2(FALSE)

# ── Rutas ─────────────────────────────────────────────────────────────────────
RUTA <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2"

# ── Parámetros ────────────────────────────────────────────────────────────────
VEL_AUTO_KMH   <- 20
VEL_TRANSP_KMH <- 12
FACTOR_TORT    <- 1.3
BUFFER_M       <- 500    # metros de expansión para opción B

# ── Origen del sistema local de hospitales ────────────────────────────────────
HOSP_LON0 <- -58.6599
HOSP_LAT0 <- -35.2641

# =============================================================================
# 1. CARGAR BARRIOS POPULARES
# =============================================================================

cat("── 1. Cargando barrios populares...\n")

barrios_raw <- read_csv(
  file.path(RUTA, "barrios_populares_poligono.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

barrios_sf <- barrios_raw |>
  mutate(geometry = st_as_sfc(geometry, crs = 4326)) |>
  st_sf(crs = 4326) |>
  rename(id_barrio = id, nombre_barrio = nombre, tipo_barrio = tipo) |>
  mutate(id_barrio = as.integer(id_barrio))

# Reparar geometrías inválidas
n_inv <- sum(!st_is_valid(barrios_sf))
if (n_inv > 0) {
  cat(sprintf("   Reparando %d geometrías inválidas...\n", n_inv))
  barrios_sf <- st_make_valid(barrios_sf)
}

# Reproyectar a POSGAR 2007 (metros) para cálculos de distancia y área
barrios_proj <- barrios_sf |> st_transform(22185)

# Área en km² (polígono original)
barrios_proj <- barrios_proj |>
  mutate(area_km2_opA = as.numeric(st_area(geometry)) / 1e6)

# ── Población: completar cuando se disponga de datos externos ─────────────────
# [POBLACION] Descomentar y adaptar cuando se tenga la tabla:
#   poblacion_barrios <- read_csv("poblacion_barrios.csv")
#   barrios_proj <- barrios_proj |>
#     left_join(poblacion_barrios, by = c("id_barrio" = "id"))
# Por ahora:
barrios_proj <- barrios_proj |> mutate(poblacion = NA_real_)

cat(sprintf("   %d barrios populares | área total: %.2f km²\n",
            nrow(barrios_proj),
            sum(barrios_proj$area_km2_opA)))

# =============================================================================
# 2. CENTROIDES DE BARRIOS
# =============================================================================

cat("── 2. Calculando centroides de barrios...\n")

centroides_proj <- barrios_proj |>
  st_centroid() |>
  select(id_barrio, nombre_barrio)

cat(sprintf("   %d centroides calculados\n", nrow(centroides_proj)))

# =============================================================================
# 3. CARGAR HOSPITALES (sistema local → WGS84)
# =============================================================================

cat("── 3. Cargando hospitales...\n")

cos_lat0 <- cos(HOSP_LAT0 * pi / 180)

hospitales_sf <- read_csv(
  file.path(RUTA, "hospitales.csv"),
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
) |>
  mutate(
    x_local = as.numeric(str_extract(geometry, "(?<=POINT \\()[-0-9.]+")),
    y_local = as.numeric(str_extract(geometry, "(?<=POINT \\([-0-9.]+ )[-0-9.]+")),
    lon = HOSP_LON0 + x_local / (111320 * cos_lat0),
    lat = HOSP_LAT0 + y_local / 111320,
    tipo_centro   = "Hospital",
    nombre_centro = fna,
    comuna_texto  = paste0("Comuna ", com)
  ) |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  select(tipo_centro, nombre_centro, comuna_texto) |>
  st_transform(22185)

cat(sprintf("   %d hospitales\n", nrow(hospitales_sf)))

# =============================================================================
# 4. CARGAR CENTROS WGS84
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

cmb_sf <- leer_wgs84(file.path(RUTA, "centros_medicos_barriales.csv"),
                     tipo = "Centro Medico Barrial")
cesac_sf <- leer_wgs84(file.path(RUTA, "centros_salud_nivel_1_cesac.csv"),
                       tipo = "CeSAC")
estaciones_sf <- leer_wgs84(file.path(RUTA, "estaciones_saludables.csv"),
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

centros_con_barrio <- st_join(
  todos_centros_sf,
  barrios_proj |> select(id_barrio, nombre_barrio),
  join   = st_nearest_feature   # cada centro al barrio más cercano
)

centros_por_barrio <- centros_con_barrio |>
  st_drop_geometry() |>
  select(id_barrio, nombre_barrio, tipo_centro, nombre_centro, comuna_texto) |>
  arrange(id_barrio, tipo_centro, nombre_centro)

cat(sprintf("   Centros asignados: %d\n", nrow(centros_por_barrio)))

# =============================================================================
# PASO 2 y 3 — DISTANCIA Y TIEMPO AL CENTRO MÁS CERCANO POR TIPO
# =============================================================================

cat("── 6. Calculando distancias y tiempos al centro más cercano por tipo...\n")

calcular_acceso <- function(tipo_sel) {
  centros_tipo <- todos_centros_sf |> filter(tipo_centro == tipo_sel)

  if (nrow(centros_tipo) == 0) {
    return(tibble(
      id_barrio = barrios_proj$id_barrio, tipo_centro = tipo_sel,
      dist_eucl_m = NA_real_, dist_eucl_km = NA_real_,
      dist_red_km = NA_real_, tiempo_auto_min = NA_real_,
      tiempo_transp_min = NA_real_
    ))
  }

  # Matriz de distancias: filas = centroides de barrios, cols = centros del tipo
  dist_matrix <- st_distance(centroides_proj, centros_tipo)

  tibble(
    id_barrio         = centroides_proj$id_barrio,
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

# =============================================================================
# PASO 4 — DENSIDAD: OPCIÓN A (polígono original) y OPCIÓN B (buffer 500 m)
# =============================================================================

cat(sprintf("── 7. Calculando densidades (Opción A: polígono | Opción B: buffer %dm)...\n",
            BUFFER_M))

# ── Opción B: expandir cada barrio 500 m en todas las direcciones ─────────────
barrios_buffer <- barrios_proj |>
  st_buffer(BUFFER_M) |>
  mutate(area_km2_opB = as.numeric(st_area(geometry)) / 1e6)

cat(sprintf("   Área promedio original (opA): %.4f km²\n",
            mean(barrios_proj$area_km2_opA)))
cat(sprintf("   Área promedio con buffer (opB): %.4f km²\n",
            mean(barrios_buffer$area_km2_opB)))

# ── Función: contar centros dentro de un conjunto de polígonos ────────────────
contar_centros_en_poligonos <- function(poligonos_sf, tipo_sel) {
  centros_tipo <- todos_centros_sf |> filter(tipo_centro == tipo_sel)
  if (nrow(centros_tipo) == 0)
    return(tibble(id_barrio = poligonos_sf$id_barrio, n_centros = 0L))

  # Para cada polígono, contar cuántos centros caen dentro
  # st_intersects devuelve una lista de índices
  intersects <- st_intersects(poligonos_sf, centros_tipo)
  tibble(
    id_barrio = poligonos_sf$id_barrio,
    n_centros = lengths(intersects)
  )
}

# ── Calcular densidades para cada tipo y opción ───────────────────────────────
calcular_densidad <- function(poligonos_sf, col_area, etiqueta) {
  bind_rows(lapply(tipos, function(t) {
    conteo <- contar_centros_en_poligonos(poligonos_sf, t)
    poligonos_sf |>
      st_drop_geometry() |>
      select(id_barrio, nombre_barrio, tipo_barrio,
             area_km2 = all_of(col_area), poblacion) |>
      left_join(conteo, by = "id_barrio") |>
      mutate(
        tipo_centro           = t,
        opcion                = etiqueta,
        n_centros             = replace_na(n_centros, 0L),
        densidad_por_km2      = round(n_centros / area_km2, 5),
        # [POBLACION] Descomentar cuando se tenga población:
        # densidad_por_100k_hab = round(n_centros / poblacion * 100000, 3),
        densidad_por_100k_hab = NA_real_
      )
  }))
}

densidad_opA <- calcular_densidad(barrios_proj,   "area_km2_opA", "A_poligono_original")
densidad_opB <- calcular_densidad(barrios_buffer, "area_km2_opB", "B_buffer_500m")

cat("\n   Densidad promedio por tipo — Opción A (polígono original):\n")
print(
  densidad_opA |>
    group_by(tipo_centro) |>
    summarise(
      total_centros   = sum(n_centros),
      dens_km2_media  = round(mean(densidad_por_km2), 4),
      dens_km2_max    = round(max(densidad_por_km2), 4),
      pct_sin_centros = round(mean(n_centros == 0) * 100, 1)
    )
)

cat("\n   Densidad promedio por tipo — Opción B (buffer 500 m):\n")
print(
  densidad_opB |>
    group_by(tipo_centro) |>
    summarise(
      total_centros   = sum(n_centros),
      dens_km2_media  = round(mean(densidad_por_km2), 4),
      dens_km2_max    = round(max(densidad_por_km2), 4),
      pct_sin_centros = round(mean(n_centros == 0) * 100, 1)
    )
)

# =============================================================================
# DATASET FINAL UNIDO POR BARRIO (formato wide)
# =============================================================================

cat("── 8. Armando dataset final por barrio...\n")

slug <- c("Hospital"              = "hosp",
          "Centro Medico Barrial" = "cmb",
          "CeSAC"                 = "cesac",
          "Estacion Saludable"    = "est_sal")

# Wide de distancias y tiempos
dist_wide <- distancias_tiempos |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(id_barrio, tipo_slug,
         dist_eucl_km, dist_red_km,
         tiempo_auto_min, tiempo_transp_min) |>
  pivot_wider(
    names_from  = tipo_slug,
    values_from = c(dist_eucl_km, dist_red_km,
                     tiempo_auto_min, tiempo_transp_min),
    names_glue  = "{.value}_{tipo_slug}",
    values_fn   = first
  )

# Wide de densidad opción A
dens_wide_opA <- densidad_opA |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(id_barrio, tipo_slug, n_centros,
         densidad_por_km2, densidad_por_100k_hab) |>
  pivot_wider(
    names_from  = tipo_slug,
    values_from = c(n_centros, densidad_por_km2, densidad_por_100k_hab),
    names_glue  = "{.value}_opA_{tipo_slug}",
    values_fn   = first
  )

# Wide de densidad opción B
dens_wide_opB <- densidad_opB |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(id_barrio, tipo_slug, n_centros,
         densidad_por_km2, densidad_por_100k_hab) |>
  pivot_wider(
    names_from  = tipo_slug,
    values_from = c(n_centros, densidad_por_km2, densidad_por_100k_hab),
    names_glue  = "{.value}_opB_{tipo_slug}",
    values_fn   = first
  )

# Join final
analisis_barrios <- barrios_proj |>
  st_drop_geometry() |>
  select(id_barrio, nombre_barrio, tipo_barrio, area_km2_opA, poblacion) |>
  left_join(
    barrios_buffer |> st_drop_geometry() |> select(id_barrio, area_km2_opB),
    by = "id_barrio"
  ) |>
  left_join(dist_wide,      by = "id_barrio") |>
  left_join(dens_wide_opA,  by = "id_barrio") |>
  left_join(dens_wide_opB,  by = "id_barrio") |>
  arrange(id_barrio)

cat(sprintf("   Dataset final: %d barrios × %d variables\n",
            nrow(analisis_barrios), ncol(analisis_barrios)))

# =============================================================================
# EXPORTAR
# =============================================================================

cat("\n── 9. Exportando resultados...\n")

write_csv(centros_por_barrio,
          file.path(RUTA, "centros_por_barrio.csv"))

write_csv(distancias_tiempos,
          file.path(RUTA, "distancias_tiempos_barrios.csv"))

write_csv(densidad_opA,
          file.path(RUTA, "densidad_barrios_opA.csv"))

write_csv(densidad_opB,
          file.path(RUTA, "densidad_barrios_opB.csv"))

write_csv(analisis_barrios,
          file.path(RUTA, "analisis_completo_barrios.csv"))

cat("✓ Archivos exportados:\n")
cat("   centros_por_barrio.csv           — centros asignados a cada barrio\n")
cat("   distancias_tiempos_barrios.csv   — distancia y tiempo al más cercano por tipo\n")
cat("   densidad_barrios_opA.csv         — densidad usando polígono original\n")
cat("   densidad_barrios_opB.csv         — densidad usando buffer de 500 m\n")
cat("   analisis_completo_barrios.csv    — dataset wide unido por barrio\n")

# =============================================================================
# ROUTING REAL CON OPENROUTESERVICE
# =============================================================================
#
# Idéntico al script de comunas. Límite plan free: 3.500 pares por request.
# Con 132 barrios y hasta 50 centros por tipo: 132 × 50 = 6.600 pares → supera el límite.
# El script divide automáticamente en chunks de 3.000 pares (≈ 60 barrios × 50 centros).

ORS_API_KEY  <- "eyJvcmciOiI1YjNjZTM1OTc4NTExMTAwMDFjZjYyNDgiLCJpZCI6IjM3OTNiYWM0NzUxNzQwZTc4MWE2YWI0YjQ3MmRiNTdkIiwiaCI6Im11cm11cjY0In0="
EJECUTAR_ORS <- T

if (EJECUTAR_ORS) {

  if (!requireNamespace("openrouteservice", quietly = TRUE))
    stop("Instalá el paquete: install.packages('openrouteservice')")

  library(openrouteservice)
  ors_api_key(ORS_API_KEY)

  # Coordenadas de centroides en WGS84
  centroides_wgs84 <- centroides_proj |> st_transform(4326)
  coords_barrios   <- st_coordinates(centroides_wgs84)
  lista_barrios    <- lapply(seq_len(nrow(coords_barrios)),
                             function(i) c(coords_barrios[i, 1], coords_barrios[i, 2]))

  coords_sf_fn <- function(sf_obj) {
    pts    <- sf_obj |> st_transform(4326)
    coords <- st_coordinates(pts)
    lapply(seq_len(nrow(coords)), function(i) c(coords[i, 1], coords[i, 2]))
  }

  # ── Función con chunking automático ───────────────────────────────────────
  # Con 132 orígenes y N destinos, un solo request puede superar el límite de
  # 3.500 pares (132 × 27 = 3.564 ya lo supera). Dividimos los orígenes en
  # chunks de tamaño tal que orig_chunk × n_destinos ≤ 3.000.

  calcular_ors_chunked <- function(tipo_sel, perfil, etiqueta) {
    cat(sprintf("   [ORS %s] %s...\n", etiqueta, tipo_sel))

    centros_tipo    <- todos_centros_sf |> filter(tipo_centro == tipo_sel)
    lista_destinos  <- coords_sf_fn(centros_tipo)
    n_dest          <- length(lista_destinos)
    n_orig          <- length(lista_barrios)

    if (n_dest == 0) {
      return(tibble(id_barrio = centroides_wgs84$id_barrio,
                    tipo_centro = tipo_sel,
                    dist_ors_km = NA_real_, tiempo_ors_min = NA_real_))
    }

    # Tamaño máximo de chunk de orígenes para no superar 3.000 pares
    chunk_size <- max(1L, floor(3000 / n_dest))
    chunks     <- split(seq_len(n_orig), ceiling(seq_len(n_orig) / chunk_size))

    resultados_chunks <- lapply(chunks, function(idx) {
      Sys.sleep(1.5)   # respetar rate limit: 40 req/min
      orig_chunk  <- lista_barrios[idx]
      n_orig_chunk <- length(orig_chunk)

      res <- tryCatch(
        ors_matrix(
          locations    = c(orig_chunk, lista_destinos),
          profile      = perfil,
          sources      = seq(0, n_orig_chunk - 1),
          destinations = seq(n_orig_chunk, n_orig_chunk + n_dest - 1),
          metrics      = c("duration", "distance"),
          units        = "km"
        ),
        error = function(e) {
          message(sprintf("   ✗ Error ORS chunk [%s - %s]: %s",
                          tipo_sel, etiqueta, e$message))
          NULL
        }
      )

      if (is.null(res)) {
        return(tibble(
          id_barrio      = centroides_wgs84$id_barrio[idx],
          tipo_centro    = tipo_sel,
          dist_ors_km    = NA_real_,
          tiempo_ors_min = NA_real_
        ))
      }

      tibble(
        id_barrio      = centroides_wgs84$id_barrio[idx],
        tipo_centro    = tipo_sel,
        dist_ors_km    = round(apply(res$distances, 1, min, na.rm = TRUE), 3),
        tiempo_ors_min = round(apply(res$durations, 1, min, na.rm = TRUE) / 60, 1)
      )
    })

    bind_rows(resultados_chunks)
  }

  # ── Ejecutar para cada tipo × perfil ──────────────────────────────────────
  perfiles_ors <- list(
    list(perfil = "driving-car",  sufijo = "auto"),
    list(perfil = "foot-walking", sufijo = "pie")
  )

  cat("── 10. Calculando tiempos reales con OpenRouteService...\n")
  cat(sprintf("    (132 barrios × N centros — con chunking automático)\n\n"))

  resultados_ors <- list()

  for (p in perfiles_ors) {
    cat(sprintf("   Perfil: %s\n", p$perfil))
    res_tipo <- bind_rows(lapply(tipos, function(t) {
      calcular_ors_chunked(t, p$perfil, p$sufijo)
    })) |>
      rename_with(~ paste0(., "_", p$sufijo),
                  c("dist_ors_km", "tiempo_ors_min"))
    resultados_ors[[p$sufijo]] <- res_tipo
  }

  # ── Unir y exportar ────────────────────────────────────────────────────────
  ors_final <- resultados_ors[["auto"]] |>
    left_join(
      resultados_ors[["pie"]] |>
        select(id_barrio, tipo_centro, dist_ors_km_pie, tiempo_ors_min_pie),
      by = c("id_barrio", "tipo_centro")
    )

  cat("\n   Resultados ORS — resumen por tipo:\n")
  print(
    ors_final |>
      group_by(tipo_centro) |>
      summarise(
        dist_auto_prom_km  = round(mean(dist_ors_km_auto,    na.rm = TRUE), 2),
        tiempo_auto_prom   = round(mean(tiempo_ors_min_auto, na.rm = TRUE), 1),
        tiempo_pie_prom    = round(mean(tiempo_ors_min_pie,  na.rm = TRUE), 1)
      )
  )

  ors_wide <- ors_final |>
    mutate(tipo_slug = slug[tipo_centro]) |>
    select(id_barrio, tipo_slug,
           dist_ors_km_auto, tiempo_ors_min_auto,
           dist_ors_km_pie,  tiempo_ors_min_pie) |>
    pivot_wider(
      names_from  = tipo_slug,
      values_from = c(dist_ors_km_auto, tiempo_ors_min_auto,
                       dist_ors_km_pie,  tiempo_ors_min_pie),
      names_glue  = "{.value}_{tipo_slug}",
      values_fn   = first
    )

  analisis_barrios <- analisis_barrios |>
    left_join(ors_wide, by = "id_barrio")

  write_csv(ors_final,
            file.path(RUTA, "distancias_tiempos_ors_barrios.csv"))

  write_csv(analisis_barrios,
            file.path(RUTA, "analisis_completo_barrios.csv"))

  cat("\n✓ Archivos ORS exportados:\n")
  cat("   distancias_tiempos_ors_barrios.csv  — dist. y tiempo ORS por tipo y modo\n")
  cat("   analisis_completo_barrios.csv       — actualizado con columnas ORS\n")

} else {
  cat("\n── ORS desactivado (EJECUTAR_ORS = FALSE)\n")
  cat("   Para activar: setear ORS_API_KEY y cambiar EJECUTAR_ORS <- TRUE\n")
  cat("   Registro gratuito: https://openrouteservice.org/dev/#/home → TOKENS\n")
}
