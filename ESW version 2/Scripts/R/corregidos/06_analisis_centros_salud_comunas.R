# =============================================================================
# Script: 06_analisis_centros_salud_comunas.R   (versión corregida)
#
# Objetivo:
#   1. Consolidar los centros de salud por tipo y asignarlos a cada comuna
#   2. Distancia del centroide de cada comuna al centro más cercano, por tipo
#   3. Tiempo estimado de viaje (auto / transporte público) al más cercano
#   4. Densidad de centros por km² y por 100.000 habitantes
#   5. (Opcional) Tiempos reales con OpenRouteService
#
# Correcciones respecto a la versión anterior:
#   · Rutas centralizadas (00_config.R); outputs a data/processed.
#   · CLAVE DE ORS: estaba escrita en el código y EJECUTAR_ORS = TRUE por defecto.
#     Ahora se lee de la variable de entorno ORS_API_KEY y ORS solo corre si
#     existe. ⚠ Como la clave estuvo en texto plano en OneDrive, conviene
#     revocarla en openrouteservice.org y generar una nueva.
#   · HOSPITALES: la versión anterior los ubicaba con un origen "empírico"
#     (-58.6599, -35.2641) distinto al CRS catastral CABA 2019 que usan 04/05/07.
#     Los dos métodos ubican los hospitales a ~2 km uno del otro. Ahora se usa el
#     mismo que 04 (cargar_centros_salud) y se agrega un DIAGNÓSTICO que compara,
#     para cada método, cuántos hospitales caen en la comuna que declara su
#     campo `com`. Revisar ese resultado: si el método "empírico" concordara
#     más, habría que revisar CRS_CABA2019 en 00_config.R.
#   · Población: pob_comuna.xlsx se busca en data/raw y se valida (15 filas,
#     columna Total) en lugar de asumir el orden de las filas.
#   · El área de comunas se valida contra el área calculada del polígono.
#   · st_join de asignación de comuna: se verifica que no duplique filas.
#   · ORS: mínimos sin warnings/Inf si toda la fila es NA, chunking común con 07.
#
# Entradas (data/raw): hospitales.csv, centros_medicos_barriales.csv,
#   centros_salud_nivel_1_cesac.csv, estaciones_saludables.csv, comunas.csv,
#   pob_comuna.xlsx
#
# Outputs (data/processed): centros_por_comuna.csv,
#   distancias_tiempos_por_tipo.csv, densidad_por_tipo.csv,
#   analisis_completo_comunas.csv, [distancias_tiempos_ors.csv]
# =============================================================================

#install.packages("usethis")   # si no lo tenés
#usethis::edit_r_environ()


CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

library(tidyr)
library(readxl)
library(stringr)

sf_use_s2(FALSE)

# ── Parámetros de estimación de tiempos ──────────────────────────────────────
VEL_AUTO_KMH   <- 20    # auto en CABA con tráfico
VEL_TRANSP_KMH <- 12    # transporte público (incluye espera)
FACTOR_TORT    <- 1.3   # tortuosidad de la red vial (Boscoe et al. 2012)

# =============================================================================
# 0. POBLACIÓN POR COMUNA (Censo 2022)
# =============================================================================
poblacion_comunas <- read_excel(buscar_archivo("pob_comuna.xlsx", RUTA_RAW))

if (!"Total" %in% names(poblacion_comunas)) {
  stop("pob_comuna.xlsx no tiene columna 'Total'. Columnas: ",
       paste(names(poblacion_comunas), collapse = ", "))
}
if (nrow(poblacion_comunas) != 15) {
  stop("pob_comuna.xlsx tiene ", nrow(poblacion_comunas),
       " filas (se esperaban 15, una por comuna en orden 1-15). ",
       "Revisar encabezados/pie del Excel.")
}
poblacion_comunas <- poblacion_comunas |>
  mutate(numero_comuna = 1:15) |>      # se asume orden de comuna 1 a 15
  rename(poblacion = Total)

# =============================================================================
# 1. COMUNAS
# =============================================================================
cat("── 1. Cargando comunas...\n")

comunas_sf <- leer_wkt_csv(buscar_archivo("comunas.csv", RUTA_RAW), 4326, delim = ";")

comunas_proj <- comunas_sf |>
  mutate(numero_comuna = as.integer(readr::parse_number(as.character(comuna)))) |>
  st_transform(CRS_PROY)

area_calc_km2 <- as.numeric(st_area(comunas_proj)) / 1e6
if ("area" %in% names(comunas_proj)) {
  area_km2 <- as.numeric(comunas_proj$area) / 1e6
  if (any(abs(area_km2 / area_calc_km2 - 1) > 0.05, na.rm = TRUE)) {
    warning("La columna `area` de comunas.csv difiere >5% del área del polígono; ",
            "se usa el área calculada.")
    area_km2 <- area_calc_km2
  }
} else {
  area_km2 <- area_calc_km2
}

comunas_proj <- comunas_proj |>
  mutate(area_km2 = area_km2,
         barrios  = if ("barrios" %in% names(comunas_proj)) barrios else NA_character_) |>
  select(numero_comuna, barrios, area_km2)

comunas_sf      <- st_transform(comunas_proj, 4326)
centroides_proj <- comunas_proj |> st_centroid() |> select(numero_comuna)

cat(sprintf("   %d comunas | área total: %.1f km²\n",
            nrow(comunas_sf), sum(comunas_sf$area_km2)))
stopifnot("Se esperaban 15 comunas" = nrow(comunas_sf) == 15)

# =============================================================================
# 2. CENTROS DE SALUD
# =============================================================================
cat("── 2. Cargando centros de salud...\n")

todos_centros_sf <- cargar_centros_salud()
tipos            <- TIPOS_CENTRO

cat(sprintf("   %s\n", paste(sprintf("%s: %d", names(table(todos_centros_sf$tipo_centro)),
                                      as.integer(table(todos_centros_sf$tipo_centro))),
                              collapse = " | ")))

# ── Diagnóstico de CRS de hospitales ─────────────────────────────────────────
# Compara con el campo `com` (comuna declarada) de hospitales.csv.
diagnosticar_hospitales <- function() {
  hosp <- read_csv(buscar_archivo("hospitales.csv", RUTA_SALUD), show_col_types = FALSE,
                   locale = locale(encoding = "UTF-8"))
  com_decl <- as.integer(readr::parse_number(as.character(hosp$com)))

  concordancia <- function(pts_sf) {
    pts <- st_transform(pts_sf, CRS_PROY)
    idx <- st_intersects(pts, comunas_proj)
    com_esp <- vapply(idx, function(i) if (length(i) == 1) comunas_proj$numero_comuna[i] else NA_integer_,
                      integer(1))
    c(dentro_de_caba = mean(lengths(idx) > 0),
      coincide_com   = mean(com_esp == com_decl, na.rm = TRUE))
  }

  # (a) CRS catastral CABA 2019 (el que usa 04)
  a <- concordancia(st_as_sf(hosp, wkt = "geometry", crs = CRS_CABA2019))

  # (b) Sistema "local empírico" que usaba la versión anterior de este script
  m <- stringr::str_match(hosp$geometry, "^POINT\\s*\\(\\s*([-0-9.]+)\\s+([-0-9.]+)\\s*\\)$")
  x <- as.numeric(m[, 2]); y <- as.numeric(m[, 3])
  lon0 <- -58.6599; lat0 <- -35.2641
  b_sf <- st_as_sf(data.frame(lon = lon0 + x / (111320 * cos(lat0 * pi / 180)),
                              lat = lat0 + y / 111320),
                   coords = c("lon", "lat"), crs = 4326)
  b <- concordancia(b_sf)

  res <- rbind(CRS_CABA2019 = a, local_empirico = b)
  cat("\n   Diagnóstico CRS hospitales (fracción de hospitales):\n")
  print(round(res, 3))
  if (b["coincide_com"] > a["coincide_com"] + 0.05) {
    warning("El método 'local_empirico' concuerda mejor con el campo `com` que ",
            "CRS_CABA2019. Revisar el CRS de hospitales en 00_config.R.")
  }
  invisible(res)
}
tryCatch(diagnosticar_hospitales(),
         error = function(e) message("   (diagnóstico de hospitales omitido: ", conditionMessage(e), ")"))

# =============================================================================
# PASO 1 — ASIGNAR COMUNA A CADA CENTRO
# =============================================================================
# Prioridad: (1) campo original `comuna_texto`; (2) centro dentro de polígono;
# (3) polígono más cercano (centros en el borde).
cat("── 3. Asignando comunas...\n")

todos_centros_sf <- todos_centros_sf |>
  mutate(comuna_num_orig = as.integer(readr::parse_number(comuna_texto)))

idx_sin <- which(is.na(todos_centros_sf$comuna_num_orig))
cat(sprintf("   Con comuna en campo original: %d | requieren sjoin: %d\n",
            sum(!is.na(todos_centros_sf$comuna_num_orig)), length(idx_sin)))

if (length(idx_sin) > 0) {
  sj <- st_join(todos_centros_sf[idx_sin, ], comunas_proj |> select(numero_comuna),
                join = st_within, left = TRUE)
  if (nrow(sj) != length(idx_sin)) {
    stop("st_join duplicó filas (un centro cae en más de una comuna).")
  }
  todos_centros_sf$comuna_num_orig[idx_sin] <- sj$numero_comuna

  idx_na <- which(is.na(todos_centros_sf$comuna_num_orig))
  if (length(idx_na) > 0) {
    nearest <- st_nearest_feature(todos_centros_sf[idx_na, ], comunas_proj)
    todos_centros_sf$comuna_num_orig[idx_na] <- comunas_proj$numero_comuna[nearest]
    cat(sprintf("   %d centros en borde asignados por vecino más cercano\n", length(idx_na)))
  }
}

centros_con_comuna <- todos_centros_sf |> rename(numero_comuna = comuna_num_orig)

centros_por_comuna <- centros_con_comuna |>
  st_drop_geometry() |>
  left_join(comunas_sf |> st_drop_geometry() |> select(numero_comuna, barrios),
            by = "numero_comuna") |>
  select(numero_comuna, barrios_comuna = barrios,
         tipo_centro, nombre_centro, comuna_original = comuna_texto) |>
  arrange(numero_comuna, tipo_centro, nombre_centro)

cat("\n   Conteo de centros por tipo y comuna:\n")
print(centros_por_comuna |> count(numero_comuna, tipo_centro) |>
        pivot_wider(names_from = tipo_centro, values_from = n, values_fill = 0) |>
        arrange(numero_comuna))

# =============================================================================
# PASO 2 y 3 — DISTANCIA Y TIEMPO AL CENTRO MÁS CERCANO POR TIPO
# =============================================================================
cat("── 4. Distancias y tiempos al centro más cercano por tipo...\n")

calcular_acceso <- function(tipo_sel) {
  centros_tipo <- centros_con_comuna |> filter(tipo_centro == tipo_sel)

  if (nrow(centros_tipo) == 0) {
    warning(sprintf("No hay centros del tipo '%s'", tipo_sel))
    return(tibble(numero_comuna = centroides_proj$numero_comuna, tipo_centro = tipo_sel,
                  dist_eucl_m = NA_real_, dist_eucl_km = NA_real_, dist_red_km = NA_real_,
                  tiempo_auto_min = NA_real_, tiempo_transp_min = NA_real_))
  }

  dist_matrix <- st_distance(centroides_proj, centros_tipo)

  tibble(
    numero_comuna     = centroides_proj$numero_comuna,
    tipo_centro       = tipo_sel,
    dist_eucl_m       = as.numeric(apply(dist_matrix, 1, min)),
    dist_eucl_km      = round(dist_eucl_m / 1000, 3),
    dist_red_km       = round(dist_eucl_km * FACTOR_TORT, 3),
    tiempo_auto_min   = round(dist_red_km / VEL_AUTO_KMH   * 60, 1),
    tiempo_transp_min = round(dist_red_km / VEL_TRANSP_KMH * 60, 1)
  )
}

distancias_tiempos <- bind_rows(lapply(tipos, calcular_acceso))

cat("\n   Resumen de acceso por tipo de centro:\n")
print(distancias_tiempos |> group_by(tipo_centro) |>
        summarise(dist_prom_km       = round(mean(dist_eucl_km,      na.rm = TRUE), 2),
                  dist_max_km        = round(max(dist_eucl_km,       na.rm = TRUE), 2),
                  tiempo_auto_prom   = round(mean(tiempo_auto_min,   na.rm = TRUE), 1),
                  tiempo_transp_prom = round(mean(tiempo_transp_min, na.rm = TRUE), 1)))

# =============================================================================
# PASO 4 — DENSIDAD
# =============================================================================
cat("── 5. Densidades...\n")

conteo_centros <- centros_con_comuna |>
  st_drop_geometry() |>
  count(numero_comuna, tipo_centro, name = "n_centros")

densidad_por_tipo <- expand_grid(numero_comuna = 1:15, tipo_centro = tipos) |>
  left_join(conteo_centros, by = c("numero_comuna", "tipo_centro")) |>
  mutate(n_centros = replace_na(n_centros, 0L)) |>
  left_join(comunas_sf |> st_drop_geometry() |> select(numero_comuna, area_km2),
            by = "numero_comuna") |>
  left_join(poblacion_comunas |> select(numero_comuna, poblacion), by = "numero_comuna") |>
  mutate(densidad_por_km2      = round(n_centros / area_km2, 4),
         densidad_por_100k_hab = round(n_centros / poblacion * 100000, 3))

cat("\n   Densidad promedio por tipo:\n")
print(densidad_por_tipo |> group_by(tipo_centro) |>
        summarise(total          = sum(n_centros),
                  dens_km2_prom  = round(mean(densidad_por_km2), 4),
                  dens_100k_prom = round(mean(densidad_por_100k_hab), 2)))

# =============================================================================
# DATASET FINAL (wide)
# =============================================================================
cat("── 6. Dataset final por comuna...\n")

slug <- c("Hospital" = "hosp", "Centro Medico Barrial" = "cmb",
          "CeSAC" = "cesac", "Estacion Saludable" = "est_sal")

dist_wide <- distancias_tiempos |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(numero_comuna, tipo_slug, dist_eucl_km, dist_red_km,
         tiempo_auto_min, tiempo_transp_min) |>
  pivot_wider(names_from = tipo_slug,
              values_from = c(dist_eucl_km, dist_red_km, tiempo_auto_min, tiempo_transp_min),
              names_glue = "{.value}_{tipo_slug}")

dens_wide <- densidad_por_tipo |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(numero_comuna, tipo_slug, n_centros, densidad_por_km2, densidad_por_100k_hab) |>
  pivot_wider(names_from = tipo_slug,
              values_from = c(n_centros, densidad_por_km2, densidad_por_100k_hab),
              names_glue = "{.value}_{tipo_slug}")

analisis_comunas <- comunas_sf |>
  st_drop_geometry() |>
  select(numero_comuna, barrios, area_km2) |>
  left_join(poblacion_comunas, by = "numero_comuna") |>
  left_join(dist_wide, by = "numero_comuna") |>
  left_join(dens_wide, by = "numero_comuna") |>
  arrange(numero_comuna)

cat(sprintf("   %d comunas × %d variables\n", nrow(analisis_comunas), ncol(analisis_comunas)))

# =============================================================================
# EXPORTAR
# =============================================================================
cat("\n── 7. Exportando...\n")
write_csv(centros_por_comuna,  file.path(RUTA_PROC, "centros_por_comuna.csv"))
write_csv(distancias_tiempos,  file.path(RUTA_PROC, "distancias_tiempos_por_tipo.csv"))
write_csv(densidad_por_tipo,   file.path(RUTA_PROC, "densidad_por_tipo.csv"))
write_csv(analisis_comunas,    file.path(RUTA_PROC, "analisis_completo_comunas.csv"))
cat("✓ Exportado en", RUTA_PROC, "\n")

# =============================================================================
# OPCIONAL: TIEMPOS REALES CON OPENROUTESERVICE
# =============================================================================
# Requiere variable de entorno ORS_API_KEY y install.packages("openrouteservice").
# Perfiles: "driving-car" (auto) y "foot-walking" (proxy conservador del
# transporte público, que ORS free no ofrece; para eso ver r5r + GTFS de CABA).
EJECUTAR_ORS <- nzchar(ors_clave())

if (EJECUTAR_ORS) {

  if (!requireNamespace("openrouteservice", quietly = TRUE)) {
    stop("Instalá el paquete: install.packages('openrouteservice')")
  }
  openrouteservice::ors_api_key(ors_clave())

  lista_comunas <- coords_lista(centroides_proj)

  perfiles_ors <- list(list(perfil = "driving-car",  sufijo = "auto"),
                       list(perfil = "foot-walking", sufijo = "pie"))

  cat("── 8. Tiempos reales con OpenRouteService...\n")

  resultados_ors <- list()
  for (p in perfiles_ors) {
    cat(sprintf("   Perfil: %s\n", p$perfil))
    res_tipo <- bind_rows(lapply(tipos, function(t) {
      cat(sprintf("   [ORS %s] %s...\n", p$sufijo, t))
      destinos <- centros_con_comuna |> filter(tipo_centro == t)
      r <- ors_min_por_origen(lista_comunas, coords_lista(destinos), p$perfil)
      tibble(numero_comuna = centroides_proj$numero_comuna, tipo_centro = t,
             dist_ors_km = r$dist_km, tiempo_ors_min = r$tiempo_min)
    })) |>
      rename_with(~ paste0(., "_", p$sufijo), c("dist_ors_km", "tiempo_ors_min"))
    resultados_ors[[p$sufijo]] <- res_tipo
  }

  ors_final <- resultados_ors[["auto"]] |>
    left_join(resultados_ors[["pie"]] |>
                select(numero_comuna, tipo_centro, dist_ors_km_pie, tiempo_ors_min_pie),
              by = c("numero_comuna", "tipo_centro"))

  cat("\n   Resultados ORS — resumen por tipo:\n")
  print(ors_final |> group_by(tipo_centro) |>
          summarise(dist_auto_prom_km = round(mean(dist_ors_km_auto,    na.rm = TRUE), 2),
                    tiempo_auto_prom  = round(mean(tiempo_ors_min_auto, na.rm = TRUE), 1),
                    tiempo_pie_prom   = round(mean(tiempo_ors_min_pie,  na.rm = TRUE), 1)))

  ors_wide <- ors_final |>
    mutate(tipo_slug = slug[tipo_centro]) |>
    select(numero_comuna, tipo_slug, dist_ors_km_auto, tiempo_ors_min_auto,
           dist_ors_km_pie, tiempo_ors_min_pie) |>
    pivot_wider(names_from = tipo_slug,
                values_from = c(dist_ors_km_auto, tiempo_ors_min_auto,
                                dist_ors_km_pie, tiempo_ors_min_pie),
                names_glue = "{.value}_{tipo_slug}", values_fn = first)

  analisis_comunas <- analisis_comunas |> left_join(ors_wide, by = "numero_comuna")

  write_csv(ors_final,        file.path(RUTA_PROC, "distancias_tiempos_ors.csv"))
  write_csv(analisis_comunas, file.path(RUTA_PROC, "analisis_completo_comunas.csv"))
  cat("\n✓ ORS exportado: distancias_tiempos_ors.csv y analisis_completo_comunas.csv actualizado\n")

} else {
  cat("\n── ORS desactivado: no hay variable de entorno ORS_API_KEY.\n")
  cat("   Para activarlo: usethis::edit_r_environ() → ORS_API_KEY=<clave> → reiniciar R\n")
  cat("   Registro gratuito: https://openrouteservice.org/dev/#/home\n")
}
