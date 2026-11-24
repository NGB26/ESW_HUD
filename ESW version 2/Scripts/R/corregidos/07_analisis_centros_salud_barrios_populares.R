# =============================================================================
# Script: 07_analisis_centros_salud_barrios_populares.R   (versión corregida)
#
# Objetivo: replicar el análisis de accesibilidad a centros de salud del script
#           06 (comunas), pero a nivel de barrio popular.
#
#   1. Centroide de cada barrio popular
#   2. Distancia euclidiana al centro más cercano por tipo
#   3. Tiempo estimado (velocidad promedio) y real (ORS, opcional)
#   4. Densidad de centros por tipo:
#        Opción A — polígono original del barrio
#        Opción B — polígono expandido 500 m (buffer)
#
# Correcciones respecto a la versión anterior:
#   · Era NO autónomo: `hospitales_sf <- hospitales_sf` dependía de un objeto de
#     otro script y luego usaba `RUTA_hosp`, que nunca definía (error "object
#     'RUTA_hosp' not found"). Ahora los centros salen de cargar_centros_salud().
#   · Rutas centralizadas (00_config.R); outputs a data/processed.
#   · CLAVE DE ORS: estaba escrita en el código y EJECUTAR_ORS = TRUE por
#     defecto. Ahora se lee de la variable de entorno ORS_API_KEY (ver 06).
#     ⚠ Revocar la clave anterior en openrouteservice.org.
#   · Se eliminaron objetos sin uso (origen local de hospitales, PATH_SHP_RADIOS).
#   · id_barrio se valida como único antes de los joins.
#   · ORS con la misma función chunkeada que 06 (ors_min_por_origen).
#
# NOTA POBLACIÓN: barrios_populares_poligono.csv no trae población; la densidad
# por habitante queda NA. Para completarla, unir una tabla externa por `id_barrio`
# en el bloque marcado [POBLACION].
#
# Entradas (data/raw): barrios_populares_poligono.csv, hospitales.csv,
#   centros_medicos_barriales.csv, centros_salud_nivel_1_cesac.csv,
#   estaciones_saludables.csv
# Outputs (data/processed): centros_por_barrio.csv, distancias_tiempos_barrios.csv,
#   densidad_barrios_opA.csv, densidad_barrios_opB.csv,
#   analisis_completo_barrios.csv, [distancias_tiempos_ors_barrios.csv]
# =============================================================================

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

library(tidyr)
library(stringr)

sf_use_s2(FALSE)

# ── Parámetros ────────────────────────────────────────────────────────────────
VEL_AUTO_KMH   <- 20
VEL_TRANSP_KMH <- 12
FACTOR_TORT    <- 1.3
BUFFER_M       <- 500    # metros de expansión para la opción B

# =============================================================================
# 1. BARRIOS POPULARES
# =============================================================================
cat("── 1. Cargando barrios populares...\n")

barrios_sf <- leer_wkt_csv(buscar_archivo("barrios_populares_poligono.csv", RUTA_RAW), 4326)

faltan <- setdiff(c("id", "nombre", "tipo"), names(barrios_sf))
if (length(faltan) > 0) {
  stop("barrios_populares_poligono.csv: faltan columnas ", paste(faltan, collapse = ", "),
       ". Columnas: ", paste(names(barrios_sf), collapse = ", "))
}

barrios_sf <- barrios_sf |>
  rename(id_barrio = id, nombre_barrio = nombre, tipo_barrio = tipo) |>
  mutate(id_barrio = as.integer(id_barrio))

stopifnot("id_barrio no es único" = !anyDuplicated(barrios_sf$id_barrio))

barrios_proj <- barrios_sf |>
  st_transform(CRS_PROY) |>
  mutate(area_km2_opA = as.numeric(st_area(geometry)) / 1e6)

# [POBLACION] Cuando se disponga de la tabla externa:
#   poblacion_barrios <- read_csv("poblacion_barrios.csv")
#   barrios_proj <- barrios_proj |> left_join(poblacion_barrios, by = c("id_barrio" = "id"))
barrios_proj <- barrios_proj |> mutate(poblacion = NA_real_)

cat(sprintf("   %d barrios populares | área total: %.2f km²\n",
            nrow(barrios_proj), sum(barrios_proj$area_km2_opA)))

# =============================================================================
# 2. CENTROIDES
# =============================================================================
centroides_proj <- barrios_proj |>
  st_centroid() |>
  select(id_barrio, nombre_barrio)
cat(sprintf("── 2. %d centroides calculados\n", nrow(centroides_proj)))

# =============================================================================
# 3. CENTROS DE SALUD
# =============================================================================
cat("── 3. Cargando centros de salud...\n")

todos_centros_sf <- cargar_centros_salud()
tipos            <- TIPOS_CENTRO

cat(sprintf("   %s\n", paste(sprintf("%s: %d", names(table(todos_centros_sf$tipo_centro)),
                                      as.integer(table(todos_centros_sf$tipo_centro))),
                              collapse = " | ")))

# =============================================================================
# PASO 1 — CENTRO MÁS CERCANO A CADA BARRIO (informativo)
# =============================================================================
cat("── 4. Asignando centros a barrios más cercanos...\n")

centros_con_barrio <- st_join(todos_centros_sf,
                              barrios_proj |> select(id_barrio, nombre_barrio),
                              join = st_nearest_feature)

centros_por_barrio <- centros_con_barrio |>
  st_drop_geometry() |>
  select(id_barrio, nombre_barrio, tipo_centro, nombre_centro, comuna_texto) |>
  arrange(id_barrio, tipo_centro, nombre_centro)

cat(sprintf("   Centros asignados: %d\n", nrow(centros_por_barrio)))

# =============================================================================
# PASO 2 y 3 — DISTANCIA Y TIEMPO AL CENTRO MÁS CERCANO POR TIPO
# =============================================================================
cat("── 5. Distancias y tiempos al centro más cercano por tipo...\n")

calcular_acceso <- function(tipo_sel) {
  centros_tipo <- todos_centros_sf |> filter(tipo_centro == tipo_sel)

  if (nrow(centros_tipo) == 0) {
    return(tibble(id_barrio = centroides_proj$id_barrio, tipo_centro = tipo_sel,
                  dist_eucl_m = NA_real_, dist_eucl_km = NA_real_, dist_red_km = NA_real_,
                  tiempo_auto_min = NA_real_, tiempo_transp_min = NA_real_))
  }

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
print(distancias_tiempos |> group_by(tipo_centro) |>
        summarise(dist_prom_km       = round(mean(dist_eucl_km,      na.rm = TRUE), 2),
                  dist_max_km        = round(max(dist_eucl_km,       na.rm = TRUE), 2),
                  tiempo_auto_prom   = round(mean(tiempo_auto_min,   na.rm = TRUE), 1),
                  tiempo_transp_prom = round(mean(tiempo_transp_min, na.rm = TRUE), 1)))

# =============================================================================
# PASO 4 — DENSIDAD: OPCIÓN A (polígono) y OPCIÓN B (buffer 500 m)
# =============================================================================
cat(sprintf("── 6. Densidades (A: polígono | B: buffer %d m)...\n", BUFFER_M))

barrios_buffer <- barrios_proj |>
  st_buffer(BUFFER_M) |>
  mutate(area_km2_opB = as.numeric(st_area(geometry)) / 1e6)

cat(sprintf("   Área promedio original (opA): %.4f km² | con buffer (opB): %.4f km²\n",
            mean(barrios_proj$area_km2_opA), mean(barrios_buffer$area_km2_opB)))

contar_centros_en_poligonos <- function(poligonos_sf, tipo_sel) {
  centros_tipo <- todos_centros_sf |> filter(tipo_centro == tipo_sel)
  if (nrow(centros_tipo) == 0) {
    return(tibble(id_barrio = poligonos_sf$id_barrio, n_centros = 0L))
  }
  tibble(id_barrio = poligonos_sf$id_barrio,
         n_centros = lengths(st_intersects(poligonos_sf, centros_tipo)))
}

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
        # [POBLACION] con población: round(n_centros / poblacion * 100000, 3)
        densidad_por_100k_hab = NA_real_
      )
  }))
}

densidad_opA <- calcular_densidad(barrios_proj,   "area_km2_opA", "A_poligono_original")
densidad_opB <- calcular_densidad(barrios_buffer, "area_km2_opB", "B_buffer_500m")

resumen_dens <- function(d) {
  d |> group_by(tipo_centro) |>
    summarise(total_centros   = sum(n_centros),
              dens_km2_media  = round(mean(densidad_por_km2), 4),
              dens_km2_max    = round(max(densidad_por_km2), 4),
              pct_sin_centros = round(mean(n_centros == 0) * 100, 1))
}
cat("\n   Densidad por tipo — Opción A (polígono original):\n"); print(resumen_dens(densidad_opA))
cat("\n   Densidad por tipo — Opción B (buffer 500 m):\n");      print(resumen_dens(densidad_opB))

# =============================================================================
# DATASET FINAL (wide)
# =============================================================================
cat("── 7. Dataset final por barrio...\n")

slug <- c("Hospital" = "hosp", "Centro Medico Barrial" = "cmb",
          "CeSAC" = "cesac", "Estacion Saludable" = "est_sal")

dist_wide <- distancias_tiempos |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(id_barrio, tipo_slug, dist_eucl_km, dist_red_km, tiempo_auto_min, tiempo_transp_min) |>
  pivot_wider(names_from = tipo_slug,
              values_from = c(dist_eucl_km, dist_red_km, tiempo_auto_min, tiempo_transp_min),
              names_glue = "{.value}_{tipo_slug}", values_fn = first)

wide_dens <- function(d, op) {
  d |>
    mutate(tipo_slug = slug[tipo_centro]) |>
    select(id_barrio, tipo_slug, n_centros, densidad_por_km2, densidad_por_100k_hab) |>
    pivot_wider(names_from = tipo_slug,
                values_from = c(n_centros, densidad_por_km2, densidad_por_100k_hab),
                names_glue = paste0("{.value}_", op, "_{tipo_slug}"), values_fn = first)
}

analisis_barrios <- barrios_proj |>
  st_drop_geometry() |>
  select(id_barrio, nombre_barrio, tipo_barrio, area_km2_opA, poblacion) |>
  left_join(barrios_buffer |> st_drop_geometry() |> select(id_barrio, area_km2_opB),
            by = "id_barrio") |>
  left_join(dist_wide,                   by = "id_barrio") |>
  left_join(wide_dens(densidad_opA, "opA"), by = "id_barrio") |>
  left_join(wide_dens(densidad_opB, "opB"), by = "id_barrio") |>
  arrange(id_barrio)

cat(sprintf("   %d barrios × %d variables\n", nrow(analisis_barrios), ncol(analisis_barrios)))

# =============================================================================
# EXPORTAR
# =============================================================================
cat("\n── 8. Exportando...\n")
write_csv(centros_por_barrio, file.path(RUTA_PROC, "centros_por_barrio.csv"))
write_csv(distancias_tiempos, file.path(RUTA_PROC, "distancias_tiempos_barrios.csv"))
write_csv(densidad_opA,       file.path(RUTA_PROC, "densidad_barrios_opA.csv"))
write_csv(densidad_opB,       file.path(RUTA_PROC, "densidad_barrios_opB.csv"))
write_csv(analisis_barrios,   file.path(RUTA_PROC, "analisis_completo_barrios.csv"))
cat("✓ Exportado en", RUTA_PROC, "\n")

# =============================================================================
# OPCIONAL: ROUTING REAL CON OPENROUTESERVICE
# =============================================================================
# Con ~130 barrios y hasta ~50 centros por tipo se superan los 3.500 pares por
# request; ors_min_por_origen() divide los orígenes en chunks automáticamente.
EJECUTAR_ORS <- nzchar(ors_clave())

if (EJECUTAR_ORS) {

  if (!requireNamespace("openrouteservice", quietly = TRUE)) {
    stop("Instalá el paquete: install.packages('openrouteservice')")
  }
  openrouteservice::ors_api_key(ors_clave())

  lista_barrios <- coords_lista(centroides_proj)

  perfiles_ors <- list(list(perfil = "driving-car",  sufijo = "auto"),
                       list(perfil = "foot-walking", sufijo = "pie"))

  cat(sprintf("── 9. Tiempos reales con ORS (%d barrios, chunking automático)...\n",
              length(lista_barrios)))

  resultados_ors <- list()
  for (p in perfiles_ors) {
    cat(sprintf("   Perfil: %s\n", p$perfil))
    resultados_ors[[p$sufijo]] <- bind_rows(lapply(tipos, function(t) {
      cat(sprintf("   [ORS %s] %s...\n", p$sufijo, t))
      destinos <- todos_centros_sf |> filter(tipo_centro == t)
      r <- ors_min_por_origen(lista_barrios, coords_lista(destinos), p$perfil)
      tibble(id_barrio = centroides_proj$id_barrio, tipo_centro = t,
             dist_ors_km = r$dist_km, tiempo_ors_min = r$tiempo_min)
    })) |>
      rename_with(~ paste0(., "_", p$sufijo), c("dist_ors_km", "tiempo_ors_min"))
  }

  ors_final <- resultados_ors[["auto"]] |>
    left_join(resultados_ors[["pie"]] |>
                select(id_barrio, tipo_centro, dist_ors_km_pie, tiempo_ors_min_pie),
              by = c("id_barrio", "tipo_centro"))

  cat("\n   Resultados ORS — resumen por tipo:\n")
  print(ors_final |> group_by(tipo_centro) |>
          summarise(dist_auto_prom_km = round(mean(dist_ors_km_auto,    na.rm = TRUE), 2),
                    tiempo_auto_prom  = round(mean(tiempo_ors_min_auto, na.rm = TRUE), 1),
                    tiempo_pie_prom   = round(mean(tiempo_ors_min_pie,  na.rm = TRUE), 1)))

  ors_wide <- ors_final |>
    mutate(tipo_slug = slug[tipo_centro]) |>
    select(id_barrio, tipo_slug, dist_ors_km_auto, tiempo_ors_min_auto,
           dist_ors_km_pie, tiempo_ors_min_pie) |>
    pivot_wider(names_from = tipo_slug,
                values_from = c(dist_ors_km_auto, tiempo_ors_min_auto,
                                dist_ors_km_pie, tiempo_ors_min_pie),
                names_glue = "{.value}_{tipo_slug}", values_fn = first)

  analisis_barrios <- analisis_barrios |> left_join(ors_wide, by = "id_barrio")

  write_csv(ors_final,        file.path(RUTA_PROC, "distancias_tiempos_ors_barrios.csv"))
  write_csv(analisis_barrios, file.path(RUTA_PROC, "analisis_completo_barrios.csv"))
  cat("\n✓ ORS exportado: distancias_tiempos_ors_barrios.csv y analisis_completo_barrios.csv actualizado\n")

} else {
  cat("\n── ORS desactivado: no hay variable de entorno ORS_API_KEY.\n")
  cat("   Sin ORS no existe tiempo_ors_min_pie_hosp, que usa 08 para colorear el mapa.\n")
  cat("   Activar: usethis::edit_r_environ() → ORS_API_KEY=<clave> → reiniciar R\n")
}
