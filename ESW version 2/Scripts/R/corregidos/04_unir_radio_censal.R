# =============================================================================
# Script: 04_unir_radio_censal.R   (versión corregida de unir_radio_censal.R)
# Descripción: Une los indicadores del Censo 2022 a nivel de radio censal
#              (Redatam/INDEC), los pega al shapefile de radios de CABA,
#              calcula centroides y la distancia/tiempo al centro de salud más
#              cercano por tipo, y arma los mapas de acceso.
#
# Correcciones respecto a la versión anterior:
#   · numero_comuna: en el shapefile se definía como as.integer(DEPTO) (7, 14,
#     ..., 105), lo que chocaba con numero_comuna (1-15) de los datos del Censo y
#     generaba numero_comuna.x (7-105) / numero_comuna.y. Ahora se usa UNA sola
#     columna `numero_comuna` (1-15) y se verifica que coincida con DEPTO/7.
#   · Las funciones de distancia estaban duplicadas (matriz completa con
#     st_distance + st_nearest_feature). Queda solo st_nearest_feature, que
#     además devuelve el nombre del centro más cercano.
#   · Centros de salud: cargar_centros_salud() (00_config.R); hospitales en el
#     CRS catastral correcto, `comuna` normalizada.
#   · leaflet/glue/ggplot2 se cargan al inicio (antes `library(leaflet)` estaba
#     en la mitad del script).
#   · Mapas y PNG se guardan en RUTA_FIG (antes dependía del working directory).
#   · Salidas intermedias guardadas en RUTA_PROC (csv/rds), para que 05 no
#     dependa de objetos en memoria / .RData.
#   · leer_radio(): robusto ante columnas faltantes y chequeo de duplicados.
#
# Output (en RUTA_PROC):
#   radio_censal_unido.csv / .xlsx   tabla por radio con todas las variables
#   radios_proj.rds                  sf de radios (CRS 22185) con datos del Censo
#   todos_centros_sf.rds             sf de centros de salud (CRS 22185)
#   base_radios.rds                  largo: radio x tipo de centro (dist./tiempos)
#
# Nota metodológica: dist_eucl_km, dist_red_km, tiempo_auto_min y
# tiempo_transp_min son transformaciones lineales exactas de la misma distancia
# (colinealidad perfecta por construcción). Usar UNA sola como covariable de
# acceso en las regresiones (Greene, Econometric Analysis, supuesto de rango
# completo, §2.3.2).
# =============================================================================

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

library(readxl)
library(tidyr)
library(writexl)
library(ggplot2)
library(leaflet)
library(glue)

# ── Archivos del Censo por radio ──────────────────────────────────────────────
archivos_radio <- tibble::tribble(
  ~archivo,                           ~var_total,   ~var_n,              ~var_pct,
  "radio_agua_corriente.xlsx",        "total_viv",  "n_agua_corriente",  "pct_agua_corriente",
  "radio_calidad_materiales.xlsx",    "total_viv",  "n_calidad_mat",     "pct_calidad_mat",
  "radio_clima_educativo_bajo.xlsx",  "total_viv",  "n_clima_educ_bajo", "pct_clima_educ_bajo",
  "radio_NBI.xlsx",                   "total_viv",  "n_NBI",             "pct_NBI",
  "radio_secundaria_completa.xlsx",   "total_viv",  "n_sec_completa",    "pct_sec_completa",
  "radio_edad_65_o_mas.xlsx",         "total_viv",  "n_edad_65_mas",     "pct_edad_65_mas",
  "radio_hacinamiento.xlsx",          "total_viv",  "n_hacinamiento",    "pct_hacinamiento",
  "radio_cond_sanitarias_A.xlsx",     "total_viv",  "n_cond_san_A",      "pct_cond_san_A",
  "radio_cond_sanitarias_B.xlsx",     "total_viv",  "n_cond_san_B",      "pct_cond_san_B"
)

# ── Función lectora ───────────────────────────────────────────────────────────
# xlsx Redatam: col A vacía, fila 12 = encabezados (Código | Total | Seleccionado
# | Porcentaje), filas 13+ = datos, últimas filas = pie INDEC (texto).
leer_radio <- function(archivo, var_total, var_n, var_pct) {

  df_raw <- read_excel(buscar_archivo(archivo, RUTA_RAW),
                       col_names = FALSE, skip = 11) |>
    slice(-1)

  if (ncol(df_raw) < 5) {
    stop(archivo, ": se esperaban >= 5 columnas (B:E) y hay ", ncol(df_raw))
  }
  nm <- names(df_raw)

  df <- tibble::tibble(
    codigo = suppressWarnings(as.integer(as.numeric(df_raw[[nm[2]]]))),
    total  = suppressWarnings(as.numeric(df_raw[[nm[3]]])),
    n      = suppressWarnings(as.numeric(df_raw[[nm[4]]])),
    pct    = suppressWarnings(as.numeric(df_raw[[nm[5]]]))
  ) |>
    filter(!is.na(codigo))                 # descarta pie INDEC

  if (anyDuplicated(df$codigo)) stop(archivo, ": códigos de radio duplicados.")

  names(df)[names(df) == "total"] <- var_total
  names(df)[names(df) == "n"]     <- var_n
  names(df)[names(df) == "pct"]   <- var_pct
  df
}

# ── Lectura y merge ───────────────────────────────────────────────────────────
cat("Leyendo archivos de radio censal...\n")

resultado <- leer_radio(archivos_radio$archivo[1], archivos_radio$var_total[1],
                        archivos_radio$var_n[1],   archivos_radio$var_pct[1])
cat(sprintf("  [1/9] %s — %d radios\n", archivos_radio$archivo[1], nrow(resultado)))

for (i in 2:nrow(archivos_radio)) {
  df_nuevo <- leer_radio(archivos_radio$archivo[i], archivos_radio$var_total[i],
                         archivos_radio$var_n[i],   archivos_radio$var_pct[i]) |>
    select(-matches("^total_viv$"))        # total_viv se conserva solo del 1º
  resultado <- full_join(resultado, df_nuevo, by = "codigo")
  cat(sprintf("  [%d/9] %s — %d radios\n", i, archivos_radio$archivo[i], nrow(df_nuevo)))
}

# ── Identificadores geográficos desde el código (AAAABBCC) ────────────────────
#   AAAA = departamento/comuna (2007 = C1, 2014 = C2, ..., 2105 = C15)
#   BB   = fracción censal   |   CC = radio censal
datos_radio_censo <- resultado |>
  mutate(
    cod_departamento = as.integer(substr(as.character(codigo), 1, 4)),
    cod_fraccion     = as.integer(substr(as.character(codigo), 5, 6)),
    cod_radio        = as.integer(substr(as.character(codigo), 7, 8)),
    numero_comuna    = as.integer(round((cod_departamento - 2007) / 7 + 1))
  ) |>
  select(codigo, cod_departamento, numero_comuna, cod_fraccion, cod_radio,
         total_viv, everything())

stopifnot("numero_comuna fuera de 1-15" = all(datos_radio_censo$numero_comuna %in% 1:15))

cat("\n=== RESUMEN DATASET RADIO CENSAL ===\n")
cat(sprintf("  Radios totales:    %d\n", nrow(datos_radio_censo)))
cat(sprintf("  Variables:         %d\n", ncol(datos_radio_censo)))
cat(sprintf("  Comunas cubiertas: %d\n", n_distinct(datos_radio_censo$numero_comuna)))
cat(sprintf("  Radios sin NA en n_agua_corriente: %d\n",
            sum(!is.na(datos_radio_censo$n_agua_corriente))))

write.csv(datos_radio_censo, file.path(RUTA_PROC, "radio_censal_unido.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write_xlsx(datos_radio_censo, file.path(RUTA_PROC, "radio_censal_unido.xlsx"))
cat("\n✓ radio_censal_unido.csv/.xlsx exportados en", RUTA_PROC, "\n")

# =============================================================================
# 1. SHAPEFILE DE RADIOS (CABA)
# =============================================================================
cat("── 1. Cargando shapefile de radios censales...\n")

Sys.setenv(SHAPE_RESTORE_SHX = "YES")
radios_nac <- st_read(PATH_SHP_RADIOS, quiet = TRUE)
cat(sprintf("   Total nacional: %d radios\n", nrow(radios_nac)))

# PROV puede venir como texto ("02") o número; se compara numéricamente.
radios_sf <- radios_nac |>
  filter(suppressWarnings(as.integer(PROV)) == 2L) |>
  mutate(
    codigo_redatam = (2000L + as.integer(DEPTO)) * 10000L +
                      as.integer(FRAC) * 100L +
                      as.integer(RADIO),
    comuna_shp     = as.integer(DEPTO) / 7      # 7→1, 14→2, ..., 105→15
  ) |>
  select(
    codigo_redatam, comuna_shp,
    link       = LINK,
    nom_depto  = NOMDEPTO,
    fraccion   = FRAC,
    radio      = RADIO,
    tipo_radio = TIPO,
    geometry
  )

cat(sprintf("   CABA: %d radios\n", nrow(radios_sf)))
stopifnot("No se encontraron radios de CABA (PROV == 2)" = nrow(radios_sf) > 0)

# =============================================================================
# 2. UNIR SHAPEFILE CON DATOS DEL CENSO
# =============================================================================
cat("── 2. Uniendo shapefile con indicadores del Censo 2022...\n")

radios_con_datos <- radios_sf |>
  left_join(datos_radio_censo, by = c("codigo_redatam" = "codigo"))

n_matched <- sum(!is.na(radios_con_datos$total_viv))
cat(sprintf("   Radios con datos de Censo: %d / %d\n", n_matched, nrow(radios_sf)))

# La comuna derivada del DEPTO del shapefile y la del código Redatam deben coincidir
inconsist <- sum(radios_con_datos$comuna_shp != radios_con_datos$numero_comuna, na.rm = TRUE)
if (inconsist > 0) warning(inconsist, " radios con numero_comuna inconsistente entre shapefile y Censo.")
radios_con_datos <- select(radios_con_datos, -comuna_shp)

# =============================================================================
# 3. PROYECCIÓN Y CENTROIDES
# =============================================================================
radios_proj <- radios_con_datos |>
  st_transform(CRS_PROY) |>
  mutate(area_km2_opA = as.numeric(st_area(geometry)) / 1e6)

centroides_radios <- suppressWarnings(st_centroid(radios_proj)) |>
  select(codigo_redatam, numero_comuna)

cat(sprintf("   %d centroides calculados\n", nrow(centroides_radios)))

# =============================================================================
# 4. CENTROS DE SALUD
# =============================================================================
cat("── 4. Cargando centros de salud...\n")

todos_centros_sf <- cargar_centros_salud()
tipos            <- TIPOS_CENTRO

cat(sprintf("   Total centros: %d (%s)\n", nrow(todos_centros_sf),
            paste(sprintf("%s: %d", names(table(todos_centros_sf$tipo_centro)),
                          as.integer(table(todos_centros_sf$tipo_centro))),
                  collapse = " | ")))

stopifnot(
  "radios_proj y todos_centros_sf deben tener el mismo CRS" =
    st_crs(radios_proj) == st_crs(todos_centros_sf)
)

# Chequeo de sanidad: todos los centros deben caer cerca de CABA
# (si no, probablemente un CRS mal asignado)
caja_caba <- st_as_sfc(st_bbox(st_buffer(st_union(st_geometry(radios_proj)), 3000)))
fuera <- !lengths(st_intersects(todos_centros_sf, caja_caba))
if (any(fuera)) {
  warning(sum(fuera), " centros quedan a >3 km de CABA: revisar CRS de origen. Tipos: ",
          paste(unique(todos_centros_sf$tipo_centro[fuera]), collapse = ", "))
}

# Centro (de cualquier tipo) más cercano a cada radio — informativo
centros_con_radio <- st_join(todos_centros_sf,
                             radios_proj |> select(codigo_redatam, numero_comuna),
                             join = st_nearest_feature)
centros_por_radio <- centros_con_radio |>
  st_drop_geometry() |>
  select(codigo_redatam, numero_comuna, tipo_centro, nombre_centro, comuna_texto) |>
  arrange(codigo_redatam, tipo_centro, nombre_centro)
cat(sprintf("   Centros asignados a un radio: %d\n", nrow(centros_por_radio)))

# =============================================================================
# 5. DISTANCIA Y TIEMPO AL CENTRO MÁS CERCANO, POR TIPO (st_nearest_feature)
# =============================================================================
cat("── 5. Distancia/tiempo al centro más cercano por tipo...\n")

VEL_AUTO_KMH   <- 20
VEL_TRANSP_KMH <- 12
FACTOR_TORT    <- 1.3

asignar_centro_cercano <- function(tipo_sel, centroides, centros_sf) {

  centros_tipo <- centros_sf |>
    filter(tipo_centro == tipo_sel, !st_is_empty(geometry))

  if (nrow(centros_tipo) == 0) {
    return(tibble::tibble(
      id_radio = centroides$codigo_redatam, tipo_centro = tipo_sel,
      nombre_centro_cercano = NA_character_,
      dist_eucl_m = NA_real_, dist_eucl_km = NA_real_, dist_red_km = NA_real_,
      tiempo_auto_min = NA_real_, tiempo_transp_min = NA_real_
    ))
  }

  idx    <- st_nearest_feature(centroides, centros_tipo)
  dist_m <- as.numeric(st_distance(centroides, centros_tipo[idx, ], by_element = TRUE))
  dist_km <- dist_m / 1000

  tibble::tibble(
    id_radio              = centroides$codigo_redatam,
    tipo_centro           = tipo_sel,
    nombre_centro_cercano = centros_tipo$nombre_centro[idx],
    dist_eucl_m           = dist_m,
    dist_eucl_km          = dist_km,
    dist_red_km           = dist_km * FACTOR_TORT,
    tiempo_auto_min       = dist_km * FACTOR_TORT / VEL_AUTO_KMH   * 60,
    tiempo_transp_min     = dist_km * FACTOR_TORT / VEL_TRANSP_KMH * 60
  ) |>
    mutate(across(c(dist_eucl_km, dist_red_km, tiempo_auto_min, tiempo_transp_min),
                  \(x) round(x, 3)))
}

distancias_tiempos <- bind_rows(lapply(tipos, asignar_centro_cercano,
                                       centroides = centroides_radios,
                                       centros_sf = todos_centros_sf))
centro_cercano_radio <- distancias_tiempos     # alias por compatibilidad

cat("\n   Resumen de acceso por tipo (promedio entre radios):\n")
distancias_tiempos |>
  group_by(tipo_centro) |>
  summarise(
    n_radios           = n(),
    n_sin_centro       = sum(is.na(dist_eucl_m)),
    dist_prom_km       = round(mean(dist_eucl_km,      na.rm = TRUE), 2),
    dist_max_km        = round(max(dist_eucl_km,       na.rm = TRUE), 2),
    tiempo_auto_prom   = round(mean(tiempo_auto_min,   na.rm = TRUE), 1),
    tiempo_transp_prom = round(mean(tiempo_transp_min, na.rm = TRUE), 1)
  ) |>
  print()

# Tabla larga (radio x tipo) con variables del Censo, y versión ancha
base_radios <- left_join(distancias_tiempos, datos_radio_censo,
                         by = c("id_radio" = "codigo"))
base_radios_wide <- base_radios |>
  pivot_wider(names_from  = tipo_centro,
              values_from = c(dist_eucl_m, dist_eucl_km, dist_red_km,
                              tiempo_auto_min, tiempo_transp_min,
                              nombre_centro_cercano))

# Guardar objetos intermedios para scripts siguientes (no depender del .RData)
saveRDS(radios_proj,       file.path(RUTA_PROC, "radios_proj.rds"))
saveRDS(todos_centros_sf,  file.path(RUTA_PROC, "todos_centros_sf.rds"))
saveRDS(base_radios,       file.path(RUTA_PROC, "base_radios.rds"))

# =============================================================================
# 6. LÍNEAS CENTROIDE → CENTRO MÁS CERCANO + MAPA LEAFLET
# =============================================================================
cat("── 6. Armando líneas centroide → centro más cercano...\n")

lineas_centro_cercano <- function(tipo_sel, centroides, centros_sf) {

  centros_tipo <- centros_sf |>
    filter(tipo_centro == tipo_sel, !st_is_empty(geometry))

  if (nrow(centros_tipo) == 0) {
    message(glue("  Sin centros de tipo '{tipo_sel}' — se omite."))
    return(NULL)
  }

  idx   <- st_nearest_feature(centroides, centros_tipo)
  lineas <- st_nearest_points(centroides, centros_tipo[idx, ], pairwise = TRUE)

  st_sf(
    id_radio              = centroides$codigo_redatam,
    tipo_centro           = tipo_sel,
    nombre_centro_cercano = centros_tipo$nombre_centro[idx],
    dist_km               = round(as.numeric(st_length(lineas)) / 1000, 3),
    geometry              = lineas
  )
}

lineas_radio_centro <- bind_rows(lapply(tipos, lineas_centro_cercano,
                                        centroides = centroides_radios,
                                        centros_sf = todos_centros_sf))

lineas_4326  <- st_transform(lineas_radio_centro, 4326)
centros_4326 <- st_transform(todos_centros_sf,    4326)

tipos_presentes <- unique(lineas_4326$tipo_centro)
pal <- colorFactor(palette = "Set1", domain = tipos_presentes)

mapa_acceso <- leaflet() |> addProviderTiles(providers$CartoDB.Positron)

for (tipo_sel in tipos_presentes) {
  mapa_acceso <- mapa_acceso |>
    addPolylines(
      data    = filter(lineas_4326, tipo_centro == tipo_sel),
      color   = ~pal(tipo_centro), weight = 1.2, opacity = 0.5,
      label   = ~paste0(id_radio, " → ", nombre_centro_cercano, " (", dist_km, " km)"),
      group   = tipo_sel
    ) |>
    addCircleMarkers(
      data = filter(centros_4326, tipo_centro == tipo_sel),
      radius = 4, color = ~pal(tipo_centro), fillOpacity = 1,
      label = ~nombre_centro, group = tipo_sel
    )
}

mapa_acceso <- mapa_acceso |>
  addLayersControl(overlayGroups = tipos_presentes,
                   options = layersControlOptions(collapsed = FALSE)) |>
  addLegend(pal = pal, values = tipos_presentes, title = "Tipo de centro")

for (tipo_sel in tipos_presentes[-1]) {      # al abrir solo se ve el primer tipo
  mapa_acceso <- mapa_acceso |> hideGroup(tipo_sel)
}
mapa_acceso

# =============================================================================
# 7. MAPA ESTÁTICO (ggplot): radios + líneas a CeSAC y Hospital más cercanos
# =============================================================================
tipos_mapa <- c("Hospital", "CeSAC")

lineas_mapa  <- lineas_radio_centro |> filter(tipo_centro %in% tipos_mapa)
centros_mapa <- todos_centros_sf    |> filter(tipo_centro %in% tipos_mapa)

# Verde más oscuro que el original (#a6dba0): sobre fondo claro casi no se veía
colores_tipo <- c("Hospital" = "#1b9e77", "CeSAC" = "#009ADE")

# Mapa base (calles): ggplot2/geom_sf NO trae fondo, hay que agregarlo.
# Requiere: install.packages(c("ggspatial", "rosm", "prettymapr")) y conexión a
# internet (descarga teselas de CARTO una vez y las cachea).
usar_base <- requireNamespace("ggspatial", quietly = TRUE) &&
             requireNamespace("rosm",      quietly = TRUE)
if (!usar_base) {
  message("Sin mapa base: instalar 'ggspatial' y 'rosm' para dibujar las calles. ",
          "Se usa un relleno gris claro en su lugar.")
}
capa_base <- if (usar_base) {
  ggspatial::annotation_map_tile(type = "cartolight", zoom = 12, progress = "none")
} else {
  NULL
}

mapa_estatico <- ggplot() +
  capa_base +
  geom_sf(data = radios_proj,
          fill  = if (usar_base) NA else "grey96",
          color = if (usar_base) "grey35" else "grey60",
          linewidth = 0.15, alpha = 0.8) +
  geom_sf(data = lineas_mapa, aes(color = tipo_centro),
          linewidth = 0.2, alpha = 0.35, show.legend = "line") +
  geom_sf(data = centros_mapa, aes(color = tipo_centro, shape = tipo_centro),
          size = 1.9, stroke = 0.6) +
  scale_color_manual(values = colores_tipo, name = "Tipo de centro") +
  scale_shape_manual(values = c("Hospital" = 17, "CeSAC" = 16), name = "Tipo de centro") +
  # Los datos están en EPSG:22185; las teselas web están en 3857. coord_sf
  # reproyecta todas las capas al CRS del mapa base.
  coord_sf(crs = if (usar_base) 3857 else CRS_PROY) +
  labs(title    = "Acceso desde cada radio censal al CeSAC/Hospital más cercano",
       subtitle = "CABA — línea recta centroide del radio → centro más cercano por tipo") +
  theme_void() +
  theme(plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey30"),
        legend.position = "bottom")

mapa_estatico

# Figura en PDF (vectorial); antes era PNG
guardar_pdf(mapa_estatico, "mapa_acceso_cesac_hospital", width = 10, height = 10)

cat("\n✓ 04 completo. Figura en", RUTA_FIG, "\n")
