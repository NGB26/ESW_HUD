# =============================================================================
# Script: 08_analisis_de_datos.R   (versión corregida)
# Autor original: Nicolás García Balus
#
# Dos mapas interactivos (leaflet):
#   MAPA 1 — barrios populares coloreados por minutos a pie al hospital más
#            cercano (ORS) + centros de salud por tipo
#   MAPA 2 — promedio de días desde la última consulta:
#              villa == 1 → promedio por barrio popular
#              villa == 0 → promedio por comuna
#
# Requiere haber corrido antes: 01 (df_final, comunas_proj, todos_centros_sf,
# piso) y 07 con ORS activo (analisis_completo_barrios.csv con
# tiempo_ors_min_pie_hosp). Si los objetos no están en memoria se leen de
# data/processed.
#
# Correcciones respecto a la versión anterior:
#   · MAPA 1: usaba `tiempo_ors_min_pie_hosp` que NO existe en df_final (el
#     colorNumeric fallaba con domain NULL). Ahora se une por id_barrio desde
#     analisis_completo_barrios.csv (salida de 07).
#   · MAPA 2: `breaks <- c(piso, 180, 200, ...)` repetía 180 (piso = 180), y
#     colorBin no admite cortes repetidos; además fallaba si el máximo era <= 280.
#     Los cortes se construyen ahora de forma única y creciente y la paleta se
#     interpola al número de intervalos.
#   · La Comuna 2 se pinta gris por ser outlier ("excluida de la escala"), pero
#     seguía entrando en el rango/leyenda. Ahora se excluye también del rango.
#   · Popup del mapa 1 decía "Tiempo al CeSAC: sin dato" siendo hospital.
#   · Salidas: HTML + PDF (vía webshot2) en RUTA_FIG; antes solo el HTML del mapa
#     2 y a la RUTA vieja. El mapa 1 antes no se guardaba.
#   · Dependencias cargadas al inicio; objetos leídos de disco si faltan.
#
# Dependencias: sf, leaflet, leaflet.extras, dplyr, htmlwidgets
#   (opcional, para PDF: webshot2 + Chrome/Edge)
# =============================================================================

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

sf_use_s2(FALSE)

# ── Objetos de entrada (memoria o disco) ──────────────────────────────────────
# Usa el objeto en memoria SOLO si tiene las columnas necesarias; si no (p. ej.
# un `df_final` viejo o de otro script), lo recarga de data/processed.
cargar_si_falta <- function(nombre, archivo, columnas = character()) {
  en_memoria_ok <- exists(nombre, envir = .GlobalEnv) &&
    all(columnas %in% names(get(nombre, envir = .GlobalEnv)))
  if (en_memoria_ok) return(invisible(TRUE))

  if (exists(nombre, envir = .GlobalEnv)) {
    message("`", nombre, "` en memoria no tiene: ",
            paste(setdiff(columnas, names(get(nombre, envir = .GlobalEnv))), collapse = ", "),
            " → se recarga de disco.")
  }
  ruta <- file.path(RUTA_PROC, archivo)
  if (!file.exists(ruta)) stop("Falta ", nombre, " válido y no existe ", ruta, ". Ejecutar 01 primero.")
  obj <- readRDS(ruta)
  faltan <- setdiff(columnas, names(obj))
  if (length(faltan) > 0) {
    stop(archivo, " no tiene las columnas: ", paste(faltan, collapse = ", "),
         ".\n  Volver a correr 01_preparar_datos.R (versión de `corregidos`).")
  }
  assign(nombre, obj, envir = .GlobalEnv)
  invisible(TRUE)
}
cargar_si_falta("df_final", "df_final.rds",
                c("villa", "barrio_norm", "geom_barrio", "id_barrio",
                  "nro_comuna", "dias_desde_ultima_consulta"))
cargar_si_falta("comunas_proj",     "comunas_proj.rds",     c("numero_comuna"))
if (!"barrios" %in% names(comunas_proj)) comunas_proj$barrios <- ""   # solo se usa en el popup
cargar_si_falta("todos_centros_sf", "todos_centros_sf.rds", c("tipo_centro", "nombre_centro"))
if (!exists("piso")) piso <- 180

path_barrios <- file.path(RUTA_PROC, "analisis_completo_barrios.csv")
if (!file.exists(path_barrios)) stop("Falta ", path_barrios, ". Ejecutar 07 primero.")
analisis_barrios <- read_csv(path_barrios, show_col_types = FALSE)

if (!"tiempo_ors_min_pie_hosp" %in% names(analisis_barrios) ||
    all(is.na(analisis_barrios$tiempo_ors_min_pie_hosp))) {
  stop("analisis_completo_barrios.csv no tiene tiempo_ors_min_pie_hosp con datos. ",
       "Correr 07 con la variable de entorno ORS_API_KEY definida.")
}

# Polígono único por barrio (villa == 1) desde df_final
poligonos_barrios <- df_final |>
  filter(villa == 1, !is.na(geom_barrio)) |>
  group_by(barrio_norm) |>
  slice(1) |>
  ungroup() |>
  mutate(geometry = st_as_sfc(geom_barrio, crs = 4326)) |>
  st_sf(crs = 4326) |>
  st_make_valid()

# =============================================================================
# MAPA 1 — Minutos a pie al hospital más cercano (barrios) + centros de salud
# =============================================================================
cat("-- MAPA 1. Preparando barrios y centros...\n")

barrios_map1 <- poligonos_barrios |>
  left_join(analisis_barrios |> select(id_barrio, tiempo_ors_min_pie_hosp),
            by = "id_barrio")

cat(sprintf("   Barrios: %d | con tiempo ORS: %d\n",
            nrow(barrios_map1), sum(!is.na(barrios_map1$tiempo_ors_min_pie_hosp))))

centros_map <- todos_centros_sf |>
  st_transform(4326) |>
  mutate(lon = st_coordinates(geometry)[, 1],
         lat = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

pal_tiempo <- colorNumeric(
  palette  = c("lightblue", "red", "darkred"),
  domain   = barrios_map1$tiempo_ors_min_pie_hosp,
  na.color = "#AAAAAA"
)

col_centros <- c("Hospital"              = "#1A5276",
                 "Centro Medico Barrial" = "#1ABC9C",
                 "CeSAC"                 = "#E67E22",
                 "Estacion Saludable"    = "#8E44AD")

iconos <- list(
  "Hospital"              = makeAwesomeIcon(icon = "plus",        library = "fa",
                                            markerColor = "blue",   iconColor = "white"),
  "Centro Medico Barrial" = makeAwesomeIcon(icon = "medkit",      library = "fa",
                                            markerColor = "green",  iconColor = "white"),
  "CeSAC"                 = makeAwesomeIcon(icon = "stethoscope", library = "fa",
                                            markerColor = "orange", iconColor = "white"),
  "Estacion Saludable"    = makeAwesomeIcon(icon = "heart",       library = "fa",
                                            markerColor = "purple", iconColor = "white")
)

barrios_map1 <- barrios_map1 |>
  mutate(popup = paste0(
    "<b>", barrio_norm, "</b><br><span style='font-size:12px'>",
    ifelse(!is.na(tiempo_ors_min_pie_hosp),
           paste0("Hospital más cercano: <b>", round(tiempo_ors_min_pie_hosp, 1),
                  " min</b> (a pie)"),
           "Tiempo al hospital: sin dato"),
    "</span>"))

centros_map <- centros_map |>
  mutate(popup = paste0(
    "<b>", nombre_centro, "</b><br>",
    "<span style='color:", unname(col_centros[tipo_centro]), "; font-weight:600'>",
    tipo_centro, "</span>",
    ifelse(!is.na(comuna_texto) & comuna_texto != "",
           paste0("<br><small>", comuna_texto, "</small>"), "")))

cat("-- MAPA 1. Construyendo...\n")

mapa1 <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron,  group = "Mapa claro") |>
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro") |>
  addProviderTiles(providers$OpenStreetMap,      group = "OpenStreetMap") |>
  setView(lng = -58.437, lat = -34.617, zoom = 12) |>
  addPolygons(
    data        = barrios_map1,
    group       = "Barrios populares",
    fillColor   = ~pal_tiempo(tiempo_ors_min_pie_hosp),
    fillOpacity = 0.7, color = "white", weight = 1, opacity = 0.8,
    popup       = ~popup,
    label       = ~paste0(barrio_norm, " (", round(tiempo_ors_min_pie_hosp, 1), " min)"),
    labelOptions     = labelOptions(style = list("font-size" = "12px")),
    highlightOptions = highlightOptions(fillOpacity = 0.9, weight = 2.5, bringToFront = TRUE)
  ) |>
  addLegend(position = "bottomleft", pal = pal_tiempo,
            values = barrios_map1$tiempo_ors_min_pie_hosp,
            title = "Minutos al hospital<br>más cercano (a pie)",
            labFormat = labelFormat(suffix = " min"), opacity = 0.8, na.label = "Sin dato")

for (tipo in names(iconos)) {
  sub <- centros_map |> filter(tipo_centro == tipo)
  if (nrow(sub) == 0) next
  mapa1 <- mapa1 |>
    addAwesomeMarkers(
      data = sub, lng = ~lon, lat = ~lat, group = tipo,
      icon = iconos[[tipo]], popup = ~popup, label = ~nombre_centro,
      clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE,
                                            maxClusterRadius = 40)
    )
}

mapa1 <- mapa1 |>
  addLegend(position = "bottomright", colors = unname(col_centros),
            labels = names(col_centros), title = "Centro de salud", opacity = 0.9) |>
  addLayersControl(
    baseGroups    = c("Mapa claro", "Mapa oscuro", "OpenStreetMap"),
    overlayGroups = c("Barrios populares", names(iconos)),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  addFullscreenControl()

mapa1
exportar_mapa_leaflet(mapa1, "mapa_tiempo_hospital_barrios",
                      titulo = "Minutos a pie al hospital - barrios populares CABA")

# =============================================================================
# MAPA 2 — Promedio de días desde la última consulta
# =============================================================================
cat("-- MAPA 2. Calculando promedios...\n")

COMUNA_OUTLIER <- 2L     # Recoleta: se pinta gris y se excluye de la escala

prom_barrio <- df_final |>
  filter(villa == 1, !is.na(dias_desde_ultima_consulta)) |>
  group_by(barrio_norm) |>
  summarise(prom_dias = round(mean(dias_desde_ultima_consulta, na.rm = TRUE), 1),
            n = n(), .groups = "drop")

barrios_map <- poligonos_barrios |>
  left_join(prom_barrio, by = "barrio_norm")

cat(sprintf("   Barrios en mapa: %d | con prom_dias: %d\n",
            nrow(barrios_map), sum(!is.na(barrios_map$prom_dias))))

prom_comuna <- df_final |>
  filter(villa == 0, !is.na(dias_desde_ultima_consulta)) |>
  group_by(nro_comuna) |>
  summarise(prom_dias = round(mean(dias_desde_ultima_consulta, na.rm = TRUE), 1),
            n = n(), .groups = "drop") |>
  mutate(nro_comuna = as.integer(nro_comuna))

cat(sprintf("   Comunas con dato: %d | prom: %.1f días\n",
            nrow(prom_comuna), mean(prom_comuna$prom_dias, na.rm = TRUE)))

comunas_map <- comunas_proj |>
  st_transform(4326) |>
  mutate(numero_comuna = as.integer(numero_comuna),
         label_comuna  = paste0("Comuna ", numero_comuna)) |>
  left_join(prom_comuna, by = c("numero_comuna" = "nro_comuna"))

cat(sprintf("   Comunas en mapa: %d | con prom_dias: %d\n",
            nrow(comunas_map), sum(!is.na(comunas_map$prom_dias))))

# ── Escala de color ──────────────────────────────────────────────────────────
valores_escala <- c(barrios_map$prom_dias,
                    comunas_map$prom_dias[comunas_map$numero_comuna != COMUNA_OUTLIER])
rango_global <- range(valores_escala, na.rm = TRUE)
cat(sprintf("   Rango (sin comuna %d): %.1f - %.1f días\n",
            COMUNA_OUTLIER, rango_global[1], rango_global[2]))

# Cortes únicos y crecientes: piso (o el mínimo si es menor), cada 10 días entre
# 200 y 280, y el máximo observado.
cortes_fijos <- seq(200, 280, by = 10)
breaks <- sort(unique(c(
  min(piso, floor(rango_global[1])),
  cortes_fijos[cortes_fijos > piso & cortes_fijos < rango_global[2]],
  ceiling(rango_global[2])
)))
stopifnot("No hay al menos 2 cortes distintos" = length(breaks) >= 2)

colores_base <- c("#FFD6D6", "#FFB3B3", "#FF9999", "#FF7777", "#FF5555", "#FF2222",
                  "#DD0000", "#BB0000", "#990000", "#770000", "#4A0000")
pal <- colorBin(
  palette  = colorRampPalette(colores_base)(length(breaks) - 1),
  domain   = range(breaks),
  bins     = breaks,
  na.color = "#CCCCCC"
)

# ── Popups ───────────────────────────────────────────────────────────────────
barrios_map <- barrios_map |>
  mutate(popup = paste0(
    "<b>", barrio_norm, "</b><br>",
    ifelse(!is.na(prom_dias),
           paste0("<b style='font-size:14px'>", prom_dias, " días</b>",
                  "<br><small>promedio de ", n, " pacientes</small>"),
           "<i style='color:#999'>Sin dato</i>")))

comunas_map <- comunas_map |>
  mutate(
    popup = paste0(
      "<b>", label_comuna, "</b><br>",
      "<span style='color:#666; font-size:12px'>", barrios, "</span><br>",
      ifelse(!is.na(prom_dias),
             paste0("<b style='font-size:14px'>", prom_dias, " días</b>",
                    "<br><small>promedio de ", n, " pacientes (villa=0)</small>"),
             "<i style='color:#999'>Sin dato</i>")),
    fill_color = if_else(numero_comuna == COMUNA_OUTLIER, "#CCCCCC", pal(prom_dias)),
    popup = if_else(
      numero_comuna == COMUNA_OUTLIER,
      "<b>Comuna 2 (Recoleta)</b><br><i style='color:#999'>Outlier — excluida de la escala</i>",
      popup)
  )

# ── Mapa ─────────────────────────────────────────────────────────────────────
cat("-- MAPA 2. Construyendo...\n")

mapa <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron,  group = "Mapa claro") |>
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro") |>
  addProviderTiles(providers$OpenStreetMap,      group = "OpenStreetMap") |>
  setView(lng = -58.437, lat = -34.617, zoom = 12) |>
  addPolygons(
    data = comunas_map, group = "Comunas (villa=0)",
    fillColor = ~fill_color, fillOpacity = 0.65,
    color = "#888888", weight = 1.2, opacity = 1,
    popup = ~popup,
    label = ~paste0(label_comuna, ": ", prom_dias, " días"),
    labelOptions     = labelOptions(style = list("font-size" = "12px")),
    highlightOptions = highlightOptions(fillOpacity = 0.85, weight = 2,
                                        color = "#444444", bringToFront = FALSE)
  ) |>
  addPolygons(
    data = barrios_map, group = "Barrios populares (villa=1)",
    fillColor = ~pal(prom_dias), fillOpacity = 0.85,
    color = "#333333", weight = 1, opacity = 1,
    popup = ~popup,
    label = ~paste0(barrio_norm, ": ", prom_dias, " días"),
    labelOptions     = labelOptions(style = list("font-size" = "12px", "font-weight" = "bold")),
    highlightOptions = highlightOptions(fillOpacity = 1, weight = 2.5,
                                        color = "#111111", bringToFront = TRUE)
  ) |>
  addLegend(position = "bottomleft", pal = pal, values = valores_escala,
            title = "Días desde última<br>consulta (promedio)",
            labFormat = labelFormat(suffix = " días"), opacity = 0.85, na.label = "Sin dato") |>
  addLayersControl(
    baseGroups    = c("Mapa claro", "Mapa oscuro", "OpenStreetMap"),
    overlayGroups = c("Comunas (villa=0)", "Barrios populares (villa=1)"),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  addFullscreenControl()

mapa
exportar_mapa_leaflet(mapa, "mapa_dias_ultima_consulta",
                      titulo = "Días desde última consulta - CABA")

cat("\n✓ 08 completo. Mapas (HTML y PDF) en", RUTA_FIG, "\n")
