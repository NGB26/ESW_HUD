

#Análisis 
# Autor: Nicolás García Balus (nicolasga@iadb.org // nicolas.gbalus@gmail.com)
# Fecha: 15/5/2026
#

RUTA <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2"

# =============================================================================
# Script: mapa_barrios_centros_salud.R
#
# Asume que ya estan cargados en el entorno:
#   todos_centros_sf  — sf con todos los centros, columna tipo_centro
#   df_final          — df con columnas nombre_barrio, villa, geometry (poligonos)
#
# Dependencias: sf, leaflet, leaflet.extras, dplyr, htmlwidgets
# =============================================================================

library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(htmlwidgets)

sf_use_s2(FALSE)


# =============================================================================
# 1. PREPARAR BARRIOS POPULARES (villa == 1 del df_final)
# =============================================================================

cat("-- 1. Preparando barrios populares...\n")

barrios_map <- df_final |>
  filter(villa == 1, !is.na(geom_barrio)) |>
  mutate(geometry = st_as_sfc(geom_barrio, crs = 4326)) |>
  st_sf(crs = 4326) |>
  st_make_valid()
# =============================================================================
# 2. PREPARAR CENTROS DE SALUD
# =============================================================================

cat("-- 2. Preparando centros de salud...\n")

centros_map <- todos_centros_sf |>
  st_transform(4326) |>
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry()

cat(sprintf("   %d centros de salud\n", nrow(centros_map)))

# =============================================================================
# 3. COLORES E ICONOS
# =============================================================================

pal_tiempo <- colorNumeric(
  palette  = c("lightblue", "red", "darkred"),
  domain   = barrios_map$tiempo_ors_min_pie_hosp,
  na.color = "#AAAAAA"
)

col_centros <- c(
  "Hospital"              = "#1A5276",
  "Centro Medico Barrial" = "#1ABC9C",
  "CeSAC"                 = "#E67E22",
  "Estacion Saludable"    = "#8E44AD"
)

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

# =============================================================================
# 4. POPUPS
# =============================================================================

barrios_map <- barrios_map |>
  mutate(popup = paste0(
    "<b>", barrio_norm, "</b><br>",
    "<span style='font-size:12px'>",
    ifelse(!is.na(tiempo_ors_min_pie_hosp),
           paste0("Hospital mas cercano: <b>", round(tiempo_ors_min_pie_hosp, 1),
                  " min</b> (A pie)"),
           "Tiempo al CeSAC: sin dato"),
    "</span>"
  ))

centros_map <- centros_map |>
  mutate(popup = paste0(
    "<b>", nombre_centro, "</b><br>",
    "<span style='color:", col_centros[tipo_centro], "; font-weight:600'>",
    tipo_centro, "</span>",
    ifelse(!is.na(comuna_texto) & comuna_texto != "",
           paste0("<br><small>", comuna_texto, "</small>"), "")
  ))

# =============================================================================
# 5. CONSTRUIR MAPA
# =============================================================================

cat("-- 3. Construyendo mapa...\n")

mapa1 <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron,   group = "Mapa claro") |>
  addProviderTiles(providers$CartoDB.DarkMatter,  group = "Mapa oscuro") |>
  addProviderTiles(providers$OpenStreetMap,       group = "OpenStreetMap") |>
  setView(lng = -58.437, lat = -34.617, zoom = 12) |>
  # Poligonos coloreados por tiempo al CeSAC
  addPolygons(
    data             = barrios_map,
    group            = "Barrios populares",
    fillColor        = ~pal_tiempo(tiempo_ors_min_pie_hosp),
    fillOpacity      = 0.7,
    color            = "white",
    weight           = 1,
    opacity          = 0.8,
    popup            = ~popup,
    label            = ~paste0(barrio_norm, " (",
                               round(tiempo_ors_min_pie_hosp, 1), " min)"),
    labelOptions     = labelOptions(style = list("font-size" = "12px")),
    highlightOptions = highlightOptions(
      fillOpacity  = 0.9,
      weight       = 2.5,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    position  = "bottomleft",
    pal       = pal_tiempo,
    values    = barrios_map$tiempo_ors_min_pie_hosp,
    title     = "Minutos al Hospital a pie<br>mas cercano",
    labFormat = labelFormat(suffix = " min"),
    opacity   = 0.8,
    na.label  = "Sin dato"
  )


# Puntos: una capa por tipo de centro con clustering
for (tipo in names(iconos)) {
  sub <- centros_map |> filter(tipo_centro == tipo)
  if (nrow(sub) == 0) next
  
  mapa1 <- mapa1 |>
    addAwesomeMarkers(
      data           = sub,
      lng            = ~lon,
      lat            = ~lat,
      group          = tipo,
      icon           = iconos[[tipo]],
      popup          = ~popup,
      label          = ~nombre_centro,
      clusterOptions = markerClusterOptions(
        showCoverageOnHover = FALSE,
        maxClusterRadius    = 40
      )
    )
}

# Leyenda de centros de salud y control de capas
mapa1 <- mapa1 |>
  addLegend(
    position = "bottomright",
    colors   = unname(col_centros),
    labels   = names(col_centros),
    title    = "Centro de salud",
    opacity  = 0.9
  ) |>
  addLayersControl(
    baseGroups    = c("Mapa claro", "Mapa oscuro", "OpenStreetMap"),
    overlayGroups = c("Barrios populares", names(iconos)),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  addFullscreenControl()

mapa1














# =============================================================================
# Script: mapa_dias_ultima_consulta.R
#
# Mapa coroplético: promedio de días desde la última consulta
#   - villa == 1 → promedio por barrio popular (polígono del barrio)
#   - villa == 0 → promedio por comuna (polígono de la comuna)
#   Gradiente: blanco (pocos días) → rojo intenso (muchos días)
#
# Objetos necesarios en el entorno:
#   df_final     — columnas: villa, barrio_norm, nro_comuna,
#                  geometry (WKT), dias_desde_ultima_consulta
#   comuna_proj  — sf con columnas: numero_comuna, barrios, area_km2, geometry
#   barrios_sf   — sf con columnas: id_barrio, nombre_barrio, tipo_barrio, geometry
#                  (el archivo original de barrios populares, para el polígono)
#
# Dependencias: sf, leaflet, leaflet.extras, dplyr, htmlwidgets
# =============================================================================



sf_use_s2(FALSE)


# =============================================================================
# 1. CALCULAR PROMEDIOS
# =============================================================================

cat("-- 1. Calculando promedios de dias_desde_ultima_consulta...\n")

# Promedio por barrio (villa == 1)
prom_barrio <- df_final |>
  filter(villa == 1, !is.na(dias_desde_ultima_consulta)) |>
  group_by(barrio_norm) |>
  summarise(
    prom_dias = round(mean(dias_desde_ultima_consulta, na.rm = TRUE), 1),
    n         = n(),
    .groups   = "drop"
  )

# Capa de barrios — tomar un polígono por barrio desde df_final directamente
barrios_map <- df_final |>
  filter(villa == 1, !is.na(geom_barrio)) |>
  group_by(barrio_norm) |>
  slice(1) |>
  ungroup() |>
  mutate(geometry = st_as_sfc(geom_barrio, crs = 4326)) |>
  st_sf(crs = 4326) |>
  st_make_valid() |>
  left_join(prom_barrio, by = "barrio_norm")

cat(sprintf("   Barrios en mapa: %d | con prom_dias: %d\n",
            nrow(barrios_map), sum(!is.na(barrios_map$prom_dias))))


# Promedio por comuna (villa == 0)
prom_comuna <- df_final |>
  filter(villa == 0, !is.na(dias_desde_ultima_consulta)) |>
  group_by(nro_comuna) |>
  summarise(
    prom_dias = round(mean(dias_desde_ultima_consulta, na.rm = TRUE), 1),
    n         = n(),
    .groups   = "drop"
  ) |>
  mutate(nro_comuna = as.integer(nro_comuna))

cat(sprintf("   Comunas con dato: %d | prom global: %.1f dias\n",
            nrow(prom_comuna), mean(prom_comuna$prom_dias, na.rm = TRUE)))

# =============================================================================
# 2. PREPARAR CAPAS ESPACIALES
# =============================================================================

cat("-- 2. Preparando capas espaciales...\n")



# ── Capa de comunas ───────────────────────────────────────────────────────────
comunas_map <- comunas_proj |>
  st_transform(4326) |>
  mutate(
    numero_comuna = as.integer(numero_comuna),
    label_comuna  = paste0("Comuna ", numero_comuna)
  ) |>
  left_join(prom_comuna, by = c("numero_comuna" = "nro_comuna"))

cat(sprintf("   Comunas en mapa: %d | con prom_dias: %d\n",
            nrow(comunas_map), sum(!is.na(comunas_map$prom_dias))))

# =============================================================================
# 3. PALETA DE COLOR COMPARTIDA
# =============================================================================

# Rango global para que la escala sea comparable entre barrios y comunas
rango_global <- range(
  c(barrios_map$prom_dias, comunas_map$prom_dias),
  na.rm = TRUE
)

cat(sprintf("   Rango global: %.1f - %.1f dias\n",
            rango_global[1], rango_global[2]))


breaks <- c(piso, 180, 200, 210, 220, 230, 240, 250, 260, 270, 280, rango_global[2])

pal <- colorBin(
  palette = c(
    "#FFD6D6",  # piso → 180  (rosa claro, no blanco)
    "#FFB3B3",  # 180 → 200
    "#FF9999",  # 200 → 210
    "#FF7777",  # 210 → 220
    "#FF5555",  # 220 → 230
    "#FF2222",  # 230 → 240
    "#DD0000",  # 240 → 250
    "#BB0000",  # 250 → 260
    "#990000",  # 260 → 270
    "#770000",  # 270 → 280
    "#4A0000"   # 280 → máximo
  ),
  domain   = c(piso, rango_global[2]),
  bins     = breaks,
  na.color = "#CCCCCC"
)
# =============================================================================
# 4. POPUPS
# =============================================================================

barrios_map <- barrios_map |>
  mutate(popup = paste0(
    "<b>", barrio_norm, "</b><br>",
    ifelse(!is.na(prom_dias),
           paste0("<b style='font-size:14px'>", prom_dias, " dias</b>",
                  "<br><small>promedio de ", n, " pacientes</small>"),
           "<i style='color:#999'>Sin dato</i>")
  ))

comunas_map <- comunas_map |>
  mutate(popup = paste0(
    "<b>", label_comuna, "</b><br>",
    "<span style='color:#666; font-size:12px'>", barrios, "</span><br>",
    ifelse(!is.na(prom_dias),
           paste0("<b style='font-size:14px'>", prom_dias, " dias</b>",
                  "<br><small>promedio de ", n, " pacientes (villa=0)</small>"),
           "<i style='color:#999'>Sin dato</i>")
  ))

# =============================================================================
# 5. CONSTRUIR MAPA
# =============================================================================

cat("-- 3. Construyendo mapa...\n")

comunas_map <- comunas_map |>
  mutate(
    fill_color = if_else(numero_comuna == 2, "#CCCCCC", pal(prom_dias)),
    popup = if_else(
      numero_comuna == 2,
      "<b>Comuna 2 (Recoleta)</b><br><i style='color:#999'>Outlier — excluida de la escala</i>",
      popup
    )
  )

mapa <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron,   group = "Mapa claro") |>
  addProviderTiles(providers$CartoDB.DarkMatter,  group = "Mapa oscuro") |>
  addProviderTiles(providers$OpenStreetMap,       group = "OpenStreetMap") |>
  setView(lng = -58.437, lat = -34.617, zoom = 12) |>
  
  # Capa de comunas (villa == 0) — se dibuja primero, queda debajo
  addPolygons(
    data             = comunas_map,
    group            = "Comunas (villa=0)",
    fillColor        = ~fill_color,
    fillOpacity      = 0.65,
    color            = "#888888",
    weight           = 1.2,
    opacity          = 1,
    popup            = ~popup,
    label            = ~paste0(label_comuna, ": ", prom_dias, " dias"),
    labelOptions     = labelOptions(style = list("font-size" = "12px")),
    highlightOptions = highlightOptions(
      fillOpacity  = 0.85,
      weight       = 2,
      color        = "#444444",
      bringToFront = FALSE
    )
  ) |>
  
  # Capa de barrios populares (villa == 1) — encima de comunas
  addPolygons(
    data             = barrios_map,
    group            = "Barrios populares (villa=1)",
    fillColor        = ~pal(prom_dias),
    fillOpacity      = 0.85,
    color            = "#333333",
    weight           = 1,
    opacity          = 1,
    popup            = ~popup,
    label            = ~paste0(barrio_norm, ": ", prom_dias, " dias"),
    labelOptions     = labelOptions(style = list("font-size"   = "12px",
                                                 "font-weight" = "bold")),
    highlightOptions = highlightOptions(
      fillOpacity  = 1,
      weight       = 2.5,
      color        = "#111111",
      bringToFront = TRUE
    )
  ) |>
  
  # Leyenda
  addLegend(
    position  = "bottomleft",
    pal       = pal,
    values    = c(barrios_map$prom_dias, comunas_map$prom_dias),
    title     = "Dias desde ultima<br>consulta (promedio)",
    labFormat = labelFormat(suffix = " dias"),
    opacity   = 0.85,
    na.label  = "Sin dato"
  ) |>
  
  # Control de capas
  addLayersControl(
    baseGroups    = c("Mapa claro", "Mapa oscuro", "OpenStreetMap"),
    overlayGroups = c("Comunas (villa=0)", "Barrios populares (villa=1)"),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  addFullscreenControl()
mapa

# =============================================================================
# 6. EXPORTAR
# =============================================================================

saveWidget(
  mapa,
  file          = file.path(RUTA, "mapa_dias_ultima_consulta.html"),
  selfcontained = TRUE,
  title         = "Dias desde ultima consulta - CABA"
)







