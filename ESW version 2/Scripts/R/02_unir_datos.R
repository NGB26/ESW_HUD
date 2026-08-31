# =============================================================================
# Script: unir_datos.R
# Descripción: Une todos los archivos de efectores y polígonos de CABA
#              en un único sf consolidado
# Fuente: datos abiertos Buenos Aires (Redatam / INDEC 2022)
# =============================================================================

library(sf)
library(dplyr)
library(readr)

setwd("C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2")

# ── Función auxiliar: leer CSV con geometría WKT ──────────────────────────────
read_geo_csv <- function(path, tipo, crs_orig = 4326) {
  df <- read_csv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

  # La columna geometry puede ser POINT o POLYGON/MULTIPOLYGON en WKT
  df <- df |>
    mutate(geometry = sf::st_as_sfc(geometry, crs = crs_orig))

  sf_obj <- sf::st_sf(df, crs = crs_orig)

  # Si el CRS original no es WGS84 (hospitales usan coordenadas proyectadas POSGAR)
  # detectamos automáticamente por magnitud de coordenadas
  bbox <- sf::st_bbox(sf_obj)
  if (abs(bbox["xmin"]) < 1000) {          # coordenadas en grados → WGS84 OK
    sf_obj <- sf::st_set_crs(sf_obj, 4326)
  } else {                                  # coordenadas métricas → POSGAR 2007
    sf_obj <- sf::st_set_crs(sf_obj, 5347) |>
      sf::st_transform(4326)
  }

  sf_obj |> mutate(tipo_efector = tipo)
}

# ── 1. CeSAC (Centros de Salud Nivel 1) ───────────────────────────────────────
cesac <- read_geo_csv(
  "centros_salud_nivel_1_cesac.csv",
  tipo = "CeSAC"
) |>
  rename(
    nombre    = nombre,
    direccion = direccion,
    barrio    = barrio,
    comuna    = comuna,
    telefono  = telefono
  ) |>
  select(tipo_efector, nombre, direccion, barrio, comuna, telefono,
         web, area_progr, especialid, geometry)

# ── 2. Centros Médicos Barriales (CMB) ────────────────────────────────────────
cmb <- read_geo_csv(
  "centros_medicos_barriales.csv",
  tipo = "CMB"
) |>
  rename(especialid = esp) |>
  select(tipo_efector, nombre, direccion, barrio, comuna, telefono,
         web, area_progr, especialid, geometry)

# ── 3. Estaciones Saludables ──────────────────────────────────────────────────
estaciones <- read_geo_csv(
  "estaciones_saludables.csv",
  tipo = "Estacion Saludable"
) |>
  mutate(
    web       = NA_character_,
    area_progr = NA_character_,
    telefono  = NA_character_,
    especialid = servicio
  ) |>
  select(tipo_efector, nombre, direccion, barrio, comuna, telefono,
         web, area_progr, especialid, geometry)

# ── 4. Hospitales ─────────────────────────────────────────────────────────────
hospitales <- read_geo_csv(
  "hospitales.csv",
  tipo = "Hospital"
) |>
  rename(
    nombre    = fna,
    direccion = dir,
    barrio    = bar,
    telefono  = tel,
    web       = web,
    area_progr = sag,
    especialid = esp
  ) |>
  mutate(
    comuna = paste0("Comuna ", com)
  ) |>
  select(tipo_efector, nombre, direccion, barrio, comuna, telefono,
         web, area_progr, especialid, geometry)

# ── 5. Barrios Populares (polígonos) ──────────────────────────────────────────
barrios_pop <- read_geo_csv(
  "barrios_populares_poligono.csv",
  tipo = "Barrio Popular"
) |>
  mutate(
    direccion  = NA_character_,
    barrio     = nombre,
    comuna     = NA_character_,
    telefono   = NA_character_,
    web        = NA_character_,
    area_progr = tipo,
    especialid = NA_character_
  ) |>
  select(tipo_efector, nombre, direccion, barrio, comuna, telefono,
         web, area_progr, especialid, geometry)

# ── 6. Comunas (polígonos administrativos) ────────────────────────────────────
# Nota: este CSV usa ";" como separador
comunas_raw <- read_delim(
  "comunas.csv",
  delim = ";",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
)

comunas <- comunas_raw |>
  mutate(geometry = sf::st_as_sfc(geometry, crs = 4326)) |>
  sf::st_sf(crs = 4326) |>
  mutate(
    tipo_efector = "Limite Comunal",
    nombre       = paste0("Comuna ", comuna),
    direccion    = NA_character_,
    barrio       = barrios,        # columna con lista de barrios incluidos
    comuna       = as.character(comuna),
    telefono     = NA_character_,
    web          = NA_character_,
    area_progr   = NA_character_,
    especialid   = NA_character_
  ) |>
  select(tipo_efector, nombre, direccion, barrio, comuna, telefono,
         web, area_progr, especialid, geometry)

# ── 7. Unión de todos los capas ───────────────────────────────────────────────
# rbind requiere mismas columnas + misma clase de geometría
# Separamos puntos de polígonos para mayor claridad

capas_punto    <- list(cesac, cmb, estaciones, hospitales)
capas_poligono <- list(barrios_pop, comunas)

efectores_punto    <- bind_rows(capas_punto)
efectores_poligono <- bind_rows(capas_poligono)

# Dataset completo (geometría mixta: POINT + POLYGON/MULTIPOLYGON)
efectores_todos <- bind_rows(efectores_punto, efectores_poligono)

# ── 8. Resumen ────────────────────────────────────────────────────────────────
cat("\n=== RESUMEN DEL DATASET UNIFICADO ===\n")
cat(sprintf("Total de registros: %d\n\n", nrow(efectores_todos)))

efectores_todos |>
  sf::st_drop_geometry() |>
  count(tipo_efector, name = "n") |>
  print()

cat("\nColumnas disponibles:\n")
print(names(efectores_todos))

cat("\nCRS:", sf::st_crs(efectores_todos)$input, "\n")

# ── 9. Exportar ───────────────────────────────────────────────────────────────
# CSV plano (sin geometría) — útil para joins tabulares
efectores_todos |>
  sf::st_drop_geometry() |>
  write_csv("efectores_unificados.csv")

# GeoPackage — formato geoespacial recomendado (soporta geometría mixta)
sf::st_write(
  efectores_todos,
  "efectores_unificados.gpkg",
  layer = "efectores",
  delete_dsn = TRUE,
  quiet = TRUE
)

# Opcionalmente, capas separadas por tipo de geometría
sf::st_write(
  efectores_punto,
  "efectores_unificados.gpkg",
  layer = "puntos",
  append = TRUE,
  quiet = TRUE
)

sf::st_write(
  efectores_poligono,
  "efectores_unificados.gpkg",
  layer = "poligonos",
  append = TRUE,
  quiet = TRUE
)

cat("\n✓ Archivos exportados:\n")
cat("  - efectores_unificados.csv   (tabla plana sin geometría)\n")
cat("  - efectores_unificados.gpkg  (capas: efectores / puntos / poligonos)\n")

