# =============================================================================
# Script: 02_unir_datos.R   (versión corregida de unir_datos.R)
# Descripción: Une efectores de salud, barrios populares y comunas en un único
#              sf consolidado (CSV plano + GeoPackage).
#
# Correcciones respecto a la versión anterior:
#   · Sin setwd() ni rutas relativas: usa 00_config.R y buscar_archivo().
#   · HOSPITALES: se leen en el CRS catastral CABA 2019 (CRS_CABA2019). Antes se
#     asignaba EPSG:5347 por la magnitud de las coordenadas, lo que los ubicaba
#     mal sin dar error.
#   · `comuna` se normaliza a "Comuna N" en todas las capas (antes mezclaba
#     numérico y texto → bind_rows fallaba), con NA real si falta.
#   · Se evita el conflicto entre el argumento `tipo` y una columna `tipo` del
#     CSV (el data-mask de dplyr la tomaba en lugar del argumento).
#   · Columnas opcionales (web, telefono, etc.) se completan con NA si no
#     existen, en vez de romper el select().
#   · Se combinan con rbind() (requiere mismas columnas) y se escribe a
#     data/processed.
# Nota: 04_unir_radio_censal.R NO depende de este script (arma sus propios
#       centros con cargar_centros_salud()). Se mantiene para el mapa/QGIS.
# =============================================================================

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

# ── Estandarizador de columnas ────────────────────────────────────────────────
# Cada argumento es el NOMBRE de la columna de origen (o NULL si no existe).
estandarizar <- function(x, tipo_efector, nombre = "nombre", direccion = "direccion",
                         barrio = "barrio", comuna = "comuna", telefono = "telefono",
                         web = "web", area_progr = "area_progr",
                         especialid = "especialid") {
  tibble::tibble(
    tipo_efector = tipo_efector,
    nombre       = col_chr(x, nombre),
    direccion    = col_chr(x, direccion),
    barrio       = col_chr(x, barrio),
    comuna       = comuna_texto_std(col_chr(x, comuna)),
    telefono     = col_chr(x, telefono),
    web          = col_chr(x, web),
    area_progr   = col_chr(x, area_progr),
    especialid   = col_chr(x, especialid)
  ) |>
    st_sf(geometry = st_geometry(x)) |>
    st_transform(4326)
}

dir_salud   <- RUTA_SALUD
buscar      <- function(nm) buscar_archivo(nm, RUTA_RAW)

# ── 1. CeSAC ──────────────────────────────────────────────────────────────────
x <- leer_wkt_csv(buscar("centros_salud_nivel_1_cesac.csv"), 4326)
cesac <- estandarizar(x, "CeSAC")

# ── 2. Centros Médicos Barriales ──────────────────────────────────────────────
x <- leer_wkt_csv(buscar("centros_medicos_barriales.csv"), 4326)
cmb <- estandarizar(x, "CMB", especialid = "esp")

# ── 3. Estaciones Saludables ──────────────────────────────────────────────────
x <- leer_wkt_csv(buscar("estaciones_saludables.csv"), 4326)
estaciones <- estandarizar(x, "Estacion Saludable",
                           web = NULL, area_progr = NULL, telefono = NULL,
                           especialid = "servicio")

# ── 4. Hospitales (CRS catastral CABA 2019) ───────────────────────────────────
x <- leer_wkt_csv(buscar("hospitales.csv"), CRS_CABA2019)
hospitales <- estandarizar(x, "Hospital",
                           nombre = "fna", direccion = "dir", barrio = "bar",
                           comuna = "com", telefono = "tel", web = "web",
                           area_progr = "sag", especialid = "esp")

# ── 5. Barrios populares (polígonos) ──────────────────────────────────────────
x <- leer_wkt_csv(buscar("barrios_populares_poligono.csv"), 4326)
barrios_pop <- estandarizar(x, "Barrio Popular",
                            direccion = NULL, barrio = "nombre", comuna = NULL,
                            telefono = NULL, web = NULL,
                            area_progr = "tipo",       # tipo de barrio popular
                            especialid = NULL)

# ── 6. Comunas (CSV con ";") ──────────────────────────────────────────────────
x <- leer_wkt_csv(buscar("comunas.csv"), 4326, delim = ";")
comunas <- estandarizar(x, "Limite Comunal",
                        nombre = NULL, direccion = NULL, barrio = "barrios",
                        comuna = "comuna", telefono = NULL, web = NULL,
                        area_progr = NULL, especialid = NULL)
comunas$nombre <- comunas$comuna     # "Comuna N"

# ── 7. Unión ──────────────────────────────────────────────────────────────────
efectores_punto    <- rbind(cesac, cmb, estaciones, hospitales)
efectores_poligono <- rbind(barrios_pop, comunas)
efectores_todos    <- rbind(efectores_punto, efectores_poligono)

# ── 8. Resumen ────────────────────────────────────────────────────────────────
cat("\n=== RESUMEN DEL DATASET UNIFICADO ===\n")
cat(sprintf("Total de registros: %d\n\n", nrow(efectores_todos)))
print(efectores_todos |> st_drop_geometry() |> count(tipo_efector, name = "n"))
cat("\nCRS:", st_crs(efectores_todos)$input, "\n")

# Chequeo rápido: los hospitales deben caer dentro de CABA
bb <- st_bbox(hospitales)
cat(sprintf("Hospitales bbox lon [%.3f, %.3f] lat [%.3f, %.3f] (CABA ≈ lon -58.54/-58.33, lat -34.71/-34.53)\n",
            bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]))

# ── 9. Exportar ───────────────────────────────────────────────────────────────
efectores_todos |> st_drop_geometry() |>
  write_csv(file.path(RUTA_PROC, "efectores_unificados.csv"))

gpkg <- file.path(RUTA_PROC, "efectores_unificados.gpkg")
st_write(efectores_todos,    gpkg, layer = "efectores", delete_dsn = TRUE, quiet = TRUE)
st_write(efectores_punto,    gpkg, layer = "puntos",    append = TRUE,     quiet = TRUE)
st_write(efectores_poligono, gpkg, layer = "poligonos", append = TRUE,     quiet = TRUE)

cat("\n✓ Exportado en", RUTA_PROC, "\n   efectores_unificados.csv / .gpkg (capas: efectores, puntos, poligonos)\n")
