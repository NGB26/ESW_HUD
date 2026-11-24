# =============================================================================
# 00_config.R  —  Rutas y funciones compartidas por los scripts 01-05
#
# Se carga con source() al inicio de cada script. Centraliza:
#   · rutas (una sola constante RAIZ para cambiar si se mueve el proyecto)
#   · CRS del proyecto
#   · buscar_archivo(): localiza un archivo por nombre dentro de data/raw
#   · leer_wkt_csv(): CSV con columna `geometry` en WKT -> sf
#   · cargar_centros_salud(): todos_centros_sf (Hospital/CMB/CeSAC/Est. Saludable)
#
# IMPORTANTE (reproducibilidad): correr los scripts en una sesión limpia.
#   RStudio > Tools > Global Options > General >
#     · desmarcar "Restore .RData into workspace at startup"
#     · "Save workspace to .RData on exit" = Never
#   y borrar el .RData de 82 MB de la carpeta de scripts. Los objetos viejos
#   guardados ahí enmascaran errores (p. ej. `datos_comuna_censo`, `radios_proj`).
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
})

# ── Rutas ─────────────────────────────────────────────────────────────────────
RAIZ <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/ESW version 2"
)

RUTA_RAW     <- file.path(RAIZ, "data/raw")
RUTA_CENSO   <- file.path(RUTA_RAW, "censo_2022")
RUTA_SALUD   <- file.path(RUTA_RAW, "salud")
RUTA_BARRIOS <- file.path(RUTA_RAW, "barrios_populares")
RUTA_SHP     <- file.path(RUTA_RAW, "radios_censales_2022/shapefile")
RUTA_PROC    <- file.path(RAIZ, "data/processed")
RUTA_FIG     <- file.path(RAIZ, "output/figures")

for (d in c(RUTA_PROC, RUTA_FIG)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

PATH_SHP_RADIOS <- file.path(RUTA_SHP, "radios2022_v1_0.shp")

# ── CRS ───────────────────────────────────────────────────────────────────────
CRS_PROY <- 22185   # CRS de trabajo (metros) para distancias y áreas

# CRS catastral CABA 2019 (EPSG:9498; false_easting=20000, false_northing=70000).
# Los hospitales vienen en este CRS (NO en POSGAR 5347 como asumía 02 original).
CRS_CABA2019 <- paste(
  "+proj=tmerc +lat_0=-34.6292666666667 +lon_0=-58.4633083333333 +k=1",
  "+x_0=20000 +y_0=70000 +ellps=WGS84 +units=m +no_defs"
)

# ── Utilidades ────────────────────────────────────────────────────────────────

# Busca `nombre` (nombre exacto de archivo) recursivamente bajo `base`.
# Evita depender de la subcarpeta exacta; falla con un mensaje claro.
buscar_archivo <- function(nombre, base = RUTA_RAW) {
  if (!dir.exists(base)) {
    stop("No existe la carpeta base: ", base,
         "\n  Revisar RAIZ en 00_config.R")
  }
  todos <- list.files(base, recursive = TRUE, full.names = TRUE)
  hit   <- todos[tolower(basename(todos)) == tolower(nombre)]
  if (length(hit) == 0) {
    stop("No encuentro '", nombre, "' bajo ", base)
  }
  if (length(hit) > 1) {
    message("Aviso: '", nombre, "' aparece ", length(hit),
            " veces; uso: ", hit[1])
  }
  hit[1]
}

# Lee un CSV con columna `geometry` en WKT y devuelve sf con el CRS dado.
leer_wkt_csv <- function(path, crs, delim = ",") {
  df <- read_delim(path, delim = delim, show_col_types = FALSE,
                   locale = locale(encoding = "UTF-8"))
  if (!"geometry" %in% names(df)) {
    stop("'", basename(path), "' no tiene columna 'geometry'. Columnas: ",
         paste(names(df), collapse = ", "))
  }
  g  <- st_as_sfc(as.character(df$geometry), crs = crs)
  df <- df[, setdiff(names(df), "geometry"), drop = FALSE]
  out <- st_sf(df, geometry = g)
  if (any(!st_is_valid(out))) out <- st_make_valid(out)
  out
}

# Devuelve la columna `nm` como character, o NA si no existe / nm es NULL.
col_chr <- function(x, nm) {
  if (is.null(nm) || !nm %in% names(x)) return(rep(NA_character_, nrow(x)))
  as.character(x[[nm]])
}

# "Comuna N" a partir de cualquier formato ("1", "01", "Comuna 1"). NA se propaga
# como NA real (paste0 convertiría NA en el string "NA").
comuna_texto_std <- function(x) {
  n <- suppressWarnings(readr::parse_number(as.character(x)))
  if_else(is.na(n), NA_character_, paste0("Comuna ", as.integer(n)))
}

# Todos los centros de salud en CRS_PROY: tipo_centro, nombre_centro, comuna_texto.
cargar_centros_salud <- function() {
  hosp <- leer_wkt_csv(buscar_archivo("hospitales.csv", RUTA_SALUD), CRS_CABA2019)
  hosp <- hosp |>
    mutate(tipo_centro   = "Hospital",
           nombre_centro = col_chr(hosp, "fna"),
           comuna_texto  = comuna_texto_std(col_chr(hosp, "com"))) |>
    select(tipo_centro, nombre_centro, comuna_texto) |>
    st_transform(CRS_PROY)

  leer_wgs84 <- function(archivo, tipo) {
    x <- leer_wkt_csv(buscar_archivo(archivo, RUTA_SALUD), 4326)
    x |>
      mutate(tipo_centro   = tipo,
             nombre_centro = col_chr(x, "nombre"),
             comuna_texto  = comuna_texto_std(col_chr(x, "comuna"))) |>
      select(tipo_centro, nombre_centro, comuna_texto) |>
      st_transform(CRS_PROY)
  }

  cmb   <- leer_wgs84("centros_medicos_barriales.csv",  "Centro Medico Barrial")
  cesac <- leer_wgs84("centros_salud_nivel_1_cesac.csv", "CeSAC")
  est   <- leer_wgs84("estaciones_saludables.csv",      "Estacion Saludable")

  bind_rows(hosp, cmb, cesac, est)
}

TIPOS_CENTRO <- c("Hospital", "Centro Medico Barrial", "CeSAC", "Estacion Saludable")

# ── Salidas gráficas: SIEMPRE en PDF (vectorial) ──────────────────────────────

# Guarda un ggplot como PDF en RUTA_FIG. `nombre` puede venir con cualquier
# extensión (se fuerza .pdf).
guardar_pdf <- function(plot, nombre, width = 10, height = 10) {
  archivo <- file.path(RUTA_FIG, paste0(tools::file_path_sans_ext(nombre), ".pdf"))
  dev <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
  ggplot2::ggsave(filename = archivo, plot = plot, width = width, height = height,
                  device = dev, bg = "white")
  message("PDF guardado: ", archivo)
  invisible(archivo)
}

# Mapa leaflet: guarda el HTML interactivo y, si hay webshot2 (+ Chrome/Edge),
# también un PDF. Los mapas leaflet no son imágenes de ggplot; el PDF se genera
# imprimiendo el HTML con un navegador sin interfaz.
exportar_mapa_leaflet <- function(mapa, nombre, titulo = nombre, delay = 4) {
  html <- file.path(RUTA_FIG, paste0(nombre, ".html"))
  pdf  <- file.path(RUTA_FIG, paste0(nombre, ".pdf"))
  htmlwidgets::saveWidget(mapa, file = html, selfcontained = TRUE, title = titulo)
  message("HTML guardado: ", html)
  if (requireNamespace("webshot2", quietly = TRUE)) {
    ok <- tryCatch({
      webshot2::webshot(html, file = pdf, delay = delay)
      TRUE
    }, error = function(e) {
      message("No se pudo generar el PDF del mapa: ", conditionMessage(e)); FALSE
    })
    if (ok) message("PDF guardado: ", pdf)
  } else {
    message("Para el PDF del mapa leaflet: install.packages('webshot2') ",
            "(requiere Google Chrome o Edge instalado).")
  }
  invisible(list(html = html, pdf = pdf))
}

# ── OpenRouteService ──────────────────────────────────────────────────────────
# La clave NO va en el código (los scripts viven en OneDrive). Se lee de la
# variable de entorno ORS_API_KEY:
#   usethis::edit_r_environ()  →  agregar la línea  ORS_API_KEY=tu_clave  → reiniciar R
ors_clave <- function() Sys.getenv("ORS_API_KEY", unset = "")

min_seguro <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)

# Lista de c(lon, lat) en WGS84 a partir de un sf de puntos
coords_lista <- function(sf_pts) {
  m <- st_coordinates(st_transform(sf_pts, 4326))
  lapply(seq_len(nrow(m)), function(i) c(m[i, 1], m[i, 2]))
}

# Para cada origen, mínimo de distancia y de duración hacia el conjunto de
# destinos (ORS Matrix API), con chunking de orígenes para respetar el límite
# de ~3.500 pares por request del plan gratuito.
#   OJO: dist_km es el mínimo de la matriz de DISTANCIA y tiempo_min el mínimo de
#   la matriz de DURACIÓN; pueden corresponder a destinos distintos.
ors_min_por_origen <- function(lista_orig, lista_dest, perfil,
                               max_pares = 3000, pausa = 1.5) {
  n_o <- length(lista_orig); n_d <- length(lista_dest)
  out <- tibble::tibble(dist_km = rep(NA_real_, n_o), tiempo_min = rep(NA_real_, n_o))
  if (n_d == 0) return(out)
  chunks <- split(seq_len(n_o), ceiling(seq_len(n_o) / max(1L, floor(max_pares / n_d))))
  for (idx in chunks) {
    Sys.sleep(pausa)                       # rate limit plan free: 40 req/min
    orig <- lista_orig[idx]; n_oc <- length(orig)
    res <- tryCatch(
      openrouteservice::ors_matrix(
        locations    = c(orig, lista_dest),
        profile      = perfil,
        sources      = seq(0, n_oc - 1),
        destinations = seq(n_oc, n_oc + n_d - 1),
        metrics      = c("duration", "distance"),
        units        = "km"
      ),
      error = function(e) { message("   ✗ Error ORS [", perfil, "]: ", conditionMessage(e)); NULL }
    )
    if (is.null(res)) next
    out$dist_km[idx]    <- round(apply(res$distances, 1, min_seguro), 3)
    out$tiempo_min[idx] <- round(apply(res$durations, 1, min_seguro) / 60, 1)
  }
  out
}

cat("✓ 00_config.R cargado. RAIZ =", RAIZ, "\n")
