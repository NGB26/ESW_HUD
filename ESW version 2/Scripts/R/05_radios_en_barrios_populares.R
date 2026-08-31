# =============================================================================
# Script: radios_en_barrios_populares.R
#
# Objetivo:
#   1. Unir los radios censales con su polígono (shapefile INDEC 2022)
#   2. Identificar qué radios caen dentro de cada barrio popular
#   3. Agregar las variables del Censo 2022 (de radio_censal_unido.csv)
#      a nivel de barrio popular
#
# Entradas:
#   radios2022_v1_0.shp/.dbf/.prj   Shapefile INDEC Censo 2022 (nacional)
#   barrios_populares_poligono.csv  Polígonos WKT de barrios populares CABA
#   radio_censal_unido.csv          Output de unir_radio_censal.R
#
# Clave de join shapefile ↔ Redatam:
#   El campo LINK del DBF tiene 9 caracteres: "02"(prov) + DEPTO(3) + FRAC(2) + RADIO(2)
#   El código Redatam es: (2000 + DEPTO) * 10000 + FRAC * 100 + RADIO
#   Ejemplo: LINK = "020070101"  →  codigo_redatam = 20070101
#
# Lógica espacial:
#   Se usa el CENTROIDE de cada radio para asignarlo a un barrio popular.
#   Un radio se asigna al barrio cuyo polígono contiene su centroide.
#   Radios sin centroide en ningún barrio quedan con barrio = NA.
#
# Agregación de indicadores:
#   Las variables de CONTEO (n_*) se SUMAN por barrio.
#   El TOTAL de viviendas (total_viv) también se suma.
#   Los PORCENTAJES se RECALCULAN desde los totales agregados
#   (no se promedian porcentajes directamente, lo que daría resultados incorrectos
#   cuando los radios tienen distinto número de viviendas).
#
# Outputs:
#   radios_con_barrio.csv          1 fila por radio — indicadores + barrio asignado
#   indicadores_por_barrio.csv     1 fila por barrio — indicadores Censo 2022 agregados
#   radios_barrios.gpkg            GeoPackage con 2 capas (para QGIS/R)
#
# Dependencias: sf, dplyr, readr
# =============================================================================

library(sf)
library(dplyr)
library(readr)

# ── Rutas — ajustar si los archivos están en otra carpeta ────────────────────
RUTA <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2"

PATH_SHP_RADIOS  <- file.path("C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/ESW version 2/data/raw/radios_censales_2022/shapefile", "radios2022_v1_0.shp")
PATH_BARRIOS     <- file.path(RUTA, "barrios_populares_poligono.csv")
PATH_DATOS_RADIO <- file.path(RUTA, "radio_censal_unido.csv")

# =============================================================================
# 1. CARGAR SHAPEFILE Y FILTRAR A CABA
# =============================================================================

cat("── 1. Cargando shapefile de radios censales.../n")

Sys.setenv(SHAPE_RESTORE_SHX = "YES")
radios_nac <- st_read(PATH_SHP_RADIOS, quiet = TRUE)
cat(sprintf("   Total nacional: %d radios/n", nrow(radios_nac)))

# Filtrar CABA (PROV == "02") y construir el código Redatam desde los campos del DBF:
#   codigo_redatam = (2000 + DEPTO) * 10000 + FRAC * 100 + RADIO
radios_sf <- radios_nac |>
  filter(PROV == "02") |>
  mutate(
    codigo_redatam = (2000L + as.integer(DEPTO)) * 10000L +
                      as.integer(FRAC) * 100L +
                      as.integer(RADIO),
    numero_comuna  = as.integer(DEPTO)   # 7 = C1, 14 = C2, ..., 105 = C15
  ) |>
  select(
    codigo_redatam,
    numero_comuna,
    link       = LINK,
    nom_depto  = NOMDEPTO,
    fraccion   = FRAC,
    radio      = RADIO,
    tipo_radio = TIPO,
    geometry
  )

cat(sprintf("   CABA: %d radios/n", nrow(radios_sf)))

# =============================================================================
# 2. UNIR SHAPEFILE CON DATOS DEL CENSO (radio_censal_unido.csv)
# =============================================================================

cat("── 2. Uniendo shapefile con indicadores del Censo 2022.../n")

datos_radio <- read_csv(PATH_DATOS_RADIO, show_col_types = FALSE)
cat(sprintf("   radio_censal_unido.csv: %d filas, %d variables/n",
            nrow(datos_radio), ncol(datos_radio)))

# Join exacto por codigo_redatam (match perfecto 3820 = 3820)
radios_con_datos <- radios_sf |>
  left_join(datos_radio, by = c("codigo_redatam" = "codigo"))

n_matched <- sum(!is.na(radios_con_datos$total_viv))
cat(sprintf("   Radios con datos de Censo: %d / %d/n", n_matched, nrow(radios_sf)))

if (n_matched < nrow(radios_sf)) {
  warning(sprintf("%d radios sin datos de Censo tras el join.",
                  nrow(radios_sf) - n_matched))
}
# =============================================================================
# 3. CARGAR Y REPARAR BARRIOS POPULARES
# =============================================================================

cat("── 3. Cargando barrios populares.../n")

barrios_raw <- read_csv(PATH_BARRIOS, show_col_types = FALSE,
                        locale = locale(encoding = "UTF-8"))

barrios_sf <- barrios_raw |>
  mutate(geometry = st_as_sfc(geometry, crs = 4326)) |>
  st_sf(crs = 4326) |>
  rename(id_barrio = id, nombre_barrio = nombre, tipo_barrio = tipo)

cat(sprintf("   %d barrios populares cargados/n", nrow(barrios_sf)))

# Reparar geometrías inválidas (vértices duplicados, loops degenerados)
n_inv <- sum(!st_is_valid(barrios_sf))
if (n_inv > 0) {
  cat(sprintf("   Reparando %d geometrías inválidas.../n", n_inv))
  barrios_sf <- st_make_valid(barrios_sf)
}

# Usar GEOS (plano) en lugar de S2 (esférico) para evitar errores de validez
# estricta en polígonos complejos. La diferencia es despreciable a escala de CABA.
sf_use_s2(FALSE)

# Reproyectar todo a POSGAR 2007 / Argentina Transverse Mercator (metros)
# OBLIGATORIO para calcular áreas correctamente
cat("   Reproyectando a POSGAR 2007 (EPSG:22185) para cálculo de áreas.../n")

radios_proj  <- radios_con_datos |> st_transform(22185)
barrios_proj <- barrios_sf       |> st_transform(22185)
# =============================================================================
# 4. INTERSECCIÓN CON PONDERACIÓN POR ÁREA
# =============================================================================
#
# Para cada par (radio, barrio) que se superponga:
#   peso = área(radio ∩ barrio) / área(radio)
#
# Un radio con peso = 1.0 cae completamente dentro del barrio.
# Un radio con peso = 0.3 tiene solo el 30% de su área dentro del barrio;
# se le atribuye solo el 30% de sus viviendas/personas al barrio.

cat("── 4. Calculando intersecciones y pesos por área.../n")

# Área de cada radio (en m²) ANTES de intersectar
radios_proj <- radios_proj |>
  mutate(area_radio_m2 = as.numeric(st_area(geometry)))

# Intersección geométrica radio × barrio
# Genera una fila por cada par que se superponga (aunque sea parcialmente)
intersecciones <- st_intersection(
  radios_proj  |> select(codigo_redatam, area_radio_m2),
  barrios_proj |> select(id_barrio, nombre_barrio, tipo_barrio)
)

cat(sprintf("   Pares radio × barrio con solapamiento: %d/n", nrow(intersecciones)))

# Calcular área de cada intersección y el peso correspondiente
intersecciones <- intersecciones |>
  mutate(
    area_interseccion_m2 = as.numeric(st_area(geometry)),
    peso                 = area_interseccion_m2 / area_radio_m2
  )

cat(sprintf("   Radios únicos que tocan algún barrio: %d/n",
            n_distinct(intersecciones$codigo_redatam)))
cat(sprintf("   Barrios únicos con algún radio:       %d/n",
            n_distinct(intersecciones$id_barrio)))

# Distribución de pesos (diagnóstico)
cat("/n   Distribución de pesos (fracción del radio dentro del barrio):/n")
print(summary(intersecciones$peso))

# =============================================================================
# 5. AGREGAR INDICADORES POR BARRIO (PONDERADO POR ÁREA)
# =============================================================================
#
# Para cada par (radio_i, barrio_j):
#   conteo_ponderado = n_var_i × peso_ij
#   total_ponderado  = total_viv_i × peso_ij
#
# Luego para cada barrio:
#   n_var_barrio   = Σ conteo_ponderado
#   total_viv_barrio = Σ total_ponderado
#   pct_var        = n_var_barrio / total_viv_barrio × 100

cat("── 5. Agregando indicadores por barrio (ponderación por área).../n")

# Variables de conteo a ponderar
vars_n <- c(
  "n_agua_corriente", "n_calidad_mat", "n_clima_educ_bajo",
  "n_NBI", "n_sec_completa", "n_edad_65_mas",
  "n_hacinamiento", "n_cond_san_A", "n_cond_san_B"
)

# Pegar datos de Censo a la tabla de intersecciones (sin geometría)
intersecciones_datos <- intersecciones |>
  st_drop_geometry() |>
  left_join(
    radios_con_datos |> st_drop_geometry() |>
      select(codigo_redatam, numero_comuna.x, total_viv, all_of(vars_n)),
    by = "codigo_redatam"
  ) |>
  # Escalar cada conteo por el peso del solapamiento
  mutate(
    across(
      all_of(c("total_viv", vars_n)),
      ~ . * peso,
      .names = "{.col}_pond"
    )
  )

# Agregar por barrio: sumar conteos ponderados
indicadores_por_barrio <- intersecciones_datos |>
  group_by(id_barrio, nombre_barrio, tipo_barrio) |>
  summarise(
    n_radios          = n(),
    comunas           = paste(sort(unique(numero_comuna.x)), collapse = ", "),
    # Denominador ponderado
    total_viv         = round(sum(total_viv_pond,         na.rm = TRUE)),
    # Conteos ponderados
    n_agua_corriente  = round(sum(n_agua_corriente_pond,  na.rm = TRUE)),
    n_calidad_mat     = round(sum(n_calidad_mat_pond,      na.rm = TRUE)),
    n_clima_educ_bajo = round(sum(n_clima_educ_bajo_pond,  na.rm = TRUE)),
    n_NBI             = round(sum(n_NBI_pond,              na.rm = TRUE)),
    n_sec_completa    = round(sum(n_sec_completa_pond,     na.rm = TRUE)),
    n_edad_65_mas     = round(sum(n_edad_65_mas_pond,      na.rm = TRUE)),
    n_hacinamiento    = round(sum(n_hacinamiento_pond,     na.rm = TRUE)),
    n_cond_san_A      = round(sum(n_cond_san_A_pond,       na.rm = TRUE)),
    n_cond_san_B      = round(sum(n_cond_san_B_pond,       na.rm = TRUE)),
    .groups = "drop"
  ) |>
  # Recalcular porcentajes desde totales ponderados
  mutate(
    pct_agua_corriente  = round(n_agua_corriente  / total_viv * 100, 2),
    pct_calidad_mat     = round(n_calidad_mat      / total_viv * 100, 2),
    pct_clima_educ_bajo = round(n_clima_educ_bajo  / total_viv * 100, 2),
    pct_NBI             = round(n_NBI              / total_viv * 100, 2),
    pct_sec_completa    = round(n_sec_completa     / total_viv * 100, 2),
    pct_edad_65_mas     = round(n_edad_65_mas      / total_viv * 100, 2),
    pct_hacinamiento    = round(n_hacinamiento     / total_viv * 100, 2),
    pct_cond_san_A      = round(n_cond_san_A       / total_viv * 100, 2),
    pct_cond_san_B      = round(n_cond_san_B       / total_viv * 100, 2)
  ) |>
  arrange(nombre_barrio)

cat(sprintf("   Barrios con al menos 1 radio: %d / %d/n",
            nrow(indicadores_por_barrio), nrow(barrios_sf)))

# Barrios sin solapamiento con ningún radio
barrios_sin_radio <- barrios_sf |>
  st_drop_geometry() |>
  filter(!id_barrio %in% indicadores_por_barrio$id_barrio) |>
  select(id_barrio, nombre_barrio, tipo_barrio)

if (nrow(barrios_sin_radio) > 0) {
  cat(sprintf("/n   Barrios sin radios solapantes (%d):/n", nrow(barrios_sin_radio)))
  print(barrios_sin_radio)
}

# =============================================================================
# 6. RESUMEN
# =============================================================================

cat("/n=== RESUMEN ===/n")
cat(sprintf("  Total radios CABA:                    %d/n", nrow(radios_sf)))
cat(sprintf("  Radios que tocan algún barrio:        %d/n",
            n_distinct(intersecciones$codigo_redatam)))
cat(sprintf("  Radios sin solapamiento con barrios:  %d/n",
            nrow(radios_sf) - n_distinct(intersecciones$codigo_redatam)))
cat(sprintf("  Barrios con datos de radios:          %d / %d/n",
            nrow(indicadores_por_barrio), nrow(barrios_sf)))

cat("/n  Indicadores por barrio (top 10 por NBI):/n")
print(
  indicadores_por_barrio |>
    arrange(desc(pct_NBI)) |>
    select(nombre_barrio, n_radios, total_viv,
           pct_NBI, pct_hacinamiento, pct_agua_corriente) |>
    head(10)
)

# =============================================================================
# 7. EXPORTAR
# =============================================================================

cat("/n── 7. Exportando resultados.../n")

# CSV 1: tabla de intersecciones con pesos (diagnóstico / trazabilidad)
intersecciones_datos |>
  select(codigo_redatam, id_barrio, nombre_barrio,
         area_radio_m2, area_interseccion_m2, peso,
         total_viv, all_of(vars_n)) |>
  write_csv(file.path(RUTA, "intersecciones_radio_barrio.csv"))

# CSV 2: indicadores agregados por barrio
write_csv(indicadores_por_barrio,
          file.path(RUTA, "indicadores_por_barrio.csv"))

# GeoPackage con 2 capas para visualización en QGIS / R
# Capa 1: polígono de cada radio + indicadores
st_write(
  radios_con_datos |> st_transform(4326),
  file.path(RUTA, "radios_barrios.gpkg"),
  layer      = "radios_caba",
  delete_dsn = TRUE,
  quiet      = TRUE
)

# Capa 2: polígono de cada barrio + indicadores Censo ponderados
barrios_con_indicadores <- barrios_sf |>
  left_join(
    indicadores_por_barrio,
    by = c("id_barrio", "nombre_barrio", "tipo_barrio")
  )

st_write(
  barrios_con_indicadores,
  file.path(RUTA, "radios_barrios.gpkg"),
  layer  = "barrios_con_indicadores",
  append = TRUE,
  quiet  = TRUE
)

cat("✓ Archivos exportados:/n")
cat("   intersecciones_radio_barrio.csv  — 1 fila por par radio×barrio con pesos/n")
cat("   indicadores_por_barrio.csv       — 1 fila por barrio con indicadores ponderados/n")
cat("   radios_barrios.gpkg              — GeoPackage con 2 capas (para QGIS/R)/n")
