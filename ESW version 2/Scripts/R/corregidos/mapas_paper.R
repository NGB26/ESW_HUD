# =============================================================================
# mapas_paper.R  —  Réplica autónoma de los gráficos del paper
#
#   1. Mapa de casos por comuna y barrio, con etiquetas "(n=…)"      [PDF]
#   2. Cuatro mapas magma: calidad de materiales, NBI, hacinamiento,
#      clima educativo bajo                                            [PDF]
#   3. ggpairs de los cuatro indicadores: Barrio Villa vs Comuna       [PDF]
#   4. (extra) Mapa de CPE por comuna, tiempos a CeSAC/hospital y días
#      desde la última consulta                                        [PDF]
#
# NO reconstruye datos: parte de archivos ya generados.
#   · df_esw.csv                (paso de Stata; se busca dentro de RAIZ)
#   · comuna_unido.csv          (03)   RUTA_PROC
#   · comunas_proj.rds          (01)   RUTA_PROC
#   · cpe_comuna.csv            (03b)  RUTA_PROC   [opcional: solo mapa CPE]
#
# Salidas: PDFs en RUTA_FIG (output/figures).
# =============================================================================

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

suppressPackageStartupMessages({
  library(tidyr); library(ggplot2); library(ggrepel)
  library(patchwork); library(purrr); library(GGally)
})
sf_use_s2(FALSE)

# ── Parámetros de estilo ──────────────────────────────────────────────────────
UMBRAL_ETIQUETA <- 100        # etiquetar villas con más de N casos
PALETA_CASOS    <- "YlOrRd"
OPCION_MAGMA    <- "magma"

# =============================================================================
# 0. Datos
# =============================================================================
path_esw <- buscar_archivo("df_esw.csv", RAIZ)
cat("df_esw desde:", path_esw, "\n")
df_esw <- read.csv(path_esw)

req <- c("villa", "barrio_norm", "comuna_ok", "fuente_poblacion", "nro_com",
         "geom_barrio", "t_ors_p_cesac", "t_ors_p_hosp", "dias_ult_cons",
         "pct_nbi_v", "pct_hacinamiento_v", "pct_clima_educ_bajo_v", "pct_calidad_mat_v",
         "pct_clima_educ_bajo", "pct_calidad_mat")
falta <- setdiff(req, names(df_esw))
if (length(falta) > 0) {
  stop("df_esw.csv no tiene: ", paste(falta, collapse = ", "),
       "\n  Columnas parecidas: ",
       paste(grep("pct_|t_ors|dias|nro_com|geom", names(df_esw), value = TRUE), collapse = ", "))
}

comuna_unido <- read_csv(file.path(RUTA_PROC, "comuna_unido.csv"), show_col_types = FALSE)
comunas_proj <- readRDS(file.path(RUTA_PROC, "comunas_proj.rds"))

df_esw <- df_esw |>
  mutate(barrio_ok = if_else(fuente_poblacion == "Barrio_Villa", barrio_norm, comuna_ok)) |>
  select(-any_of(c("pct_hogares_nbi", "pct_hogares_NBI", "pct_hogares_hacinamiento"))) |>
  left_join(comuna_unido |> select(numero_comuna, pct_hogares_NBI, pct_hogares_hacinamiento),
            by = c("nro_com" = "numero_comuna")) |>
  left_join(comunas_proj |> select(numero_comuna, geometry),
            by = c("nro_com" = "numero_comuna")) |>
  mutate(
    geom_barrio_sfc = st_as_sfc(
      ifelse(is.na(geom_barrio), "GEOMETRYCOLLECTION EMPTY", geom_barrio),
      crs = 4326) |> st_transform(CRS_PROY),
    geometry = st_transform(geometry, CRS_PROY),
    geom_ok  = if_else(villa == 1, geom_barrio_sfc, geometry)
  ) |>
  select(-geom_barrio_sfc)

# Unificación barrio (villa) / comuna
vars_pct <- c(pct_nbi             = "pct_hogares_NBI",
              pct_hacinamiento    = "pct_hogares_hacinamiento",
              pct_clima_educ_bajo = "pct_clima_educ_bajo",
              pct_calidad_mat     = "pct_calidad_mat")
for (v in names(vars_pct)) {
  df_esw[[paste0(v, "_t")]] <- if_else(df_esw$fuente_poblacion == "Barrio_Villa",
                                       df_esw[[paste0(v, "_v")]],
                                       df_esw[[vars_pct[[v]]]])
}

# =============================================================================
# 1. Mapa de casos por comuna y barrio con etiquetas
# =============================================================================
resumen_barrio <- df_esw |>
  group_by(barrio_ok) |>
  summarise(n_casos = n(), fuente = first(fuente_poblacion),
            geom_ok = first(geom_ok), .groups = "drop") |>
  arrange(fuente == "Barrio_Villa") |>          # villas encima de las comunas
  st_as_sf(sf_column_name = "geom_ok")

centroides_villa <- resumen_barrio |>
  filter(fuente == "Barrio_Villa", !st_is_empty(geom_ok)) |>
  st_make_valid()
centroides_villa <- suppressWarnings(st_centroid(centroides_villa))

coords_villa <- centroides_villa |>
  mutate(x = st_coordinates(geom_ok)[, 1],
         y = st_coordinates(geom_ok)[, 2],
         label = paste0(barrio_ok, "\n(n=", n_casos, ")")) |>
  st_drop_geometry()

bb    <- st_bbox(resumen_barrio)                  # metros (EPSG:22185)
lim_x <- unname(c(bb["xmin"] - 0.30 * (bb["xmax"] - bb["xmin"]),
                  bb["xmax"] + 0.30 * (bb["xmax"] - bb["xmin"])))
lim_y <- unname(c(bb["ymin"] - 0.15 * (bb["ymax"] - bb["ymin"]),
                  bb["ymax"] + 0.15 * (bb["ymax"] - bb["ymin"])))

mapa_casos <- ggplot(resumen_barrio) +
  geom_sf(aes(fill = n_casos), color = "white", linewidth = 0.2) +
  geom_label_repel(
    data = coords_villa |> filter(n_casos > UMBRAL_ETIQUETA),
    aes(x = x, y = y, label = label),
    size = 2.3, color = "black", fill = NA, label.size = 0,
    label.padding = unit(0.2, "lines"),
    segment.color = "grey40", segment.size = 0.35, segment.linetype = "solid",
    min.segment.length = 0, box.padding = 0.6, point.padding = 0.3,
    force = 8, force_pull = 0.1, xlim = lim_x, ylim = lim_y,
    direction = "both", seed = 123
  ) +
  scale_fill_distiller(palette = PALETA_CASOS, direction = 1, name = "",
                       na.value = "grey85") +
  coord_sf(xlim = lim_x, ylim = lim_y) +
  labs(title   = "Distribución de casos de la muestra por comuna y barrio",
       caption = paste0("Nota: etiquetas en villas con más de ", UMBRAL_ETIQUETA,
                        " casos. Fuente: datos propios")) +
  theme_void(base_size = 11) +
  theme(plot.title   = element_text(size = 12, face = "bold", family = "serif",
                                    hjust = 0.5, margin = margin(b = 8)),
        plot.caption = element_text(size = 8, family = "serif", color = "grey40",
                                    hjust = 0, margin = margin(t = 6)),
        legend.position = "right",
        legend.key.height = unit(1.5, "cm"), legend.key.width = unit(0.4, "cm"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 8),
        plot.margin = margin(10, 10, 10, 10))
print(mapa_casos)
guardar_pdf(mapa_casos, "mapa_casos_comuna_barrio_etiquetas", width = 10, height = 8)

# =============================================================================
# 2. Cuatro mapas magma (villas encima de comunas)
# =============================================================================
resumen_barrios_amp <- df_esw |>
  group_by(barrio_ok) |>
  summarise(
    n_casos        = n(),
    nbi            = mean(pct_nbi_t,               na.rm = TRUE),
    calmat         = 100 - mean(pct_calidad_mat_t, na.rm = TRUE),
    climaedu       = mean(pct_clima_educ_bajo_t,   na.rm = TRUE),
    hacinamiento   = mean(pct_hacinamiento_t,      na.rm = TRUE),
    t_p_cesac      = mean(t_ors_p_cesac,           na.rm = TRUE),
    t_p_hosp       = mean(t_ors_p_hosp,            na.rm = TRUE),
    dias_ult_cons  = mean(dias_ult_cons,           na.rm = TRUE),
    fuente         = first(fuente_poblacion),
    geom_ok        = st_union(geom_ok),
    .groups        = "drop"
  ) |>
  st_as_sf(sf_column_name = "geom_ok") |>
  filter(!st_is_empty(geom_ok)) |>
  st_cast("MULTIPOLYGON") |>
  arrange(fuente == "Barrio_Villa")

villas  <- resumen_barrios_amp |> filter(fuente == "Barrio_Villa")
barrios <- resumen_barrios_amp |> filter(fuente != "Barrio_Villa")

make_map <- function(var, titulo, leyenda = NULL) {
  ggplot() +
    geom_sf(data = barrios, aes(fill = .data[[var]]), color = "white", linewidth = 0.2) +
    geom_sf(data = villas,  aes(fill = .data[[var]]), linewidth = 0.01) +
    scale_fill_viridis_c(option = OPCION_MAGMA, direction = -1) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 8, hjust = 0.5),
          legend.position = "bottom", legend.key.width = unit(0.6, "cm"),
          legend.text = element_text(size = 6)) +
    labs(title = titulo, fill = leyenda)
}

mapas_indicadores <- (make_map("calmat",       "Calidad de materiales (%)") |
                      make_map("nbi",          "NBI (%)")) /
                     (make_map("hacinamiento", "Hacinamiento (%)") |
                      make_map("climaedu",     "Clima educativo bajo (%)"))
print(mapas_indicadores)
guardar_pdf(mapas_indicadores, "mapas_indicadores_barrios", width = 9, height = 10)

# =============================================================================
# 3. ggpairs: Barrio Villa vs Comuna
# =============================================================================
df_plot <- resumen_barrios_amp |>
  st_drop_geometry() |>
  select(calmat, nbi, hacinamiento, climaedu, fuente) |>
  drop_na()

g_pairs <- ggpairs(
  data = df_plot, columns = c("calmat", "nbi", "hacinamiento", "climaedu"),
  mapping = aes(color = fuente, alpha = 0.5),
  upper = list(continuous = wrap("cor", size = 2)),
  lower = list(continuous = wrap("points", size = 1.5)),
  diag  = list(continuous = "densityDiag")
)
print(g_pairs)
dev_pdf <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
dev_pdf(file.path(RUTA_FIG, "ggpairs_indicadores_fuente.pdf"), width = 9, height = 9)
print(g_pairs)
invisible(dev.off())
message("PDF guardado: ", file.path(RUTA_FIG, "ggpairs_indicadores_fuente.pdf"))

# =============================================================================
# 4. Extras: tiempos, días desde última consulta y CPE
# =============================================================================
mapas_tiempos <- make_map("t_p_cesac", "Tiempo al CeSAC más cercano (min a pie)", "min") |
                 make_map("t_p_hosp",  "Tiempo al Hospital más cercano (min a pie)", "min")
guardar_pdf(mapas_tiempos, "mapas_tiempo_cesac_hospital", width = 11, height = 6)
guardar_pdf(make_map("dias_ult_cons", "Días desde última consulta (promedio)", "días"),
            "mapa_dias_ultima_consulta", width = 7, height = 7)

path_cpe <- file.path(RUTA_PROC, "cpe_comuna.csv")
if (file.exists(path_cpe)) {
  cpe <- read_csv(path_cpe, show_col_types = FALSE)
  comunas_cpe <- comunas_proj |>
    select(numero_comuna) |>
    left_join(cpe |> select(numero_comuna, pct_cpe_c), by = "numero_comuna")
  mapa_cpe <- ggplot(comunas_cpe) +
    geom_sf(aes(fill = pct_cpe_c), color = "white", linewidth = 0.4) +
    geom_sf_text(aes(label = paste0("C", numero_comuna, "\n", round(pct_cpe_c, 1), "%")),
                 size = 2.8, fontface = "bold") +
    scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "% con CPE") +
    labs(title = "Población con cobertura pública exclusiva por comuna",
         subtitle = "CABA, Censo 2022 (INDEC / Redatam)") +
    theme_void(base_size = 11) + theme(plot.title = element_text(face = "bold"))
  guardar_pdf(mapa_cpe, "mapa_cpe_comuna", width = 8, height = 8)
} else {
  message("Sin cpe_comuna.csv (03b): se omite el mapa de CPE.")
}

cat("\n✓ mapas_paper.R terminado. PDFs en", RUTA_FIG, "\n")
