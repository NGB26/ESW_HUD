############################################################
# Análisis ensayo HTA – CABA / BID
# Autor: Nicolás García Balus (nicolasga@iadb.org // nicolas.gbalus@gmail.com)
# Fecha: 17/12/2025
#

############################################################

## 0. Paquetes ---------------------------------------------------------



library(readxl)
library(dplyr)
library(gtsummary)
library(broom)
library(margins)
library(tibble)
library(tidyr)
library(lubridate)
library(writexl)

## 1. Lectura de bases -------------------------------------------------

# Ajustar esta ruta según corresponda a cada entorno...
ruta_base <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH"

# datos_matcheados_share <- read_excel(
#   file.path(ruta_base, "datos_matcheados_hta_bid_share.xlsx")
# )

tabla_receptora_share <- read_excel(
  file.path(ruta_base, "Re_ Informe alejandro macchia/tabla_receptora_hta_bid_share.xlsx")
)

df<-tabla_receptora_share

df <- df %>%
  mutate(
    turno = if_else(
      (!is.na(servicio) & servicio != '') | (!is.na(fecha_turno)),
      1L, 0L
    )
  )

df<-df%>%mutate(villa=if_else(!is.na(asentamiento),1,0))
df <- df %>%
  mutate(dbt = replace_na(dbt, 0))



df <- df %>%
  mutate(
    fecha_corte = as.Date(fecha_ultima_consulta) + dias_desde_ultima_consulta,
    
    dias_deteccion = as.numeric(fecha_corte - as.Date(fecha_deteccion)),
    
    dias_dispensa  = as.numeric(fecha_corte-as.Date(ultima_dispensa_hta_fecha))
  )


df<-df%>%mutate(edad_deteccion=edad-dias_deteccion/360)

df %>%
  group_by(villa, dbt) %>%
  summarise(
    edad_media           = mean(edad, na.rm = TRUE),
    edad_deteccion_media = mean(edad_deteccion, na.rm = TRUE),
    n                    = n()
  )


cant_villas=df%>%count(comuna, asentamiento, villa )
df%>%count( villa )

df_barrios_normalizado<-read_excel("C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2/Data frames consolidados/barrios_populares_normalizados.xlsx")

df <- df %>%
  left_join(
    df_barrios_normalizado %>%
      select(id_persona, comuna_barrio, barrio_norm, geom_barrio),
    by = "id_persona"
  )


#extaer el nro de comuna 
ruta_c<-"C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2/Data frames consolidados"
comuna_unido<- read_excel(file.path(ruta_c, "comuna_unido.xlsx"))

df <- df %>%
  mutate(nro_comuna = as.numeric(gsub("[^0-9]", "", comuna)))

df <- df %>%
  left_join(
    comuna_unido,
    by = c("nro_comuna" = "numero_comuna")
  )

#Uno el dataframe de variables sociodemográficas para cada villa 

indicadores_por_barrio<-read.csv(file.path(ruta_c,"indicadores_por_barrio.csv"))

indicadores_por_barrio_v <- indicadores_por_barrio %>%
  rename_with(~ paste0(.x, "_v"), -nombre_barrio)

df <- df %>%
  left_join(
    indicadores_por_barrio_v,
    by = c("barrio_norm" = "nombre_barrio")
  )


#guardo una copia de backup
df_backup<-df

# =============================================================================
# Script: unir_barrios_comunas.R
#
# Objetivo: Construir un dataset unificado que combine información de barrios
#           populares y de comunas, con la siguiente lógica:
#
#   1. Join del df original (tabla_receptora o similar) con analisis_barrios
#      por nombre de barrio. Solo se toman las variables de analisis_barrios
#      para observaciones con villa == 1.
#
#   2. Join con analisis_comunas por nombre de comuna. Solo se toman las
#      variables de analisis_comunas para observaciones que no hayan
#      recibido valores en el paso anterior (es decir, villa != 1 o sin match).
#
#   3. De analisis_barrios se elimina la opción A de densidad y se renombran
#      las columnas de opción B quitando el sufijo "_opB" para que queden
#      con los mismos nombres que analisis_comunas.
#
# Inputs:
#   df_original             — el dataframe base (ajustar nombre y ruta)
#   analisis_completo_barrios.csv
#   analisis_completo_comunas.csv
#
# Output:
#   dataset_unificado.csv
# =============================================================================


RUTA <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2/Data frames consolidados"

# =============================================================================
# 1. CARGAR DATASETS
# =============================================================================

cat("── 1. Cargando datasets...\n")

# ── Dataset original ──────────────────────────────────────────────────────────
# Ajustar según el nombre real del archivo y las columnas clave:
#   col_barrio  = columna con el nombre del barrio popular
#   col_comuna  = columna con el nombre/número de la comuna
#   col_villa   = columna indicadora (1 = vive en barrio popular)

df_orig <- df


COL_BARRIO <- "barrio_norm"    # <-- ajustar si el nombre de columna difiere
COL_COMUNA <- "nro_comuna"    # <-- ajustar si el nombre de columna difiere
COL_VILLA  <- "villa"            # <-- ajustar si el nombre de columna difiere

cat(sprintf("   df_original: %d filas\n", nrow(df_orig)))

# ── analisis_barrios: cargar, limpiar opción A y renombrar opción B ───────────
analisis_barrios_raw <- read_csv(
  file.path(RUTA, "analisis_completo_barrios.csv"),
  show_col_types = FALSE
)

# Columnas a conservar de analisis_barrios:
# - Eliminar todas las columnas _opA_*
# - Eliminar area_km2_opA
# - Renombrar _opB_ → quitar "_opB" (ej. n_centros_opB_hosp → n_centros_hosp)
analisis_barrios <- analisis_barrios_raw |>
  select(-matches("_opA_"), -any_of("area_km2_opA")) |>
  rename_with(
    ~ str_replace(., "_opB_", "_"),   # n_centros_opB_hosp → n_centros_hosp
    matches("_opB_")
  ) |>
  rename_with(
    ~ str_replace(., "area_km2_opB", "area_km2"),
    any_of("area_km2_opB")
  )

cat(sprintf("   analisis_barrios: %d barrios × %d variables\n",
            nrow(analisis_barrios), ncol(analisis_barrios)))

# ── analisis_comunas ──────────────────────────────────────────────────────────
analisis_comunas <- read_csv(
  file.path(RUTA, "analisis_completo_comunas.csv"),
  show_col_types = FALSE
)

cat(sprintf("   analisis_comunas: %d comunas × %d variables\n",
            nrow(analisis_comunas), ncol(analisis_comunas)))

# =============================================================================
# 2. VERIFICAR QUE LAS COLUMNAS DE ANÁLISIS COINCIDAN
# =============================================================================

# Las variables de análisis deben tener los mismos nombres en ambos datasets
# (excepto las columnas de identificación: nombre_barrio vs barrios/numero_comuna)
vars_analisis_barrio <- analisis_barrios |>
  select(-any_of(c("id_barrio", "nombre_barrio", "tipo_barrio",
                   "area_km2", "poblacion"))) |>
  names()

vars_analisis_comuna <- analisis_comunas |>
  select(-any_of(c("numero_comuna", "barrios", "area_km2", "poblacion"))) |>
  names()

vars_solo_barrio <- setdiff(vars_analisis_barrio, vars_analisis_comuna)
vars_solo_comuna <- setdiff(vars_analisis_comuna, vars_analisis_barrio)

if (length(vars_solo_barrio) > 0)
  cat(sprintf("   Variables solo en barrios: %s\n",
              paste(vars_solo_barrio, collapse = ", ")))
if (length(vars_solo_comuna) > 0)
  cat(sprintf("   Variables solo en comunas: %s\n",
              paste(vars_solo_comuna, collapse = ", ")))
if (length(vars_solo_barrio) == 0 && length(vars_solo_comuna) == 0)
  cat("   ✓ Variables de análisis idénticas en ambos datasets\n")

# =============================================================================
# 3. PREPARAR CLAVES DE JOIN
# =============================================================================

# Normalizar nombres para el join (minúsculas, sin espacios extra)
normalizar <- function(x) str_to_lower(str_squish(x))

analisis_barrios <- analisis_barrios |>
  mutate(join_key_barrio = normalizar(nombre_barrio))

analisis_comunas <- analisis_comunas |>
  mutate(join_key_comuna = normalizar(paste0("comuna ", numero_comuna)))

# Prefijo "comuna N" o número directo según el formato de COL_COMUNA en df_orig
df_orig <- df_orig |>
  mutate(
    join_key_barrio = normalizar(.data[[COL_BARRIO]]),
    join_key_comuna = if_else(
      is.na(.data[[COL_COMUNA]]),
      NA_character_,
      normalizar(paste0("comuna ", .data[[COL_COMUNA]]))
    )
  )
# =============================================================================
# 4. JOIN CON BARRIOS (solo para villa == 1)
# =============================================================================

cat("\n── 2. Join con analisis_barrios (villa == 1)...\n")

# Columnas de análisis a traer del dataset de barrios
vars_traer_barrio <- analisis_barrios |>
  select(-any_of(c("id_barrio", "nombre_barrio", "tipo_barrio",
                   "area_km2", "poblacion", "join_key_barrio"))) |>
  names()

# Columnas que vienen del join — las mismas que están en vars_traer_barrio
# más las que generan conflicto (.x/.y)
cols_conflicto <- names(df_orig)[names(df_orig) %in% 
                                   c("area_km2", vars_traer_barrio,
                                     # las otras que aparecen duplicadas
                                     "codigo", "departamento",
                                     "n_agua_corriente", "pct_agua_corriente",
                                     "n_calidad_mat", "pct_calidad_mat",
                                     "n_clima_educ_bajo", "pct_clima_educ_bajo",
                                     "n_sec_completa", "pct_sec_completa",
                                     "n_hogares_hacinamiento", "pct_hogares_hacinamiento",
                                     "id_barrio_v", "tipo_barrio_v", "n_radios_v",
                                     "comunas_v", "total_viv_v",
                                     "n_agua_corriente_v", "n_calidad_mat_v",
                                     "n_clima_educ_bajo_v", "n_sec_completa_v",
                                     "n_edad_65_mas_v", "n_hacinamiento_v",
                                     "pct_agua_corriente_v", "pct_calidad_mat_v",
                                     "pct_clima_educ_bajo_v", "pct_sec_completa_v",
                                     "pct_edad_65_mas_v", "pct_hacinamiento_v")]

cat("Columnas a eliminar de df_orig antes del join:\n")
print(cols_conflicto)

# Eliminarlas y hacer el join limpio
df_con_barrio <- df_orig |>
  select(-any_of(cols_conflicto)) |>
  left_join(
    analisis_barrios |>
      select(join_key_barrio, area_km2_barrio = area_km2,
             all_of(vars_traer_barrio)),
    by = "join_key_barrio"
  )

# Verificar que no quedaron sufijos
cat("Columnas con .x o .y:\n")
print(names(df_con_barrio)[grepl("\\.x$|\\.y$", names(df_con_barrio))])

# Verificar que las columnas de interés llegaron limpias
cat("Columnas faltantes de vars_traer_barrio:\n")
print(vars_traer_barrio[!vars_traer_barrio %in% names(df_con_barrio)])

n_villa_match <- sum(df_con_barrio$match_barrio, na.rm = TRUE)
n_villa_nomatch <- sum(df_con_barrio[[COL_VILLA]] == 1 &
                         !df_con_barrio$match_barrio, na.rm = TRUE)

cat(sprintf("   villa == 1 con match en analisis_barrios:    %d\n", n_villa_match))
cat(sprintf("   villa == 1 sin match (barrio no encontrado): %d\n", n_villa_nomatch))

# =============================================================================
# 5. JOIN CON COMUNAS (solo para los que no tienen datos de barrio)
# =============================================================================

cat("── 3. Join con analisis_comunas (sin datos de barrio)...\n")

# Columnas de análisis a traer del dataset de comunas
vars_traer_comuna <- analisis_comunas |>
  select(-any_of(c("numero_comuna", "barrios", "area_km2",
                   "poblacion", "join_key_comuna"))) |>
  names()

# Join con comunas usando sufijo "_com" para distinguir columnas duplicadas
df_con_comuna <- df_con_barrio |>
  left_join(
    analisis_comunas |>
      select(join_key_comuna, area_km2_comuna = area_km2,
             poblacion_comuna = poblacion,
             all_of(vars_traer_comuna)),
    by     = "join_key_comuna",
    suffix = c("", "_com")
  )

# Para cada variable de análisis: si match_barrio → usar valor de barrio (ya está en la col).
# Si no match_barrio → usar valor de comuna (columna con sufijo "_com").
# coalesce() toma el primer valor no-NA: barrio primero, comuna como fallback.
# El if_else sobre match_barrio garantiza que aunque barrio tenga NA, no use comuna para villa==1.

for (v in vars_traer_comuna) {
  v_com <- paste0(v, "_com")
  if (v_com %in% names(df_con_comuna)) {
    df_con_comuna[[v]] <- case_when(
      df_con_comuna[[COL_VILLA]] == 1 ~ df_con_comuna[[v]],        # villa==1: valor de barrio
      df_con_comuna[[COL_VILLA]] == 0 ~ coalesce(df_con_comuna[[v]], df_con_comuna[[v_com]]),  # villa==0: valor de comuna
      TRUE                            ~ NA_real_                    # cualquier otro caso
    )
  }
}

df_final <- df_con_comuna |>
  mutate(
    area_km2 = if_else(
      df_con_comuna[[COL_VILLA]] == 1,
      area_km2_barrio,
      area_km2_comuna
    ),
    poblacion_src = case_when(
      df_con_comuna[[COL_VILLA]] == 1 & !is.na(area_km2_barrio) ~ "barrio",
      df_con_comuna[[COL_VILLA]] == 0 & !is.na(area_km2_comuna) ~ "comuna",
      TRUE                                                       ~ NA_character_
    )
  ) |>
  select(-ends_with("_com"),
         -any_of(c("area_km2_barrio", "area_km2_comuna",
                   "poblacion_comuna", "join_key_barrio",
                   "join_key_comuna", "match_barrio")))

n_con_barrio <- sum(df_final$poblacion_src == "barrio", na.rm = TRUE)
n_con_comuna <- sum(df_final$poblacion_src == "comuna", na.rm = TRUE)
n_sin_nada   <- sum(is.na(df_final$poblacion_src))

cat(sprintf("   Observaciones con datos de barrio: %d\n",  n_con_barrio))
cat(sprintf("   Observaciones con datos de comuna: %d\n",  n_con_comuna))
cat(sprintf("   Observaciones sin datos de ninguno: %d\n", n_sin_nada))
cat(sprintf("   Total: %d (igual a input: %s)\n",
            nrow(df_final), nrow(df_final) == nrow(df_orig)))

# =============================================================================
# 6. RESUMEN Y EXPORTAR
# =============================================================================

cat("\n── 4. Resumen del dataset unificado:\n")
cat(sprintf("   Filas:     %d\n", nrow(df_final)))
cat(sprintf("   Columnas:  %d\n", ncol(df_final)))

cat("\n   Fuente de datos por observación:\n")
print(df_final |> count(poblacion_src, name = "n") |> arrange(desc(n)))

write_csv(df_final, file.path(RUTA, "dataset_unificado.csv"))

cat("\n✓ Exportado: dataset_unificado.csv\n")


# =============================================================================
# 7. Datos Stata 
# =============================================================================



df_esw<-read.csv(file.path(RUTA,"df_esw.csv"))

df_esw<-df_esw%>%mutate(barrio_ok=if_else(fuente_poblacion=="Barrio_Villa", barrio_norm, comuna_ok))

df_esw<-df_esw%>%select(-pct_hogares_nbi,-pct_hogares_hacinamiento)%>%left_join(
  comuna_unido |> select(numero_comuna, pct_hogares_NBI, pct_hogares_hacinamiento), by= c("nro_com"= "numero_comuna"))

df_esw<-df_esw%>%left_join(comunas_proj %>% select(numero_comuna,geometry), by=c("nro_com"="numero_comuna"))


df_esw <- df_esw |>
  mutate(
    geom_barrio_sfc = st_as_sfc(
      ifelse(is.na(geom_barrio), "GEOMETRYCOLLECTION EMPTY", geom_barrio),
      crs = 4326
    ) |> st_transform(crs = 22185),
    geometry = st_transform(geometry, crs = 22185),
    geom_ok = if_else(
      villa == 1,
      geom_barrio_sfc,
      geometry
    )
  ) |>
  select(-geom_barrio_sfc)

resumen_barrio <- df_esw %>%
  group_by(barrio_ok) %>%
  summarise(
    n_casos        = n(),
    fuente         = first(fuente_poblacion),
    geom_ok        = first(geom_ok),
    .groups        = "drop"
  ) %>%
  st_as_sf(sf_column_name = "geom_ok")

write_xlsx(df_esw, file.path(RUTA, "data_revisada_ok.xlsx"))

library(ggplot2)
library(sf)

ggplot(resumen_barrio) +
  geom_sf(aes(fill = n_casos), color = "white", linewidth = 0.2) +
  scale_fill_distiller(
    palette   = "YlOrRd",
    direction = 1,
    name      = "N",
    na.value  = "grey85"
  ) +
  labs(
    title    = "Distribución de casos por barrio — CABA",
    caption  = "Fuente: datos propios"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(size = 12, face = "plain", margin = margin(b = 8)),
    plot.caption    = element_text(size = 8, color = "grey50", margin = margin(t = 6)),
    legend.position = "right",
    legend.key.height = unit(1.2, "cm"),
    legend.key.width  = unit(0.35, "cm"),
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8),
    plot.margin     = margin(10, 10, 10, 10)
  )

library(ggrepel)

# Bounding box del mapa para forzar etiquetas afuera
bbox <- st_bbox(resumen_barrio)

# Extraer coordenadas de centroides de villas
coords_villa <- centroides_villa %>%
  mutate(
    x     = st_coordinates(.)[, 1],
    y     = st_coordinates(.)[, 2],
    label = paste0(barrio_ok, "\n(n=", n_casos, ")")
  ) %>%
  st_drop_geometry()

ggplot(resumen_barrio) +
  geom_sf(aes(fill = n_casos), color = "white", linewidth = 0.2) +
  geom_label_repel(
    data               = coords_villa %>% filter(n_casos > 100),
    aes(x = x, y = y, label = label),
    size               = 2.3,
    color              = "black",
    fill               = NA,
    label.size         = 0,
    label.padding      = unit(0.2, "lines"),
    segment.color      = "grey40",
    segment.size       = 0.35,
    segment.linetype   = "solid",
    min.segment.length = 0,
    box.padding        = 0.6,
    point.padding      = 0.3,
    force              = 8,
    force_pull         = 0.1,
    xlim               = c(bbox["xmin"] - 0.08, bbox["xmax"] + 0.08),
    ylim               = c(bbox["ymin"] - 0.06, bbox["ymax"] + 0.06),
    direction          = "both"
  ) +
  scale_fill_distiller(
    palette   = "YlOrRd",
    direction = 1,
    name      = "",
    na.value  = "grey85"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"] - 0.08, bbox["xmax"] + 0.08),
    ylim = c(bbox["ymin"] - 0.06, bbox["ymax"] + 0.06)
  ) +
  labs(
    title   = "Distribución de casos de la muestra por comuna y barrio",
    caption = "Nota: etiquetas en villas con más de 100 casos. Fuente: datos propios"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(
      size    = 12,
      face    = "bold",
      family  = "serif",
      hjust   = 0.5,           # centrado
      margin  = margin(b = 8)
    ),
    plot.caption = element_text(
      size    = 8,
      family  = "serif",
      color   = "grey40",
      hjust   = -1,             # alineado a la izquierda
      margin  = margin(t = 6)
    ),
    legend.position   = "right",
    legend.key.height = unit(1.5, "cm"),
    legend.key.width  = unit(0.4, "cm"),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    plot.margin       = margin(10, 10, 10, 10)
  )


vars_pct <- list(
  pct_nbi             = "pct_hogares_NBI",
  pct_hacinamiento    = "pct_hogares_hacinamiento",
  pct_clima_educ_bajo = "pct_clima_educ_bajo",
  pct_calidad_mat     = "pct_calidad_mat"
)

for (v in names(vars_pct)) {
  df_esw <- df_esw |> mutate(
    !!paste0(v, "_t") := if_else(
      fuente_poblacion == "Barrio_Villa",
      .data[[paste0(v, "_v")]],
      .data[[vars_pct[[v]]]]
    )
  )
}

resumen_barrios_amp <- df_esw %>%
  group_by(barrio_ok) %>%
  summarise(
    n_casos      = n(),
    nbi          = mean(pct_nbi_t,          na.rm = TRUE),
    calmat       = 100 - mean(pct_calidad_mat_t,  na.rm = TRUE),
    climaedu     = mean(pct_clima_educ_bajo_t, na.rm = TRUE),
    hacinamiento = mean(pct_hacinamiento_t,  na.rm = TRUE),
    t_p_cesac = mean(t_ors_p_cesac,  na.rm = TRUE),
    t_p_hosp = mean(t_ors_p_hosp,  na.rm = TRUE),
    dias_ult_cons = mean(dias_ult_cons,  na.rm = TRUE),
    l_dias_ultcons = mean(log(dias_ult_cons),  na.rm = TRUE),
    fuente       = first(fuente_poblacion),
    geom_ok      = st_union(geom_ok),
    .groups      = "drop"
  ) %>%
  st_as_sf(sf_column_name = "geom_ok")

resumen_barrios_amp <- resumen_barrios_amp |> 
  filter(!st_is_empty(geom_ok))

resumen_barrios_amp <- resumen_barrios_amp |>
  filter(!st_is_empty(geom_ok)) |>
  st_cast("MULTIPOLYGON")

plot(select(resumen_barrios_amp,calmat, nbi, hacinamiento, climaedu))

resumen_barrios_amp |> 
  select(calmat, nbi, hacinamiento, climaedu, geom_ok) |> 
  pivot_longer(cols = c(calmat, nbi, hacinamiento, climaedu),
               names_to = "variable",
               values_to = "valor") |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = valor), color = "white", linewidth = 0.2) +
  facet_wrap(~variable, ncol = 2) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  theme_void() +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(fill = NULL)

vars_labels <- c(
  calmat       = "Calidad de materiales (%)",
  nbi          = "NBI (%)",
  hacinamiento = "Hacinamiento (%)",
  climaedu     = "Clima educativo bajo (%)"
)

library(patchwork)
library(purrr)
plots <- imap(vars_labels, function(label, var) {
  resumen_barrios_amp |> 
    select(all_of(var), geom_ok) |> 
    st_as_sf(sf_column_name = "geom_ok") |> 
    ggplot() +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 8, hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = unit(0.6, "cm"),
      legend.text = element_text(size = 6)
    ) +
    labs(title = label, fill = NULL)
})

wrap_plots(plots, ncol = 2)


library(GGally)


df_plot <- resumen_barrios_amp |> 
  st_drop_geometry() |> 
  select(calmat, nbi, hacinamiento, climaedu, fuente) |> 
  drop_na()

ggpairs(
  data = df_plot |> select(-fuente),
  mapping = aes(color = df_plot$fuente, alpha = 0.5),
  upper = list(continuous = wrap("cor", size = 2)),
  lower = list(continuous = wrap("points", size = 1.5)),
  diag  = list(continuous = "densityDiag")
)



villas <- resumen_barrios_amp |> filter(fuente == "Barrio_Villa")
barrios <- resumen_barrios_amp |> filter(fuente != "Barrio_Villa")

make_map <- function(var, titulo) {
  ggplot() +
    geom_sf(data = barrios, aes(fill = .data[[var]]), color = "white", linewidth = 0.2) +
    geom_sf(data = villas, aes(fill = .data[[var]]), linewidth = 0.01) +
    scale_fill_viridis_c(option = "magma", direction = -1) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 8, hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = unit(0.6, "cm"),
      legend.text = element_text(size = 6)
    ) +
    labs(title = titulo, fill = NULL)
}

p1 <- make_map("calmat",       "Calidad de materiales (%)")
p2 <- make_map("nbi",          "NBI (%)")
p3 <- make_map("hacinamiento", "Hacinamiento (%)")
p4 <- make_map("climaedu",     "Clima educativo bajo (%)")

(p1 | p2) / (p3 | p4)



make_map <- function(var, titulo, unidad = "min") {
  ggplot() +
    geom_sf(data = barrios, aes(fill = .data[[var]]), color = "white", linewidth = 0.2) +
    geom_sf(data = villas,  aes(fill = .data[[var]]), linewidth = 0.01) +
    scale_fill_viridis_c(
      option    = "magma",
      direction = -1,
      name      = unidad
    ) +
    theme_void() +
    theme(
      plot.title       = element_text(face = "bold", size = 8, hjust = 0.5),
      legend.position  = "bottom",
      legend.key.width = unit(0.6, "cm"),
      legend.text      = element_text(size = 6)
    ) +
    labs(title = titulo, fill = NULL)
}

p_cesac <- make_map("t_p_cesac", "Tiempo al CeSAC más cercano (min a pie)")
p_hosp  <- make_map("t_p_hosp",  "Tiempo al Hospital más cercano (min a pie)")
p_dias <- make_map("dias_ult_cons", "Días desde última consulta (promedio)")
p_ldias <- make_map("l_dias_ultcons", "Días desde última consulta (promedio)")

p_cesac | p_hosp 


library(ggplot2)
library(patchwork)
library(sf)
library(dplyr)

# Separar capas
villas  <- resumen_barrios_amp |> filter(fuente == "Barrio_Villa") |> st_as_sf(sf_column_name = "geom_ok")
barrios <- resumen_barrios_amp |> filter(fuente != "Barrio_Villa") |> st_as_sf(sf_column_name = "geom_ok")

# Centros de salud separados por tipo
cesacs  <- todos_centros_sf |> filter(tipo_centro == "CeSAC")        |> st_transform(st_crs(barrios))
hosps   <- todos_centros_sf |> filter(tipo_centro == "Hospital")      |> st_transform(st_crs(barrios))

lim_cesac <- range(
  resumen_barrios_amp %>% 
    filter(fuente == "Barrio_Villa") %>% 
    pull(t_p_cesac),
  na.rm = TRUE
)

base_mapa <- function(mostrar_leyenda = FALSE) {
  list(
    geom_sf(data = barrios, fill="#F5F5F5" , color = "black",   linewidth = 0.1),
    geom_sf(data = villas,  aes(fill = t_p_cesac), color = "black",   linewidth = 0.05),
    scale_fill_gradientn(
      colours  = c("yellow", "red"),
      na.value = "#CCCCCC",
      limits   = lim_cesac,
      name     = "Minutos"
    ),
    theme_void(base_size = 8),
    theme(
      legend.position  = if (mostrar_leyenda) "bottom" else "none",
      legend.key.width = unit(1, "cm"),
      legend.text      = element_text(size = 7),
      legend.title     = element_text(size = 7),
      plot.margin      = margin(4, 4, 4, 4)
    )
  )
}

p1 <- ggplot() + base_mapa(FALSE) + labs(title = "(A)")
p2 <- ggplot() + base_mapa(FALSE) + labs(title = "(B)") +
  geom_sf(data = cesacs, color = "#E67E22", shape = 21,
          fill = "white", size = 1.5, stroke = 0.6)
p3 <- ggplot() + base_mapa(TRUE)  + labs(title = "(C)") +
  geom_sf(data = cesacs, color = "#E67E22", shape = 21,
          fill = "white", size = 1.8, stroke = 0.6) +
  geom_sf(data = hosps,  color = "#1A5276", shape = 23,
          fill = "#1A5276", size = 2, stroke = 0.5)

(p1 | p2 ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5, size = 9, face = "bold")
  )






library(dplyr)
library(gt)
library(gtsummary)

# =============================================================================
# TABLA DESCRIPTIVA — Total | Villa==0 | Villa==1
# Estética LaTeX/publicación con gt
# =============================================================================

tabla_gt <- df_esw |>
  mutate(
    villa_label = factor(villa, levels = c(0, 1), labels = c("No villa", "Villa")),
    genero = factor(genero)
  ) |>
  select(
    villa_label, edad, genero, dbt,
    pct_nbi_t, pct_calidad_mat_t, pct_hacinamiento_t, pct_clima_educ_bajo_t,
    t_ors_p_cesac, t_ors_p_hosp, dias_ult_cons, dias_dispensa
  ) |>
  tbl_summary(
    by = villa_label,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      edad                  ~ "Edad (años)",
      genero                ~ "Sexo",
      dbt                   ~ "Diabetes",
      pct_nbi_t             ~ "NBI (%)",
      pct_calidad_mat_t     ~ "Calidad de materiales (%)",
      pct_hacinamiento_t    ~ "Hacinamiento (%)",
      pct_clima_educ_bajo_t ~ "Clima educativo bajo (%)",
      t_ors_p_cesac         ~ "Tiempo al CeSAC más cercano (min a pie)",
      t_ors_p_hosp          ~ "Tiempo al Hospital más cercano (min a pie)",
      dias_ult_cons         ~ "Días desde última consulta",
      dias_dispensa         ~ "Días desde última dispensa de medicación"
    ),
    missing = "no"
  ) |>
  add_overall(last = FALSE) |>
  modify_header(
    label  ~ "**Variable**",
    stat_0 ~ "**Total**",
    stat_1 ~ "**No villa**",
    stat_2 ~ "**Villa**"
  ) |>
  modify_spanning_header(c(stat_1, stat_2) ~ "**Residencia**") |>
  bold_labels() |>
  italicize_levels() |>
  modify_footnote(
    all_stat_cols() ~ "Media (DE) para variables continuas; N (%) para categóricas"
  ) |>
  as_gt() |>
  tab_options(
    table.font.names                   = "Times New Roman",
    table.font.size                    = px(12),
    table.border.top.style             = "solid",
    table.border.top.width             = px(2),
    table.border.top.color             = "black",
    table.border.bottom.style          = "solid",
    table.border.bottom.width          = px(2),
    table.border.bottom.color          = "black",
    column_labels.border.top.style     = "none",
    column_labels.border.bottom.style  = "solid",
    column_labels.border.bottom.width  = px(1),
    column_labels.border.bottom.color  = "black",
    column_labels.border.lr.style      = "none",
    table.background.color             = "white",
    column_labels.background.color     = "white",
    row.striping.include_table_body    = FALSE,
    data_row.padding                   = px(4),
    column_labels.padding              = px(6)
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_spanners()
  )

tabla_gt

# Exportar
tabla_gt |> gtsave(file.path(RUTA, "tabla_descriptiva.docx"))
tabla_gt |> gtsave(file.path(RUTA, "tabla_descriptiva.html"))
tabla_gt |> gtsave(file.path(RUTA, "tabla_descriptiva.png"), zoom = 3)
