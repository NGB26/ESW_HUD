# =============================================================================
# COMPLEMENTO: RADIOS EN BARRIOS POPULARES + BASE DE PROMEDIOS POR COMUNA
# =============================================================================
#
# Sigue a:
#   - 05_radios_en_barrios_populares.R   → radios_proj, barrios_proj
#   - base_acceso_radios.R (script anterior de este chat) → base_acceso_radios
#
# Requiere en el entorno:
#   - radios_proj        : sf de radios censales en CRS 22185, con numero_comuna
#                           nativo (campo DEPTO del shapefile INDEC) y todas las
#                           variables de NBI/Censo 2022 ya unidas
#   - barrios_proj        : sf de barrios populares en CRS 22185
#   - base_acceso_radios  : distancia/tiempo al centro más cercano por tipo,
#                           en formato largo (1 fila por radio × tipo_centro),
#                           del script anterior
#
# Qué hace:
#   1. Para cada radio censal, calcula qué % de su área cae dentro de algún
#      barrio popular (radios que no tocan ningún barrio quedan en 0%).
#   2. Arma una base ANCHA a nivel de radio (base_radios) que junta:
#        - numero_comuna y variables de NBI/Censo (de radios_proj)
#        - % de solapamiento con barrio popular
#        - distancia/tiempo al centro más cercano, por tipo (pivotado de
#          base_acceso_radios, mismo slug que ya usás en tu script 06)
#   3. Promedia por comuna TODAS las variables numéricas de base_radios.
#
# Dependencias: sf, dplyr, tidyr
# =============================================================================

library(sf)
library(dplyr)
library(tidyr)

cat("── Complemento: radios en barrios populares + promedios por comuna...\n")

stopifnot(
  "radios_proj y barrios_proj deben compartir el mismo CRS" =
    st_crs(radios_proj) == st_crs(barrios_proj)
)

# -----------------------------------------------------------------------------
# 1) % del área de cada radio que cae en algún barrio popular
# -----------------------------------------------------------------------------
# Misma lógica de ponderación por área que 05_radios_en_barrios_populares.R:
#   peso = área(radio ∩ barrio) / área(radio)
# Si un radio toca más de un barrio popular, se suman los pesos: la fracción
# total de su superficie que cae en "algún" barrio popular, sin importar en
# cuántos distintos.

cat("   1. Calculando solapamiento radio × barrio popular...\n")

radios_area <- radios_proj |>
  mutate(area_radio_m2 = as.numeric(st_area(geometry))) |>
  select(codigo_redatam, area_radio_m2)

interseccion_villas <- st_intersection(
  radios_area,
  barrios_proj |> select(id_barrio, nombre_barrio, tipo_barrio)
) |>
  mutate(
    area_interseccion_m2 = as.numeric(st_area(geometry)),
    peso                  = area_interseccion_m2 / area_radio_m2
  )

overlap_por_radio <- interseccion_villas |>
  st_drop_geometry() |>
  group_by(codigo_redatam) |>
  summarise(
    pct_area_villa      = round(pmin(sum(peso), 1) * 100, 2),
    n_barrios_populares  = n_distinct(id_barrio),
    barrios_populares    = paste(sort(unique(nombre_barrio)), collapse = "; "),
    .groups = "drop"
  )

cat(sprintf("   Radios con algún solapamiento: %d / %d\n",
            nrow(overlap_por_radio), nrow(radios_area)))

# -----------------------------------------------------------------------------
# 2) Distancia/tiempo por tipo, pivotado a ancho (mismo slug que script 06)
# -----------------------------------------------------------------------------

cat("   2. Pivotando distancias/tiempos a formato ancho por radio...\n")

slug <- c("Hospital"              = "hosp",
          "Centro Medico Barrial" = "cmb",
          "CeSAC"                 = "cesac",
          "Estacion Saludable"    = "est_sal")

distancias_wide <- base_radios |>
  mutate(tipo_slug = slug[tipo_centro]) |>
  select(id_radio, tipo_slug,
         dist_eucl_km, dist_red_km, tiempo_auto_min, tiempo_transp_min) |>
  pivot_wider(
    names_from  = tipo_slug,
    values_from = c(dist_eucl_km, dist_red_km, tiempo_auto_min, tiempo_transp_min),
    names_glue  = "{.value}_{tipo_slug}"
  )

# -----------------------------------------------------------------------------
# 3) Base ancha a nivel de radio: NBI/Censo + villa + distancias/tiempos
# -----------------------------------------------------------------------------
# numero_comuna y todas las variables de NBI/Censo salen de radios_proj —
# es la fuente ya unida en 05_radios_en_barrios_populares.R, no hace falta
# reconstruirla acá.

cat("   3. Armando base_radios (ancha, 1 fila por radio censal)...\n")

base_radios1 <- radios_proj |>
  st_drop_geometry() |>
  left_join(overlap_por_radio, by = "codigo_redatam") |>
  mutate(
    pct_area_villa       = replace_na(pct_area_villa, 0),
    n_barrios_populares   = replace_na(n_barrios_populares, 0L),
    barrios_populares     = if_else(is.na(barrios_populares), NA_character_, barrios_populares),
    es_barrio_popular     = pct_area_villa > 0
  ) |>
  left_join(distancias_wide, by = c("codigo_redatam" = "id_radio")) |>
  rename(id_radio = codigo_redatam)

cat(sprintf("   base_radios: %d radios x %d variables\n",
            nrow(base_radios), ncol(base_radios)))
cat(sprintf("   Radios que son (total o parcialmente) barrio popular: %d (%.1f%%)\n",
            sum(base_radios$es_barrio_popular),
            100 * mean(base_radios$es_barrio_popular)))

# -----------------------------------------------------------------------------
# 4) Promedio por comuna de TODAS las variables numéricas
# -----------------------------------------------------------------------------
# Se excluyen identificadores (no tiene sentido "promediar" un código de radio,
# fracción o número de radio). Ajustar esta lista si tu radios_proj real trae
# otras columnas de identificación además de las que aparecen en 05.

cat("   4. Promediando por comuna...\n")

cols_id_excluir <- c("id_radio", "fraccion", "radio")

promedios_por_comuna <- base_radios1 |>
  select(-any_of(cols_id_excluir)) |>
  group_by(nom_depto) |>
  summarise(
    n_radios         = n(),
    pct_radios_villa = round(mean(es_barrio_popular, na.rm = TRUE) * 100, 1),
    across(where(is.numeric), \(x) round(mean(x, na.rm = TRUE), 3)),
    .groups = "drop"
  )

cat(sprintf("   promedios_por_comuna: %d comunas x %d variables\n",
            nrow(promedios_por_comuna), ncol(promedios_por_comuna)))

cat("\n   Vista previa — promedios_por_comuna:\n")
print(
  promedios_por_comuna |>
    select(nom_depto, n_radios, pct_radios_villa, pct_area_villa,
           starts_with("dist_eucl_km"))
)

# -----------------------------------------------------------------------------
# 5) Exportar
# -----------------------------------------------------------------------------

write_csv(base_radios,          "base_radios.csv")
saveRDS(base_radios,             "base_radios.rds")
write_csv(promedios_por_comuna, "promedios_por_comuna.csv")
saveRDS(promedios_por_comuna,    "promedios_por_comuna.rds")

cat("\n✓ Archivos exportados:\n")
cat("   base_radios.csv/.rds           — 1 fila por radio: NBI/Censo + villa + acceso\n")
cat("   promedios_por_comuna.csv/.rds  — 1 fila por comuna: promedio de todo lo anterior\n")



base_radios1<- radios_proj%>%select(codigo_redatam, geometry)%>%left_join(
  base_radios1, by=c("codigo_redatam"="id_radio")) 

mapa_calor_cesac<-ggplot()+
  geom_sf(data = base_radios1, aes(fill=tiempo_transp_min_cesac), color="white", linewidth=NA)+
  labs(
    title="Tiempo estimado al CeSAC más cercano", 
    subtitle="Por radio censal", 
    caption="Estimado en función de la distancia euclidiana, un factor de tortuosidad urbano y la velocidad promedio del transporte público")+
  theme_void()+ theme(    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
                          plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey30"),
                          plot.caption  = element_text(size = 7, color = "grey50", hjust = 0.5),
                          legend.position = "right"
  )+  scale_fill_distiller(
    palette   = "YlGnBu",
    direction = 1,             # amarillo = cerca/rápido, rojo = lejos/lento
    name      = "Minutos",
    na.value  = "grey85"
  ) 

mapa_calor_cesac


comuna<-base_radios1%>%group_by(nom_depto)%>%summarise(tiempo_transp_cesac=mean(tiempo_transp_min_cesac))

mapa_calor_comuna<-ggplot()+ 
  geom_sf(data=comuna, aes(fill=tiempo_transp_cesac), color="white", linewidth=NA)

mapa_calor_comuna
