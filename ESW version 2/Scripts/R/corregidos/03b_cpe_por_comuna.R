# =============================================================================
# Script: 03b_cpe_por_comuna.R
#
# Objetivo: construir la población con Cobertura Pública Exclusiva (CPE) por
#           radio censal y por comuna (Censo 2022, extracción Redatam+SP).
#
# Input  : xlsx de Redatam con la variable CPE por radio censal
#          (por defecto se busca "cpe_redatam_caba.xlsx" dentro de RAIZ; si tiene
#          otro nombre, cualquier .xlsx cuyo nombre contenga "cpe").
#          Layout esperado (igual que el resto de los Redatam):
#            col B = código de radio (8 dígitos = (2000 + DEPTO)*10000 + FRAC*100 + RADIO)
#            col C = total de personas, col D = personas con CPE        (3 cols de datos)
#            ó col C = departamento (texto), D = total, E = CPE, F = %  (layout de 03)
#
# Output (en RUTA_PROC / RUTA_FIG):
#   cpe_radio.csv     codigo_redatam, numero_comuna, total_pers, n_cpe, pct_cpe_r
#   cpe_comuna.csv    numero_comuna, total_pers, n_cpe, pct_cpe_c,
#                     share_cpe_comuna (N_CPE_c / N_CPE_CABA)
#   mapa_cpe_comuna.pdf
#
# Chequeos:
#   · comuna = DEPTO / 7 debe ser entero 1..15 en todos los radios
#   · n_cpe <= total_pers en cada radio
#   · total CABA razonable (~3,1 millones de personas, Censo 2022)
#   · compara pct_cpe_c con los 15 valores hardcodeados en generar_pct_cpe_c.do
#     (aviso si difieren más de 0,05 pp: ahí la etiqueta dice "% viviendas",
#      acá se calcula % de PERSONAS)
#
# Si no se encuentra el Excel se usan los 15 valores del do-file (solo comuna).
# =============================================================================

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

library(readxl)
library(ggplot2)
library(readr)

# Valores de referencia (generar_pct_cpe_c.do, Redatam 15/05/2026)
cpe_ref <- tibble::tibble(
  numero_comuna = 1:15,
  pct_cpe_do = c(27.93, 7.68, 20.11, 28.54, 12.44, 7.99, 17.50, 41.57,
                 22.26, 14.67, 10.47, 8.63, 6.09, 7.42, 13.65)
)

# ── 1. Localizar el Excel ─────────────────────────────────────────────────────
cands <- list.files(RAIZ, pattern = "cpe.*\\.xlsx$", recursive = TRUE,
                    full.names = TRUE, ignore.case = TRUE)
cands <- cands[!grepl("~\\$", basename(cands))]      # archivos temporales de Excel
if (length(cands) == 0) {
  warning("No encuentro un .xlsx de CPE bajo ", RAIZ,
          ". Se usan los valores del do-file (sin nivel radio).")
}
if (length(cands) > 1) {
  message("Varios candidatos de CPE; uso el primero:\n  ",
          paste(cands, collapse = "\n  "))
}

if (length(cands) > 0) {
  path_cpe <- cands[1]
  cat("-- CPE desde:", path_cpe, "\n")

  hojas <- excel_sheets(path_cpe)
  hoja  <- if ("Output" %in% hojas) "Output" else hojas[1]
  raw   <- read_excel(path_cpe, sheet = hoja, col_names = FALSE)

  # Se queda con las filas cuyo col 2 es un código de radio de 7-8 dígitos; así
  # no depende de cuántas filas de metadatos trae Redatam.
  cod_num <- suppressWarnings(as.numeric(raw[[2]]))
  es_radio <- !is.na(cod_num) & cod_num >= 1e6 & cod_num < 1e9
  raw <- raw[es_radio, , drop = FALSE]
  if (nrow(raw) == 0) stop("No se encontraron códigos de radio en la columna B de ", path_cpe)

  idx_total <- if (ncol(raw) >= 5) 4 else 3
  idx_cpe   <- idx_total + 1

  cpe_radio <- tibble::tibble(
    codigo_redatam = as.integer(as.numeric(raw[[2]])),
    total_pers     = suppressWarnings(as.numeric(raw[[idx_total]])),
    n_cpe          = suppressWarnings(as.numeric(raw[[idx_cpe]]))
  ) |>
    mutate(
      depto         = codigo_redatam %/% 10000L - 2000L,
      numero_comuna = depto / 7,
      pct_cpe_r     = if_else(total_pers > 0, 100 * n_cpe / total_pers, NA_real_)
    )

  # ── Chequeos ────────────────────────────────────────────────────────────────
  stopifnot("Hay códigos de radio duplicados" = !anyDuplicated(cpe_radio$codigo_redatam))
  if (any(is.na(cpe_radio$total_pers) | is.na(cpe_radio$n_cpe))) {
    warning(sum(is.na(cpe_radio$total_pers) | is.na(cpe_radio$n_cpe)),
            " radios con total o CPE no numérico (¿columnas corridas?).")
  }
  if (any(cpe_radio$numero_comuna != round(cpe_radio$numero_comuna) |
          cpe_radio$numero_comuna < 1 | cpe_radio$numero_comuna > 15, na.rm = TRUE)) {
    stop("DEPTO/7 no da una comuna entera 1..15 en algunos radios. Revisar la ",
         "columna de código (esperado: (2000+DEPTO)*10000 + FRAC*100 + RADIO).")
  }
  if (any(cpe_radio$n_cpe > cpe_radio$total_pers, na.rm = TRUE)) {
    warning(sum(cpe_radio$n_cpe > cpe_radio$total_pers, na.rm = TRUE),
            " radios con CPE > total: probablemente las columnas total/CPE están invertidas.")
  }
  tot_caba <- sum(cpe_radio$total_pers, na.rm = TRUE)
  cat(sprintf("   %d radios | población total %s | con CPE %s (%.1f%%)\n",
              nrow(cpe_radio), format(tot_caba, big.mark = "."),
              format(sum(cpe_radio$n_cpe, na.rm = TRUE), big.mark = "."),
              100 * sum(cpe_radio$n_cpe, na.rm = TRUE) / tot_caba))
  if (tot_caba < 2.9e6 || tot_caba > 3.3e6) {
    warning("La población total (", format(tot_caba, big.mark = "."),
            ") no está cerca de los ~3,1 millones de CABA. ¿Es la columna correcta?")
  }

  cpe_radio <- cpe_radio |>
    mutate(numero_comuna = as.integer(numero_comuna)) |>
    select(codigo_redatam, numero_comuna, total_pers, n_cpe, pct_cpe_r)

  cpe_comuna <- cpe_radio |>
    group_by(numero_comuna) |>
    summarise(total_pers = sum(total_pers, na.rm = TRUE),
              n_cpe      = sum(n_cpe,      na.rm = TRUE),
              .groups = "drop") |>
    mutate(pct_cpe_c        = 100 * n_cpe / total_pers,
           share_cpe_comuna = n_cpe / sum(n_cpe))

  stopifnot("Se esperaban 15 comunas" = nrow(cpe_comuna) == 15)

  # Comparación con el do-file
  comp <- cpe_comuna |>
    left_join(cpe_ref, by = "numero_comuna") |>
    mutate(dif_pp = round(pct_cpe_c - pct_cpe_do, 2))
  cat("\n   pct_cpe_c (R, % personas) vs do-file de Stata:\n")
  print(comp |> select(numero_comuna, pct_cpe_c, pct_cpe_do, dif_pp), n = 15)
  if (any(abs(comp$dif_pp) > 0.05, na.rm = TRUE)) {
    warning("pct_cpe_c difiere del do-file en más de 0,05 pp en ",
            sum(abs(comp$dif_pp) > 0.05, na.rm = TRUE),
            " comunas: confirmar cuál definición se quiere usar (personas vs viviendas).")
  }

  write_csv(cpe_radio, file.path(RUTA_PROC, "cpe_radio.csv"))

} else {
  cpe_comuna <- cpe_ref |>
    transmute(numero_comuna, total_pers = NA_real_, n_cpe = NA_real_,
              pct_cpe_c = pct_cpe_do, share_cpe_comuna = NA_real_)
}

write_csv(cpe_comuna, file.path(RUTA_PROC, "cpe_comuna.csv"))
cat("\n✓ Exportado: cpe_comuna.csv", if (length(cands) > 0) "y cpe_radio.csv", "en", RUTA_PROC, "\n")

# ── 2. Mapa coroplético por comuna (PDF) ──────────────────────────────────────
path_cp <- file.path(RUTA_PROC, "comunas_proj.rds")
if (file.exists(path_cp)) {
  comunas_cpe <- readRDS(path_cp) |>
    select(numero_comuna) |>
    left_join(cpe_comuna |> select(numero_comuna, pct_cpe_c), by = "numero_comuna")

  mapa_cpe <- ggplot(comunas_cpe) +
    geom_sf(aes(fill = pct_cpe_c), color = "white", linewidth = 0.4) +
    geom_sf_text(aes(label = paste0("C", numero_comuna, "\n", round(pct_cpe_c, 1), "%")),
                 size = 2.8, fontface = "bold", color = "black") +
    scale_fill_distiller(palette = "YlGnBu", direction = 1,
                         name = "% con CPE") +
    labs(title    = "Población con cobertura pública exclusiva por comuna",
         subtitle = "CABA, Censo 2022 (INDEC / Redatam)") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  guardar_pdf(mapa_cpe, "mapa_cpe_comuna", width = 8, height = 8)
} else {
  message("No existe ", path_cp, " (lo genera 01_preparar_datos.R): se omite el mapa.")
}
