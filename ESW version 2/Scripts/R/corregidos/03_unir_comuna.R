# =============================================================================
# Script: 03_unir_comuna.R   (versión corregida de unir_comuna_v2.R)
# Descripción: Lee y une los archivos de indicadores a nivel de comuna
#              (Redatam / INDEC Censo 2022, CABA).
#
# Cambios respecto a la versión anterior:
#   · Rutas centralizadas en 00_config.R; los xlsx se localizan con
#     buscar_archivo() bajo data/raw (ya no depende de `files_censo22`).
#   · Todos los joins son solo por `codigo` (antes se unía también por
#     `departamento`; una diferencia de texto/espacios generaba filas duplicadas
#     con NA). `departamento` se toma de un único archivo.
#   · leer_comuna_pct() es robusto a archivos con menos de 6 columnas (si la
#     columna de porcentaje viene vacía, read_excel la descarta): en ese caso
#     calcula pct = 100 * n / total y avisa.
#   · Se chequea que haya exactamente 15 comunas y que `total` sea consistente.
#   · Salida a data/processed (donde la lee 01_preparar_datos.R).
#
# Estructura esperada de cada xlsx (Redatam / INDEC):
#   Fila 12: encabezados (col B → Código | Departamento | Total | Seleccionado | %)
#   Filas 13+: datos (col A vacía), 15 comunas; al final pie INDEC (se descarta)
#   Código: 2007 = C1, 2014 = C2, ..., 2105 = C15  → comuna = (codigo-2007)/7 + 1
#
# Output: comuna_unido.csv / comuna_unido.xlsx en RUTA_PROC
# Dependencias: readxl, dplyr, writexl
# =============================================================================

CONFIG <- paste0(
  "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/",
  "Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/",
  "ESW version 2/Scripts/R/corregidos/00_config.R"
)
source(CONFIG)

library(readxl)
library(writexl)

# ── Función lectora ───────────────────────────────────────────────────────────
leer_comuna_pct <- function(archivo, var_n, var_pct) {

  df_raw <- read_excel(buscar_archivo(archivo, RUTA_RAW),
                       col_names = FALSE,
                       skip      = 11) |>   # fila 12 (encabezado) pasa a ser la 1ª
    slice(-1)                                # descarta la fila de encabezado

  if (ncol(df_raw) < 5) {
    stop(archivo, ": se esperaban >= 5 columnas (B:F) y hay ", ncol(df_raw))
  }

  df <- df_raw |>
    transmute(
      codigo       = suppressWarnings(as.integer(as.numeric(.data[[names(df_raw)[2]]]))),
      departamento = as.character(.data[[names(df_raw)[3]]]),
      total        = suppressWarnings(as.numeric(.data[[names(df_raw)[4]]])),
      n            = suppressWarnings(as.numeric(.data[[names(df_raw)[5]]])),
      pct          = if (ncol(df_raw) >= 6) {
        suppressWarnings(as.numeric(.data[[names(df_raw)[6]]]))
      } else {
        NA_real_
      }
    ) |>
    filter(!is.na(codigo))                   # descarta pie INDEC (texto)

  if (ncol(df_raw) < 6) {
    warning(archivo, ": sin columna de porcentaje; se calcula 100*n/total.")
    df$pct <- 100 * df$n / df$total
  }

  names(df)[names(df) == "n"]   <- var_n
  names(df)[names(df) == "pct"] <- var_pct
  df
}

# ── Lectura ───────────────────────────────────────────────────────────────────
cat("Leyendo archivos de comuna...\n")

archivos_comuna <- tibble::tribble(
  ~archivo,                           ~var_n,                    ~var_pct,
  "comuna_agua_corriente.xlsx",       "n_agua_corriente",        "pct_agua_corriente",
  "comuna_calidad_materiales.xlsx",   "n_calidad_mat",           "pct_calidad_mat",
  "comuna_clima_educativo_bajo.xlsx", "n_clima_educ_bajo",       "pct_clima_educ_bajo",
  "comuna_secundaria_completa.xlsx",  "n_sec_completa",          "pct_sec_completa",
  "nbi_comuna.xlsx",                  "n_hogares_NBI",           "pct_hogares_NBI",
  "hacinamiento_comuna.xlsx",         "n_hogares_hacinamiento",  "pct_hogares_hacinamiento"
)

lista <- vector("list", nrow(archivos_comuna))
for (i in seq_len(nrow(archivos_comuna))) {
  lista[[i]] <- leer_comuna_pct(archivos_comuna$archivo[i],
                                archivos_comuna$var_n[i],
                                archivos_comuna$var_pct[i])
  cat(sprintf("  [%d/%d] %-34s — %d comunas\n", i, nrow(archivos_comuna),
              archivos_comuna$archivo[i], nrow(lista[[i]])))
}

# ── Diagnóstico de cobertura ──────────────────────────────────────────────────
codigos_ref <- sort(unique(lista[[1]]$codigo))
if (length(codigos_ref) != 15) {
  warning("El primer archivo trae ", length(codigos_ref), " códigos (esperado: 15).")
}
for (i in seq_along(lista)) {
  faltan <- setdiff(codigos_ref, lista[[i]]$codigo)
  sobran <- setdiff(lista[[i]]$codigo, codigos_ref)
  if (length(faltan)) cat(sprintf("  ⚠ %s: faltan códigos %s\n",
                                  archivos_comuna$archivo[i], paste(faltan, collapse = ", ")))
  if (length(sobran)) cat(sprintf("  ⚠ %s: códigos inesperados %s\n",
                                  archivos_comuna$archivo[i], paste(sobran, collapse = ", ")))
  if (anyDuplicated(lista[[i]]$codigo)) {
    stop(archivos_comuna$archivo[i], ": códigos duplicados; revisar el xlsx.")
  }
}

# ── Merge (solo por `codigo`) ─────────────────────────────────────────────────
# `total` y `departamento` se conservan del primer archivo (agua corriente).
# Del resto se descartan, pero antes se chequea la consistencia de `total`.
base <- lista[[1]]
resultado <- base
for (i in 2:length(lista)) {
  chk <- lista[[i]] |> select(codigo, total_i = total) |>
    left_join(base |> select(codigo, total_ref = total), by = "codigo")
  if (any(abs(chk$total_i - chk$total_ref) > 0, na.rm = TRUE)) {
    cat(sprintf("  ⚠ ADVERTENCIA: `total` difiere entre %s y %s\n",
                archivos_comuna$archivo[i], archivos_comuna$archivo[1]))
  }
  resultado <- full_join(
    resultado,
    lista[[i]] |> select(-total, -departamento),
    by = "codigo"
  )
}

# ── Número de comuna e indicadores ────────────────────────────────────────────
datos_comuna_censo <- resultado |>
  mutate(numero_comuna = as.integer(round((codigo - 2007) / 7 + 1))) |>
  select(
    codigo, numero_comuna, departamento, total,
    n_agua_corriente,       pct_agua_corriente,
    n_calidad_mat,          pct_calidad_mat,
    n_clima_educ_bajo,      pct_clima_educ_bajo,
    n_sec_completa,         pct_sec_completa,
    n_hogares_NBI,          pct_hogares_NBI,
    n_hogares_hacinamiento, pct_hogares_hacinamiento
  ) |>
  arrange(numero_comuna)

stopifnot(
  "Se esperaban 15 comunas"                = nrow(datos_comuna_censo) == 15,
  "numero_comuna fuera del rango 1-15"     = all(datos_comuna_censo$numero_comuna %in% 1:15),
  "numero_comuna duplicado"                = !anyDuplicated(datos_comuna_censo$numero_comuna)
)

cat("\n=== RESUMEN DATASET COMUNAS ===\n")
cat(sprintf("  Comunas:   %d\n", nrow(datos_comuna_censo)))
cat(sprintf("  Variables: %d\n", ncol(datos_comuna_censo)))
print(datos_comuna_censo |>
        select(numero_comuna, departamento, total, pct_agua_corriente,
               pct_hogares_NBI, pct_hogares_hacinamiento))

# ── Exportar ──────────────────────────────────────────────────────────────────
write.csv(datos_comuna_censo,
          file         = file.path(RUTA_PROC, "comuna_unido.csv"),
          row.names    = FALSE,
          fileEncoding = "UTF-8")
write_xlsx(datos_comuna_censo, path = file.path(RUTA_PROC, "comuna_unido.xlsx"))

cat("\n✓ Archivos exportados en", RUTA_PROC, "\n   comuna_unido.csv\n   comuna_unido.xlsx\n")
