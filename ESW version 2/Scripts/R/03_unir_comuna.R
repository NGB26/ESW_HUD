# =============================================================================
# Script: unir_comuna_v2.R
# Descripción: Lee y une todos los archivos de indicadores a nivel de
#              comuna (Redatam / INDEC Censo 2022, CABA).
#
# CAMBIO PRINCIPAL respecto a unir_comuna.R
#   - NBI y hacinamiento ahora se leen desde sus archivos específicos:
#       nbi_comuna.xlsx          → n_hogares_NBI / pct_hogares_NBI
#       hacinamiento_comuna.xlsx → n_hogares_hacinamiento / pct_hogares_hacinamiento
#   - Se eliminan hogares_A, hogares_B y hogares_C (archivos genéricos)
#     que estaban siendo usados incorrectamente para estas variables.
#
# Estructura esperada de cada xlsx (Redatam / INDEC):
#   - Fila 12   : encabezados (col B → "Código | Departamento | Total | Seleccionado | [Porcentaje]")
#   - Filas 13+ : datos (col A vacía, datos desde col B), 15 comunas
#   - Últimas filas: pie INDEC (texto libre, se descartan por filtro numérico en código)
#   - Código    : 4 dígitos (2007=C1, 2014=C2, ..., 2105=C15)
#
# Archivos utilizados:
#   CON porcentaje (5 columnas):
#     agua_corriente       comuna_agua_corriente.xlsx
#     calidad_materiales   comuna_calidad_materiales.xlsx
#     clima_educ_bajo      comuna_clima_educativo_bajo.xlsx
#     sec_completa_mas     comuna_secundaria_completa.xlsx
#
#   CON porcentaje (fuentes específicas NBI y hacinamiento):
#     nbi                  nbi_comuna.xlsx
#     hacinamiento         hacinamiento_comuna.xlsx
#
# Output:
#   comuna_unido.csv   — tabla a nivel comuna con todas las variables
#   comuna_unido.xlsx  — mismo contenido en Excel
#
# Dependencias: readxl, dplyr, writexl
# =============================================================================

library(readxl)
library(dplyr)
library(writexl)

# ── Configuración ─────────────────────────────────────────────────────────────

RUTA <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2/files_censo22"

# ── Función lectora — archivos CON porcentaje (5 columnas) ───────────────────
# Aplica a: agua corriente, calidad materiales, clima educativo,
#           secundaria completa, NBI, hacinamiento

leer_comuna_pct <- function(archivo, var_n, var_pct) {
  # Columnas esperadas: B=Código | C=Departamento | D=Total | E=Seleccionado | F=Porcentaje
  df <- read_excel(
    file.path(RUTA, archivo),
    col_names = FALSE,
    skip      = 11          # salta hasta fila 12 (encabezado), lee desde fila 13
  ) |>
    slice(-1) |>            # descarta la fila de encabezado leída como dato
    select(
      codigo       = 2,
      departamento = 3,
      total        = 4,     # total de hogares/viviendas del denominador
      n            = 5,     # conteo del indicador (Seleccionado)
      pct          = 6      # porcentaje ya calculado por INDEC/Redatam
    ) |>
    filter(
      !is.na(codigo),
      suppressWarnings(!is.na(as.numeric(codigo)))   # descarta pie INDEC
    ) |>
    mutate(
      codigo       = as.integer(codigo),
      departamento = as.character(departamento),
      total        = as.numeric(total),
      n            = as.numeric(n),
      pct          = as.numeric(pct)
    )
  
  # Renombrar n y pct con los nombres específicos de la variable
  names(df)[names(df) == "n"]   <- var_n
  names(df)[names(df) == "pct"] <- var_pct
  
  df
}

# ── Lectura ────────────────────────────────────────────────────────────────────

cat("Leyendo archivos de comuna...\n")

# [1] Agua corriente
agua        <- leer_comuna_pct(
  "comuna_agua_corriente.xlsx",
  "n_agua_corriente", "pct_agua_corriente"
)
cat(sprintf("  [1/6] comuna_agua_corriente.xlsx       — %d comunas\n", nrow(agua)))

# [2] Calidad de materiales
calidad_mat <- leer_comuna_pct(
  "comuna_calidad_materiales.xlsx",
  "n_calidad_mat", "pct_calidad_mat"
)
cat(sprintf("  [2/6] comuna_calidad_materiales.xlsx   — %d comunas\n", nrow(calidad_mat)))

# [3] Clima educativo bajo
clima_educ  <- leer_comuna_pct(
  "comuna_clima_educativo_bajo.xlsx",
  "n_clima_educ_bajo", "pct_clima_educ_bajo"
)
cat(sprintf("  [3/6] comuna_clima_educativo_bajo.xlsx — %d comunas\n", nrow(clima_educ)))

# [4] Secundaria completa
sec_comp    <- leer_comuna_pct(
  "comuna_secundaria_completa.xlsx",
  "n_sec_completa", "pct_sec_completa"
)
cat(sprintf("  [4/6] comuna_secundaria_completa.xlsx  — %d comunas\n", nrow(sec_comp)))

# [5] NBI — ARCHIVO ESPECÍFICO (antes: comuna_hogares_A.xlsx — INCORRECTO)
nbi         <- leer_comuna_pct(
  "nbi_comuna.xlsx",
  "n_hogares_NBI", "pct_hogares_NBI"
)
cat(sprintf("  [5/6] nbi_comuna.xlsx                  — %d comunas\n", nrow(nbi)))

# [6] Hacinamiento — ARCHIVO ESPECÍFICO (antes: comuna_hogares_B.xlsx — INCORRECTO)
hacinamiento <- leer_comuna_pct(
  "hacinamiento_comuna.xlsx",
  "n_hogares_hacinamiento", "pct_hogares_hacinamiento"
)
cat(sprintf("  [6/6] hacinamiento_comuna.xlsx         — %d comunas\n", nrow(hacinamiento)))

# ── Diagnóstico rápido de cobertura ───────────────────────────────────────────
# Verifica que los 6 archivos traigan las mismas 15 comunas

codigos_esperados <- unique(agua$codigo)

archivos_diag <- list(
  agua        = agua$codigo,
  calidad_mat = calidad_mat$codigo,
  clima_educ  = clima_educ$codigo,
  sec_comp    = sec_comp$codigo,
  nbi         = nbi$codigo,
  hacinamiento = hacinamiento$codigo
)

for (nm in names(archivos_diag)) {
  faltan <- setdiff(codigos_esperados, archivos_diag[[nm]])
  sobran <- setdiff(archivos_diag[[nm]], codigos_esperados)
  if (length(faltan) > 0)
    cat(sprintf("  ⚠ %s: faltan códigos %s\n", nm, paste(faltan, collapse = ", ")))
  if (length(sobran) > 0)
    cat(sprintf("  ⚠ %s: códigos inesperados %s\n", nm, paste(sobran, collapse = ", ")))
}

# ── Merge ──────────────────────────────────────────────────────────────────────
# La columna `total` (total de hogares) debería ser consistente entre archivos.
# Se retiene la de `agua` como referencia y se descarta en los restantes.
# Si hubiera discrepancias, el diagnóstico de abajo lo detectará.

resultado <- agua |>
  full_join(
    calidad_mat  |> select(-total),
    by = c("codigo", "departamento")
  ) |>
  full_join(
    clima_educ   |> select(-total),
    by = c("codigo", "departamento")
  ) |>
  full_join(
    sec_comp     |> select(-total),
    by = c("codigo", "departamento")
  ) |>
  full_join(
    nbi          |> select(-total, -departamento),   # departamento ya viene de agua
    by = "codigo"
  ) |>
  full_join(
    hacinamiento |> select(-total, -departamento),
    by = "codigo"
  )

# ── Diagnóstico: consistencia del total entre archivos ────────────────────────
# Compara el total de nbi y hacinamiento contra el de agua como referencia

total_nbi   <- nbi          |> select(codigo, total_nbi   = total)
total_hacin <- hacinamiento |> select(codigo, total_hacin = total)
total_ref   <- agua         |> select(codigo, total_ref   = total)

check_totales <- total_ref |>
  left_join(total_nbi,   by = "codigo") |>
  left_join(total_hacin, by = "codigo") |>
  mutate(
    diff_nbi   = abs(total_ref - total_nbi),
    diff_hacin = abs(total_ref - total_hacin)
  )

if (any(check_totales$diff_nbi   > 0, na.rm = TRUE))
  cat("  ⚠ ADVERTENCIA: el total de hogares difiere entre nbi_comuna.xlsx y agua_corriente.xlsx\n")
if (any(check_totales$diff_hacin > 0, na.rm = TRUE))
  cat("  ⚠ ADVERTENCIA: el total de hogares difiere entre hacinamiento_comuna.xlsx y agua_corriente.xlsx\n")

# ── Agregar número de comuna e indicadores derivados ──────────────────────────
# Código departamento CABA: 2007=C1, 2014=C2, ..., 2105=C15
# Fórmula: numero_comuna = (codigo - 2007) / 7 + 1

datos_comuna_censo <- resultado |>
  mutate(
    numero_comuna = as.integer(((codigo - 2007) / 7) + 1)
  ) |>
  select(
    codigo,
    numero_comuna,
    departamento,
    total,
    # Agua corriente
    n_agua_corriente,        pct_agua_corriente,
    # Calidad de materiales
    n_calidad_mat,           pct_calidad_mat,
    # Clima educativo bajo
    n_clima_educ_bajo,       pct_clima_educ_bajo,
    # Secundaria completa
    n_sec_completa,          pct_sec_completa,
    # NBI — ahora desde nbi_comuna.xlsx
    n_hogares_NBI,           pct_hogares_NBI,
    # Hacinamiento — ahora desde hacinamiento_comuna.xlsx
    n_hogares_hacinamiento,  pct_hogares_hacinamiento
  ) |>
  arrange(numero_comuna)

# ── Resumen final ──────────────────────────────────────────────────────────────

cat("\n=== RESUMEN DATASET COMUNAS ===\n")
cat(sprintf("  Comunas:   %d\n",  nrow(datos_comuna_censo)))
cat(sprintf("  Variables: %d\n",  ncol(datos_comuna_censo)))
cat("  Columnas:\n")
cat(paste0("    ", names(datos_comuna_censo), collapse = "\n"), "\n")

cat("\nPrimeras filas (variables clave):\n")
print(
  datos_comuna_censo |>
    select(
      numero_comuna,
      departamento,
      total,
      pct_agua_corriente,
      pct_hogares_NBI,
      pct_hogares_hacinamiento
    )
)

# ── Exportar ───────────────────────────────────────────────────────────────────

write.csv(
  datos_comuna_censo,
  file         = file.path(RUTA, "comuna_unido.csv"),
  row.names    = FALSE,
  fileEncoding = "UTF-8"
)

write_xlsx(
  datos_comuna_censo,
  path = file.path(RUTA, "comuna_unido.xlsx")
)

cat("\n✓ Archivos exportados:\n")
cat("   comuna_unido.csv\n")
cat("   comuna_unido.xlsx\n")