# =============================================================================
# Script: unir_radio_censal.R
# Descripción: Lee y une todos los archivos de indicadores a nivel de
#              radio censal (Redatam / INDEC Censo 2022, CABA).
#
# Estructura de cada xlsx:
#   - Fila 12   : encabezados en columna B → "Código | Total | Seleccionado | Porcentaje"
#   - Filas 13+ : datos (col A vacía, datos desde col B)
#   - Últimas 3 : pie INDEC (texto libre, se descartan)
#   - Código    : 8 dígitos (ej. 20070101 = Departamento 2007, Radio 01, Sector 01)
#
# Variables unidas
#   agua_corriente      radio_agua_corriente.xlsx
#   calidad_materiales  radio_calidad_materiales.xlsx
#   clima_educ_bajo     radio_clima_educativo_bajo.xlsx
#   NBI                 radio_NBI.xlsx
#   sec_completa_mas    radio_secundaria_completa.xlsx
#   edad_65_mas         radio_edad_65_o_mas.xlsx
#   hacinamiento        radio_hacinamiento.xlsx
#   cond_sanitarias_A   radio_cond_sanitarias_A.xlsx
#   cond_sanitarias_B   radio_cond_sanitarias_B.xlsx
#
# Output:
#   radio_censal_unido.csv   — tabla larga con todas las variables
#   radio_censal_unido.xlsx  — mismo contenido en Excel
#
# Dependencias: readxl, dplyr, tidyr, writexl
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# ── Configuración ─────────────────────────────────────────────────────────────

# Carpeta donde están los xlsx (ajustar si es necesario)
RUTA <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/ESW version 2/files_censo22"

# Definición de archivos: nombre_archivo -> nombre_variable de destino
# Cada fila tiene: archivo, variable_total, variable_n, variable_pct
#   variable_total = denominador (total viviendas del radio)
#   variable_n     = numerador  (viviendas con la característica)
#   variable_pct   = porcentaje calculado por Redatam

archivos_radio <- tribble(
  ~archivo,                           ~var_total,           ~var_n,                    ~var_pct,
  "radio_agua_corriente.xlsx",        "total_viv",          "n_agua_corriente",         "pct_agua_corriente",
  "radio_calidad_materiales.xlsx",    "total_viv",          "n_calidad_mat",            "pct_calidad_mat",
  "radio_clima_educativo_bajo.xlsx",  "total_viv",          "n_clima_educ_bajo",        "pct_clima_educ_bajo",
  "radio_NBI.xlsx",                   "total_viv",          "n_NBI",                    "pct_NBI",
  "radio_secundaria_completa.xlsx",   "total_viv",          "n_sec_completa",           "pct_sec_completa",
  "radio_edad_65_o_mas.xlsx",         "total_viv",          "n_edad_65_mas",            "pct_edad_65_mas",
  "radio_hacinamiento.xlsx",          "total_viv",          "n_hacinamiento",           "pct_hacinamiento",
  "radio_cond_sanitarias_A.xlsx",     "total_viv",          "n_cond_san_A",             "pct_cond_san_A",
  "radio_cond_sanitarias_B.xlsx",     "total_viv",          "n_cond_san_B",             "pct_cond_san_B"
)

# ── Función lectora ────────────────────────────────────────────────────────────

leer_radio <- function(archivo, var_total, var_n, var_pct) {

  ruta_completa <- file.path(RUTA, archivo)

  # Los xlsx de Redatam tienen:
  #   - col A vacía, datos desde col B
  #   - fila 12 = encabezados (Código | Total | Seleccionado | Porcentaje)
  #   - fila 13 en adelante = datos
  #   - últimas 3 filas = pie INDEC (texto, se filtran por tipo de Código)
  df_raw <- read_excel(
    ruta_completa,
    col_names = FALSE,     # leemos sin encabezado para controlar todo
    skip       = 11        # saltamos filas 1-11 (metadatos Redatam)
  )

  # Después del skip, fila 1 = encabezados, fila 2 en adelante = datos
  # Las columnas útiles son B, C, D, E → posiciones 2, 3, 4, 5 del df
  df <- df_raw |>
    slice(-1) |>                        # descartar fila de encabezados (ya sabemos qué son)
    select(codigo = 2,                  # col B: código radio (8 dígitos)
           total  = 3,                  # col C: total viviendas
           n      = 4,                  # col D: viviendas con característica
           pct    = 5) |>               # col E: porcentaje
    filter(
      !is.na(codigo),
      suppressWarnings(!is.na(as.numeric(codigo)))   # descarta filas INDEC (texto)
    ) |>
    mutate(
      codigo = as.integer(codigo),
      total  = as.numeric(total),
      n      = as.numeric(n),
      pct    = as.numeric(pct)
    )

  # Renombrar columnas con nombres de variable
  names(df)[names(df) == "total"] <- var_total
  names(df)[names(df) == "n"]     <- var_n
  names(df)[names(df) == "pct"]   <- var_pct

  df
}

# ── Lectura y merge ────────────────────────────────────────────────────────────

# Leer el primer archivo como base
cat("Leyendo archivos de radio censal...\n")

base <- leer_radio(
  archivos_radio$archivo[1],
  archivos_radio$var_total[1],
  archivos_radio$var_n[1],
  archivos_radio$var_pct[1]
)

cat(sprintf("  [1/9] %s — %d radios\n", archivos_radio$archivo[1], nrow(base)))

# Unir el resto con full_join para detectar eventuales radios ausentes
resultado <- base

for (i in seq(2, nrow(archivos_radio))) {

  df_nuevo <- leer_radio(
    archivos_radio$archivo[i],
    archivos_radio$var_total[i],
    archivos_radio$var_n[i],
    archivos_radio$var_pct[i]
  ) |>
    # El total de viviendas es el mismo en todos → lo eliminamos antes del join
    # para no duplicarlo (lo mantenemos solo del primer archivo)
    select(-matches("^total_viv$"))

  resultado <- full_join(resultado, df_nuevo, by = "codigo")

  cat(sprintf("  [%d/9] %s — %d radios\n",
              i, archivos_radio$archivo[i], nrow(df_nuevo)))
}

# ── Derivar identificadores geográficos desde el código ───────────────────────
# Código radio (8 dígitos): PPMMRRSS
#   PP = Provincia (02 = CABA, pero Redatam usa 2007 etc → ver abajo)
# Estructura real Redatam CABA: AAAABBCC
#   AAAA = código departamento/comuna (ej. 2007 = Comuna 1)
#   BB   = fracción censal
#   CC   = radio censal
# Extraemos:
#   cod_comuna   = primeros 4 dígitos  (ej. 2007)
#   cod_fraccion = dígitos 5-6         (ej. 01)
#   cod_radio    = dígitos 7-8         (ej. 01)

datos_radio_censo <- resultado |>
  mutate(
    cod_departamento = as.integer(substr(as.character(codigo), 1, 4)),
    cod_fraccion     = as.integer(substr(as.character(codigo), 5, 6)),
    cod_radio        = as.integer(substr(as.character(codigo), 7, 8)),
    # Número de comuna: Redatam asigna 2007=C1, 2014=C2, ... 2105=C15
    # Fórmula: (cod_departamento - 2007) / 7 + 1
    numero_comuna    = ((cod_departamento - 2007) / 7) + 1
  ) |>
  select(
    codigo,
    cod_departamento,
    numero_comuna,
    cod_fraccion,
    cod_radio,
    total_viv,
    everything()
  )

# ── Resumen ────────────────────────────────────────────────────────────────────

cat("\n=== RESUMEN DATASET RADIO CENSAL ===\n")
cat(sprintf("  Radios totales:    %d\n",   nrow(resultado)))
cat(sprintf("  Variables:         %d\n",   ncol(resultado)))
cat(sprintf("  Comunas cubiertas: %d\n",   n_distinct(resultado$numero_comuna)))
cat(sprintf("  Columnas:\n"))
cat(paste0("    ", names(resultado), collapse = "\n"), "\n")

# Verificar que todos los archivos aportaron el mismo conjunto de radios
cat(sprintf("\n  Radios sin NA en n_agua_corriente: %d\n",
            sum(!is.na(resultado$n_agua_corriente))))

# ── Exportar ───────────────────────────────────────────────────────────────────

write.csv(datos_radio_censo,
          file      = file.path(RUTA, "radio_censal_unido.csv"),
          row.names = FALSE,
          fileEncoding = "UTF-8")

write_xlsx(datos_radio_censo,
           path = file.path(RUTA, "radio_censal_unido.xlsx"))

cat("\n✓ Archivos exportados:\n")
cat("   radio_censal_unido.csv\n")
cat("   radio_censal_unido.xlsx\n")




