# =============================================================================
# SIMULACIÓN MONTE CARLO: ASIGNACIÓN ALEATORIA DE RADIO CENSAL POR INDIVIDUO
# =============================================================================
#
# Motivación: cada individuo de data_revisada_ok solo tiene comuna de
# residencia (agregado), no radio censal exacto. La variable de interés
# (tiempo_transp_min_cesac) está definida a nivel de radio censal, más fino.
# Para propagar esa incertidumbre de agregación en vez de ignorarla, se
# sortea repetidamente (1000 veces) un radio al azar dentro de la comuna de
# cada individuo, ponderado por población (total_viv), y se toma su tiempo.
#
# No es una cadena de Markov: cada sorteo es independiente del anterior, sin
# memoria de estado. Es una simulación Monte Carlo por remuestreo — el
# promedio entre las M corridas da un punto estimado por individuo, y la
# varianza entre corridas cuantifica la incertidumbre geográfica.
#
# Requiere en el entorno:
#   - base_radios1 : con columnas nom_depto, id_radio, tiempo_transp_min_cesac,
#                     total_viv, y las variables de NBI/censo
#
# Se sortea el ID del radio censal (no el tiempo directamente), para poder
# recuperar después CUALQUIER variable de ese radio por join — no solo el
# tiempo. Ver punto 4.2.
#
# Salidas:
#   simulacion_larga    : id_individuo, comuna_norm, iteracion, id_radio_sim
#                          (1 fila por individuo x iteración)
#   simulacion_completa : lo anterior + todas las variables de data_revisada
#                          y de base_radios1 (NBI, censo, tiempo, etc.)
#   simulacion_resumen  : 1 fila por individuo — media/DE/mediana entre corridas
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

set.seed(1234)   # reproducibilidad — documentar esta semilla si esto va al paper

N_SIM                   <- 1000
PONDERAR_POR_POBLACION  <- TRUE   # TRUE: pondera por total_viv | FALSE: sorteo uniforme entre radios

# -----------------------------------------------------------------------------
# 0) Importar datos
# -----------------------------------------------------------------------------

RUTA_DATOS <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/ESW version 2/data/processed"

cat("── 1. Importando data_revisada_ok.xlsx...\n")

data_revisada <- read_excel(file.path(RUTA_DATOS, "data_revisada_ok.xlsx"))

cat(sprintf("   %d filas, %d columnas\n", nrow(data_revisada), ncol(data_revisada)))
stopifnot("La columna 'comuna' no está en data_revisada_ok" = "comuna" %in% names(data_revisada))

# -----------------------------------------------------------------------------
# 1) Normalizar "comuna" al formato de nom_depto en base_radios1
# -----------------------------------------------------------------------------
# Se extrae el número de comuna de cualquier formato de entrada (numérico,
# "1", "01", "Comuna 1", "COMUNA  01", etc.) y se reconstruye como
# "Comuna N" sin cero a la izquierda — el formato usado en el resto de tu
# pipeline (comuna_texto, comuna_lookup en el script 06). Si el chequeo de
# abajo muestra que nom_depto en tu shapefile real SÍ usa cero a la
# izquierda ("Comuna 01"), ajustar con sprintf("Comuna %02d", comuna_num).

cat("── 2. Normalizando columna comuna...\n")

data_revisada <- data_revisada |>
  mutate(
    comuna_num  = as.integer(str_extract(as.character(comuna), "\\d+")),
    comuna_norm = paste0("Comuna ", comuna_num)
  )

# --- 1b) Chequeo de correspondencia con nom_depto ---------------------------
comunas_datos  <- sort(unique(data_revisada$comuna_norm))
comunas_radios <- sort(unique(base_radios1$nom_depto))

sin_match_datos  <- setdiff(comunas_datos, comunas_radios)
sin_match_radios <- setdiff(comunas_radios, comunas_datos)

if (length(sin_match_datos) > 0) {
  cat("   ⚠ Valores de comuna_norm SIN correspondencia en nom_depto:\n")
  print(sin_match_datos)
  cat("   Revisar formato (¿cero a la izquierda? ¿tildes? ¿comuna fuera de CABA?)\n")
}
if (length(sin_match_radios) > 0) {
  cat("   ⚠ Comunas de nom_depto que no aparecen en los datos:\n")
  print(sin_match_radios)
}

n_sin_comuna <- sum(is.na(data_revisada$comuna_num))
if (n_sin_comuna > 0) {
  warning(sprintf("%d individuos sin comuna identificable tras la normalización.", n_sin_comuna))
}

# -----------------------------------------------------------------------------
# 2) Identificador único de individuo (por si no hay uno explícito)
# -----------------------------------------------------------------------------
if (!"id_individuo" %in% names(data_revisada)) {
  data_revisada <- data_revisada |>
    mutate(id_individuo = row_number(), .before = 1)
}

# -----------------------------------------------------------------------------
# 3) Pool de radios (tiempo + peso poblacional) por comuna
# -----------------------------------------------------------------------------
# Solo se incluyen radios con tiempo_transp_min_cesac no faltante — un radio
# sin CeSAC alcanzable no debería poder ser sorteado como si tuviera dato.

cat("── 3. Armando el pool de radios censales por comuna...\n")

pool_por_comuna <- base_radios1 |>
  filter(!is.na(tiempo_transp_min_cesac)) |>
  group_by(nom_depto) |>
  summarise(
    n_radios_pool = n(),
    radios_pool   = list(codigo_redatam),               # se sortea el ID, no el tiempo directamente
    pesos         = list(replace_na(total_viv, 0)),
    .groups = "drop"
  )

cat("   Radios disponibles por comuna:\n")
print(pool_por_comuna |> select(nom_depto, n_radios_pool))

# -----------------------------------------------------------------------------
# 4) Simulación: N_SIM sorteos de radio censal por individuo
# -----------------------------------------------------------------------------

cat(sprintf("── 4. Sorteando %d radios censales por individuo (ponderado por población: %s)...\n",
            N_SIM, PONDERAR_POR_POBLACION))

sortear_radios <- function(radios_pool, pesos_pool, n_sim, ponderar) {
  if (is.null(radios_pool) || length(radios_pool) == 0) {
    return(rep(NA_integer_, n_sim))
  }
  usar_pesos <- ponderar && !is.null(pesos_pool) && sum(pesos_pool, na.rm = TRUE) > 0
  if (usar_pesos) {
    sample(radios_pool, size = n_sim, replace = TRUE, prob = pesos_pool)
  } else {
    sample(radios_pool, size = n_sim, replace = TRUE)
  }
}

individuos_pool <- data_revisada |>
  select(id_individuo, comuna_norm) |>
  left_join(pool_por_comuna, by = c("comuna_norm" = "nom_depto"))

n_sin_pool <- sum(is.na(individuos_pool$n_radios_pool))
if (n_sin_pool > 0) {
  warning(sprintf(
    "%d individuos con comuna sin ningún radio con tiempo válido — quedan en NA en las %d simulaciones.",
    n_sin_pool, N_SIM
  ))
}

# ADVERTENCIA DE TAMAÑO: esto genera n_individuos x N_SIM filas.
# Con, por ejemplo, 5.000 individuos x 1000 corridas = 5.000.000 de filas.
simulacion_larga <- individuos_pool |>
  mutate(sim = map2(radios_pool, pesos, ~ sortear_radios(.x, .y, N_SIM, PONDERAR_POR_POBLACION))) |>
  select(id_individuo, comuna_norm, sim) |>
  unnest_longer(sim, values_to = "id_radio_sim", indices_to = "iteracion")

cat(sprintf("   simulacion_larga: %d filas (%d individuos x %d iteraciones)\n",
            nrow(simulacion_larga), n_distinct(simulacion_larga$id_individuo), N_SIM))

# -----------------------------------------------------------------------------
# 4.2) Pegar variables de individuo (data_revisada) y de radio (base_radios1)
# -----------------------------------------------------------------------------
# Dos joins por claves distintas:
#   - id_individuo   -> variables originales del individuo (data_revisada)
#   - id_radio_sim   -> variables del radio sorteado EN ESA iteración
#                       (NBI/censo, tiempo al CeSAC, y cualquier otra columna
#                       de base_radios1 — no elijo a mano las de NBI porque no
#                       tengo certeza de sus nombres exactos en tu
#                       radio_censal_unido.csv más allá de las que aparecen
#                       en tu script 05; traigo todo y filtrás después
#                       las que no necesites con select()).

cat("── 4.2 Pegando variables de individuo y de radio (NBI/censo)...\n")

# Variables del individuo: todo data_revisada, salvo las columnas que
# generamos nosotros mismos en el punto 1 (para no duplicar).
data_revisada_covariables <- data_revisada |>
  select(-comuna_num, -comuna_norm)

# Variables del radio: todo base_radios1, salvo nom_depto (ya tenemos
# comuna_norm del lado del individuo — sería redundante, y con otro formato).
base_radios1_para_join <- base_radios1 |>
  select(-nom_depto)

simulacion_completa <- simulacion_larga |>
  left_join(data_revisada_covariables, by = "id_individuo") |>
  left_join(base_radios1_para_join, by = c("id_radio_sim" = "codigo_redatam"))

cat(sprintf("   simulacion_completa: %d filas x %d columnas\n",
            nrow(simulacion_completa), ncol(simulacion_completa)))

n_sin_radio_sim <- sum(is.na(simulacion_completa$id_radio_sim))
cat(sprintf("   Filas sin radio sorteado (comuna sin pool válido): %d de %d\n",
            n_sin_radio_sim, nrow(simulacion_completa)))






















# -----------------------------------------------------------------------------
# 5) Resumen por individuo (media/DE/mediana entre las N_SIM corridas)
# -----------------------------------------------------------------------------

cat("── 5. Resumiendo por individuo...\n")

simulacion_resumen <- simulacion_completa |>
  group_by(id_individuo, comuna_norm) |>
  summarise(
    tiempo_transp_prom_sim = round(mean(tiempo_transp_min_cesac, na.rm = TRUE), 2),
    tiempo_transp_sd_sim   = round(sd(tiempo_transp_min_cesac,   na.rm = TRUE), 2),
    tiempo_transp_p50_sim  = round(median(tiempo_transp_min_cesac, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n   Vista previa — simulacion_resumen:\n")
print(head(simulacion_resumen))

# -----------------------------------------------------------------------------
# 6) Exportar
# -----------------------------------------------------------------------------
# simulacion_larga se guarda en .rds (compacto, preserva tipos) — un .csv de
# varios millones de filas es innecesariamente pesado si no lo vas a abrir
# fuera de R.

cat("── 6. Exportando...\n")

saveRDS(simulacion_completa,  "simulacion_completa_tiempo_cesac.rds")
saveRDS(simulacion_resumen,   "simulacion_resumen_tiempo_cesac.rds")
write_csv(simulacion_resumen, "simulacion_resumen_tiempo_cesac.csv")

cat("✓ Archivos exportados:\n")
cat("   simulacion_completa_tiempo_cesac.rds     — 1 fila por individuo x iteración, con NBI/censo del radio sorteado\n")
cat("   simulacion_resumen_tiempo_cesac.rds/.csv — 1 fila por individuo, resumen de las 1000 corridas\n")