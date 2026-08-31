# Datos

Esta carpeta no se sube a GitHub (ver `.gitignore` en la raíz del proyecto):
pesa mucho y parte de los datos son sensibles (nivel paciente / radio censal).

- `raw/`: datos de origen sin modificar (censo 2022, radios censales
  INDEC/CONICET, red de efectores de salud, barrios populares, límites de
  comuna).
- `processed/`: bases cruzadas y consolidadas, resultado de correr los
  scripts de `scripts/R` y `scripts/Stata` sobre `raw/` (incluye
  `df_esw.csv` / `df_esw.dta`, la base principal del análisis).

Si necesitás las bases para reproducir el análisis, pedilas aparte
(Drive/OneDrive/Teams) — no están en el repositorio de GitHub.
