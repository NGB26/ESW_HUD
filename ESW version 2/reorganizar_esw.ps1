# ================================================================
#  Reorganizacion de "ESW version 2" (ESW HUD-SPH - CABA / Boti)
#  Generado por Claude - revisa el script antes de correrlo
# ================================================================
#  Que hace:
#   1) Crea la nueva estructura de carpetas (scripts/, data/, outputs/, docs/, _archivo_a_revisar/)
#   2) MUEVE los archivos a su lugar (no copia duplicados, no borra nada que no sea una
#      carpeta que quedo vacia despues de mover su contenido)
#   3) Renombra los scripts de R del pipeline principal con prefijo numerico (01_, 02_, ...)
#   4) Escribe README.md, .gitignore y data/README.md
#
#  Nada del contenido de tus archivos se borra. Los duplicados y archivos de origen
#  dudoso se mueven a _archivo_a_revisar/ para que los mires y decidas si los borras.
#
#  Como correrlo:
#   1) Abri PowerShell
#   2) Navega a esta carpeta (o simplemente hace doble clic si tenes .ps1 habilitado)
#   3) .\reorganizar_esw.ps1
#      (si PowerShell bloquea el script: Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass)
# ================================================================

$ErrorActionPreference = "Continue"

$Root = "C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\Documents\IDB\ESW HUD-SPH\Paper versión final - peer rev\ESW_HUD\ESW version 2"

if (-not (Test-Path -LiteralPath $Root)) {
    Write-Error "No encuentro la carpeta: $Root`nEdita la variable `$Root al principio del script y volve a correrlo."
    exit 1
}

Set-Location -LiteralPath $Root

function New-Dir($path) {
    $full = Join-Path $Root $path
    if (-not (Test-Path -LiteralPath $full)) {
        New-Item -ItemType Directory -Path $full -Force | Out-Null
    }
}

function Move-Safe($from, $toDir) {
    $src = Join-Path $Root $from
    $dstDir = Join-Path $Root $toDir
    if (-not (Test-Path -LiteralPath $src)) {
        Write-Warning "No encontrado (salteado): $from"
        return
    }
    if (-not (Test-Path -LiteralPath $dstDir)) {
        New-Item -ItemType Directory -Path $dstDir -Force | Out-Null
    }
    $leaf = Split-Path $src -Leaf
    $destPath = Join-Path $dstDir $leaf
    if (Test-Path -LiteralPath $destPath) {
        Write-Warning "Ya existe en destino (salteado): $destPath"
        return
    }
    try {
        Move-Item -LiteralPath $src -Destination $dstDir -ErrorAction Stop
        Write-Host "OK: $from -> $toDir"
    } catch {
        Write-Warning "Error moviendo '$from': $_"
    }
}

# --- 1) Estructura de carpetas -----------------------------------------
$dirs = @(
    "scripts\R",
    "scripts\Stata",
    "data\raw\censo_2022",
    "data\raw\radios_censales_2022\shapefile",
    "data\raw\geo",
    "data\raw\barrios_populares",
    "data\raw\salud",
    "data\raw\_descargas_originales",
    "data\processed",
    "outputs\figures\mapas",
    "outputs\tables",
    "outputs\logs",
    "docs",
    "_archivo_a_revisar\tmp_root_censo22_duplicados",
    "_archivo_a_revisar\duplicados_root_vs_dataframes_consolidados",
    "_archivo_a_revisar\duplicado_radios_censales_root",
    "_archivo_a_revisar\duplicado_df_esw2_en_Scripts",
    "_archivo_a_revisar\posible_otro_proyecto"
)
foreach ($d in $dirs) { New-Dir $d }

# --- 2) Scripts R --------------------------------------------------------
Move-Safe "Scripts\preparar_datos.R" "scripts\R"
Move-Safe "Scripts\unir_datos.R" "scripts\R"
Move-Safe "Scripts\unir_comuna.R" "scripts\R"
Move-Safe "Scripts\unir_radio_censal.R" "scripts\R"
Move-Safe "Scripts\radios_en_barrios_populares.R" "scripts\R"
Move-Safe "Scripts\analisis_centros_salud_comunas.R" "scripts\R"
Move-Safe "Scripts\analisis_centros_salud_barrios_populares.R" "scripts\R"
Move-Safe "Scripts\analisis de datos.R" "scripts\R"
Move-Safe "Scripts\mainscript.R" "scripts\R"
Move-Safe "Scripts\mainscript corregido.R" "scripts\R"

# Prefijo numerico = orden sugerido del pipeline (basado en el nombre de cada script)
$renames = [ordered]@{
    "scripts\R\preparar_datos.R"                            = "01_preparar_datos.R"
    "scripts\R\unir_datos.R"                                = "02_unir_datos.R"
    "scripts\R\unir_comuna.R"                               = "03_unir_comuna.R"
    "scripts\R\unir_radio_censal.R"                         = "04_unir_radio_censal.R"
    "scripts\R\radios_en_barrios_populares.R"               = "05_radios_en_barrios_populares.R"
    "scripts\R\analisis_centros_salud_comunas.R"            = "06_analisis_centros_salud_comunas.R"
    "scripts\R\analisis_centros_salud_barrios_populares.R"  = "07_analisis_centros_salud_barrios_populares.R"
    "scripts\R\analisis de datos.R"                         = "08_analisis_de_datos.R"
    "scripts\R\mainscript corregido.R"                      = "mainscript_corregido.R"
}
foreach ($k in $renames.Keys) {
    $p = Join-Path $Root $k
    if (Test-Path -LiteralPath $p) {
        Rename-Item -LiteralPath $p -NewName $renames[$k] -Force
    }
}

# --- 3) Scripts Stata ------------------------------------------------------
Move-Safe "Scripts\mainscript_v2.do" "scripts\Stata"
Move-Safe "Scripts\main_dofile_results.do" "scripts\Stata"
Move-Safe "Scripts\density.do" "scripts\Stata"
Move-Safe "Scripts\generar_pct_cpe_c.do" "scripts\Stata"
Move-Safe "Scripts\heatmap_correlaciones.do" "scripts\Stata"
Move-Safe "Scripts\labels_variables.do" "scripts\Stata"

# --- 4) Salidas/logs que estaban sueltos en Scripts ------------------------
Move-Safe "Scripts\ef_het.doc" "outputs\logs"
Move-Safe "Scripts\ef_het.txt" "outputs\logs"
Move-Safe "Scripts\esw_v.doc" "outputs\logs"
Move-Safe "Scripts\esw_v.txt" "outputs\logs"

# --- 5) Base de datos que estaba mezclada dentro de Scripts (duplicada) ----
Move-Safe "Scripts\df_esw2.dta" "_archivo_a_revisar\duplicado_df_esw2_en_Scripts"

# Si "Scripts" quedo vacia, se borra el cascaron vacio
if (Test-Path -LiteralPath (Join-Path $Root "Scripts")) {
    if ((Get-ChildItem -LiteralPath (Join-Path $Root "Scripts") -Recurse -File | Measure-Object).Count -eq 0) {
        Remove-Item -LiteralPath (Join-Path $Root "Scripts") -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# --- 6) Censo 2022 (raw) ----------------------------------------------------
if (Test-Path -LiteralPath (Join-Path $Root "files_censo22")) {
    Get-ChildItem -LiteralPath (Join-Path $Root "files_censo22") -File | ForEach-Object {
        Move-Item -LiteralPath $_.FullName -Destination (Join-Path $Root "data\raw\censo_2022") -ErrorAction SilentlyContinue
    }
    if ((Get-ChildItem -LiteralPath (Join-Path $Root "files_censo22") -Recurse -File | Measure-Object).Count -eq 0) {
        Remove-Item -LiteralPath (Join-Path $Root "files_censo22") -Recurse -Force -ErrorAction SilentlyContinue
    }
}
Move-Safe "CPV2022-par.xlsx" "data\raw\censo_2022"

# --- 7) Radios censales 2022 (raw, geo) -------------------------------------
Move-Safe "Conicet digital 11336_238198\Metodologia_radios_2022_v1_0.docx" "data\raw\radios_censales_2022"
Move-Safe "Conicet digital 11336_238198\RADIOS_2022_v1_0.rar" "data\raw\radios_censales_2022"
if (Test-Path -LiteralPath (Join-Path $Root "Conicet digital 11336_238198\RADIOS_2022_v1_0")) {
    Get-ChildItem -LiteralPath (Join-Path $Root "Conicet digital 11336_238198\RADIOS_2022_v1_0") -File | ForEach-Object {
        Move-Item -LiteralPath $_.FullName -Destination (Join-Path $Root "data\raw\radios_censales_2022\shapefile") -ErrorAction SilentlyContinue
    }
}
Get-ChildItem -LiteralPath $Root -Directory -Filter "Conicet digital*" -ErrorAction SilentlyContinue | ForEach-Object {
    if ((Get-ChildItem -LiteralPath $_.FullName -Recurse -File | Measure-Object).Count -eq 0) {
        Remove-Item -LiteralPath $_.FullName -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# Duplicado del shapefile de radios censales que estaba suelto en la raiz
Move-Safe "radios2022_v1_0.dbf" "_archivo_a_revisar\duplicado_radios_censales_root"
Move-Safe "radios2022_v1_0.prj" "_archivo_a_revisar\duplicado_radios_censales_root"
Move-Safe "radios2022_v1_0.shp" "_archivo_a_revisar\duplicado_radios_censales_root"
Move-Safe "radios2022_v1_0.shx" "_archivo_a_revisar\duplicado_radios_censales_root"

# --- 8) Otros datos raw sueltos en la raiz -----------------------------------
Move-Safe "comunas.csv" "data\raw\geo"
Move-Safe "barrios_populares_poligono.csv" "data\raw\barrios_populares"
Move-Safe "barrios_populares_poligono.zip" "data\raw\barrios_populares"
Move-Safe "centros_medicos_barriales.csv" "data\raw\salud"
Move-Safe "centros_salud_nivel_1_cesac.csv" "data\raw\salud"
Move-Safe "estaciones_saludables.csv" "data\raw\salud"
Move-Safe "hospitales.csv" "data\raw\salud"
Move-Safe "files.zip" "data\raw\_descargas_originales"

# --- 9) "Data frames consolidados" -> data\processed (+ salidas mezcladas) --
$dfc = "Data frames consolidados"
Move-Safe "$dfc\analisis_completo_barrios.csv" "data\processed"
Move-Safe "$dfc\analisis_completo_comunas.csv" "data\processed"
Move-Safe "$dfc\barrios_populares_normalizados.xlsx" "data\processed"
Move-Safe "$dfc\centros_por_barrio.csv" "data\processed"
Move-Safe "$dfc\centros_por_comuna.csv" "data\processed"
Move-Safe "$dfc\comuna_unido.csv" "data\processed"
Move-Safe "$dfc\comuna_unido.xlsx" "data\processed"
Move-Safe "$dfc\data_revisada_ok.csv" "data\processed"
Move-Safe "$dfc\data_revisada_ok.xlsx" "data\processed"
Move-Safe "$dfc\dataset_unificado.csv" "data\processed"
Move-Safe "$dfc\densidad_barrios_opA.csv" "data\processed"
Move-Safe "$dfc\densidad_barrios_opB.csv" "data\processed"
Move-Safe "$dfc\densidad_por_tipo.csv" "data\processed"
Move-Safe "$dfc\df_esw.csv" "data\processed"
Move-Safe "$dfc\df_esw.dta" "data\processed"
Move-Safe "$dfc\df_esw2.dta" "data\processed"
Move-Safe "$dfc\distancias_tiempos_barrios.csv" "data\processed"
Move-Safe "$dfc\distancias_tiempos_ors_barrios.csv" "data\processed"
Move-Safe "$dfc\distancias_tiempos_ors.csv" "data\processed"
Move-Safe "$dfc\distancias_tiempos_por_tipo.csv" "data\processed"
Move-Safe "$dfc\efectores_unificados.csv" "data\processed"
Move-Safe "$dfc\efectores_unificados.gpkg" "data\processed"
Move-Safe "$dfc\indicadores_por_barrio.csv" "data\processed"
Move-Safe "$dfc\intersecciones_radio_barrio.csv" "data\processed"
Move-Safe "$dfc\radio_censal_unido.csv" "data\processed"
Move-Safe "$dfc\radio_censal_unido.xlsx" "data\processed"
Move-Safe "$dfc\radios_barrios.gpkg" "data\processed"

Move-Safe "$dfc\tabla_descriptiva.docx" "outputs\tables"
Move-Safe "$dfc\tabla_descriptiva.html" "outputs\tables"
Move-Safe "$dfc\mapa_dias_ultima_consulta.html" "outputs\figures\mapas"
Move-Safe "$dfc\mapa_dias_ultima_consulta_files" "outputs\figures\mapas"

# Duplicados de la raiz que ya existian (probablemente mas viejos) en "Data frames consolidados"
Move-Safe "analisis_completo_comunas.csv" "_archivo_a_revisar\duplicados_root_vs_dataframes_consolidados"
Move-Safe "centros_por_comuna.csv" "_archivo_a_revisar\duplicados_root_vs_dataframes_consolidados"
Move-Safe "densidad_por_tipo.csv" "_archivo_a_revisar\duplicados_root_vs_dataframes_consolidados"
Move-Safe "distancias_tiempos_ors.csv" "_archivo_a_revisar\duplicados_root_vs_dataframes_consolidados"
Move-Safe "distancias_tiempos_por_tipo.csv" "_archivo_a_revisar\duplicados_root_vs_dataframes_consolidados"
Move-Safe "efectores_unificados.csv" "_archivo_a_revisar\duplicados_root_vs_dataframes_consolidados"
Move-Safe "efectores_unificados.gpkg" "_archivo_a_revisar\duplicados_root_vs_dataframes_consolidados"

if (Test-Path -LiteralPath (Join-Path $Root $dfc)) {
    if ((Get-ChildItem -LiteralPath (Join-Path $Root $dfc) -Recurse -File | Measure-Object).Count -eq 0) {
        Remove-Item -LiteralPath (Join-Path $Root $dfc) -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# --- 10) Figuras, tablas y notas tecnicas -------------------------------------
Move-Safe "heatmap_correlaciones.png" "outputs\figures"
Move-Safe "heatmap_correlaciones_matched.png" "outputs\figures"
Move-Safe "heatmap_correlaciones_spearman.png" "outputs\figures"
Move-Safe "tablas experimento.xlsx" "outputs\tables"
Move-Safe "Nota técnica - HUD-HNP Draft.pdf" "docs"
Move-Safe "Nota técnica - metodologia y resultado - draft- NGB .pdf" "docs"
Move-Safe "Nota técnica.docx" "docs"

# --- 11) Duplicados/temporales de la raiz (xlsx sueltos del censo) -----------
Move-Safe "_tmp_12022801 (1).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_12022801 (2).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228101 (1).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228101.xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228121 (1).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228121.xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228141.xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228161.xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_12022821 (1).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_12022841 (1).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_12022841 (2).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228441.xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228461.xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228481.xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_120228531.xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_12022861 (1).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"
Move-Safe "_tmp_12022881 (1).xlsX" "_archivo_a_revisar\tmp_root_censo22_duplicados"

# --- 12) Posible script de otro proyecto --------------------------------------
Move-Safe "script neuquen.R" "_archivo_a_revisar\posible_otro_proyecto"

# --- 13) README.md -------------------------------------------------------------
$readme = @'
# ESW HUD-SPH — CABA (Boti / Hipertensión)

Análisis de una intervención de salud digital en CABA: contacto a pacientes
hipertensos vía el chatbot del Gobierno de la Ciudad (Boti) para mejorar la
adherencia al tratamiento.

## Estructura del repositorio

```
scripts/
  R/          scripts en R. Prefijo numérico 01_.. 08_ = orden sugerido del pipeline
  Stata/      scripts .do de Stata
data/         bases de datos — NO se sube a GitHub (ver .gitignore)
  raw/        datos de origen sin modificar
  processed/  bases cruzadas/consolidadas, listas para análisis
outputs/
  figures/    gráficos y mapas
  tables/     tablas de resultados
  logs/       salidas de texto de Stata
docs/         notas técnicas y metodología
_archivo_a_revisar/   duplicados o archivos de origen incierto detectados
                       automáticamente — revisar a mano y borrar lo que no sirva
```

## Pipeline sugerido (R)

`01_preparar_datos.R` → `02_unir_datos.R` → `03_unir_comuna.R` →
`04_unir_radio_censal.R` → `05_radios_en_barrios_populares.R` →
`06_analisis_centros_salud_comunas.R` → `07_analisis_centros_salud_barrios_populares.R` →
`08_analisis_de_datos.R`

El orden es una inferencia a partir de los nombres de archivo — revisalo y
ajustalo si no corresponde.

`mainscript.R` y `mainscript_corregido.R` parecen ser dos versiones de un
script orquestador. **Confirmá cuál es la vigente** y movė la otra a
`_archivo_a_revisar/` (o borrala) para no tener ambigüedad en el repo.

## Stata

`mainscript_v2.do` y `main_dofile_results.do` parecen cumplir un rol similar
(script principal de resultados) — mismo pedido: confirmá cuál es el vigente.

## Datos

Las bases NO están versionadas en git: pesan mucho (varias superan el límite
de 100MB de GitHub, una supera los 400MB) y algunas contienen información a
nivel de paciente / radio censal. Quedan organizadas localmente en
`data/raw/` y `data/processed/`. Ver `data/README.md`.

Vas a incorporar datos nuevos a nivel de radio censal: `data/raw/radios_censales_2022/`
ya tiene el shapefile de radios censales 2022 (INDEC/CONICET) con su
metodología. Sumá ahí los datos nuevos, o creá una carpeta
`data/raw/radios_censales_<fuente_o_año>/` si es una fuente distinta.

## Pendiente / a revisar

- `_archivo_a_revisar/`: duplicados detectados automáticamente — versiones
  viejas en la raíz de archivos que ya estaban en "Data frames consolidados",
  el shapefile de radios censales duplicado, xlsx temporales del censo, un
  `df_esw2.dta` que estaba mezclado dentro de `Scripts/`, y `script neuquen.R`
  que no parece pertenecer a este proyecto (revisar si es de otra intervención).
- Los scripts probablemente usan rutas (`setwd`, rutas relativas o absolutas)
  apuntando a la vieja ubicación de los datos. Al separar `data/` de
  `scripts/` esas rutas se rompen y hay que actualizarlas — recomendado usar
  rutas relativas desde la raíz del proyecto (`here::here()` en R, o definir
  una macro/global con la raíz al principio del .do file en Stata).
- `.RData`, `.RDataTmp` y `.Rhistory` quedaron en la raíz del proyecto (para
  que RStudio los siga usando con normalidad) pero están excluidos vía
  `.gitignore`.
'@
Set-Content -LiteralPath (Join-Path $Root "README.md") -Value $readme -Encoding UTF8

# --- 14) .gitignore --------------------------------------------------------------
$gitignore = @'
# Bases de datos: no se versionan (tamaño + datos sensibles a nivel de paciente/radio censal)
/data/*
!/data/README.md

# Carpeta de revision manual (duplicados/temporales) - no debe subirse
/_archivo_a_revisar/

# Sesion de R
.RData
.RDataTmp
.Rhistory
.Rproj.user/

# Archivos temporales de Office
~$*.docx
~$*.xlsx
~$*.pptx

# Sistema
.DS_Store
Thumbs.db
desktop.ini
'@
Set-Content -LiteralPath (Join-Path $Root ".gitignore") -Value $gitignore -Encoding UTF8

# --- 15) data/README.md -----------------------------------------------------------
$dataReadme = @'
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
'@
New-Dir "data"
Set-Content -LiteralPath (Join-Path $Root "data\README.md") -Value $dataReadme -Encoding UTF8

Write-Host ""
Write-Host "================================================================"
Write-Host " Listo. Revisa _archivo_a_revisar\ antes de borrar nada a mano."
Write-Host " README.md, .gitignore y data\README.md fueron creados/actualizados."
Write-Host "================================================================"
