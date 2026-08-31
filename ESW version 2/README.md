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
