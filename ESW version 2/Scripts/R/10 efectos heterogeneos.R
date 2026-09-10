# =============================================================================
# Script: 10_efectos_heterogeneos.R
# Descripción: Regresiones adicionales (Het 1, Het 2) y tabla de efectos
#              heterogéneos de villa por género.
#
# REQUIERE correr 09_simulacion_de_asignacion_individuo_radios_censales_v3.R
# COMPLETO en la MISMA sesión de R antes de este script. Reutiliza objetos y
# funciones ya creados ahí: base_unida, datos_por_iteracion, ejecutar_todas(),
# calcular_resumen(), calcular_modelo_stats(), armar_tabla_flextable(),
# etiquetar_termino(), orden_variables, nota_rubin, N_SIM.
#
# SUPUESTO A VALIDAR: "log(tiempo_consulta)" del pedido se interpreta como
# log(dias_ult_cons) — el outcome usado en toda la Parte V del script 9
# ("días desde la última consulta"). No existe ninguna variable llamada
# tiempo_consulta en la base. Si el outcome real es otro (p. ej. duración de
# la consulta en sí), cambiar OUTCOME_HET más abajo — es el único lugar que
# hay que tocar.
#
# DIFERENCIA METODOLÓGICA DELIBERADA vs. reg1–reg7 del script 9: acá el FE es
# factor(comuna_ok) (comuna GEOGRÁFICA real de cada individuo, villa incluida)
# en vez de factor(comuna_regresion) (que fuerza a todas las villas a compartir
# la categoría "BP"). Con comuna_ok, el contraste villa/no-villa se identifica
# DENTRO de cada comuna en vez de ENTRE comunas — más exigente y más
# conservador (Wooldridge 2002, Cap. 14, sobre qué variación identifica el
# efecto en un modelo de efectos fijos). Consecuencia: villa con comuna_ok NA
# se cae de estas regresiones, algo que NUNCA pasaba en reg1–reg7 (ahí villa
# siempre iba a "BP", nunca a NA). Se cuantifica en el diagnóstico de abajo
# antes de correr nada, para que la caída de N no sea una sorpresa.
# =============================================================================

OUTCOME_HET <- "dias_ult_cons"

cat("═══════════════════════════════════════════════════════════════════\n")
cat(" BLOQUE ADICIONAL — EFECTOS HETEROGÉNEOS (requiere 09_....R ya corrido)\n")
cat("═══════════════════════════════════════════════════════════════════\n")

stopifnot(
  "Correr primero 09_simulacion_de_asignacion_individuo_radios_censales_v3.R" =
    exists("base_unida") && exists("datos_por_iteracion") &&
    exists("ejecutar_todas") && exists("calcular_resumen") &&
    exists("calcular_modelo_stats") && exists("armar_tabla_flextable") &&
    exists("etiquetar_termino") && exists("orden_variables")
)


# =============================================================================
# A. VARIABLE UNIFICADA: DISTANCIA EUCLIDIANA AL CeSAC (dist_eucl_km_cesac_t)
# =============================================================================
# Necesaria para Reg 2 y todavía no existe en base_unida (sí existen sus dos
# componentes crudos, dist_eucl_km_cesac_radio / _barrio, generados por el
# join de II.4 del script 9). Se unifica con EXACTAMENTE el mismo criterio y
# el mismo fallback que tiempo_transporte_cesac_t en III.6 del script 9:
#   - Villa con id_barrio matcheado y dato de barrio no-NA -> valor de barrio.
#   - Villa con id_barrio matcheado pero dato de barrio NA en la fuente ->
#     fallback al radio sorteado del individuo (mismo criterio que tiempo;
#     ver salvedad metodológica ya documentada en 09_...R, III.6: el radio
#     sale del pool general de la comuna, no está restringido a la villa).
#   - No-villa -> radio sorteado, siempre.
#   - Villa sin id_barrio -> NA (sin fallback posible, igual que tiempo).
# Se agrega a base_unida (extiende la tabla, no rompe nada de lo ya calculado
# en el script 9) y se re-parte datos_por_iteracion para que la nueva columna
# viaje en cada slice de iteración.

cat("\n── A. Unificando dist_eucl_km_cesac_t (mismo criterio que tiempo)...\n")

stopifnot(
  "Faltan dist_eucl_km_cesac_radio/_barrio en base_unida — revisar join II.4 del script 9" =
    all(c("dist_eucl_km_cesac_radio", "dist_eucl_km_cesac_barrio") %in% names(base_unida))
)

base_unida <- base_unida |>
  mutate(
    dist_eucl_km_cesac_t = case_when(
      !is.na(asentamiento) & !is.na(id_barrio) & !is.na(dist_eucl_km_cesac_barrio) ~
        dist_eucl_km_cesac_barrio,
      !is.na(asentamiento) & !is.na(id_barrio) & is.na(dist_eucl_km_cesac_barrio)  ~
        dist_eucl_km_cesac_radio,                                  # fallback (igual que tiempo)
      is.na(asentamiento) ~
        dist_eucl_km_cesac_radio,
      TRUE ~ NA_real_   # villa sin id_barrio
    ),
    dist_imputada_por_radio = !is.na(asentamiento) & !is.na(id_barrio) &
      is.na(dist_eucl_km_cesac_barrio)
  )

n_dist_na       <- sum(is.na(base_unida$dist_eucl_km_cesac_t))
n_dist_imputada <- sum(base_unida$dist_imputada_por_radio, na.rm = TRUE)
cat(sprintf("   NA en dist_eucl_km_cesac_t: %d (%.1f%%) | imputadas por fallback a radio: %d\n",
            n_dist_na, 100 * n_dist_na / nrow(base_unida), n_dist_imputada))

# Re-partir por iteración con la nueva columna ya incorporada
datos_por_iteracion <- split(base_unida, base_unida$iteracion)


# =============================================================================
# B. DIAGNÓSTICO PREVIO: comuna_ok vs comuna_regresion (cambio de FE)
# =============================================================================

cat("\n── B. Diagnóstico: impacto de usar comuna_ok como FE (vs comuna_regresion) ──\n")
base_unida |>
  filter(iteracion == 1) |>
  summarise(
    n_total                   = n(),
    n_villa                   = sum(villa == 1),
    n_villa_sin_comuna_ok     = sum(villa == 1 & is.na(comuna_ok)),
    n_no_villa_sin_comuna_ok  = sum(villa == 0 & is.na(comuna_ok)),
    pct_villa_perdido         = round(100 * n_villa_sin_comuna_ok / n_villa, 1)
  ) |>
  print()
cat("   Estas filas se excluyen SOLO en este bloque (Het 1 / Het 2), no en reg1–reg7.\n")


# =============================================================================
# C. ESPECIFICACIONES — HET 1, HET 2 y HET 3
# =============================================================================
#
# Het 1: log(dias_ult_cons) ~ edad_grupo + villa*género + log(tiempo) + NBI +
#        hacinamiento + FE comuna_ok
#   "villa + villa*sexo + sexo" en la fórmula de R se escribe de forma
#   compacta como factor(villa)*factor(genero): el operador `*` ya expande
#   automáticamente a villa + genero + villa:genero (los tres términos
#   pedidos) sin necesidad de listarlos por separado.
#
# Het 2: log(dias_ult_cons) ~ edad_grupo + género + villa + distancia + tiempo
#        + tiempo*distancia + tiempo*villa + villa*distancia +
#        distancia*tiempo*villa + NBI + hacinamiento + FE comuna_ok
#   Los 7 términos de villa/tiempo/distancia (3 principales + 3 interacciones
#   dobles + 1 triple) se obtienen con una sola expresión compacta:
#   log(tiempo_transporte_cesac_t) * dist_eucl_km_cesac_t * factor(villa)
#   — el operador `*` en R expande A*B*C a A+B+C+A:B+A:C+B:C+A:B:C, que es
#   EXACTAMENTE el conjunto de términos pedido.
#
# Het 3: log(dias_ult_cons) ~ edad_grupo + género + tiempo + villa*NBI +
#        hacinamiento + FE comuna_ok
#   Heterogeneidad SOCIOECONÓMICA (a diferencia de Het 1, demográfica, y
#   Het 2, geográfica): ¿la brecha villa/no-villa es mayor en radios/barrios
#   con más NBI? factor(villa)*pct_NBI_t expande a villa + NBI + villa:NBI.
#   A diferencia de Het 2, NBI entra en NIVEL (no en log) en todo el script
#   (reg1-reg7, Het1, Het2 ya la tratan así como control), así que el efecto
#   marginal de villa como función de NBI sale una RECTA, no una curva — no
#   hay logaritmo de por medio que la curve.

especificaciones_het <- list(
  
  het1 = list(
    label   = "Het 1: Villa \u00d7 G\u00e9nero",
    formula = paste0(
      "log(", OUTCOME_HET, ") ~ factor(edad_grupo) + factor(villa)*factor(genero) + ",
      "log(tiempo_transporte_cesac_t) + pct_NBI_t + pct_hacinamiento_t + factor(comuna_ok)"
    ),
    coefs = c(
      "factor(villa)1",
      "log(tiempo_transporte_cesac_t)",
      "pct_NBI_t", "pct_hacinamiento_t"
    ),
    extra_patterns = c(
      "^factor\\(edad_grupo\\)",
      "^factor\\(genero\\)",
      "^factor\\(villa\\)1:factor\\(genero\\)"   # término de interacción villa:género
    ),
    tiene_fe_comuna = TRUE
  ),
  
  het2 = list(
    label   = "Het 2: Villa \u00d7 Tiempo \u00d7 Distancia",
    formula = paste0(
      "log(", OUTCOME_HET, ") ~ factor(edad_grupo) + factor(genero) + ",
      "log(tiempo_transporte_cesac_t) * dist_eucl_km_cesac_t * factor(villa) + ",
      "pct_NBI_t + pct_hacinamiento_t + factor(comuna_ok)"
    ),
    coefs = c(
      "factor(villa)1",
      "log(tiempo_transporte_cesac_t)",
      "dist_eucl_km_cesac_t",
      "log(tiempo_transporte_cesac_t):dist_eucl_km_cesac_t",
      "log(tiempo_transporte_cesac_t):factor(villa)1",
      "dist_eucl_km_cesac_t:factor(villa)1",
      "log(tiempo_transporte_cesac_t):dist_eucl_km_cesac_t:factor(villa)1",
      "pct_NBI_t", "pct_hacinamiento_t"
    ),
    extra_patterns = c(
      "^factor\\(edad_grupo\\)",
      "^factor\\(genero\\)"
    ),
    tiene_fe_comuna = TRUE
  ),
  
  het3 = list(
    label   = "Het 3: Villa \u00d7 NBI",
    formula = paste0(
      "log(", OUTCOME_HET, ") ~ factor(edad_grupo) + factor(genero) + ",
      "log(tiempo_transporte_cesac_t) + factor(villa)*pct_NBI_t + ",
      "pct_hacinamiento_t + factor(comuna_ok)"
    ),
    coefs = c(
      "factor(villa)1",
      "log(tiempo_transporte_cesac_t)",
      "pct_NBI_t",
      "factor(villa)1:pct_NBI_t",
      "pct_hacinamiento_t"
    ),
    extra_patterns = c(
      "^factor\\(edad_grupo\\)",
      "^factor\\(genero\\)"
    ),
    tiene_fe_comuna = TRUE
  )
)

# --- Chequeo de nombres de término ANTES de correr 1000 iteraciones --------
# El orden exacto de los términos de interacción que produce R (p. ej. si
# aparece "tiempo:villa" o "villa:tiempo") depende del orden en que las
# variables aparecen en la fórmula. Se verifica contra una sola iteración
# antes de lanzar el loop completo — si algún nombre de `coefs` no matchea,
# esa fila va a salir vacía en la tabla final en vez de fallar silenciosamente,
# así que este chequeo es la forma barata de detectarlo temprano.

cat("\n── C. Verificando nombres de término (iteración 1, antes del loop completo) ──\n")
for (spec_id in names(especificaciones_het)) {
  spec <- especificaciones_het[[spec_id]]
  fit_chk <- feols(as.formula(spec$formula), data = datos_por_iteracion[[1]])
  terminos_reales <- names(coef(fit_chk))
  coefs_no_encontrados <- setdiff(spec$coefs, terminos_reales)
  cat(sprintf("   %s: %d/%d términos de `coefs` encontrados en el modelo real.\n",
              spec_id, length(spec$coefs) - length(coefs_no_encontrados), length(spec$coefs)))
  if (length(coefs_no_encontrados) > 0) {
    cat("     ⚠ NO encontrados (van a salir vacíos en la tabla):\n")
    print(coefs_no_encontrados)
    cat("     Términos reales del modelo (para corregir `coefs` a mano si hace falta):\n")
    print(terminos_reales)
  }
}


# =============================================================================
# D. CORRIENDO LAS 1000 ITERACIONES (FE comuna_ok, EE clusterizados por comuna_ok)
# =============================================================================

cat("\n── D. Corriendo Het 1 y Het 2 sobre las 1000 iteraciones...\n")
cat("     (FE = factor(comuna_ok), EE clusterizados por comuna_ok, según lo pedido)\n")

resultados_het   <- ejecutar_todas(especificaciones_het, datos_por_iteracion, vcov_type = "comuna_ok")
resumen_het      <- calcular_resumen(resultados_het)     # incluye SE de Rubin, FMI, etc.
modelo_stats_het <- calcular_modelo_stats(resultados_het)

cat("\n   N por especificación:\n")
print(modelo_stats_het)


# =============================================================================
# E. EXTENDER ETIQUETADO Y ORDEN DE VARIABLES PARA LOS TÉRMINOS NUEVOS
# =============================================================================
# etiquetar_termino() y orden_variables son GLOBALES, usados internamente por
# armar_tabla_flextable() (no son parámetros de la función). Se extienden acá
# de forma ADITIVA (nunca se borra ni se modifica ningún case_when existente),
# lo cual es seguro: las tablas del script 9 (ft_estandar, ft_cluster, etc.)
# ya están completamente materializadas en objetos y no se recalculan al
# reasignar esta función — extenderla ahora no las altera retroactivamente.

etiquetar_termino_base <- etiquetar_termino   # conservar la versión original por si hace falta

etiquetar_termino <- function(term) {
  case_when(
    term == "dist_eucl_km_cesac_t" ~
      "Distancia eucl. a CeSAC (km)",
    term == "log(tiempo_transporte_cesac_t):dist_eucl_km_cesac_t" ~
      "Tiempo (log) \u00d7 Distancia",
    term == "dist_eucl_km_cesac_t:factor(villa)1" ~
      "Distancia \u00d7 Villa",
    term == "log(tiempo_transporte_cesac_t):dist_eucl_km_cesac_t:factor(villa)1" ~
      "Tiempo (log) \u00d7 Distancia \u00d7 Villa",
    term == "factor(villa)1:pct_NBI_t" ~
      "Villa \u00d7 NBI (%)",
    str_detect(term, "^factor\\(villa\\)1:factor\\(genero\\)") ~
      paste0("Villa \u00d7 G\u00e9nero: ", str_remove(term, "^factor\\(villa\\)1:factor\\(genero\\)")),
    TRUE ~ etiquetar_termino_base(term)   # todo lo demás, igual que antes
  )
}

orden_variables <- c(
  orden_variables,
  "Distancia eucl. a CeSAC (km)",
  "Tiempo (log) \u00d7 Distancia",
  "Distancia \u00d7 Villa",
  "Tiempo (log) \u00d7 Distancia \u00d7 Villa",
  "Villa \u00d7 NBI (%)",
  "Villa \u00d7 G\u00e9nero: Masculino", "Villa \u00d7 G\u00e9nero: Femenino"
)


# =============================================================================
# F. TABLA 1 — RESULTADOS DE REGRESIÓN (Het 1 y Het 2 lado a lado)
# =============================================================================

nota_dist <- paste(
  sprintf("Para %d observaciones villa con barrio identificado pero sin distancia calculada a", n_dist_imputada),
  "nivel de barrio, dist_eucl_km_cesac_t se imputa con la distancia del radio censal sorteado",
  "en esa iteraci\u00f3n (mismo criterio que tiempo_imputado_por_radio, ver script 09)."
)

nota_fe_comuna_ok <- paste(
  "FE = factor(comuna_ok) (comuna GEOGR\u00c1FICA real, incluye villas) y EE clusterizados por",
  "comuna_ok — a diferencia de reg1-reg7 (FE/cluster por comuna_regresion, que agrupa TODAS",
  "las villas en una \u00fanica categor\u00eda \"BP\"). El contraste villa/no-villa aqu\u00ed se identifica",
  "DENTRO de cada comuna, no entre comunas (Wooldridge 2002, Cap. 14)."
)

ft_het <- armar_tabla_flextable(
  resumen_het, modelo_stats_het, especificaciones_het,
  nota_pie = paste(
    "Nota: coeficiente promedio entre iteraciones.", nota_rubin, nota_dist,
    "* p<0.10, ** p<0.05, *** p<0.01.", nota_fe_comuna_ok
  )
)

ft_het


# =============================================================================
# G. TABLA 2 — EFECTOS HETEROGÉNEOS DE VILLA POR GÉNERO (a partir de Het 1)
# =============================================================================
# El coeficiente villa:género de Het 1 (Tabla 1) YA es la prueba formal de si
# el efecto de villa difiere entre varones y mujeres — es la respuesta a "¿es
# significativa la heterogeneidad?". Lo que falta para una tabla "prolija" es
# el efecto de villa DENTRO de cada género por separado (no solo la
# diferencia). En vez de reconstruirlo a mano con el método delta (que exige
# extraer la covarianza villa~villa:género de cada una de las 1000 iteraciones
# del modelo), se usa el truco est\u00e1ndar de re-nivelar el factor de referencia
# y reestimar: correr Het 1 dos veces, una con género="Femenino" como
# referencia y otra con género="Masculino" como referencia. El coeficiente
# principal de factor(villa)1 en cada corrida ES DIRECTAMENTE el efecto de
# villa dentro de ese género, con su error estándar correcto sin necesidad de
# calcular a mano la varianza de una combinación lineal (Wooldridge 2002,
# Cap. 7, sobre interacciones y efectos por subgrupo vía releveling).

cat("\n── G. Efectos heterogéneos de villa por género (releveling + reestimación) ──\n")

especificaciones_het_genero <- list(
  villa_mujeres = list(
    label   = "Efecto villa en mujeres",
    formula = paste0(
      "log(", OUTCOME_HET, ") ~ factor(edad_grupo) + ",
      "factor(villa)*relevel(factor(genero), ref = 'Femenino') + ",
      "log(tiempo_transporte_cesac_t) + pct_NBI_t + pct_hacinamiento_t + factor(comuna_ok)"
    ),
    coefs           = c("factor(villa)1"),
    extra_patterns  = character(0),
    tiene_fe_comuna = TRUE
  ),
  villa_varones = list(
    label   = "Efecto villa en varones",
    formula = paste0(
      "log(", OUTCOME_HET, ") ~ factor(edad_grupo) + ",
      "factor(villa)*relevel(factor(genero), ref = 'Masculino') + ",
      "log(tiempo_transporte_cesac_t) + pct_NBI_t + pct_hacinamiento_t + factor(comuna_ok)"
    ),
    coefs           = c("factor(villa)1"),
    extra_patterns  = character(0),
    tiene_fe_comuna = TRUE
  )
)

# Chequeo de niveles reales de género antes de asumir "Femenino"/"Masculino"
niveles_genero <- sort(unique(na.omit(base_unida$genero)))
cat("   Niveles de `genero` encontrados en los datos:\n")
print(niveles_genero)
if (!all(c("Femenino", "Masculino") %in% niveles_genero)) {
  stop("Los niveles de 'genero' no son exactamente 'Femenino'/'Masculino' — ",
       "ajustar el argumento ref = ... de relevel() en especificaciones_het_genero arriba.")
}

resultados_het_genero <- ejecutar_todas(especificaciones_het_genero, datos_por_iteracion,
                                        vcov_type = "comuna_ok")
resumen_het_genero    <- calcular_resumen(resultados_het_genero)

# --- Armar la tabla de efectos heterogéneos a mano (formato distinto: filas =
# subgrupo, no columnas = especificación, así que no reusa armar_tabla_flextable)

tabla_heterogeneos <- resumen_het_genero |>
  transmute(
    Subgrupo    = if_else(regresion_id == "villa_mujeres", "Mujeres", "Varones"),
    `Efecto villa (coef.)` = round(media_coef_raw, 3),
    `EE (Rubin)`           = round(se_rubin, 3),
    `IC 95%`               = sprintf("[%.3f, %.3f]", ic_low_r, ic_high_r),
    `p-valor`               = round(p_rubin, 3),
    Signif = case_when(
      p_rubin < 0.01 ~ "***",
      p_rubin < 0.05 ~ "**",
      p_rubin < 0.10 ~ "*",
      TRUE ~ ""
    )
  ) |>
  arrange(Subgrupo)   # orden alfabético: "Mujeres" < "Varones" -> Mujeres primero

# Fila de diferencia: el coeficiente villa:género de Het 1 (Tabla 1) ES la
# diferencia formal entre estos dos efectos — se trae directamente de ahí en
# vez de recalcularla, para que la tabla sea internamente consistente con la
# Tabla 1 (mismo número, dos presentaciones).
fila_diferencia <- resumen_het |>
  filter(regresion_id == "het1", str_detect(term, "^factor\\(villa\\)1:factor\\(genero\\)")) |>
  transmute(
    Subgrupo               = "Diferencia (villa \u00d7 g\u00e9nero, de Tabla 1)",
    `Efecto villa (coef.)` = round(media_coef_raw, 3),
    `EE (Rubin)`           = round(se_rubin, 3),
    `IC 95%`               = sprintf("[%.3f, %.3f]", ic_low_r, ic_high_r),
    `p-valor`               = round(p_rubin, 3),
    Signif = case_when(
      p_rubin < 0.01 ~ "***",
      p_rubin < 0.05 ~ "**",
      p_rubin < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

tabla_heterogeneos <- bind_rows(tabla_heterogeneos, fila_diferencia)

cat("\n   ── Tabla de efectos heterogéneos (villa por género) ──\n")
print(as.data.frame(tabla_heterogeneos))

ft_heterogeneos <- flextable(tabla_heterogeneos) |>
  theme_booktabs() |>
  align(align = "center", part = "all") |>
  align(j = "Subgrupo", align = "left", part = "all") |>
  fontsize(size = 9, part = "body") |>
  bold(part = "header") |>
  bold(i = ~ str_detect(Subgrupo, "Diferencia"), part = "body") |>
  hline(i = 2, part = "body") |>   # separa Mujeres/Varones de la fila de Diferencia
  autofit() |>
  add_footer_lines(paste(
    "Nota: efecto de villa sobre log(", OUTCOME_HET, ") dentro de cada g\u00e9nero,",
    "obtenido re-nivelando la referencia de g\u00e9nero y reestimando Het 1 (Wooldridge 2002, Cap. 7).",
    "EE de Rubin (1987) e IC 95% con grados de libertad ajustados (Barnard-Rubin 1999).",
    "La fila 'Diferencia' es el coeficiente de la interacci\u00f3n villa\u00d7g\u00e9nero de la Tabla 1 (Het 1),",
    "reportado ac\u00e1 para lectura directa: si es significativo, el efecto de villa SÍ difiere por g\u00e9nero.",
    "FE = factor(comuna_ok), EE clusterizados por comuna_ok."
  )) |>
  fontsize(size = 7, part = "footer")

ft_heterogeneos


# =============================================================================
# H. EXPORTAR
# =============================================================================

save_as_docx(ft_het,          path = "tabla_regresiones_heterogeneidad.docx")
save_as_docx(ft_heterogeneos, path = "tabla_efectos_heterogeneos_villa_genero.docx")

cat("\n✓ Exportado:\n")
cat("   tabla_regresiones_heterogeneidad.docx\n")
cat("   tabla_efectos_heterogeneos_villa_genero.docx\n")

cat("\n── Nota para extender ──\n")
cat("   La Tabla 2 cubre villa×género. La Parte I cubre villa×tiempo (distancia\n")
cat("   fija en su media). La Parte J cubre villa×NBI. El espejo villa×distancia\n")
cat("   (tiempo fijo) es un swap directo de variables en el bloque de la Parte I\n")
cat("   si lo necesitás; lo mismo villa×hacinamiento calcando la Parte J.\n")


# =============================================================================
# I. GRÁFICO DE EFECTOS MARGINALES — VILLA × TIEMPO (estilo margins/marginsplot)
# =============================================================================
# Traduce a R lo que en Stata sería:
#   margins, dydx(villa) at(tiempo=(1(0.5)20) dist=r(mean)) level(90)
#   marginsplot
#
# Sobre Het 2 (interacción triple tiempo × distancia × villa), se grafica el
# efecto marginal de villa (villa: 0->1) sobre log(dias_ult_cons) COMO FUNCIÓN
# del tiempo a pie al CeSAC, manteniendo dist_eucl_km_cesac_t fija en su media
# muestral (análogo al comportamiento por defecto de `margins` cuando no se
# varía explícitamente un continuo).
#
# DERIVACIÓN: en Het 2, el efecto marginal de villa es una combinación lineal
# de 4 coeficientes:
#   ME_villa(tiempo, dist) = b_villa + b_(tiempo:villa)*log(tiempo)
#                            + b_(dist:villa)*dist + b_(tiempo:dist:villa)*log(tiempo)*dist
# Su varianza requiere la matriz de covarianza COMPLETA de esos 4 términos
# (método delta), no solo sus EE marginales — por eso este bloque REESTIMA
# Het 2 sobre las 1000 iteraciones de forma independiente (duplica el costo
# de cómputo de la Parte D, pero solo para Het 2), extrayendo vcov(fit) en
# cada una en vez de reusar ajustar_spec()/coeftable() (que solo devuelve la
# diagonal). La combinación entre iteraciones usa la MISMA regla de Rubin
# (1987) del resto del pipeline: Ubar (var. delta-method promedio dentro de
# cada iteración) + (1+1/m)*B (var. del punto estimado ENTRE iteraciones).
# El IC usa cuantiles normales (no t de Barnard-Rubin) por simplicidad: con
# m=1000 y los grados de libertad ya observados en la Tabla 8 (~1500-1800),
# la diferencia normal vs. t es despreciable en la práctica.

cat("\n── I. Gráfico de efectos marginales: Villa × Tiempo (Het 2) ──\n")

TERMINOS_ME <- c(
  "factor(villa)1",
  "log(tiempo_transporte_cesac_t):factor(villa)1",
  "dist_eucl_km_cesac_t:factor(villa)1",
  "log(tiempo_transporte_cesac_t):dist_eucl_km_cesac_t:factor(villa)1"
)

# --- Grid de tiempo y valor de referencia de distancia ---------------------
# Grid derivado de los datos (percentiles 2-98 de tiempo_transporte_cesac_t),
# no fijo, para no extrapolar más allá del soporte empírico. log(tiempo)
# exige tiempo > 0, así que el piso del grid nunca baja de 1 minuto.

rango_tiempo <- base_unida |>
  filter(!is.na(tiempo_transporte_cesac_t), tiempo_transporte_cesac_t > 0) |>
  summarise(
    p_low  = quantile(tiempo_transporte_cesac_t, 0.02, na.rm = TRUE),
    p_high = quantile(tiempo_transporte_cesac_t, 0.98, na.rm = TRUE)
  )

grid_tiempo <- seq(max(1, floor(rango_tiempo$p_low)), ceiling(rango_tiempo$p_high), by = 0.5)
dist_ref    <- mean(base_unida$dist_eucl_km_cesac_t, na.rm = TRUE)

cat(sprintf("   Grid de tiempo: %.1f a %.1f minutos (percentiles 2-98 de la muestra).\n",
            min(grid_tiempo), max(grid_tiempo)))
cat(sprintf("   Distancia de referencia (media muestral): %.2f km.\n", dist_ref))

# --- Loop Monte Carlo: efecto marginal + varianza delta-method por iteración

calcular_me_una_iteracion <- function(data_iter, formula_str, grid_t, dist0, vcov_type = "comuna_ok") {
  fit <- feols(as.formula(formula_str), data = data_iter,
               vcov = as.formula(paste0("~", vcov_type)))
  
  b_todos <- coef(fit)
  
  # Si algún término no está en el modelo (p. ej. colinealidad puntual en esa
  # iteración), se devuelve NA para toda la iteración en vez de fallar.
  if (!all(TERMINOS_ME %in% names(b_todos))) {
    return(tibble(tiempo = grid_t, me_log = NA_real_, var_within = NA_real_))
  }
  
  b <- b_todos[TERMINOS_ME]
  V <- vcov(fit)[TERMINOS_ME, TERMINOS_ME]
  
  log_t <- log(grid_t)
  # Matriz de contrastes: una fila por valor de tiempo, columnas en el mismo
  # orden que TERMINOS_ME (villa, tiempo:villa, dist:villa, tiempo:dist:villa)
  A <- cbind(1, log_t, dist0, log_t * dist0)
  
  me_log     <- as.numeric(A %*% b)
  var_within <- rowSums((A %*% V) * A)   # a' V a por fila de A, vectorizado
  
  tibble(tiempo = grid_t, me_log = me_log, var_within = var_within)
}

resultados_me <- imap_dfr(
  datos_por_iteracion,
  ~ calcular_me_una_iteracion(.x, especificaciones_het$het2$formula, grid_tiempo, dist_ref) |>
    mutate(iteracion = .y)
)

# --- Combinación de Rubin por punto del grid --------------------------------

m_me <- length(datos_por_iteracion)

me_rubin <- resultados_me |>
  filter(!is.na(me_log)) |>
  group_by(tiempo) |>
  summarise(
    media_log = mean(me_log),
    U_bar     = mean(var_within),
    B_between = var(me_log),
    .groups = "drop"
  ) |>
  mutate(
    B_between   = if_else(is.na(B_between), 0, B_between),
    T_total     = U_bar + (1 + 1 / m_me) * B_between,
    se_log      = sqrt(T_total),
    z90         = qnorm(0.95),   # IC 90% -> percentil 95 de cada cola
    ic_low_log  = media_log - z90 * se_log,
    ic_high_log = media_log + z90 * se_log,
    # Transformación EXACTA a % (no la aproximación lineal 100*beta), correcta
    # para modelos semi-log (Wooldridge 2002, Cap. 6-7).
    efecto_pct  = 100 * (exp(media_log)   - 1),
    ic_low_pct  = 100 * (exp(ic_low_log)  - 1),
    ic_high_pct = 100 * (exp(ic_high_log) - 1)
  )

cat(sprintf("   Combinado con regla de Rubin sobre %d iteraciones (por punto del grid).\n", m_me))

# --- Gráfico -----------------------------------------------------------------

p_margins_villa_tiempo <- ggplot(me_rubin, aes(x = tiempo)) +
  geom_ribbon(aes(ymin = ic_low_pct, ymax = ic_high_pct, fill = "IC [90%]"), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(aes(y = efecto_pct), linewidth = 0.7, color = "black") +
  scale_fill_manual(name = NULL, values = c("IC [90%]" = "grey50")) +
  labs(
    x = "Tiempo a pie al CeSAC m\u00e1s cercano (minutos)",
    y = "Efecto de Villa sobre d\u00edas desde \u00faltima consulta (%)",
    title = "Efecto marginal de Villa seg\u00fan tiempo de acceso al CeSAC",
    subtitle = sprintf(
      "Distancia eucl. fijada en su media muestral (%.2f km). Bandas: IC 90%% (Rubin, 1987).",
      dist_ref
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 8.5, color = "grey30")
  )

p_margins_villa_tiempo

ggsave("grafico_efecto_marginal_villa_tiempo.png", p_margins_villa_tiempo,
       width = 7, height = 5, dpi = 300, bg = "white")

cat("   ✓ Exportado: grafico_efecto_marginal_villa_tiempo.png\n")

cat("\n   Chequeo rápido de coherencia: comparar el signo y la magnitud de este\n")
cat("   gráfico cerca del tiempo mediano contra el coeficiente de la fila\n")
cat("   'Tiempo transporte (log) × Villa' de la Tabla 1 (Het 1/Het 2) — deberían\n")
cat("   contar la misma historia direccional, aunque no coincidan exactamente\n")
cat("   (acá la distancia está fija en su media; la tabla no condiciona en eso).\n")


# =============================================================================
# J. GRÁFICO DE EFECTOS MARGINALES — VILLA × NBI (estilo margins/marginsplot)
# =============================================================================
# Mismo enfoque que la Parte I, pero sobre Het 3 (interacción de 2 vías, no
# triple). En Stata sería:
#   margins, dydx(villa) at(nbi=(0(1)60)) level(90)
#   marginsplot
#
# DERIVACIÓN: en Het 3, el efecto marginal de villa es
#   ME_villa(nbi) = b_villa + b_(villa:NBI) * nbi
# Combinación lineal de SOLO 2 coeficientes (más simple que la Parte I, que
# necesitaba 4) — pero la varianza sigue necesitando la covarianza ENTRE esos
# dos términos, no solo sus EE marginales, así que el enfoque es idéntico:
# reestimar Het 3 en las 1000 iteraciones extrayendo vcov(fit) completa, y
# combinar entre iteraciones con la regla de Rubin (1987).
#
# NBI entra en NIVEL (no en log) en toda la especificación — a diferencia de
# la Parte I, acá no hay logaritmo que curve la relación: ME_villa(nbi) es
# EXACTAMENTE lineal en nbi, así que el gráfico va a salir una recta (con
# banda de confianza que se abre hacia los extremos por menor soporte de
# datos ahí, no por curvatura del efecto en sí).

cat("\n── J. Gráfico de efectos marginales: Villa × NBI (Het 3) ──\n")

TERMINOS_ME_NBI <- c("factor(villa)1", "factor(villa)1:pct_NBI_t")

# --- Grid de NBI -------------------------------------------------------------
# Igual que en la Parte I: derivado de los datos (percentiles 2-98 de
# pct_NBI_t), no fijo, para no extrapolar más allá del soporte empírico.

rango_nbi <- base_unida |>
  filter(!is.na(pct_NBI_t)) |>
  summarise(
    p_low  = quantile(pct_NBI_t, 0.02, na.rm = TRUE),
    p_high = quantile(pct_NBI_t, 0.98, na.rm = TRUE)
  )

grid_nbi <- seq(floor(rango_nbi$p_low), ceiling(rango_nbi$p_high), length.out = 60)

cat(sprintf("   Grid de NBI: %.2f a %.2f (percentiles 2-98 de la muestra).\n",
            min(grid_nbi), max(grid_nbi)))

# --- Loop Monte Carlo: efecto marginal + varianza delta-method por iteración
# (misma lógica que calcular_me_una_iteracion() de la Parte I, adaptada a un
# contraste lineal de 2 términos en vez de 4 — se escribe una función propia
# en vez de generalizar la de la Parte I, para que cada bloque sea legible
# de forma autónoma)

calcular_me_nbi_una_iteracion <- function(data_iter, formula_str, grid_x, vcov_type = "comuna_ok") {
  fit <- feols(as.formula(formula_str), data = data_iter,
               vcov = as.formula(paste0("~", vcov_type)))
  
  b_todos <- coef(fit)
  
  if (!all(TERMINOS_ME_NBI %in% names(b_todos))) {
    return(tibble(nbi = grid_x, me = NA_real_, var_within = NA_real_))
  }
  
  b <- b_todos[TERMINOS_ME_NBI]
  V <- vcov(fit)[TERMINOS_ME_NBI, TERMINOS_ME_NBI]
  
  # Matriz de contrastes: una fila por valor de NBI, columnas (villa, villa:NBI)
  A <- cbind(1, grid_x)
  
  me         <- as.numeric(A %*% b)
  var_within <- rowSums((A %*% V) * A)
  
  tibble(nbi = grid_x, me = me, var_within = var_within)
}

resultados_me_nbi <- imap_dfr(
  datos_por_iteracion,
  ~ calcular_me_nbi_una_iteracion(.x, especificaciones_het$het3$formula, grid_nbi) |>
    mutate(iteracion = .y)
)

# --- Combinación de Rubin por punto del grid --------------------------------

m_me_nbi <- length(datos_por_iteracion)

me_nbi_rubin <- resultados_me_nbi |>
  filter(!is.na(me)) |>
  group_by(nbi) |>
  summarise(
    media_log = mean(me),
    U_bar     = mean(var_within),
    B_between = var(me),
    .groups = "drop"
  ) |>
  mutate(
    B_between   = if_else(is.na(B_between), 0, B_between),
    T_total     = U_bar + (1 + 1 / m_me_nbi) * B_between,
    se_log      = sqrt(T_total),
    z90         = qnorm(0.95),
    ic_low_log  = media_log - z90 * se_log,
    ic_high_log = media_log + z90 * se_log,
    efecto_pct  = 100 * (exp(media_log)   - 1),
    ic_low_pct  = 100 * (exp(ic_low_log)  - 1),
    ic_high_pct = 100 * (exp(ic_high_log) - 1)
  )

cat(sprintf("   Combinado con regla de Rubin sobre %d iteraciones (por punto del grid).\n", m_me_nbi))

# --- Gráfico -----------------------------------------------------------------

p_margins_villa_nbi <- ggplot(me_nbi_rubin, aes(x = nbi)) +
  geom_ribbon(aes(ymin = ic_low_pct, ymax = ic_high_pct, fill = "IC [90%]"), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(aes(y = efecto_pct), linewidth = 0.7, color = "black") +
  scale_fill_manual(name = NULL, values = c("IC [90%]" = "grey50")) +
  labs(
    x = "NBI (%)",
    y = "Efecto de Villa sobre d\u00edas desde \u00faltima consulta (%)",
    title = "Efecto marginal de Villa seg\u00fan % de NBI",
    subtitle = "Bandas: IC 90% (Rubin, 1987)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 8.5, color = "grey30")
  )

p_margins_villa_nbi

ggsave("grafico_efecto_marginal_villa_nbi.png", p_margins_villa_nbi,
       width = 7, height = 5, dpi = 300, bg = "white")

cat("   ✓ Exportado: grafico_efecto_marginal_villa_nbi.png\n")

cat("\n   Chequeo rápido de coherencia: el signo de la pendiente de esta recta\n")
cat("   debe coincidir con el signo de 'Villa × NBI (%)' en la Tabla 1 (Het 3) —\n")
cat("   pendiente positiva si villa:NBI > 0 (la brecha villa crece con el NBI\n")
cat("   del entorno), negativa si villa:NBI < 0 (se achica).\n")