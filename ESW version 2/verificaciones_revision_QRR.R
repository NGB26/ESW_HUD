############################################################################
# Verificaciones numéricas para la respuesta al peer review (QRR)
# Nota Técnica ESW HUD-HNP: residencia en barrios populares, tiempo desde
# la última consulta registrada e intervención de contacto proactivo (Boti)
#
# Autor: Nicolás García Balus (con asistencia de Claude)
# Fecha: 30/10/2026
#
# Qué hace este script
# --------------------
# Reproduce TODOS los números nuevos que se incorporaron a la versión
# «BID_ESW_nota_tecnica_VF_revision_QRR.docx». Cada sección indica:
#   (a) la fila del Excel QRR que atiende,
#   (b) qué número del Word alimenta y en qué sección del Word aparece,
#   (c) advertencias metodológicas o decisiones tomadas.
#
# Insumos
# -------
#   - df_esw2.dta           universo de 7.082 personas (datos/processed)
#   - base_esw_hud_hnp.dta  cohorte experimental de 998 personas
#   - base_radios.csv       radios censales con tiempos a CeSAC/hospital y
#                           variables censales (repositorio NGB26/ESW_HUD,
#                           ESW version 2/Scripts/R)
#
# Paquetes: haven, dplyr, sandwich, lmtest, clubSandwich, AER.
# No usa fixest para que corra sin dependencias pesadas: los efectos fijos
# de comuna se incluyen como dummies en lm(), lo que da coeficientes
# idénticos a feols(... | comuna).
#
# IMPORTANTE: la sección 5 (Monte Carlo) es una REPLICACIÓN SIMPLIFICADA del
# procedimiento de «09 simulaciones BIS.R». Reproduce la Reg. 5 de la Tabla 4
# con diferencias menores (β = 0,148 vs. 0,141 en la VF), porque parte de
# df_esw2.dta y no de data_revisada_ok.xlsx. Los valores CR2 y del bootstrap
# salvaje que figuran en el Word deben regenerarse con el pipeline oficial.
############################################################################

## 0. Configuración ---------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(sandwich)
  library(lmtest)
  library(clubSandwich)
  library(AER)
})

ruta_macchia <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/Re_ Informe alejandro macchia"
ruta_esw2    <- "C:/Users/NICOLASGA/OneDrive - Inter-American Development Bank Group/Documents/IDB/ESW HUD-SPH/Paper versión final - peer rev/ESW_HUD/ESW version 2"

archivo_universo <- file.path(ruta_esw2, "data/processed/df_esw2.dta")
archivo_cohorte  <- file.path(ruta_macchia, "base_esw_hud_hnp.dta")
archivo_radios   <- file.path(ruta_esw2, "Scripts/R/base_radios.csv")

# Permite sobreescribir rutas por variables de entorno (útil para correr en otro equipo)
if (nzchar(Sys.getenv("ESW_UNIVERSO"))) archivo_universo <- Sys.getenv("ESW_UNIVERSO")
if (nzchar(Sys.getenv("ESW_COHORTE")))  archivo_cohorte  <- Sys.getenv("ESW_COHORTE")
if (nzchar(Sys.getenv("ESW_RADIOS")))   archivo_radios   <- Sys.getenv("ESW_RADIOS")

N_SIM_MC   <- 200   # imputaciones para Rubin (la VF usa 1.000; 200 alcanzan para verificar)
N_SIM_WCB  <- 40    # imputaciones sobre las que se corre el bootstrap salvaje
B_WCB      <- 499   # réplicas bootstrap por imputación
set.seed(1234)

# Funciones auxiliares ------------------------------------------------------

smd <- function(x1, x0) {                         # diferencia estandarizada (Austin, 2009)
  (mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) /
    sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE)) / 2)
}
tabla_smd <- function(datos, grupo, vars) {      # grupo: lógico; TRUE vs FALSE
  do.call(rbind, lapply(vars, function(v) {
    x1 <- datos[[v]][datos[[grupo]]]; x0 <- datos[[v]][!datos[[grupo]]]
    data.frame(variable = v, media_1 = mean(x1, na.rm = TRUE), media_0 = mean(x0, na.rm = TRUE),
               smd = smd(x1, x0), p_welch = t.test(x1, x0)$p.value)
  }))
}
ci_lm <- function(modelo, termino, vc = vcovHC(modelo, type = "HC1"), escala = 100) {
  b  <- coef(modelo)[termino]; se <- sqrt(vc[termino, termino])
  c(est = b * escala, li = (b - 1.96 * se) * escala, ls = (b + 1.96 * se) * escala,
    p = 2 * pnorm(-abs(b / se)))
}
# CRV1 manual (equivale a sandwich::vcovCL(type = "HC1"), más rápido dentro del Monte Carlo)
crv1 <- function(X, u, g) {
  XtXi <- solve(crossprod(X)); G <- length(unique(g)); n <- nrow(X); k <- ncol(X)
  S <- rowsum(X * as.vector(u), g)                       # scores sumados por conglomerado
  G / (G - 1) * (n - 1) / (n - k) * XtXi %*% crossprod(S) %*% XtXi
}
# Bootstrap salvaje por conglomerados restringido (WCR), pesos de Webb
# (Cameron, Gelbach y Miller, 2008; Webb, 2023). Devuelve el p-valor de H0: beta_j = 0.
wcr_boot <- function(X, y, g, j, B = 499) {
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  gid  <- as.integer(factor(g)); G <- max(gid)
  b    <- qr.coef(qr(X), y); u <- y - X %*% b
  t0   <- b[j] / sqrt(crv1(X, u, gid)[j, j])
  Xr   <- X[, -j, drop = FALSE]; br <- qr.coef(qr(Xr), y)
  yr   <- Xr %*% br; ur <- y - yr
  P    <- solve(crossprod(X), t(X))
  tb   <- replicate(B, {
    ys <- yr + ur * sample(webb, G, replace = TRUE)[gid]
    bs <- P %*% ys; us <- ys - X %*% bs
    bs[j] / sqrt(crv1(X, us, gid)[j, j])
  })
  mean(abs(tb) >= abs(t0))
}

## 1. Carga y construcción de variables ------------------------------------

d <- read_dta(archivo_universo) |> zap_labels()
b <- read_dta(archivo_cohorte)  |> zap_labels()

vacio <- function(x) is.na(x) | trimws(x) == ""
d <- d |>
  mutate(
    comuna_ok   = ifelse(vacio(comuna_ok), NA_character_, comuna_ok),
    comuna_norm = ifelse(vacio(comuna) | startsWith(comuna, "Inv"), NA_character_, comuna),  # "Inválido"
    comuna_fb   = coalesce(comuna_ok, comuna_norm),
    # Definición del pipeline: todas las villas en una categoría "BP"
    comuna_regresion = ifelse(villa == 1, "BP", comuna_fb),
    ly          = log(dias_ult_cons),
    masc        = as.integer(genero == "Masculino"),
    edad_grupo  = cut(edad, c(0, 45, 65, Inf), right = FALSE, labels = c("18-44", "45-64", "65+")),
    com_num     = as.integer(gsub("\\D", "", comuna_ok))
  )

# Servicios pertinentes para HTA (misma lista que turno_relevante en R y Stata)
servicios_validos <- c("CLINICA MEDICA", "MEDICINA FAMILIAR", "CARDIOLOGIA",
                       "DIVISION CARDIOLOGIA", "DIVISION CLINICA MEDICA",
                       "HIPERTENSION ARTERIAL", "UNIDAD CARDIOLOGIA",
                       "SALUD INTEGRAL", "UNIDAD CLINICA MEDICA")

## 2. Filas 7, 9, 10 y 57 – Flujo muestral y excluidos vs. retenidos --------
# Word: sección 3.3 (párrafos en amarillo) y notas de las Tablas 2, 3, 4 y A.
# Hallazgo: las ~775 exclusiones NO son residentes en BP sin barrio identificado
# (como decía la VF), sino 774 NO residentes en BP sin comuna válida (comuna_ok
# vacía y campo comuna = "Inválido"). Además, 22 residentes en BP no tienen
# comuna_ok (por eso la Tabla 4 tiene 6.286 obs.).
# Advertencia: el pipeline reporta 6.302 obs. en Reg. 3 de la Tabla 2, es decir
# 6 residentes en BP menos que los 1.235 que hay aquí; el origen exacto de esos 6
# y de las 11 obs. sin tiempo al CeSAC debe confirmarse con el diagnóstico de NA
# de la sección III.7 de «09 simulaciones BIS.R».

cat("\n===== 2. FLUJO MUESTRAL =====\n")
print(table(villa = d$villa, comuna_ok_faltante = is.na(d$comuna_ok)))
cat("No residentes sin comuna válida (ni comuna_ok ni comuna):",
    sum(d$villa == 0 & is.na(d$comuna_fb)), "\n")
cat("N con FE comuna_regresion (BP como categoría):", sum(!is.na(d$comuna_regresion)), "\n")
cat("N con FE comuna_ok:", sum(!is.na(d$comuna_ok)),
    " (no residentes:", sum(!is.na(d$comuna_ok) & d$villa == 0),
    "; residentes en BP:", sum(!is.na(d$comuna_ok) & d$villa == 1), ")\n")

# Fila 10: comparación de no residentes excluidos vs. retenidos.
# Word: 3.3, segundo párrafo amarillo (edad 59,8 vs 59,6; varones 38,9% vs 41,3%;
# diabetes 30,6% vs 34,4%, p = 0,04; días 226,1 vs 216,9; todas las SMD < 0,1).
nb <- d |> filter(villa == 0) |> mutate(excluido = is.na(comuna_ok))
cat("\nNo residentes: excluidos (TRUE) vs. retenidos (FALSE)\n")
print(tabla_smd(nb, "excluido", c("edad", "masc", "dbt", "ly", "dias_ult_cons", "dias_dispensa")), digits = 3)
print(nb |> group_by(excluido) |> summarise(n = n(), mediana_dias = median(dias_ult_cons)))

## 3. Fila 56 – Tabla 1: sexo sin dato -------------------------------------
# Word: fila «Sin dato» agregada a la Tabla 1 (amarillo). Hay 1 no residente con
# sexo vacío, por eso las filas de sexo sumaban 5.846 y no 5.847.
cat("\n===== 3. SEXO POR RESIDENCIA =====\n")
print(table(genero = ifelse(vacio(d$genero), "(sin dato)", d$genero), villa = d$villa))

## 4. Fila 8 – Descriptivos de la muestra analítica ------------------------
# Word: 4.2, párrafo amarillo posterior a la Tabla 1 (216,9 y 225,8 días; medianas
# 149 y 178; edad 59,6 y 56,9; varones 41,3% y 30,4%).
cat("\n===== 4. DESCRIPTIVOS MUESTRA ANALÍTICA (comuna_ok válida) =====\n")
print(d |> filter(!is.na(comuna_ok)) |> group_by(villa) |>
        summarise(n = n(), media_dias = mean(dias_ult_cons), mediana = median(dias_ult_cons),
                  edad = mean(edad), pct_varones = 100 * mean(masc)))

## 5. Fila 17 – Definición del outcome observacional ------------------------
# Word: 3.6 (fecha de corte 30/10/2025, sin ceros ni faltantes, mínimo 15 días,
# mediana 160, 53% con < 180 días porque la inclusión es por falta de RETIRO de
# medicación y no por falta de consulta).
# Advertencia: la base no trae el servicio de la última consulta; los tipos de
# consulta se tomaron del entregable de Macchia y de la lista de servicios del
# script de Stata. Confirmar con el Ministerio.
cat("\n===== 5. OUTCOME OBSERVACIONAL =====\n")
print(table(fecha_corte = d$fec_corte))
print(summary(d$dias_ult_cons))
cat("Faltantes:", sum(is.na(d$dias_ult_cons)), " Ceros:", sum(d$dias_ult_cons == 0, na.rm = TRUE),
    " % con < 180 días:", round(100 * mean(d$dias_ult_cons < 180), 1), "\n")
print(table(alerta = d$alerta_hta))

## 6. Fila 15/20 – Qué medida de tiempo usa cada tabla ----------------------
# Word: 3.5. Tabla 1 usa t_ors_p_cesac (Open Route Service peatonal: 15,8 y 4,6 min);
# los modelos usan tiempo_transp_min_cesac (distancia por red a ~12 km/h).
cat("\n===== 6. MEDIDAS DE TIEMPO =====\n")
print(d |> group_by(villa) |> summarise(t_ors_pie = mean(t_ors_p_cesac, na.rm = TRUE),
                                         t_transporte = mean(t_transp_cesac, na.rm = TRUE)))

## 7. Fila 25 – Brecha descriptiva (8,4 días) vs. asociación en logaritmos ---
# Word: 4.3, párrafo nuevo. El modelo en logs compara MEDIAS GEOMÉTRICAS:
# medianas 179 vs 151 (+18,5%), medias geométricas 133,1 vs 121,1 (+9,9%),
# medias aritméticas +3,9%. El ajuste por sexo y edad agranda la brecha porque
# la población de BP es más joven y más femenina (Reg. 1 → Reg. 2).
cat("\n===== 7. LOG vs NIVELES =====\n")
print(d |> group_by(villa) |> summarise(media = mean(dias_ult_cons), mediana = median(dias_ult_cons),
                                         media_geometrica = exp(mean(ly))))
r1 <- lm(ly ~ villa, d)
r2 <- lm(ly ~ villa + masc + edad_grupo, d)
cat("Reg. 1:", round(coef(r1)["villa"], 4), "  Reg. 2:", round(coef(r2)["villa"], 4),
    "  (VF: 0,090 y 0,134; la diferencia menor se debe a que la VF parte de data_revisada_ok.xlsx)\n")
cat("En días (niveles): sin controles", round(coef(lm(dias_ult_cons ~ villa, d))["villa"], 1),
    "; con sexo y edad", round(coef(lm(dias_ult_cons ~ villa + masc + edad_grupo, d))["villa"], 1), "\n")
# Fila 26: transformación correcta del coeficiente (nota al pie 4): (exp(b) - 1) * 100
cat("Rango Reg. 1-5 de la Tabla 2 en %:", round((exp(c(0.090, 0.167)) - 1) * 100, 1), "\n")

## 8. Filas 23 y 27 – Colinealidad de comuna_regresion e inferencia ---------
# Word: 3.6 (rojo), notas de Tablas 2-4 y 4.3 (azul).
# HALLAZGO: con FE = comuna_regresion, la categoría "BP" coincide exactamente con
# villa == 1. R/fixest eliminan una categoría por colinealidad y el coeficiente de
# villa queda medido contra la comuna omitida. fixest elimina la última
# (Comuna Nº 9): por eso la Tabla 2 reporta 0,167 (EE 0,064). Con otra comuna de
# referencia el coeficiente cambia (0,135 a 0,199): no es una comparación dentro de
# comuna. La especificación correctamente identificada es la de comuna_ok (Tabla 4).
cat("\n===== 8. COLINEALIDAD comuna_regresion =====\n")
s_reg <- d |> filter(!is.na(comuna_regresion))
niveles <- levels(factor(s_reg$comuna_regresion))
for (nro in c(9, 1, 8, 4)) {
  ref <- niveles[grepl(paste0("\\D", nro, "$"), niveles)]   # evita problemas de codificación con "Nº"
  dd <- s_reg |> mutate(cr = relevel(factor(comuna_regresion), ref = ref))
  m  <- lm(ly ~ villa + masc + edad_grupo + cr, dd)        # lm descarta "BP" (NA) por colinealidad
  cat(sprintf("Referencia %-12s villa = %.3f (EE %.3f)\n", ref, coef(m)["villa"],
              sqrt(vcov(m)["villa", "villa"])))
}

# Reg. 3 con FE comuna_ok (sin imputación): EE clásico, CRV1, CR2 y bootstrap salvaje.
# Ilustra que, con 15 conglomerados, el CRV1 (0,017) es menor que el clásico (0,043):
# síntoma de subestimación con pocos conglomerados (MacKinnon, Nielsen y Webb, 2023).
s_ok <- d |> filter(!is.na(comuna_ok))
m3   <- lm(ly ~ villa + masc + edad_grupo + factor(comuna_ok), s_ok)
v_cr2 <- vcovCR(m3, cluster = s_ok$comuna_ok, type = "CR2")
cat("\nReg. 3 (FE comuna_ok): beta =", round(coef(m3)["villa"], 4),
    "| EE clásico =", round(sqrt(vcov(m3)["villa", "villa"]), 4),
    "| EE CRV1 =", round(sqrt(vcovCL(m3, cluster = ~comuna_ok, type = "HC1")["villa", "villa"]), 4),
    "| EE CR2 =", round(sqrt(v_cr2["villa", "villa"]), 4), "\n")
print(coef_test(m3, vcov = v_cr2, test = "Satterthwaite", coefs = "villa"))
X3 <- model.matrix(m3); j3 <- which(colnames(X3) == "villa")
cat("p-valor bootstrap salvaje (Webb, B = 1999):",
    wcr_boot(X3, s_ok$ly, s_ok$comuna_ok, j3, B = 1999), "\n")

## 9. Filas 23, 27 y 22 – Replicación del Monte Carlo (Reg. 5, FE comuna_ok) --
# Word: 4.3, párrafo azul («En una replicación del procedimiento de imputación con
# corrección CR2, el IC95% es [0,038; 0,258] y el bootstrap salvaje ... p mediano 0,03»).
# Procedimiento (simplificado de «09 simulaciones BIS.R»):
#   - Residentes en BP: tiempo de transporte, NBI y hacinamiento del BARRIO (fijos).
#   - No residentes: en cada iteración se sortea un radio censal de su comuna con
#     probabilidad proporcional a total_viv (PONDERAR_POR_POBLACION = TRUE).
#   - Reg. 5: log(días) ~ villa + sexo + edad + hacinamiento + NBI + log(tiempo) + FE comuna_ok.
#   - Varianza total por reglas de Rubin: T = Ubar + (1 + 1/m) B.
# ADVERTENCIA (fila 14): base_radios.csv no trae el indicador de radio en BP, así que
# aquí, igual que en el código del repositorio, el sorteo NO excluye radios de BP.
# Si se adopta la frase del Word («el sorteo se realizó exclusivamente entre radios
# que no pertenecen a barrios populares»), hay que agregar
#   filter(!es_barrio_popular)   # o pct_area_villa == 0
# al construir pool_por_comuna en la sección II.1 del pipeline y volver a correrlo.

cat("\n===== 9. MONTE CARLO Reg. 5 =====\n")
radios <- read.csv(archivo_radios) |> filter(tipo_centro == "CeSAC", !is.na(tiempo_transp_min))
pools  <- split(radios, radios$numero_comuna)

s5   <- d |> filter(!is.na(comuna_ok))
Xfix <- model.matrix(~ villa + masc + edad_grupo + factor(comuna_ok), s5)
g5   <- s5$comuna_ok

sortear <- function() {
  t <- s5$t_transp_cesac; nbi <- s5$pct_nbi_t; hac <- s5$pct_hacinamiento_t
  for (cm in names(pools)) {
    idx <- which(s5$villa == 0 & s5$com_num == as.integer(cm))
    if (!length(idx)) next
    pl <- pools[[cm]]; w <- pl$total_viv; w[is.na(w)] <- 0
    k  <- sample.int(nrow(pl), length(idx), replace = TRUE, prob = w)
    t[idx] <- pl$tiempo_transp_min[k]; nbi[idx] <- pl$pct_NBI[k]; hac[idx] <- pl$pct_hacinamiento[k]
  }
  ok <- t > 0 & !is.na(t) & !is.na(nbi) & !is.na(hac)
  X  <- cbind(Xfix, hac = hac, nbi = nbi, ltime = log(t))[ok, ]
  list(X = X, y = s5$ly[ok], g = g5[ok], ok = ok, t = t, nbi = nbi, hac = hac)
}
rubin <- function(betas, vars) {
  m <- length(betas); Bv <- var(betas)
  c(beta = mean(betas), se = sqrt(mean(vars) + (1 + 1 / m) * Bv))
}

res <- t(replicate(N_SIM_MC, {
  sm <- sortear(); fit <- lm.fit(sm$X, sm$y); u <- fit$residuals
  j  <- which(colnames(sm$X) == "villa")
  c(beta = unname(fit$coefficients[j]),
    v_clas = sum(u^2) / (nrow(sm$X) - ncol(sm$X)) * solve(crossprod(sm$X))[j, j],
    v_crv1 = crv1(sm$X, u, as.integer(factor(sm$g)))[j, j],
    n = nrow(sm$X))
}))
rc <- rubin(res[, "beta"], res[, "v_clas"]); rv <- rubin(res[, "beta"], res[, "v_crv1"])
cat("Reg. 5 replicada: beta =", round(rc["beta"], 3), "| EE clásico Rubin =", round(rc["se"], 4),
    "| EE CRV1 Rubin =", round(rv["se"], 4), "| N medio =", mean(res[, "n"]),
    "\n(VF: Tabla 4 = 0,141 (0,046); Tabla A = 0,141 (0,063); N = 6.275)\n")

# IC de la especificación principal con los números de la VF (Word 4.3, azul)
ic <- function(bt, se, crit) round(c(bt - crit * se, bt + crit * se), 3)
cat("IC95% Tabla A (EE 0,063, normal):", ic(0.141, 0.063, 1.96),
    " en %:", round((exp(ic(0.141, 0.063, 1.96)) - 1) * 100, 1), "\n")
cat("IC95% Tabla 4 (EE 0,046, t14):  ", ic(0.141, 0.046, qt(0.975, 14)),
    " en %:", round((exp(ic(0.141, 0.046, qt(0.975, 14))) - 1) * 100, 1), "\n")
cat("IC95% Tabla 2 Reg. 5 (0,165; EE 0,090):", ic(0.165, 0.090, 1.96), "\n")

# CR2 + bootstrap salvaje en un subconjunto de imputaciones
wcb <- t(replicate(N_SIM_WCB, {
  sm <- sortear(); j <- which(colnames(sm$X) == "villa")
  mm <- lm(sm$y ~ sm$X - 1)
  vc <- vcovCR(mm, cluster = sm$g, type = "CR2")
  c(beta = unname(coef(mm)[j]), v_cr2 = vc[j, j],
    p_wcb = wcr_boot(sm$X, sm$y, sm$g, j, B = B_WCB))
}))
r2c <- rubin(wcb[, "beta"], wcb[, "v_cr2"])
cat("CR2 + Rubin: beta =", round(r2c["beta"], 3), " EE =", round(r2c["se"], 4),
    " IC95% t14 =", ic(r2c["beta"], r2c["se"], qt(0.975, 14)), "\n")
cat("Bootstrap salvaje: p mediano =", round(median(wcb[, "p_wcb"]), 3),
    " rango =", round(range(wcb[, "p_wcb"]), 3), "\n")

## 10. Fila 22 – ANOVA de variables territoriales ---------------------------
# Word: marcador amarillo en 4.3 ([INSERTAR AQUÍ la tabla de ANOVA ...]).
# Para cada variable territorial: R² sobre FE de comuna (varianza ENTRE comunas) y
# R² sobre FE + controles + resto de territoriales (variación NO independiente; VIF = 1/(1-R²)).
cat("\n===== 10. VARIABLES TERRITORIALES =====\n")
anova_terr <- replicate(50, {
  sm <- sortear(); ok <- sm$ok
  V  <- data.frame(villa = s5$villa[ok], ltime = log(sm$t[ok]), nbi = sm$nbi[ok], hac = sm$hac[ok])
  FE <- s5$comuna_ok[ok]; ctr <- s5[ok, c("masc", "edad_grupo")]
  sapply(names(V), function(v) {
    r2_fe <- summary(lm(V[[v]] ~ factor(FE)))$r.squared
    otros <- V[, setdiff(names(V), v), drop = FALSE]
    r2_all <- summary(lm(V[[v]] ~ factor(FE) + ctr$masc + ctr$edad_grupo + as.matrix(otros)))$r.squared
    c(r2_entre_comunas = r2_fe, r2_resto = r2_all)
  })
}, simplify = "array")
print(round(apply(anova_terr, c(1, 2), mean), 3))
cat("VIF:", round(1 / (1 - apply(anova_terr, c(1, 2), mean)["r2_resto", ]), 1), "\n")
sm <- sortear()
print(round(cor(cbind(villa = s5$villa[sm$ok], ltime = log(sm$t[sm$ok]), nbi = sm$nbi[sm$ok], hac = sm$hac[sm$ok])), 2))

## 11. Filas 33-37 – Conformación de la cohorte experimental ----------------
# Word: 3.9 (rojo), 4.5 (rojo), Resumen ejecutivo (rojo), 3.11 y 5.5.
# Reconstrucción a partir de los datos:
#   - grupo_aleat (Precoz/Tardía) existe para las 7.082 personas: el universo
#     entero fue aleatorizado (3.510 / 3.572) y está balanceado.
#   - intervencion == 1 para 499 personas del brazo Precoz (los contactados); las
#     3.011 restantes del brazo Precoz tienen intervencion missing.
#   - Los 499 controles de base_esw_hud_hnp.dta pertenecen al brazo Tardía
#     (apareamiento 1:1, como dice Macchia: «cohorte emparejada»).
#   - Los contactados NO son una muestra aleatoria del brazo Precoz: se concentran en
#     identificadores altos (≈ orden del listado) y difieren en villa, días y
#     antigüedad del diagnóstico. Confirmar con A. Macchia el criterio de orden.
cat("\n===== 11. COHORTE EXPERIMENTAL =====\n")
ids_ctrl <- as.numeric(b$id_persona[b$intervencion == 0])
d <- d |>
  mutate(
    precoz = as.integer(grupo_aleat == "Precoz"),
    tr     = as.integer(!is.na(intervencion) & intervencion == 1),
    ct     = as.integer(id %in% ids_ctrl),
    rel    = as.integer(servicio %in% servicios_validos),
    # Outcome ITT: turno en servicio pertinente o turno asignado durante el contacto
    y      = as.integer(rel == 1 | (!is.na(resp_efectivo) & resp_efectivo == 1)),
    ldias  = log(dias_ult_cons),
    ldet   = log(pmax(dias_detec, 1)),
    ldisp  = log(pmax(dias_dispensa, 1)),
    nohta  = as.integer(turno == 1 & rel == 0)
  )
print(table(brazo = d$grupo_aleat, contactado = d$tr, control_apareado = d$ct))

# Chequeo: la construcción de y reproduce outcome_itt en las 998 personas
chk <- d |> filter(tr == 1 | ct == 1) |> select(id, y) |>
  inner_join(b |> transmute(id = as.numeric(id_persona), outcome_itt), by = "id")
cat("Coincidencia con outcome_itt:", mean(chk$y == chk$outcome_itt), "\n")

vars_bal <- c("edad", "masc", "dbt", "villa", "dias_ult_cons", "dias_detec", "dias_dispensa")
cat("\nBalance del universo aleatorizado: Precoz vs. Tardía (SMD < 0,03)\n")
print(tabla_smd(d |> mutate(g = precoz == 1), "g", vars_bal), digits = 3)
cat("\nContactados vs. Precoz no contactados (11,2% vs 18,2% BP; 321 vs 206 días; 806 vs 1.320 días desde detección)\n")
print(tabla_smd(d |> filter(precoz == 1) |> mutate(g = tr == 1), "g", vars_bal), digits = 3)
cat("\nTratamiento vs. control apareado (fila 37: SMD de la Tabla 6 y antigüedad del diagnóstico −0,63)\n")
print(tabla_smd(d |> filter(tr == 1 | ct == 1) |> mutate(g = tr == 1), "g", vars_bal), digits = 3)
cat("\nComposición: universo vs cohorte\n")
print(rbind(universo = c(villa = mean(d$villa), dias = mean(d$dias_ult_cons)),
            cohorte  = c(mean(d$villa[d$tr == 1 | d$ct == 1]), mean(d$dias_ult_cons[d$tr == 1 | d$ct == 1]))))
id_q <- d |> filter(precoz == 1) |> group_by(tr) |> summarise(id_medio = mean(id))
print(id_q)

## 12. Fila 34 – Robustez del efecto a la conformación de la cohorte ---------
# Word: 4.6, párrafo rojo «Robustez del efecto a la conformación de la cohorte».
# Modelos de probabilidad lineal con errores HC1. El outcome para quienes no están
# en la cohorte se construye igual (misma extracción y misma ventana de fechas de turno).
cat("\n===== 12. ROBUSTEZ EXPERIMENTAL =====\n")
covs <- "+ edad + masc + dbt + villa + ldias"
tc   <- d |> filter(tr == 1 | ct == 1)
tdif <- d |> filter(tr == 1 | precoz == 0)
mods <- list(
  "T vs C crudo (4,8 pp)"                 = lm(y ~ tr, tc),
  "T vs C + detección y dispensa (5,5)"   = lm(as.formula(paste("y ~ tr", covs, "+ ldet + ldisp")), tc),
  "T vs todo el brazo diferido (5,1)"     = lm(y ~ tr, tdif),
  "T vs brazo diferido ajustado (5,7)"    = lm(as.formula(paste("y ~ tr", covs, "+ ldet + ldisp")), tdif)
)
print(round(t(sapply(mods, ci_lm, termino = "tr")), 2))
itt <- lm(y ~ precoz, d)
cat("ITT de la asignación aleatoria (0,75 pp):", round(ci_lm(itt, "precoz"), 3), "\n")
cat("Placebo: Precoz no contactados vs Tardía (0,03 pp):",
    round(ci_lm(lm(y ~ precoz, d |> filter(tr == 0)), "precoz"), 3), "\n")
cat("Proporción contactada del brazo Precoz:", round(mean(d$tr[d$precoz == 1]), 4), "\n")
# VI / Wald: instrumento = brazo precoz; tratamiento = contacto (no cumplimiento unilateral;
# Bloom, 1984; Angrist, Imbens y Rubin, 1996)
iv <- ivreg(y ~ tr | precoz, data = d)
cat("VI (5,3 pp, IC 0,03 a 10,6):", round(ci_lm(iv, "tr", vc = vcovHC(iv, type = "HC1")), 3), "\n")
cat("Tasas de turno relevante: C =", round(mean(d$y[d$ct == 1]), 4),
    "| resto diferido =", round(mean(d$y[d$precoz == 0 & d$ct == 0]), 4),
    "| precoz no contactado =", round(mean(d$y[d$precoz == 1 & d$tr == 0]), 4), "\n")

## 13. Fila 36 – Fechas y embudo de contacto ---------------------------------
# Word: 3.9 (inicio 06/11/2025; 102 respondieron; 29 «no era la persona»; 28 turnos en contacto).
cat("\n===== 13. FECHAS Y EMBUDO =====\n")
ev <- b |> filter(evento_itt == 1)
print(table(inicio = as.Date(ev$fecha_evento_itt) - ev$t_evento_itt))
print(table(b$respondio_al_invitacion[b$intervencion == 1], useNA = "ifany"))
print(table(b$accion[b$intervencion == 1]))

## 14. Filas 47 y 58 – Turnos no vinculados con HTA --------------------------
# Word: 4.8 (azul) y marcador amarillo de la tabla sugerida.
# La diferencia (cualquier turno − turno HTA) se estima directamente con su EE.
# ADVERTENCIA: los controles apareados tienen menos turnos no-HTA (4,6%) que el resto
# del brazo diferido (9,1%), así que este resultado es sensible al grupo de comparación.
cat("\n===== 14. OTROS TURNOS =====\n")
nb_tc <- tc |> filter(villa == 0) |> mutate(dif = turno - y)
cat("No BP – turno HTA (5,2):", round(ci_lm(lm(y ~ tr, nb_tc), "tr"), 2), "\n")
cat("No BP – cualquier turno (5,5; 1,7 a 9,4):", round(ci_lm(lm(turno ~ tr, nb_tc), "tr"), 2), "\n")
cat("No BP – diferencia (0,3; −2,6 a 3,2; p = 0,83):", round(ci_lm(lm(dif ~ tr, nb_tc), "tr"), 2), "\n")
cat("Cualquier turno vs todo el brazo diferido:", round(ci_lm(lm(turno ~ tr, tdif), "tr"), 2), "\n")
print(d |> mutate(grupo = case_when(tr == 1 ~ "T", ct == 1 ~ "C apareado",
                                    precoz == 1 ~ "Precoz no contactado", TRUE ~ "Resto diferido")) |>
        group_by(grupo) |> summarise(turno_hta = mean(rel), no_hta = mean(nohta)))

## 15. Filas 45, 54 y 62 – Heterogeneidad por densidad de CeSAC (Figura 7) ----
# Word: 4.7 (texto nuevo) y comentarios en 3.10 y 4.7.
# Replica el modelo del script de Stata (líneas 159-160):
#   reg outcome_itt i.intervencion##c.densidad##c.densidad if villa==0, r
# OJO: es un MPL con término cuadrático, no el logit que describe la ecuación de 3.10.
cat("\n===== 15. DENSIDAD DE CeSAC =====\n")
dens <- b |> transmute(id = as.numeric(id_persona), intervencion, outcome_itt) |>
  inner_join(d |> select(id, densidad_por_km2_cesac, villa, comuna_ok), by = "id") |>
  filter(villa == 0, !is.na(densidad_por_km2_cesac))
md <- lm(outcome_itt ~ intervencion * (densidad_por_km2_cesac + I(densidad_por_km2_cesac^2)), dens)
vd <- vcovHC(md, type = "HC1")
cat("N =", nobs(md), "\n")
grilla <- seq(0.10, 0.60, by = 0.05)
ame <- t(sapply(grilla, function(x) {
  gv <- setNames(rep(0, length(coef(md))), names(coef(md)))
  gv["intervencion"] <- 1
  gv["intervencion:densidad_por_km2_cesac"] <- x
  gv["intervencion:I(densidad_por_km2_cesac^2)"] <- x^2
  e <- sum(gv * coef(md)); se <- sqrt(drop(t(gv) %*% vd %*% gv))
  c(densidad = x, ame = e, li90 = e - 1.645 * se, ls90 = e + 1.645 * se,
    li95 = e - 1.96 * se, ls95 = e + 1.96 * se)
}))
print(round(ame, 3))
# Prueba conjunta de las interacciones (F = 0,38; p = 0,69): no hay evidencia de heterogeneidad
print(waldtest(md, . ~ . - intervencion:densidad_por_km2_cesac - intervencion:I(densidad_por_km2_cesac^2),
               vcov = vd, test = "F"))
# Recomendado: agrupar por comuna, porque la densidad solo varía entre comunas
cat("Prueba conjunta con EE agrupados por comuna (CRV1, interpretar con cautela: pocos conglomerados):\n")
print(waldtest(md, . ~ . - intervencion:densidad_por_km2_cesac - intervencion:I(densidad_por_km2_cesac^2),
               vcov = vcovCL(md, cluster = ~comuna_ok, type = "HC1"), test = "F"))

## 16. Filas 48-49 – Número necesario a contactar ----------------------------
# Word: 5.4 (≈21 personas por turno adicional; IC 13 a 48). Insumo para un futuro
# cálculo de costo por turno adicional = costo total / (Δ × 499 ≈ 24 turnos).
cat("\n===== 16. NNT =====\n")
cat("NNT =", round(1 / 0.048, 1), " IC95% =", round(1 / c(0.075, 0.021)), "\n")
cat("Turnos adicionales en el grupo tratado: ", round(0.048 * 499), " (IC:", round(c(0.021, 0.075) * 499), ")\n")

cat("\nFin de las verificaciones.\n")
