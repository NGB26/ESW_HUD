/*==============================================================================
Nota técnica ESW HUD - HNP                                  (versión corregida)

Autor original: Nicolás García Balus en base a Alejandro Macchia
                (nicolas.gbalus@gmail.com / nicolasga@iadb.org)
Fecha original: 8/5/2026

CORRECCIONES respecto de la versión anterior
  1. label define sexo ... fallaba (r(110)): `encode` ya crea el label "sexo".
     Ahora sexo se genera sin encode, con chequeo de los valores de `genero`.
  2. merge con df_esw2: las filas "using only" (_merge==2) se agregaban a la
     base con todo missing y outcome 0 por construcción. Se descartan, y se
     reportan las filas master-only (sin dato de villa/densidad/servicio).
     Además solo se traen con keepusing() las variables que NO existen ya en
     memoria (si no, el merge las ignora en silencio o falla).
  3. pob_c se armaba matcheando strings "Comuna Nº 1": el "º" depende de la
     codificación (UTF-8 vs ANSI) y, si no matchea, pob_c queda missing y las
     regresiones pierden TODA la muestra sin avisar. Ahora se usa el número de
     comuna (nro_com o extraído de `comuna`) y se chequea que no queden
     missing.
  4. `tab densidad_por_km2_cesac villa` sobre una variable continua (tabla
     enorme / error "too many values"). Reemplazado por tabstat.
  5. Los tres marginsplot usaban addplot(scatter b x if x==.) con variables
     `b` y `x` que no existen en la base de marginsplot (error). Se reemplazó
     por un programa único (plot_ame) con leyenda por nota, y se exportan PDF.
  6. turno_nohta = turno_todos - outcome_itt podía dar -1 (si outcome_itt
     incluye "respondedor efectivo" sin turno). Se define como
     turno_todos - turno_relevante (siempre >= 0) y se reporta cuántos casos
     negativos habría dado la definición anterior.
  7. Cálculos de potencia con proporciones y n tipeados a mano (0.0212766,
     0.0144377, 0.0265487, 0.0524581, n1(51)): ahora se calculan de los datos,
     y se muestra el valor original al lado para chequear.
  8. Bloque repetido de logit/reg (outcome_itt / outcome_de_proceso /
     turno_todos x total / no villa / villa) reemplazado por loops.
  9. Rutas por globals (una sola línea a cambiar); tablas y gráficos a $OUT.
 10. table1_mc: se instala si falta. `nbi` y `intervencion_fac`: se chequea
     existencia antes de usarlos.
 11. Comentarios sobre supuestos: pob_c (año/fuente), suest, outcome_turno.
==============================================================================*/


*------------------------------------------------------------------------------
* 0. RUTAS
*------------------------------------------------------------------------------
global BASE  "C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\Documents\IDB\ESW HUD-SPH"
global DATA  "$BASE\Re_ Informe alejandro macchia"
global ESW2  "$BASE\Paper versión final - peer rev\ESW_HUD\ESW version 2\data\processed\df_esw2.dta"
global OUT   "$BASE\Paper versión final - peer rev\ESW_HUD\ESW version 2\output\stata"

cd "$DATA"
capture mkdir "$OUT"

* Paquetes
capture which table1_mc
if _rc ssc install table1_mc, replace

*import excel "datos_matcheados_hta_bid_share", firstrow clear
*rename *, lower
*save base_esw_hud_hnp, replace

**# Cargo base y genero variables clave
use base_esw_hud_hnp, clear
describe

rename id_persona id
destring id, replace
isid id            // falla si hay ids duplicados (el merge 1:1 también fallaría)

* Traer de df_esw2 SOLO lo que no está ya en la base
local trae ""
foreach v in servicio densidad_por_km2_cesac densidad_por_km2_hosp villa nro_com {
    capture confirm variable `v'
    if _rc local trae `trae' `v'
}
display as text "Variables que se traen de df_esw2: `trae'"

if "`trae'" != "" {
    merge 1:1 id using "$ESW2", keepusing(`trae')
    tab _merge
    * _merge==2: pacientes que están en df_esw2 pero NO en la base experimental
    * -> no pertenecen a la muestra; si se dejan, entran con outcomes = 0.
    drop if _merge == 2
    * _merge==1: pacientes de la base sin match en df_esw2 (sin villa/densidad/
    * servicio). Se conservan para el ITT total, pero se informan.
    count if _merge == 1
    display as text "Pacientes de la base sin match en df_esw2: " r(N)
    drop _merge
}

*------------------------------------------------------------------------------
* Variables de outcome
*------------------------------------------------------------------------------
* turno_todos: cualquier turno (servicio informado)
gen turno_todos = (servicio != "")
* OJO: un servicio vacío puede significar "sin turno" O "sin match en df_esw2".
* Para los sin match, turno_todos queda 0 por construcción (ver count de arriba).

* turno_relevante: turno en un servicio relacionado con HTA
gen turno_relevante = 0
foreach s in "CLINICA MEDICA" "MEDICINA FAMILIAR" "CARDIOLOGIA"   ///
             "DIVISION CARDIOLOGIA" "DIVISION CLINICA MEDICA"    ///
             "HIPERTENSION ARTERIAL" "UNIDAD CARDIOLOGIA"        ///
             "SALUD INTEGRAL" "UNIDAD CLINICA MEDICA" {
    replace turno_relevante = 1 if servicio == "`s'" & !missing(servicio)
}

/*
* --- Outcome ITT (referencia: así se construye outcome_itt en la base) ---
* 1 si turno_relevante == 1 OR RESPONDEDOR_EFECTIVO == 1, para ramas 0 y 1
gen outcome_itt_calc = .
replace outcome_itt_calc = 1 if inlist(Intervencion, 0, 1) & ///
    (turno_relevante == 1 | RESPONDEDOR_EFECTIVO == 1)
replace outcome_itt_calc = 0 if inlist(Intervencion, 0, 1) & ///
    missing(outcome_itt_calc)
*/

* outcome_turno es un alias de outcome_itt (NO de turno_relevante)
gen outcome_turno = outcome_itt

* Sexo: 0 = Masculino, 1 = Femenino (sin encode, para no depender del orden)
tab genero, missing
gen sexo = .
replace sexo = 0 if genero == "Masculino"
replace sexo = 1 if genero == "Femenino"
count if missing(sexo) & !missing(genero)
if r(N) > 0 display as error "Hay valores de genero distintos de Masculino/Femenino: revisar tab de arriba"
label define sexo 0 "Masculino" 1 "Femenino", replace
label values sexo sexo

* intervencion_fac (si no existe, se crea desde intervencion)
capture confirm variable intervencion_fac
if _rc {
    label define interv_lbl 0 "Control" 1 "Tratamiento", replace
    gen intervencion_fac = intervencion
    label values intervencion_fac interv_lbl
}

tab intervencion outcome_itt, missing

**# Estadísticos descriptivos de balance entre grupos
table () (intervencion_fac), stat(mean villa edad sexo)
table (villa) (intervencion_fac), stat(mean outcome_itt outcome_de_proceso)

table1_mc, by(intervencion) ///
    vars(sexo bin \ edad contn \ villa bin \ dbt bin \ dias_desde_ultima_consulta conts) ///
    onecol nospace test statistic saving("$OUT\tabla_bs.xlsx", replace)

* Sin diferencias en caracterización sociodemográfica: la aleatorización
* parece bien hecha.

**# Estimación de OR y AME
* OR (total, no villa, villa)
logit outcome_turno i.intervencion, r or
logit outcome_turno i.intervencion if villa == 0, or r
logit outcome_turno i.intervencion if villa == 1, or r

* AME (total y por villa)
logit outcome_turno i.intervencion, r
margins, dydx(intervencion)

logit outcome_turno i.intervencion##i.villa, r
margins villa, dydx(intervencion)

* MPL
reg outcome_itt i.intervencion, r
reg outcome_itt i.intervencion##i.villa, r
reg outcome_itt i.intervencion if villa == 0, r
reg outcome_itt i.intervencion if villa == 1, r

**# Potencia: ¿qué n se necesita para que el efecto en villas sea significativo?
* Se calculan de los datos (antes estaban tipeados a mano).
quietly {
    reg outcome_itt i.intervencion if villa == 1, r
    scalar ate_v = _b[1.intervencion]
    summarize outcome_itt if villa == 1 & intervencion == 0
    scalar p0_v  = r(mean)
    scalar n0_v  = r(N)

    reg outcome_itt i.intervencion if villa == 0, r
    scalar ate_nv = _b[1.intervencion]
    summarize outcome_itt if villa == 0 & intervencion == 0
    scalar p0_nv = r(mean)
}
display as text "Villa:    p_control = " %9.7f p0_v  "  ATE = " %9.7f ate_v  "  n_control = " n0_v
display as text "No villa: p_control = " %9.7f p0_nv "  ATE = " %9.7f ate_nv
display as text "(Valores originales tipeados: villa 0.0212766 / 0.0144377 ; no villa 0.0265487 / 0.0524581 ; n1=51)"

* (a) n necesario para detectar el efecto observado en villa (poder 80%)
power twoproportions `=p0_v', diff(`=ate_v')

* (b) poder según n por grupo, con el efecto observado en villa
power twoproportions `=p0_v', n1(50 100) power(0.8)

* (c) Si el verdadero efecto fuera el de no villa: ¿qué poder tiene una muestra
*     del tamaño de la de villas?
power twoproportions `=p0_nv', diff(`=ate_nv')
power twoproportions `=p0_nv', diff(`=ate_nv') n1(`=n0_v')

**# Diferencias entre villa y no villa
table (villa), stat(mean intervencion sexo edad dias_desde_ultima_consulta)
table (villa) (intervencion_fac), stat(mean sexo edad dias_desde_ultima_consulta)

table1_mc, by(villa) ///
    vars(sexo bin \ edad contn \ intervencion bin \ dbt bin \ dias_desde_ultima_consulta conts) ///
    onecol nospace test statistic saving("$OUT\tabla_villa.xlsx", replace)

* Días desde la última consulta según villa
reg dias_desde_ultima_consulta villa sexo c.edad##c.edad dbt, r
reg dias_desde_ultima_consulta i.villa##i.dbt sexo c.edad##c.edad, r

**# Efecto de la intervención: outcome_itt, outcome_de_proceso y turno_todos
* (reemplaza los bloques repetidos; mismas especificaciones que antes)
foreach y in outcome_itt outcome_de_proceso turno_todos {
    display as result _n "===== `y' ====="
    foreach cond in "" "if villa == 0" "if villa == 1" {
        display as text _n "--- `y' `cond' ---"
        logit `y' i.intervencion `cond', r or
        reg   `y' i.intervencion `cond', r
    }
}

tabstat outcome_itt outcome_de_proceso, by(intervencion) stat(mean n)
tabstat outcome_itt outcome_de_proceso if villa == 1, by(intervencion) stat(mean n)

**# Heterogeneidad por densidad de CeSAC
reg outcome_itt i.intervencion##c.densidad_por_km2_cesac, r
summarize densidad_por_km2_cesac, detail
tabstat densidad_por_km2_cesac, by(villa) stat(n mean sd min p50 max)

*------------------------------------------------------------------------------
* Programa para el gráfico de AME por densidad (antes copiado 3 veces)
*------------------------------------------------------------------------------
capture program drop plot_ame
program define plot_ame
    args archivo
    margins, at(densidad_por_km2_cesac = (0.1(0.05)0.6)) dydx(intervencion) level(90)
    marginsplot,                                                         ///
        recast(line) recastci(rarea)                                     ///
        ci1opt(color("grey%30") lwidth(none))                            ///
        plot1opts(lcolor("black") lwidth(medium))                        ///
        yline(0, lpattern(dash) lcolor(black%70) lwidth(thin))           ///
        ytitle("AME", size(small))                                       ///
        xtitle("CeSAC por km2", size(small))                             ///
        title("") xlab(, labsize(small))                                 ///
        note("Línea: AME de la intervención. Área sombreada: IC 90%.", size(vsmall)) ///
        legend(off) graphregion(color(white))
    graph export "$OUT/`archivo'.pdf", replace
end

* (1) cuadrática en densidad, no villa
reg outcome_itt i.intervencion##c.densidad_por_km2_cesac##c.densidad_por_km2_cesac ///
    if villa == 0, r
plot_ame ame_densidad_cuad

*------------------------------------------------------------------------------
* Población por comuna (control)
*------------------------------------------------------------------------------
* Número de comuna: de nro_com si existe; si no, se extrae de `comuna`.
* (Antes se matcheaba el string "Comuna Nº N": el "º" rompe según codificación
*  y pob_c quedaba missing en silencio.)
capture drop comuna_n
gen comuna_n = .
capture confirm variable nro_com
if !_rc {
    capture confirm numeric variable nro_com
    if !_rc replace comuna_n = nro_com
    else    replace comuna_n = real(regexs(1)) if regexm(nro_com, "([0-9]+)")
} else {
    capture confirm string variable comuna
    if !_rc replace comuna_n = real(regexs(1)) if regexm(comuna, "([0-9]+)")
}
tab comuna_n, missing

* ATENCIÓN: verificar año/fuente. Estos valores suman 2.870.151, por debajo
* de los ~3,1 M del Censo 2022 que usa el resto del pipeline (Redatam). Si el
* paper usa Censo 2022, reemplazar por total_pers de cpe_comuna.csv.
local pob 205886 159594 188138 217805 179005 219736 218245 187237 161250 ///
          166022 190965 200116 229626 224093 122433
gen long pob_c = .
local i = 1
foreach p of local pob {
    replace pob_c = `p' if comuna_n == `i'
    local ++i
}
count if missing(pob_c) & villa == 0
if r(N) > 0 display as error "pob_c missing en " r(N) " casos no-villa: revisar comuna_n"

* (2) cuadrática + población de la comuna (control lineal)
reg outcome_itt i.intervencion##c.densidad_por_km2_cesac##c.densidad_por_km2_cesac ///
    c.pob_c if villa == 0, r
plot_ame ame_densidad_cuad_pob

* (3) densidad x población (interacción triple)
reg outcome_itt i.intervencion##c.densidad_por_km2_cesac##c.pob_c if villa == 0, r
plot_ame ame_densidad_pob

reg outcome_itt densidad_por_km2_cesac c.pob_c if villa == 0, r

**# Turnos fuera de HTA vs. turnos relevantes
* turno_nohta = turno en servicios NO relacionados con HTA.
* La definición anterior (turno_todos - outcome_itt) daba -1 para quien tiene
* outcome_itt = 1 por ser "respondedor efectivo" sin turno.
count if turno_todos - outcome_itt < 0
display as text "Casos con turno_todos - outcome_itt < 0 (definición anterior): " r(N)
gen turno_nohta = turno_todos - turno_relevante

foreach y in turno_nohta turno_todos outcome_itt {
    reg   `y' i.intervencion if villa == 0, r
    logit `y' i.intervencion if villa == 0, r or
}

**# Test de igualdad de efectos: turno_todos vs outcome_itt (no villa)
* suest combina los dos modelos (misma muestra) y permite testear la igualdad
* de coeficientes entre ecuaciones. Los EE de suest son robustos por defecto.
quietly reg turno_todos i.intervencion if villa == 0
estimates store m1
quietly reg outcome_itt i.intervencion if villa == 0
estimates store m2
suest m1 m2
test [m1_mean]1.intervencion = [m2_mean]1.intervencion

**# Días desde la última consulta y NBI
capture confirm variable nbi
if _rc display as error "La variable nbi no existe en la base: traerla de df_esw2 (keepusing) antes de esta regresión."
else   reg dias_desde_ultima_consulta i.villa##c.nbi, r







