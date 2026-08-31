

/*ESW HUD-HNP

Titulo: Estimación del efecto de la accesibilidad urbana sobre el tratamiento en hipertensión. 
Autor: Nicolás García Balus (NICOLASGA@IADB.ORG // nicolas.gbalus@gmail.com)
*/


**# Importación y consistencia de la base de datos generada con datos censales, datos abiertos GCBA y datos de la HCE de CABA. 
cd"C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\Documents\IDB\ESW HUD-SPH\ESW version 2\Data frames consolidados"

*import delimited "C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\Documents\IDB\ESW HUD-SPH\ESW version 2\Data frames consolidados\df_esw.csv", clear 
*save df_esw.dta , replace 

global ruta "C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\Documents\IDB\ESW HUD-SPH\ESW version 2"

use df_esw, clear 
describe

ds, has(type string)
foreach var of varlist `r(varlist)' {
    replace `var' = "" if `var' == "NA"
}
destring *, replace 
do  "${ruta}\Scripts\labels_variables"
do  "${ruta}\Scripts\generar_pct_cpe_c"

*Ponderación por fracción de CPE 
gen w=1 
replace w=pct_cpe_c/100 if villa==0

encode genero, gen(sexo)

codebook*, compact
desc pct_*_v

rename pct_agua_corriente       pct_ac_c
rename pct_calidad_mat          pct_calmat_c
rename pct_clima_educ_bajo      pct_educ_bajo_c
rename pct_sec_completa         pct_sec_c
rename pct_hogares_nbi          pct_nbi_c
rename pct_hogares_hacinamiento pct_hacin_c
rename pct_hogares_cond_san     pct_cond_san_c


* ── 2. RENOMBRE VARIABLES DE VILLA (_v) ──────────────────

rename pct_agua_corriente_v     pct_ac_v
rename pct_calidad_mat_v        pct_calmat_v
rename pct_clima_educ_bajo_v    pct_educ_bajo_v
rename pct_nbi_v                pct_nbi_v        // ya tiene nombre correcto
rename pct_sec_completa_v       pct_sec_v
rename pct_hacinamiento_v       pct_hacin_v
rename pct_edad_65_mas_v        pct_edad65_v     // solo existe en _v, sin par en _c
rename pct_cond_san_a_v         pct_cond_san_v // sin par directo en _c


* ── 3. VARIABLES UNIFICADAS (_t) ─────────────────────────
* Criterio: si existe valor de villa (_v), usarlo; si no, usar el de comuna (_c)
* Equivalente al coalesce() de R: prioriza la fuente más específica al individuo

* Pares completos (tienen _c y _v)
gen pct_ac_t        = cond(!missing(pct_ac_v),        pct_ac_v,        pct_ac_c)
gen pct_calmat_t    = cond(!missing(pct_calmat_v),    pct_calmat_v,    pct_calmat_c)
gen pct_educ_bajo_t = cond(!missing(pct_educ_bajo_v), pct_educ_bajo_v, pct_educ_bajo_c)
gen pct_sec_t       = cond(!missing(pct_sec_v),       pct_sec_v,       pct_sec_c)
gen pct_nbi_t       = cond(!missing(pct_nbi_v),       pct_nbi_v,       pct_nbi_c)
gen pct_hacin_t     = cond(!missing(pct_hacin_v),     pct_hacin_v,     pct_hacin_c)

gen pct_cond_san_t  = cond(!missing(pct_cond_san_v), ///
                           pct_cond_san_v , ///
                           pct_cond_san_c)

*Genero indice de privaciones basadas en los datos censales 

gen mala_acc=((100-pct_calmat_t)+pct_educ_bajo_t+pct_hacin_t)/300 
replace mala_acc=0 if mala_acc<0

bysort villa: sum mala_acc,d 
sum mala_acc if villa==1 ,d 
gen acc_dico=.
replace acc_dico=1 if villa==1 & mala_acc>`r(p50)'
replace acc_dico=2 if villa==1 & mala_acc<=`r(p50)'
sum mala_acc if villa==0 ,d 
replace acc_dico=3 if villa==0 & mala_acc>`r(p50)'
replace acc_dico=4 if villa==0 & mala_acc<=`r(p50)'

label define acc_dico 1"Villa mala" 2"Villa buena" 3"NoVilla mala" 4"NoVilla buena" 
label values acc_dico acc_dico
tab acc_dico

sum mala_acc ,d 
gen acc_d2=0 
replace acc_d2=1 if mala_acc>`r(p50)'


*Distirbución de días 

sum dias_ult_cons,d 
gen l_dias_ultcons=ln(dias_ult_cons)

sum dias_dispensa,d 
replace dias_dispensa=. if dias_dispensa<200 
gen l_d_dispensa=ln(dias_dispensa)


**# Resultados preliminares 
tabstat dias_ult_cons l_dias_ultcons, by(acc_dico )


reg dias_ult_cons ib4.acc_dico [aw=w]
reg l_dias_ultcons ib4.acc_dico [aw=w]

*Factores sociodemográficos
reg l_dias_ultcons ib4.acc_dico i.sexo c.edad##c.edad [aw=w],r 
reg dias_ult_cons ib4.acc_dico i.sexo c.edad##c.edad  [aw=w],r 


reg l_dias_ultcons i.villa##i.acc_d2 i.sexo c.edad##c.edad [aw=w],r 
margins villa, dydx(acc_d2)


*Factores sociodemográficos + controles censo 
reg l_dias_ultcons i.villa i.sexo c.edad##c.edad [aw=w],r 
reg l_dias_ultcons i.villa i.sexo c.edad##c.edad pct_educ_bajo_t pct_calmat_t pct_nbi_t [aw=w],r 
reg l_dias_ultcons i.villa i.sexo c.edad##c.edad pct_educ_bajo_t pct_calmat_t pct_nbi_t t_ors_p_cesac [aw=w],r 
reg l_dias_ultcons i.villa i.sexo c.edad##c.edad pct_educ_bajo_t pct_calmat_t pct_nbi_t d_eucl_cesac[aw=w],r 



*Factores sociodemograficos + tiempo al cesac 
reg l_dias_ultcons ib4.acc_dico i.sexo c.edad##c.edad t_transp_cesac [aw=w]
reg l_dias_ultcons ib4.acc_dico##c.t_transp_cesac i.sexo c.edad##c.edad  [aw=w]
reg l_dias_ultcons ib4.acc_dico i.sexo c.edad##c.edad t_transp_hosp [aw=w]

reg l_dias_ultcons i.villa##c.pct_nbi_t c.t_transp_hosp  i.sexo c.edad##c.edad  [aw=w]
reg l_dias_ultcons i.villa##c.pct_cond_san_t c.t_transp_hosp  i.sexo c.edad##c.edad  [aw=w]
reg l_dias_ultcons i.villa##c.pct_educ_bajo_t c.t_transp_hosp  i.sexo c.edad##c.edad  [aw=w]
reg l_dias_ultcons i.villa##c.pct_calmat_t c.t_transp_hosp  i.sexo c.edad##c.edad  [aw=w]

**************************************************
**************************************************
*ESPECIFICACION QUE SIRVE 
gen mala_const=0 
replace mala_const =1 if pct_calmat_t<60
label define mala_const 0"Buenos materiales" 1"Materiales precarios" 
label values mala_const mala_const

reg l_dias_ultcons i.villa##i.mala_const i.sexo  c.edad##c.edad [aw=w],r
margins villa, dydx(mala_const)

reg l_dias_ultcons i.villa##i.mala_const i.sexo  c.edad##c.edad t_ors_p_cesac [aw=w],r
margins villa, dydx(mala_const)

*****************************************************

/*Factores sociodemograficos + densidad de cesac en cercanía  - REVIAR HAY ERROR 
reg l_dias_ultcons ib4.acc_dico i.sexo c.edad##c.edad densidad_por_km2_cesac [aw=w]
reg l_dias_ultcons ib4.acc_dico i.sexo c.edad##c.edad densidad_por_100k_hab_cesac [aw=w]
*/ 
/*
reg l_dias_ultcons ib4.acc_dico i.sexo c.edad##c.edad [aw=w],r 
est store r_main

coefplot r_main, keep(*.acc_dico) baselevels note("Nota:CI [95%]", size(small)) title("Efecto marginal de la accesibilidad urbana", size(medsmall)) ylab(, labsize(small)) xline(0, lcolor(balck)) xtit("Efecto porcentual sobre el rezago en los días desde la última consulta médica por HTA", size(small))


logit villa dbt i.sexo c.edad##c.edad  dias_dispensa 
predict pvilla, pr 
gen ipw=1/pvilla 
replace ipw = 1 if villa ==1  
*/

tabstat pct_*_t, by(villa)

reg l_dias_ultcons i.villa##c.pct_calmat_t i.sexo  c.edad##c.edad t_ors_p_cesac [aw=w],r




tab mala_const villa, nof cell

tabstat mala_acc, by(villa)
sum mala_acc if villa==0,d 

gen acc_alt_d=0
replace acc_alt_d=1 if mala_acc>0.25
tab acc_alt_d villa, nof cell 



reg l_dias_ultcons i.villa##c.t_ors_p_cesac i.mala_const i.sexo  c.edad##c.edad  [aw=w],r

reg l_dias_ultcons i.villa##c.t_ors_p_cesac i.sexo c.edad##c.edad  [aw=w],r
reg l_dias_ultcons i.villa##c.t_ors_p_hosp i.sexo c.edad##c.edad  [aw=w],r

reg l_dias_ultcons i.villa##c.d_ors_p_cesac i.sexo c.edad##c.edad  [aw=w],r


 gen turno_ok=0
 replace turno_ok=1 if servicio!=""
 
tab turno_ok villa, nof cell 
logit turno_ok i.villa##i.mala_const , r 
margins villa, dydx(mala_const)

logit turno_ok i.villa  i.sexo c.edad d_eucl_hosp , r 

sum dias_ult_cons if villa==0
local nv_m=`r(mean)'
sum dias_ult_cons if villa==1
local v_m=`r(mean)'
sum dias_ult_cons,d 
cdfplot dias_ult_cons if dias_ult_cons>= `r(p5)' &  dias_ult_cons<=`r(p95)'  , by(villa) legend(order(1 "No villa" 2 "Villa" )) xline(`nv_m') xline(`v_m')


sum l_dias_ultcons if villa==0
local nv_m=`r(mean)'
sum l_dias_ultcons if villa==1
local v_m=`r(mean)'
sum l_dias_ultcons,d 
cdfplot l_dias_ultcons if l_dias_ultcons>= `r(p25)' &  l_dias_ultcons<=`r(p75)'  , by(villa) legend(order(1 "No villa" 2 "Villa" )) xline(`nv_m') xline(`v_m')


sum dias_ult_cons if villa==0
local nv_m=`r(mean)'
sum dias_ult_cons if villa==1
local v_m=`r(mean)'
sum dias_ult_cons,d 
cdfplot dias_ult_cons if dias_ult_cons>= `r(p10)' &  dias_ult_cons<=`r(p90)'  , by(acc_dico)  xline(`nv_m') xline(`v_m')

reg l_dias_ultcons i.villa##c.t_ors_p_cesac i.sexo c.edad##c.edad    [aw=w],r 
margins villa, dydx(t_ors_p_cesac)















gen t_c_cat=. 
replace t_c_cat=1 if t_ors_p_cesac<=5
replace t_c_cat=2 if t_ors_p_cesac>5 & t_ors_p_cesac<=10 
replace t_c_cat=3 if t_ors_p_cesac>10














logit villa dbt i.sexo c.edad##c.edad  
predict pvilla, pr 
gen ipw=1/pvilla 
replace ipw = 1/(1-pvilla) if villa ==1  

reg l_dias_ultcons i.villa##c.t_ors_p_cesac##c.d_eucl_cesac i.sexo c.edad##c.edad   [aw=w*ipw],r 
margins  villa , at(t_ors_p_cesac=(1(2)30)) 
marginsplot, noci 

reg dias_ult_cons i.villa##c.t_ors_p_cesac##c.d_eucl_cesac i.sexo c.edad##c.edad   [aw=w*ipw],r 
margins  villa , dydx(t_ors_p_cesac)

reg l_dias_ultcons i.villa##c.t_ors_p_cesac##c.t_ors_p_cesac##c.t_ors_p_cesac##c.d_eucl_cesac i.sexo c.edad##c.edad   [aw=w*ipw],r 
margins  villa , dydx(t_ors_p_cesac)


reg l_dias_ultcons i.villa##c.t_ors_p_cesac##c.t_ors_p_cesac##c.t_ors_p_cesac##c.d_eucl_cesac i.sexo c.edad##c.edad   [aw=w*ipw],r 
margins  villa , at(t_ors_p_cesac=(1(2)30)) 
marginsplot, noci 

reg l_dias_ultcons i.villa##c.t_ors_p_cesac##c.t_ors_p_cesac##c.d_eucl_cesac i.sexo c.edad##c.edad   [aw=w*ipw],r 
margins  villa , at(t_ors_p_cesac=(1(2)30)) 
marginsplot, noci legend(order (1 "No villa" 2 "Villa"))


reg l_dias_ultcons i.villa##c.t_ors_p_cesac##c.t_ors_p_cesac##c.d_eucl_cesac i.sexo c.edad##c.edad   [aw=w],r 
margins  , at(t_ors_p_cesac=(1(2)30)) dydx(villa)
marginsplot, level(90) note("Nota: CI [90%]") ytit("Incremento porcentual del tiempo al CeSAC", size(small)) xtit("Tiempo a pie al CeSAC más cercano (minutos)", size(small)) title("") xlab(, labsize())

reg l_dias_ultcons i.villa##c.t_ors_p_cesac##c.t_ors_p_cesac##c.d_eucl_cesac i.sexo c.edad##c.edad   [aw=w],r 
margins  , at(t_ors_p_cesac=(1(1)25)) dydx(villa)
marginsplot, ///
    recast(line) recastci(rarea)                                         ///
    ci1opt(color("grey%10") lwidth(none) label("IC[90%]"))                           ///
    plotopts(lcolor("black") lwidth(medium))                        ///
    yline(0, lpattern(dash) lcolor(black%70) lwidth(thin))                 ///
    ytitle("Incremento porcentual del tiempo al CeSAC", size(small))     ///
    xtitle("Tiempo a pie al CeSAC más cercano (minutos)", size(small))   ///
    title("")                                                             ///
    xlab(, labsize(small))                                               ///
    note("Nota: CI [90%]", size(vsmall))                                 ///
    graphregion(color(white)) level(90) 


marginsplot, ///
    recast(line) recastci(rarea)                                         ///
    ci1opt(color("grey%10") lwidth(none))                                ///
    plot1opts(lcolor("black") lwidth(medium))                            ///
    yline(0, lpattern(dash) lcolor(black%70) lwidth(thin))               ///
    ytitle("Incremento porcentual del tiempo al CeSAC", size(small))     ///
    xtitle("Tiempo a pie al CeSAC más cercano (minutos)", size(small))   ///
    title("")                                                             ///
    xlab(, labsize(small))                                               ///
    graphregion(color(white)) level(90)                                  ///
    addplot(scatter b x if x==., msymbol(square) mcolor("grey%10")      ///
            msize(large) legend(label(3 "IC [90%]")))                    ///
    legend(order(1 "IC [90%]")    pos("bottom")                   ///
           rows(1) size(small) region(lwidth(none)))






