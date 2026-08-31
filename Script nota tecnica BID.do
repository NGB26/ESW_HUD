
/*
Nota técnica ESW HUD - HNP

Autor: Nicolás García Balus en base a Alejandro Macchia (nicolas.gbalus@gmail.com / nicolasga@iadb.org)
Fecha: 8/5/2026
*/

cd "C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\Documents\IDB\ESW HUD-SPH\Re_ Informe alejandro macchia"

*import excel "datos_matcheados_hta_bid_share", firstrow clear
*rename *, lower 
*save base_esw_hud_hnp, replace 

**# Cargo base y genero variables clave 

use base_esw_hud_hnp, clear 
desc 


rename id_persona id 
destring id, replace 
merge 1:1 id using "C:\Users\NICOLASGA\OneDrive - Inter-American Development Bank Group\Documents\IDB\ESW HUD-SPH\ESW version 2\Data frames consolidados\df_esw2.dta", keepusing(servicio densidad_por_km2_cesac densidad_por_km2_hosp villa nro_com)
drop if _merge!=3 

gen turno_todos=0 
replace turno_todos=1 if servicio!=""


*Genero variable de outcome de proceso 
gen turno_relevante = 0
foreach s in "CLINICA MEDICA" "MEDICINA FAMILIAR" "CARDIOLOGIA" ///
             "DIVISION CARDIOLOGIA" "DIVISION CLINICA MEDICA"    ///
             "HIPERTENSION ARTERIAL" "UNIDAD CARDIOLOGIA"        ///
             "SALUD INTEGRAL" "UNIDAD CLINICA MEDICA" {
    replace turno_relevante = 1 if servicio == "`s'" & !missing(servicio)
}

/*
* --- Outcome ITT ---
* 1 si turno_relevante == 1 OR RESPONDEDOR_EFECTIVO == 1, para ramas 0 y 1
gen outcome_itt_calc = .
replace outcome_itt_calc = 1 if inlist(Intervencion, 0, 1) & ///
    (turno_relevante == 1 | RESPONDEDOR_EFECTIVO == 1)
replace outcome_itt_calc = 0 if inlist(Intervencion, 0, 1) & ///
    missing(outcome_itt_calc)
*/
gen outcome_turno=outcome_itt 

encode genero, gen(sexo)
replace sexo=0 if sexo==2 
label define sexo 0"Masculino" 1"Femenino"
label values sexo sexo 

**# Estadísticos descriptivos de balance entre grupos 
table () (intervencion_fac), stat(mean villa edad sexo)
table (villa) (intervencion_fac), stat(mean outcome_itt outcome_de_proceso)


*ssc install table1_mc, replace
table1_mc, by(intervencion) vars(sexo bin \ edad contn \  villa bin \ dbt bin \ dias_desde_ultima_consulta conts) onecol nospace test statistic saving("tabla_bs.xlsx", replace)

*No hay diferencias en variables de caracterización sociodemográfica. La aleatorizacion está bien hecha. 

**# Estimación de OR y AME 

*Estimación de OR para total y efecto en villas 
logit outcome_turno i.intervencion,r or 
logit outcome_turno i.intervencion if villa==0,  or r 
logit outcome_turno i.intervencion if villa==1,  or r 

*Estimación de AME para total y efecto en villas 
logit outcome_turno i.intervencion,r or 
margins, dydx(intervencion)

logit outcome_turno i.intervencion##i.villa, or r 
margins villa, dydx(intervencion)

*Estimacion de MPL 
reg outcome_itt i.intervencion,r 
reg outcome_itt i.intervencion##i.villa,r
 
reg outcome_itt i.intervencion if villa==0,r 
reg outcome_itt i.intervencion if villa==1,r 

*Estimacion de potencia necesaria para que el efecto de villa sea significativo 

power twoproportions 0.0212766, diff(0.0144377) 

power twoproportions 0.0212766, n1(50 100) power(0.8)


*Asumiendo que el efecto verdadero es el de no villa, en una muestra del tamaño de villas cual es mi potencia? 

power twoproportions 0.0265487, diff(0.0524581) 
power twoproportions 0.0265487, diff(0.0524581) n1(51)

**# Diferencias entre villa y no villa 

*Diferencia en características 
table (villa) , stat(mean intervencion sexo edad dias_desde_ultima_consulta)
table (villa) (intervencion_fac) , stat(mean sexo edad dias_desde_ultima_consulta)

table1_mc, by(villa) vars(sexo bin \ edad contn \  intervencion bin \ dbt bin \ dias_desde_ultima_consulta conts) onecol nospace test statistic saving("tabla_villa.xlsx", replace)

*Diferencia en cantidad de días desde la última consulta según villa 

reg dias_desde_ultima_consulta villa sexo c.edad##c.edad dbt , r 
reg dias_desde_ultima_consulta i.villa##i.dbt sexo c.edad##c.edad  , r 



logit outcome_de_proceso i.intervencion,r or 
reg outcome_de_proceso i.intervencion,r 


logit outcome_itt i.intervencion ,r or 
reg outcome_itt i.intervencion ,r 
logit outcome_de_proceso i.intervencion ,r or 
reg outcome_de_proceso i.intervencion ,r 


logit outcome_itt i.intervencion if villa==1 ,r or 
reg outcome_itt i.intervencion if villa==1 ,r 
logit outcome_de_proceso i.intervencion if villa==1 ,r or 
reg outcome_de_proceso i.intervencion if villa==1 ,r 

tabstat outcome_itt outcome_de_proceso, by(intervencion)
tabstat outcome_itt outcome_de_proceso if villa==1 , by(intervencion)



*Robustez con turno todos 
logit turno_todos i.intervencion ,r or 
reg turno_todos i.intervencion if villa==0 ,r 
reg outcome_itt i.intervencion if villa==0 ,r 

		*Villa
logit turno_todos i.intervencion if villa==1 ,r or 
reg turno_todos i.intervencion if villa==1 ,r 


reg outcome_itt i.intervencion##c.densidad_por_km2_cesac ,r 
sum densidad_por_km2_cesac,d 
tab densidad_por_km2_cesac villa 




reg outcome_itt i.intervencion##c.densidad_por_km2_cesac##c.densidad_por_km2_cesac if villa==0 ,r 
margins, at(densidad_por_km2_cesac=(0.1(0.05)0.6)) dydx(intervencion)
marginsplot, ///
    recast(line) recastci(rarea)                                         ///
    ci1opt(color("grey%10") lwidth(none))                                ///
    plot1opts(lcolor("black") lwidth(medium))                            ///
    yline(0, lpattern(dash) lcolor(black%70) lwidth(thin))               ///
    ytitle("AME", size(small))     ///
    xtitle("CeSaC por km2", size(small))   ///
    title("")                                                             ///
    xlab(, labsize(small))                                               ///
    graphregion(color(white)) level(90)                                  ///
    addplot(scatter b x if x==., msymbol(square) mcolor("grey%10")      ///
            msize(large) legend(label(3 "IC [90%]")))                    ///
    legend(order(1 "IC [90%]")    pos("bottom")                   ///
           rows(1) size(small) region(lwidth(none)))



gen long pob_c = .
replace pob_c = 205886 if comuna == "Comuna Nº 1"
replace pob_c = 159594 if comuna == "Comuna Nº 2"
replace pob_c = 188138 if comuna == "Comuna Nº 3"
replace pob_c = 217805 if comuna == "Comuna Nº 4"
replace pob_c = 179005 if comuna == "Comuna Nº 5"
replace pob_c = 219736 if comuna == "Comuna Nº 6"
replace pob_c = 218245 if comuna == "Comuna Nº 7"
replace pob_c = 187237 if comuna == "Comuna Nº 8"
replace pob_c = 161250 if comuna == "Comuna Nº 9"
replace pob_c = 166022 if comuna == "Comuna Nº 10"
replace pob_c = 190965 if comuna == "Comuna Nº 11"
replace pob_c = 200116 if comuna == "Comuna Nº 12"
replace pob_c = 229626 if comuna == "Comuna Nº 13"
replace pob_c = 224093 if comuna == "Comuna Nº 14"
replace pob_c = 122433 if comuna == "Comuna Nº 15"



reg outcome_itt i.intervencion##c.densidad_por_km2_cesac##c.densidad_por_km2_cesac c.pob_c if villa==0 ,r 
margins, at(densidad_por_km2_cesac=(0.1(0.05)0.6)) dydx(intervencion)
marginsplot, ///
    recast(line) recastci(rarea)                                         ///
    ci1opt(color("grey%10") lwidth(none))                                ///
    plot1opts(lcolor("black") lwidth(medium))                            ///
    yline(0, lpattern(dash) lcolor(black%70) lwidth(thin))               ///
    ytitle("AME", size(small))     ///
    xtitle("CeSaC por km2", size(small))   ///
    title("")                                                             ///
    xlab(, labsize(small))                                               ///
    graphregion(color(white)) level(90)                                  ///
    addplot(scatter b x if x==., msymbol(square) mcolor("grey%10")      ///
            msize(large) legend(label(3 "IC [90%]")))                    ///
    legend(order(1 "IC [90%]")    pos("bottom")                   ///
           rows(1) size(small) region(lwidth(none)))

reg outcome_itt i.intervencion##c.densidad_por_km2_cesac##c.pob_c if villa==0 ,r 
margins, at(densidad_por_km2_cesac=(0.1(0.05)0.6)) dydx(intervencion)
marginsplot, ///
    recast(line) recastci(rarea)                                         ///
    ci1opt(color("grey%10") lwidth(none))                                ///
    plot1opts(lcolor("black") lwidth(medium))                            ///
    yline(0, lpattern(dash) lcolor(black%70) lwidth(thin))               ///
    ytitle("AME", size(small))     ///
    xtitle("CeSaC por km2", size(small))   ///
    title("")                                                             ///
    xlab(, labsize(small))                                               ///
    graphregion(color(white)) level(90)                                  ///
    addplot(scatter b x if x==., msymbol(square) mcolor("grey%10")      ///
            msize(large) legend(label(3 "IC [90%]")))                    ///
    legend(order(1 "IC [90%]")    pos("bottom")                   ///
           rows(1) size(small) region(lwidth(none)))

reg outcome_itt densidad_por_km2_cesac c.pob_c if villa==0 ,r 



gen turno_nohta=turno_todos-outcome_itt 

reg turno_nohta i.intervencion if villa==0, r 
reg turno_todos i.intervencion if villa==0, r 
reg outcome_itt i.intervencion if villa==0, r 

logit turno_nohta i.intervencion if villa==0, r or 
logit turno_todos i.intervencion if villa==0, r or 
logit outcome_itt i.intervencion if villa==0, r or 




reg turno_todos i.intervencion if villa==0 
est store m1

reg outcome_itt i.intervencion if villa==0
est store m2

suest m1 m2 

test [m1_mean]1.intervencion = [m2_mean]1.intervencion
