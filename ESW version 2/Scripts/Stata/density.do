


	summarize dias_ult_cons, detail
local p10 = r(p10)
local p90 = r(p90)
kdensity dias_ult_cons if villa==0 & dias_ult_cons >= `p10' & dias_ult_cons <= `p90', generate(x0 d0) nograph
kdensity dias_ult_cons if villa==1 & dias_ult_cons >= `p10' & dias_ult_cons <= `p90', generate(x1 d1) nograph
gen a=0
* Gráfico con áreas superpuestas
twoway ///
    (rarea d0 a x0, color(blue%30)) ///
    (rarea d1 a x1, color(maroon%40)) ///
    (line d0 x0, lcolor(navy)) ///
    (line d1 x1, lcolor(maroon)) ///
    , legend(order(1 "No villa" 2 "Villa")) ytit("") ylab("") xtitle("Días desde la última consulta por HTA" "(condicional a la ausencia de retiro de medicación)" , size(small))
	
	
	
	summarize dias_ult_cons, detail
local p10 = r(p10)
local p90 = r(p90)
kdensity dias_ult_cons if acc_dico==1 & dias_ult_cons >= `p10' & dias_ult_cons <= `p90', generate(a1 z1) nograph
kdensity dias_ult_cons if acc_dico==2 & dias_ult_cons >= `p10' & dias_ult_cons <= `p90', generate(a2 z2) nograph
kdensity dias_ult_cons if acc_dico==3 & dias_ult_cons >= `p10' & dias_ult_cons <= `p90', generate(a3 z3) nograph
kdensity dias_ult_cons if acc_dico==4 & dias_ult_cons >= `p10' & dias_ult_cons <= `p90', generate(a4 z4) nograph

* Gráfico con áreas superpuestas
twoway ///
    (rarea z1 a a1, color(navy%30)) ///
    (rarea z2 a a2, color(maroon%30)) ///
	(rarea z3 a a3, color(red%30)) ///
    (rarea z4 a a4, color(yellow%30)) ///
    (line z1 a1, lcolor(navy)) ///
    (line z2 a2, lcolor(maroon)) ///
	(line z3 a3, lcolor(red)) ///
    (line z4 a4, lcolor(yellow)), legend(order(1 "Villa alto NBI " 2 "Villa bajo NBI" 3 "No villa alto NBI" 4 "No villa bajo NBI"))
