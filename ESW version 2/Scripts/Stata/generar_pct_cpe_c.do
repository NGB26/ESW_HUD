

gen pct_cpe_c = .

replace pct_cpe_c = 27.93  if nro_com == 1
replace pct_cpe_c =  7.68  if nro_com == 2
replace pct_cpe_c = 20.11  if nro_com == 3
replace pct_cpe_c = 28.54  if nro_com == 4
replace pct_cpe_c = 12.44  if nro_com == 5
replace pct_cpe_c =  7.99  if nro_com == 6
replace pct_cpe_c = 17.50  if nro_com == 7
replace pct_cpe_c = 41.57  if nro_com == 8
replace pct_cpe_c = 22.26  if nro_com == 9
replace pct_cpe_c = 14.67  if nro_com == 10
replace pct_cpe_c = 10.47  if nro_com == 11
replace pct_cpe_c =  8.63  if nro_com == 12
replace pct_cpe_c =  6.09  if nro_com == 13
replace pct_cpe_c =  7.42  if nro_com == 14
replace pct_cpe_c = 13.65  if nro_com == 15


* ── LABEL ────────────────────────────────────────────────

label variable pct_cpe_c "% viviendas con cobertura pública exclusiva por comuna (Censo 2022)"


* ── VERIFICACIÓN ─────────────────────────────────────────

* Confirmar que no quedaron missings en individuos con comuna válida
count if missing(pct_cpe_c) & !missing(nro_com)
if r(N) > 0 {
    di as error "ADVERTENCIA: " r(N) " observaciones con nro_com válido pero sin pct_cpe_c"
    di as error "Revisar si hay valores de nro_com fuera del rango 1-15"
    tab nro_com if missing(pct_cpe_c)
}

* Distribución por comuna
di as text _newline "Distribución de pct_cpe_c por comuna:"
tabstat pct_cpe_c, by(nro_com) stat(mean n) format(%6.2f)
