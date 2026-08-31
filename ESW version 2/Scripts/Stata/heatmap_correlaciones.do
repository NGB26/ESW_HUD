/*===========================================================
  HEATMAP DE CORRELACIONES - DATASET HTA CABA
  
  Requiere:
    ssc install heatplot    (Ben Jann, 2019)
    ssc install palettes    (dependencia de heatplot)
    ssc install colrspace   (dependencia de heatplot)

  Referencia:
    Jann, B (2019) heatplot: Stata module to create heat plots 
    and hexagon plots Statistical Software Components S458598
    https://ideasrepecorg/c/boc/bocode/s458598html
===========================================================*/


* ── 0 INSTALAR PAQUETES (correr solo la primera vez) ────

* ssc install heatplot,  replace
* ssc install palettes,  replace
* ssc install colrspace, replace


* ── 1 DEFINIR VARIABLES A INCLUIR ──────────────────────
* Ajustar según el subset analítico que uses

* Variables del análisis principal
local vars_clinicas     dias_ult_cons edad dbt villa intervencion turno
local vars_acceso       t_transp_cmb t_transp_hosp t_transp_cesac ///
                        d_red_cmb d_red_hosp d_eucl_cmb
local vars_socioeco     pct_nbi_t pct_hacin_t pct_sec_t ///
                        pct_educ_bajo_t pct_ac_t pct_calmat_t pct_cond_san_t

local todas_vars `vars_clinicas' `vars_acceso' `vars_socioeco'


* ── 2 CALCULAR MATRIZ DE CORRELACIONES ──────────────────

* Opción A: correlaciones de Pearson (variables continuas)
pwcorr `todas_vars', sig star(005)

* Guardar la matriz para usarla en heatplot
matrix C = r(C)


* ── 3 DEFINIR LABELS CORTOS PARA EL GRÁFICO ────────────
* heatplot usa los nombres de fila/columna de la matriz
* Los reemplazamos con etiquetas legibles

* Renombrar filas y columnas de la matriz con labels cortos
matrix rownames C = ///
    "Días últ consulta"  ///
    "Edad"                ///
    "Diabetes"            ///
    "Villa"               ///
    "Intervención"        ///
    "Turno obtenido"      ///
    "T transp CMB"      ///
    "T transp Hospital" ///
    "T transp CESAC"    ///
    "Dist red CMB"       ///
    "Dist red Hospital"  ///
    "Dist eucl CMB"     ///
    "% NBI"               ///
    "% Hacinamiento"      ///
    "% Sec completa"     ///
    "% Educ bajo"        ///
    "% Agua corriente"    ///
    "% Calidad mat"      ///
    "% Cond sanitarias"

matrix colnames C = ///
    "Días últ consulta"  ///
    "Edad"                ///
    "Diabetes"            ///
    "Villa"               ///
    "Intervención"        ///
    "Turno obtenido"      ///
    "T transp CMB"      ///
    "T transp Hospital" ///
    "T transp CESAC"    ///
    "Dist red CMB"       ///
    "Dist red Hospital"  ///
    "Dist eucl CMB"     ///
    "% NBI"               ///
    "% Hacinamiento"      ///
    "% Sec completa"     ///
    "% Educ bajo"        ///
    "% Agua corriente"    ///
    "% Calidad mat"      ///
    "% Cond sanitarias"


* ── 4 HEATMAP ───────────────────────────────────────────

* Versión 1: con valores de correlación en cada celda
heatplot C,                          ///
    color(RdBu, reverse)             ///  rojo=negativo, azul=positivo
    cuts(-1(01)1)                   ///  escala de -1 a 1
    values( size(vsmall)) /// mostrar r en cada celda
    lower                            ///  solo triángulo inferior
    xscale(alt)                      ///  labels eje X arriba
    xlabel(, angle(45) labsize(vsmall)) ///
    ylabel(, labsize(vsmall) angle(0))  ///
    xtitle("")                       ///
    ytitle("")                       ///
    title("Matriz de correlaciones", size(medium) color(black)) ///
    subtitle("Dataset HTA CABA — cohorte completa (n=7082)", size(small)) ///
    note("Escala: rojo = correlación negativa | azul = correlación positiva" ///
         "* p<005  Pearson", size(vsmall)) ///
    legend(subtitle("r de Pearson", size(vsmall))) ///
    graphregion(color(white)) ///
    name(heatmap_full, replace)

graph export "${ruta}\heatmap_correlaciones.png", ///
    replace width(2400) height(2000)


* ── 5 VERSIÓN SOLO COHORTE MATCHEADA ───────────────────
* Filtrar brazo analítico (intervención + control matcheado)

preserve
    keep if intervencion == 0 | intervencion == 1

    pwcorr `todas_vars', sig
    matrix C_matched = r(C)

    * Copiar los mismos rownames/colnames
    matrix rownames C_matched = ///
        "Días últ consulta"  "Edad"                "Diabetes"            ///
        "Villa"               "Intervención"        "Turno obtenido"      ///
        "T transp CMB"      "T transp Hospital" "T transp CESAC"    ///
        "Dist red CMB"       "Dist red Hospital"  "Dist eucl CMB"     ///
        "% NBI"               "% Hacinamiento"      "% Sec completa"     ///
        "% Educ bajo"        "% Agua corriente"    "% Calidad mat"      ///
        "% Cond sanitarias"

    matrix colnames C_matched = ///
        "Días últ consulta"  "Edad"                "Diabetes"            ///
        "Villa"               "Intervención"        "Turno obtenido"      ///
        "T transp CMB"      "T transp Hospital" "T transp CESAC"    ///
        "Dist red CMB"       "Dist red Hospital"  "Dist eucl CMB"     ///
        "% NBI"               "% Hacinamiento"      "% Sec completa"     ///
        "% Educ bajo"        "% Agua corriente"    "% Calidad mat"      ///
        "% Cond sanitarias"

    heatplot C_matched,                      ///
        color(RdBu, reverse)                 ///
        cuts(-1(01)1)                       ///
        values( size(vsmall))   ///
        lower                                ///
        xscale(alt)                          ///
        xlabel(, angle(45) labsize(vsmall))  ///
        ylabel(, labsize(vsmall) angle(0))   ///
        xtitle("") ytitle("")                ///
        title("Matriz de correlaciones", size(medium) color(black)) ///
        subtitle("Cohorte matcheada (n≈998)", size(small)) ///
        note("Escala: rojo = correlación negativa | azul = correlación positiva" ///
             "Pearson", size(vsmall)) ///
        legend(subtitle("r de Pearson", size(vsmall))) ///
        graphregion(color(white)) ///
        name(heatmap_matched, replace)

    graph export "${ruta}\heatmap_correlaciones_matched.png", ///
        replace width(2400) height(2000)
restore


* ── 6 VARIANTE: SPEARMAN (para variables no normales) ──
* Recomendado si vas a incluir variables ordinales o con distribución asimétrica

spearman `todas_vars', matrix stats(rho)
matrix S = r(Rho)

matrix rownames S = ///
    "Días últ consulta"  "Edad"                "Diabetes"            ///
    "Villa"               "Intervención"        "Turno obtenido"      ///
    "T transp CMB"      "T transp Hospital" "T transp CESAC"    ///
    "Dist red CMB"       "Dist red Hospital"  "Dist eucl CMB"     ///
    "% NBI"               "% Hacinamiento"      "% Sec completa"     ///
    "% Educ bajo"        "% Agua corriente"    "% Calidad mat"      ///
    "% Cond sanitarias"

matrix colnames S = ///
    "Días últ consulta"  "Edad"                "Diabetes"            ///
    "Villa"               "Intervención"        "Turno obtenido"      ///
    "T transp CMB"      "T transp Hospital" "T transp CESAC"    ///
    "Dist red CMB"       "Dist red Hospital"  "Dist eucl CMB"     ///
    "% NBI"               "% Hacinamiento"      "% Sec completa"     ///
    "% Educ bajo"        "% Agua corriente"    "% Calidad mat"      ///
    "% Cond sanitarias"

heatplot S,                              ///
    color(RdBu, reverse)                 ///
    cuts(-1(01)1)                       ///
    values( size(vsmall))   ///
    lower                                ///
    xscale(alt)                          ///
    xlabel(, angle(45) labsize(vsmall))  ///
    ylabel(, labsize(vsmall) angle(0))   ///
    xtitle("") ytitle("")                ///
    title("Matriz de correlaciones (Spearman)", size(medium) color(black)) ///
    subtitle("Dataset HTA CABA — cohorte completa (n=7082)", size(small)) ///
    note("Escala: rojo = correlación negativa | azul = correlación positiva" ///
         "ρ de Spearman", size(vsmall)) ///
    legend(subtitle("ρ de Spearman", size(vsmall))) ///
    graphregion(color(white)) ///
    name(heatmap_spearman, replace)

graph export "${ruta}\heatmap_correlaciones_spearman.png", ///
    replace width(2400) height(2000)
