/*===========================================================
  LABELS DE VARIABLES - DATASET HTA CABA
  Pegar en do-file después de importar el .dta
  
  Convención de sufijos usada en el dataset:
    _hosp   = Hospital general
    _cmb    = Centro de salud (CMB / CeSAC / CESAC)
    _cesac  = CESAC específicamente
    _v      = Variable del barrio/villa (solo para residentes en villas)
    d_eucl_ = Distancia euclidiana (línea recta), en km
    d_red_  = Distancia por red vial, en km
    d_ors_a = Distancia por red vial (ORS, auto), en km
    d_ors_p = Distancia por red vial (ORS, a pie), en km
    t_auto_ = Tiempo en auto (Google Maps), en minutos
    t_transp_ = Tiempo en transporte público (Google Maps), en minutos
    t_ors_a = Tiempo en auto (OpenRouteService), en minutos
    t_ors_p = Tiempo a pie (OpenRouteService), en minutos
    n_      = Conteo absoluto
    pct_    = Porcentaje
===========================================================*/




* ── 1. IDENTIFICACIÓN Y DISEÑO ───────────────────────────

label variable id                  "ID único del paciente"
label variable intervencion        "Brazo de intervención (1=intervención, 0=control)"
label variable grupo_aleat         "Grupo de aleatorización (Precoz/Tardía)"


* ── 2. VARIABLES DEMOGRÁFICAS ────────────────────────────

label variable edad                "Edad en años al momento de inclusión"
label variable edad_deteccion      "Edad en años al momento de detección de HTA"
label variable genero              "Sexo biológico (Femenino/Masculino)"


* ── 3. VARIABLES CLÍNICAS ────────────────────────────────

label variable dbt                 "Diabetes concomitante (1=sí, 0=no)"
label variable alerta_hta          "Tipo de alerta HTA (sin retiro / con retiro)"
label variable fec_detec           "Fecha de detección de HTA en el sistema"
label variable fec_dispensa_hta    "Fecha de última dispensación de medicación HTA"
label variable fec_ult_cons        "Fecha de última consulta registrada en HCE"
label variable fec_corte           "Fecha de corte del seguimiento (oct 2025)"
label variable dias_ult_cons       "Días desde la última consulta hasta el corte"
label variable dias_detec          "Días desde la detección de HTA hasta el corte"
label variable dias_dispensa       "Días desde la última dispensación hasta el corte"


* ── 4. INTERVENCIÓN Y PROCESO ────────────────────────────

label variable fec_turno           "Fecha del turno otorgado (si corresponde)"
label variable servicio            "Servicio donde se otorgó el turno"
label variable respondio_a_invitacion "Paciente respondió al contacto (1=sí)"
label variable accion              "Acción realizada en el contacto (ej: turno, derivación)"
label variable respondedor         "Tipo de respondedor (agente de contacto)"
label variable resp_num            "Respondedor en formato numérico"
label variable resp_efectivo       "Contacto resultó en respuesta efectiva (1=sí)"
label variable turno               "Turno obtenido por cualquier vía - outcome ITT (1=sí)"
label variable turnos_relevantes   "Número de turnos relevantes obtenidos"


* ── 5. VARIABLES GEOGRÁFICAS ─────────────────────────────

label variable comuna              "Comuna CABA (texto)"
label variable nro_com             "Número de comuna CABA (1-15)"
label variable comuna_ok           "Comuna validada/normalizada"
label variable asentamiento        "Nombre del asentamiento informal (si corresponde)"
label variable barrio_norm         "Nombre de barrio normalizado"
label variable villa               "Residente en villa o asentamiento (1=sí, 0=no)"
label variable codigo              "Código censal del radio/comuna"
label variable departamento        "Departamento/sección censal"
label variable area_km2            "Área del radio censal o barrio en km²"
label variable fuente_poblacion    "Fuente del denominador poblacional (barrio/comuna)"


* ── 6. VARIABLES CENSALES DEL RADIO ─────────────────────

label variable total_viv           "Total de viviendas en el radio censal"
label variable n_agua_corriente    "N° viviendas con acceso a agua corriente"
label variable pct_agua_corriente  "% viviendas con acceso a agua corriente"
label variable n_calidad_mat       "N° viviendas con materiales de calidad adecuada"
label variable pct_calidad_mat     "% viviendas con materiales de calidad adecuada"
label variable n_clima_educ_bajo   "N° hogares con clima educativo bajo"
label variable pct_clima_educ_bajo "% hogares con clima educativo bajo"
label variable n_sec_completa      "N° personas con secundario completo"
label variable pct_sec_completa    "% personas con secundario completo"
label variable n_hogares_nbi       "N° hogares con al menos una NBI"
label variable pct_hogares_nbi     "% hogares con al menos una NBI"
label variable n_hogares_hacinamiento   "N° hogares con hacinamiento crítico"
label variable pct_hogares_hacinamiento "% hogares con hacinamiento crítico"
label variable n_hogares_cond_san  "N° hogares con condiciones sanitarias deficientes"
label variable pct_hogares_cond_san "% hogares con condiciones sanitarias deficientes"


* ── 7. VARIABLES DEL BARRIO/VILLA (_v) ───────────────────
* Solo tienen valor para residentes en villas (villa==1)
* El resto tiene missing estructural

label variable id_barrio_v         "ID del barrio/villa (fuente: registro oficial de villas)"
label variable tipo_barrio_v       "Tipo de barrio informal (villa/asentamiento)"
label variable n_radios_v          "N° de radios censales que componen el barrio"
label variable comunas_v           "Comunas que abarca el barrio"
label variable total_viv_v         "Total de viviendas en el barrio informal"
label variable n_agua_corriente_v  "N° viviendas con agua corriente (barrio)"
label variable n_calidad_mat_v     "N° viviendas con materiales adecuados (barrio)"
label variable n_clima_educ_bajo_v "N° hogares con clima educativo bajo (barrio)"
label variable n_nbi_v             "N° hogares con NBI (barrio)"
label variable n_sec_completa_v    "N° personas con secundario completo (barrio)"
label variable n_edad_65_mas_v     "N° personas de 65 años o más (barrio)"
label variable n_hacinamiento_v    "N° hogares con hacinamiento (barrio)"
label variable n_cond_san_a_v      "N° hogares con saneamiento tipo A (barrio)"
label variable n_cond_san_b_v      "N° hogares con saneamiento tipo B (barrio)"
label variable pct_agua_corriente_v  "% viviendas con agua corriente (barrio)"
label variable pct_calidad_mat_v     "% viviendas con materiales adecuados (barrio)"
label variable pct_clima_educ_bajo_v "% hogares con clima educativo bajo (barrio)"
label variable pct_nbi_v             "% hogares con NBI (barrio)"
label variable pct_sec_completa_v    "% personas con secundario completo (barrio)"
label variable pct_edad_65_mas_v     "% personas de 65 años o más (barrio)"
label variable pct_hacinamiento_v    "% hogares con hacinamiento (barrio)"
label variable pct_cond_san_a_v      "% hogares con saneamiento tipo A (barrio)"
label variable pct_cond_san_b_v      "% hogares con saneamiento tipo B (barrio)"


* ── 8. ACCESIBILIDAD - N° DE CENTROS ────────────────────

label variable n_centros_hosp      "N° de hospitales en radio de accesibilidad"
label variable n_centros_cmb       "N° de centros de salud (CMB) en radio de accesibilidad"
label variable n_centros_cesac     "N° de CESAC en radio de accesibilidad"
label variable n_centros_est_sal   "N° de establecimientos de salud en radio de accesibilidad"


* ── 9. ACCESIBILIDAD - DENSIDAD ──────────────────────────

label variable densidad_por_km2_hosp      "Hospitales por km²"
label variable densidad_por_km2_cmb       "Centros de salud (CMB) por km²"
label variable densidad_por_km2_cesac     "CESAC por km²"
label variable densidad_por_km2_est_sal   "Establecimientos de salud por km²"
label variable densidad_por_100k_hab_hosp    "Hospitales por 100.000 habitantes"
label variable densidad_por_100k_hab_cmb     "Centros de salud (CMB) por 100.000 hab."
label variable densidad_por_100k_hab_cesac   "CESAC por 100.000 hab."
label variable densidad_por_100k_hab_est_sal "Establecimientos de salud por 100.000 hab."


* ── 10. DISTANCIAS EUCLIDIANAS (línea recta) ─────────────

label variable d_eucl_hosp         "Distancia euclidiana al hospital más cercano (km)"
label variable d_eucl_cmb          "Distancia euclidiana al CMB más cercano (km)"
label variable d_eucl_cesac        "Distancia euclidiana al CESAC más cercano (km)"


* ── 11. DISTANCIAS POR RED VIAL ─────────────────────────

label variable d_red_hosp          "Distancia por red vial al hospital más cercano (km)"
label variable d_red_cmb           "Distancia por red vial al CMB más cercano (km)"
label variable d_red_cesac         "Distancia por red vial al CESAC más cercano (km)"


* ── 12. TIEMPOS DE VIAJE - GOOGLE MAPS ──────────────────

label variable t_auto_hosp         "Tiempo en auto al hospital más cercano - Google (min)"
label variable t_auto_cmb          "Tiempo en auto al CMB más cercano - Google (min)"
label variable t_auto_cesac        "Tiempo en auto al CESAC más cercano - Google (min)"
label variable t_transp_hosp       "Tiempo en transp. público al hospital - Google (min)"
label variable t_transp_cmb        "Tiempo en transp. público al CMB - Google (min)"
label variable t_transp_cesac      "Tiempo en transp. público al CESAC - Google (min)"


* ── 13. DISTANCIAS Y TIEMPOS - OPENROUTESERVICE (ORS) ───

label variable d_ors_a_hosp        "Distancia en auto al hospital más cercano - ORS (km)"
label variable d_ors_a_cmb         "Distancia en auto al CMB más cercano - ORS (km)"
label variable d_ors_a_cesac       "Distancia en auto al CESAC más cercano - ORS (km)"
label variable t_ors_a_hosp        "Tiempo en auto al hospital más cercano - ORS (min)"
label variable t_ors_a_cmb         "Tiempo en auto al CMB más cercano - ORS (min)"
label variable t_ors_a_cesac       "Tiempo en auto al CESAC más cercano - ORS (min)"
label variable d_ors_p_hosp        "Distancia a pie al hospital más cercano - ORS (km)"
label variable d_ors_p_cmb         "Distancia a pie al CMB más cercano - ORS (km)"
label variable d_ors_p_cesac       "Distancia a pie al CESAC más cercano - ORS (km)"
label variable t_ors_p_hosp        "Tiempo a pie al hospital más cercano - ORS (min)"
label variable t_ors_p_cmb         "Tiempo a pie al CMB más cercano - ORS (min)"
label variable t_ors_p_cesac       "Tiempo a pie al CESAC más cercano - ORS (min)"


* ── 14. VALUE LABELS ─────────────────────────────────────

label define lbl_intervencion 0 "Control" 1 "Intervención"
label values intervencion lbl_intervencion

label define lbl_binario 0 "No" 1 "Sí"
label values dbt             lbl_binario
label values villa           lbl_binario
label values turno           lbl_binario
label values resp_efectivo   lbl_binario

label define lbl_grupo 0 "Tardía (control)" 1 "Precoz (intervención)"
* (grupo_aleat es string; si se convierte a numeric, aplicar este label)



* ── VERIFICACIÓN FINAL ───────────────────────────────────
* Correr para confirmar que los labels se aplicaron correctamente

describe
codebook intervencion villa dbt genero dias_ult_cons, compact
