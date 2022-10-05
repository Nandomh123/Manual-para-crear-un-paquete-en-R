message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tTablas demograficos' )

load( paste0( parametros$RData_seg, 'IESS_AVE_tablas_demografico.RData' ) )

# Poblacion historica ----
aux <- copy( afi_histo )
setnames(aux,  c('Año', 'mujeres', 'hombres', 'total') )

aux_xtable <- xtable( aux, digits = c( rep( 0, 5 ) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_asegu_hist_rpc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# --------------------------------------------------------------------------
# Poblacion asegurada 2020 ----
aux <- copy( per_pais )
setnames(aux,  c('País', 'hombres', 'mujeres', 'total') )

aux_xtable <- xtable( aux, digits = c( rep( 0, 5 ) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_asegu_rpc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Edad promedio por sexo (2020/12/31)
aux1 <- copy( edad_prom )
setnames(aux1,  c('genero', 'numero_personas', 'edad_promedio') )

aux_xtable1 <- xtable( aux1, digits = c( rep( 0, 4 ) ) )
print( aux_xtable1,
       file = paste0( parametros$resultado_tablas, 'iess_asegu_edad_prom_rpc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Numero de personas por edad y sexo (Gráfico de piramides)
aux2 <- copy( edad_sexo_perso )
setnames(aux2,  c('edad', 'genero', 'Total') )

aux_xtable2 <- xtable( aux2, digits = c( rep( 0, 4 ) ) )
print( aux_xtable1,
       file = paste0( parametros$resultado_tablas, 'iess_asegu_edad_sexo_perso_rpc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# ----------------------------------------------------------------------------------------

# Poblacion de afiliados fallecidos 2010-2020 ----(afil)
aux3 <- copy( afili_fall )
aux3[, ANIO_FALLECIMIENTO := as.character( ANIO_FALLECIMIENTO ) ]
setnames(aux3,  c('Anio_Muerte', 'hombres', 'mujeres', 'total','Crecimiento') )

aux_xtablex3 <- xtable( aux3, digits = c( rep( 0, 6 ) ) )
print( aux_xtablex3,
       file = paste0( parametros$resultado_tablas, 'iess_asegu_afi_falle_rpc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Fallecidos por País en el 2020
aux4 <- copy( muertos_pais2020)
aux4[, PAIS_FALLECE := as.character( PAIS_FALLECE ) ]
setnames(aux4,  c('País_fallece', 'total') )

aux_xtablex4 <- xtable( aux4, digits = c( rep( 0, 3 ) ) )
print( aux_xtablex4,
       file = paste0( parametros$resultado_tablas, 'iess_asegu_muertos_pais2020_rpc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 5,
       sanitize.text.function = identity )

# Numero de personas fallecidas por edad y sexo (Gráfico de piramides)
aux5 <- copy( muertos_edad_sexo )
setnames(aux5,  c('edad', 'sexo', 'numero_personas') )

aux_xtablex5 <- xtable( aux5, digits = c( rep( 0, 4 ) ) )
print( aux_xtablex5,
       file = paste0( parametros$resultado_tablas, 'iess_asegu_muertos_edad_sexo_rpc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Numero de personas fallecidas por edad y numero de aportes ----
aux6 <- copy( EdadXAniosporte1 )

aux_xtablex6 <- xtable( aux6, digits = c( rep( 0, 13 ) ) )
print( aux_xtablex6,
       file = paste0( parametros$resultado_tablas, 'iess_asegu_EdadXAniosporte1_rpc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 12,
       sanitize.text.function = identity )


# ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()



