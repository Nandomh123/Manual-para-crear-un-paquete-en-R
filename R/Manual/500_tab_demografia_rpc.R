message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tTablas Familia ' )

load( paste0( parametros$RData, 'Familia_muestra.RData' ) )

# Poblacion historica ----
aux <- copy( Familia )
aux <- aux[, Tipo := as.character(Tipo)]
aux_xtable <- xtable( aux, digits = c( rep( 0, 4 ) ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'Familia_muestra', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

