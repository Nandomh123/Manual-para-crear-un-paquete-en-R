message( paste( rep('-', 100 ), collapse = '' ) )
message( "\t Creación de la base de datos Familias" )

Familia <- as.data.table( c(1:200) )
setnames(Familia, c('Id'))
Familia <- Familia[ , Personas := sample(1:10, size = 200,
                                         replace = TRUE) ]
Familia[ Personas <= 4, Tipo := 'Familia Normal' ]
Familia[ Personas >= 4 & Personas <= 6 ,
         Tipo := 'Familia Media' ]
Familia[ Personas >= 7 , Tipo := 'Familia Numerosa' ]
Familia <- head(Familia, 10)


# Guardando ---------------------------------------------------------------

lista <- c( 'Familia' )
save( list = lista,
      file = paste0( parametros$RData, 'Familia_muestra.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
