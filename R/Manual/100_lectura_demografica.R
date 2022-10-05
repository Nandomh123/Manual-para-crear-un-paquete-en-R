message( paste( rep('-', 100 ), collapse = '' ) )

# Afiliados -------------
message('\tLectura del Afiliados en el Exterior')
file <- paste0( parametros$Data_seg, 'DemografiaAfiliadosExterior.xlsx')
Afiliados_exterior <- as.data.table( read_excel( file, sheet = "Afiliados_exterior", col_names = TRUE))
sapply( Afiliados_exterior, class )

# Corrección de fechas
Afiliados_exterior$FECHA_NACIMIENTO <- as.Date( Afiliados_exterior$FECHA_NACIMIENTO, format = "%d/%m/%Y")


# Fallecidos -------------
message( paste( rep('-', 100 ), collapse = '' ) )
message('\tLectura de Fallecidos en el Exterior')
Muertos_exterior <- as.data.table( read_excel( file, sheet = "Muertos_exterior", col_names = TRUE))
Muertos_exterior <- Muertos_exterior[, list(CEDULA,FECHA_NACIMIENTO,FECHA_FALLECIMIENTO,EDAD_FALL,COD_SEXO,DES_SEXO,
               COD_ESTADO_CIVIL,DES_EST_CIV,PAIS_FALLECE, afil,NUMTOTAPO)]

# Tabla edad vs numero de aportaciones -------------
message( paste( rep('-', 100 ), collapse = '' ) )
message('\tLectura de Tabla de edad por años de aportaciones')
EdadXAniosporte <- as.data.table( read_excel( file, sheet = "EdadXAnioaporta", col_names = TRUE))

# Historico ----
message( paste( rep('-', 100 ), collapse = '' ) )
message('\tLectura de afiliados historicos')
afi_histo <- as.data.table( read_excel( file, sheet = "Historico", col_names = TRUE))

# base_actgx_sgo -----
# Mujeres
message( paste( rep('-', 100 ), collapse = '' ) )
message('\tNumero de personas mujeres')
file <- paste0( parametros$Data_seg, 'base_actgx_sgo_female_50_.xlsx')
Mujeres_actgx <- as.data.table(read_excel( file, sheet = "actgx", col_names = TRUE, skip = 1))
Mujeres_actgx <- Mujeres_actgx[ , !c('...1')] # Eliminamos la primera comuna
names(Mujeres_actgx)[names(Mujeres_actgx)=='...2'] <- 'Edad'  # Cambiamos el nombre de la variable

# Hombres
message( paste( rep('-', 100 ), collapse = '' ) )
message('\tNumero de personas hombres')
file <- paste0( parametros$Data_seg, 'base_actgx_sgo_male_50_.xlsx')
Hombres_actgx <- as.data.table(read_excel( file, sheet = "actgx", col_names = TRUE, skip = 1))
Hombres_actgx <- Hombres_actgx[ , !c('...1')] # Eliminamos la primera comuna
names(Hombres_actgx)[names(Hombres_actgx)=='...2'] <- 'Edad'  # Cambiamos el nombre de la variable


# base_tact_sgo -----
# Mujeres
message( paste( rep('-', 100 ), collapse = '' ) )
message('\tNumero de personas mujeres')
file <- paste0( parametros$Data_seg, 'base_tact_sgo_female_50_.xlsx')
Mujeres_tact <- as.data.table(read_excel( file, sheet = "Tact", col_names = TRUE, skip = 1))
Mujeres_tact <- Mujeres_tact[ , !c('...1')] # Eliminamos la primera comuna
setnames(Mujeres_tact, c('Anio', 'mujeres')) # Renombramos las variables

# Hombres
message( paste( rep('-', 100 ), collapse = '' ) )
message('\tNumero de personas hombres')
file <- paste0( parametros$Data_seg, 'base_tact_sgo_male_50_.xlsx')
Hombres_tact <- as.data.table(read_excel( file, sheet = "Tact", col_names = TRUE, skip = 1))
Hombres_tact <- Hombres_tact[ , !c('...1')] # Eliminamos la primera comuna
setnames(Hombres_tact, c('Anio', 'hombres')) # Renombramos las variables


# Guardando ---------------------------------------------------------------

lista <- c( 'Afiliados_exterior', 'Muertos_exterior', 'EdadXAniosporte',
            'afi_histo','Mujeres_actgx', 'Hombres_actgx', 'Mujeres_tact', 
            'Hombres_tact' )
save( list = lista,
      file = paste0( parametros$RData_seg, 'IESS_AVE_analisis_demografico.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

