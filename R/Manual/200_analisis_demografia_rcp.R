message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAnalisis demografico' )

# Afiliados Exteriores
# Lectura datos BIESS
load(paste0( parametros$RData_seg, 'IESS_AVE_analisis_demografico.RData' ))
gc() 

# Tabla1 ---------------------------------------
Tabla1 <- Afiliados_exterior
rm(Afiliados_exterior)

# Edad
Tabla1[, Fecha_corte := as.Date("2020-12-31")]
Tabla1[ , y_b := year( FECHA_NACIMIENTO ) ]
Tabla1[ , y_c := year( Fecha_corte ) ]
Tabla1[ , m_b := month( FECHA_NACIMIENTO ) ]
Tabla1[ , m_c := month( Fecha_corte) ]
Tabla1[ , edad := ( y_c - y_b - 1 + 1 * ( m_c > m_b ) ) ]

# Edad promedio por sexo (12/31/2020)
Tabla1[ , list(CEDULA,SEXO,edad)]
edad_prom <- Tabla1[ , .( NUMERO_PERSONAS = uniqueN( CEDULA ), EDAD_MEDIA = mean(edad)), by = c( 'SEXO' )]

# Número de personas por Paises 
per_pais <- Tabla1[ , .(CEDULA,SEXO, PROVINCIA)]
per_pais <- per_pais[ , .( NUMERO_PERSONAS = uniqueN( CEDULA )), by = c( 'PROVINCIA','SEXO' )]
per_pais <- dcast( per_pais, PROVINCIA  ~ SEXO, value.var = 'NUMERO_PERSONAS' )
per_pais <- as.data.table(per_pais)
per_pais[ , Total :=  H + M]
per_pais <- rbind( per_pais, data.table(PROVINCIA = 'Total', 
                                        H= c(colSums(per_pais[, 2:2]) ),
                                        M = colSums(per_pais[, 3:3] ),
                                        Total = colSums(per_pais[, 4:4])) )

# Tabla: Edad, sexo, numero de personas (Gráficos de pirámides)
edad_sexo_perso <- Tabla1[ , .(NUMERO_PERSONAS = uniqueN( CEDULA ) ), by = c( 'edad', 'SEXO') ]
gc()

# Fallecidos ---------------------------------------

afili_fall <- Muertos_exterior[afil == "afil"]
afili_fall[ , anio_fallec := year(FECHA_FALLECIMIENTO)]
afili_fall <- afili_fall[ , list(CEDULA, DES_SEXO, anio_fallec)]
setnames(afili_fall, c( 'CEDULA','GENERO','ANIO_FALLECIMIENTO') )
afili_fall[GENERO == 'MASCULINO', GENERO := 'H']
afili_fall[GENERO == 'FEMENINO', GENERO := 'M']
afili_fall <- afili_fall[ , .( NUMERO_PERSONAS = uniqueN( CEDULA )), by = c( 'ANIO_FALLECIMIENTO','GENERO')]
afili_fall <- dcast( afili_fall, ANIO_FALLECIMIENTO  ~ GENERO, value.var = 'NUMERO_PERSONAS' )
afili_fall <- as.data.table(afili_fall)
afili_fall[ , Total := H + M ]
afili_fall <- afili_fall[!(ANIO_FALLECIMIENTO >= 2021)] # Filtramos solo hasta el año 2020
afili_fall <- afili_fall[, Crecimiento := round(((Total - lag(Total))/lag(Total))*100, 2)]



# Año 2020 ----
# Numero de personas muertas por pais (5 paises)
muertos2020 <- Muertos_exterior[afil == "afil"]
muertos2020 <- muertos2020[ ,  anio_fallecimiento := year(FECHA_FALLECIMIENTO)]
muertos2020 <- muertos2020[anio_fallecimiento == 2020]
muertos_pais2020 <- muertos2020[ , .( Numero_Muertos = uniqueN( CEDULA )), by = c( 'PAIS_FALLECE')]
muertos_pais2020 <- muertos_pais2020[with(muertos_pais2020, order(-Numero_Muertos)), ] 
muertos_pais2020[Numero_Muertos < 14 , PAIS_FALLECE := 'Otros' ]
muertos_pais2020 <- muertos_pais2020[ , list(Muertos = sum(Numero_Muertos)), by = c( 'PAIS_FALLECE')]
muertos_pais2020 <- rbind( muertos_pais2020, data.table(PAIS_FALLECE = 'Total', 
                                              Muertos = c(colSums(muertos_pais2020[, 2:2])  )))  # El total de columnas
# muertos_pais2020$PAIS_FALLECE <- gsub('', 'Ñ', muertos_pais2020$PAIS_FALLECE)


# Número de personas fallecidas por edad y sexo (Gráficos de piramides)
muertos2020[DES_SEXO == 'MASCULINO', SEXO := 'H']
muertos2020[DES_SEXO == 'FEMENINO', SEXO := 'M']
muertos_edad_sexo <- muertos2020[ , .(NUMERO_PERSONAS = uniqueN( CEDULA ) ), by = c( 'EDAD_FALL', 'SEXO') ]
gc()


EdadXAniosporte <- EdadXAniosporte
setnames( EdadXAniosporte, c( 'Eda_Aportes', '[1,5]','[6,10]','[11,15]','[16,20]','[21,25]',
                              '[26,30]','[31,35]','[36,40]','[41,45]','[46,50]', 'Total') )
# EdadXAniosporte[ Eda_Aportes == "16-20", Eda_Aportes := '[16,20]']
# EdadXAniosporte[ Eda_Aportes == "21-25", Eda_Aportes := '[21,25]']
# EdadXAniosporte[ Eda_Aportes == "26-30", Eda_Aportes := '[26,30]']
# EdadXAniosporte[ Eda_Aportes == "31-35", Eda_Aportes := '[31,35]']
# EdadXAniosporte[ Eda_Aportes == "36-40", Eda_Aportes := '[36,40]']
# EdadXAniosporte[ Eda_Aportes == "41-45", Eda_Aportes := '[41,45]']
# EdadXAniosporte[ Eda_Aportes == "46-50", Eda_Aportes := '[46,50]']
# EdadXAniosporte[ Eda_Aportes == "51-55", Eda_Aportes := '[51,55]']
# EdadXAniosporte[ Eda_Aportes == "56-60", Eda_Aportes := '[56,60]']
# EdadXAniosporte[ Eda_Aportes == "61-65", Eda_Aportes := '[61,65]']
# EdadXAniosporte[ Eda_Aportes == "66-70", Eda_Aportes := '[66,70]']
EdadXAniosporte[ Eda_Aportes == "Total general", Eda_Aportes := 'Total']
EdadXAniosporte1 <- EdadXAniosporte

# Resumen demografico muertos ----
resu_falle <- muertos2020[ , .(edad_pro = mean(EDAD_FALL), muertos = .N), by = SEXO ]

#------------------------------------------------------------------------------.
# Guarda resultados ----
resultados <- c('edad_prom', 'per_pais', 'edad_sexo_perso', 'afi_histo', 'afili_fall',
                'muertos_pais2020', 'muertos_edad_sexo', 'EdadXAniosporte1', 'resu_falle'
                )
save( list =  resultados,
      file = paste0( parametros$RData_seg, 'IESS_AVE_tablas_demografico.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()




