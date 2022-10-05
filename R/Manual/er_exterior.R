library( tidyverse )
library( data.table )
library( lubridate )
library( RcppRoll )
library( splines )

# Preectura ----
colClasses = sapply( fread( "Y:/IESS_AVE_repatriacion/Data/RPC/DNAC/Voluntarios Exterior_Planillas con fecnac.csv", 
                            header = TRUE, 
                            sep = 'auto',
                            showProgress = TRUE), class )

colClasses[2] <- 'character'
# Lectura ----
m_d_m <- fread("Y:/IESS_AVE_repatriacion/Data/RPC/DNAC/Voluntarios Exterior_Planillas con fecnac.csv"
               , sep = ';', colClasses = colClasses )
# View(head(m_d_m, 1000))
m_d_m$FECNAC <- as.Date( m_d_m$FECNAC, "%d/%m/%Y" )
m_d_m <- m_d_m[ SEXO == 'H' | SEXO == 'M' ]
m_d_m <- m_d_m[ !is.na( FECNAC ) ]

# REGISTRO CIVIL ----
reg <- m_d_m[ , list( NUMAFI, SEXO, FECNAC ) ]
# reg[ nchar( FECHA_MUERTE ) < 7, FECHA_MUERTE := '31/12/2050' ]
# reg[ , FECNAC := as.Date( FECNAC, format="%d/%m/%Y" ) ]
# reg[ , FECHA_MUERTE := as.Date( FECHA_MUERTE, format="%d/%m/%Y" ) ]

# reg[ , y_d := year( FECHA_MUERTE ) ]
reg[ , y_b := year( FECNAC ) ]
reg[ , m_b := month( FECNAC ) ]
reg <- reg[ !duplicated( reg, by = 'NUMAFI' ) ]

# DICCIONARIO DE REGISTRO CIVIL
sex_dict <- reg[ , list( NUMAFI, SEXO ) ]
y_b_dict <- reg[ , list( NUMAFI, y_b ) ]
m_b_dict <- reg[ , list( NUMAFI, m_b ) ]
# m_d_dict <- reg[ , list( NUMAFI, y_d ) ]
rm( reg )

# Pivot ----
# m_d_m <- m_d_m[ DESRELTRA == '69-VOLUNTARIO ECUATORIANO DOMICILIADO EN EL EXTERIOR' ]

# aux <- dcast( m_d_m, NUMAFI ~ ANIPER + MESPER, fun.agg = function(x) sum( !is.na(x) ), value.var = "VALSUE" )
m_d_m <- as.data.table( pivot_wider( m_d_m[, list( NUMAFI, VALSUE, ANIPER, MESPER ) ], 
                                   names_from =  c( ANIPER , MESPER ), 
                                   values_from = VALSUE,
                                   values_fill = 0,
                                   values_fn = sum
                                   ) )
gc()

# FUNCIONES DE ACTUALIZACION ----
n_m <- function( m )
{ if ( m == 12 ){ return( 1 ) }
  else{
    return( m + 1 )
  }
}

n_y <- function( y, m){
  if ( m == 12 ){
    return( y + 1 )
  }
  else{
    return( y )
  }
}

# REGISTRO CORTO ----
aux <- m_d_m[ , list( NUMAFI ) ]
setkey( aux, NUMAFI )
aux <- merge( aux, sex_dict, all.x = TRUE, by = 'NUMAFI' )
aux <- merge( aux, y_b_dict, all.x = TRUE, by = 'NUMAFI' )
aux <- merge( aux, m_b_dict, all.x = TRUE, by = 'NUMAFI' )
# aux <- merge( aux, m_d_dict, all.x = TRUE, by = 'NUMAFI' )
m_r_c <- aux
rm( aux )

# MATRIZ ACTIVOS ----
m_a_e <- m_d_m[ , list ( NUMAFI ) ]
m <- 12
y <- 2012
for ( i in 453:549  ){ #dim( m_d_m )[2]
  m_a_e[ , paste0( y, "_", m ) := rowSums( m_d_m[ , ( i - 11 ):i ], dims = 1, na.rm = TRUE ) ]
  y <- n_y( y, m )
  m <- n_m( m )
}

m_a_e <- as.data.table( ( m_a_e > 0 ) * 1 )
m_a_e[ , NUMAFI := m_d_m[ , NUMAFI ] ]
gc()

# MATRIZ ACTIVOS POR EDAD ----
m <- 12
y <- 2012
yf <- 2021
m_a_p_e <- m_a_e[ , list( NUMAFI ) ]
i <- 2
while ( y != yf ) {
  m_a_p_e[ , paste0( y, "_", m ) := m_a_e[ , i:i ] * ( y -  m_r_c[ , y_b ] - 1 + 1 * ( m > m_r_c[ , m_b ] )   )  ]
  y <- n_y( y, m )
  m <- n_m( m )
  i <- i + 1
}
m_a_p_e[ , sexo := m_r_c[ , SEXO ] ]
rm( m_a_e, m_d_m, sex_dict, y_b_dict, m_b_dict )
gc()

# AUXILIAR PARA ACTIVOS POR EDAD----
y <- 2012
m <- 12
i <- 3
yf <- 2021
a_p_e <- m_a_p_e[ , list( sexo, NUMAFI, `2012_12` ) ]
y <- n_y( y, m )
m <- n_m( m )
while ( y != yf ) {
  a_p_e[ , paste0( y, "_", m ) := ( m_a_p_e[ , i:i ] - ( 1 - 1 * ( m_a_p_e[ ,(i-1):(i-1) ] < 0 ) ) * 
                                      m_a_p_e[ , (i-1):(i-1) ] ) + 
           m_a_p_e[ , (i-1):(i-1) ] * ( 1 * ( (m_a_p_e[ , i:i ] - m_a_p_e[ , (i-1):(i-1) ] ) == 1 ) ) ]
  y <- n_y( y, m )
  m <- n_m( m )
  i <- i + 1
}
setkey( a_p_e, NUMAFI )
gc()

# CAMBIO ESTRUCTURA MATRIZ DE ACTIVOS----
y <- 2013
yf <- 2021
m <- 12
j <- 3
aux <- cbind( a_p_e[,list(sexo, NUMAFI )], a_p_e[ , j:j ]  )
setnames( aux, c('sexo', 'NUMAFI', 'edad'))
aux <- aux[ edad > 0 ]
activos <- aux
y <- n_y( y, m )
m <- n_m( m )
j <- j + 1
while (y!=yf) {
  aux <- cbind( a_p_e[,list(sexo, NUMAFI )], a_p_e[ , j:j ]  )
  setnames( aux, c('sexo', 'NUMAFI', 'edad'))
  aux<-aux[ edad > 0 ]
  activos <- rbind( activos, aux )
  y <- n_y( y, m )
  m <- n_m( m )
  j <- j + 1
}
rm( aux )
gc()
activos_acu <- activos[ , .N, by = c( 'edad', 'sexo' ) ]
activos_acu <- activos_acu[ edad >= 15 & edad <= 70 ]
rm( activos )

# TRANSICIONES, ELIMINO MUERTES ----
y <- 2013
m <- 12
i <- 14
yf <- 2021
m_t <- m_a_p_e[ , i:i ] - m_a_p_e[ , (i-12):(i-12) ]
y <- n_y( y, m )
m <- n_m( m )
while ( y != yf ) {
  # m_t[ , paste0( y, "_", m ) := ( 1 - 1 * ( y >=  m_r_c[ , y_d ] ) ) * m_a_p_e[ , i:i ] - m_a_p_e[ , (i-12):(i-12) ] ]
  m_t[ , paste0( y, "_", m ) := m_a_p_e[ , i:i ] - m_a_p_e[ , (i-12):(i-12) ] ]
  y <- n_y( y, m )
  m <- n_m( m )
  i <- i + 1
}
m_t <- cbind( NUMAFI = m_a_p_e[ , NUMAFI ], m_t )
m_t <- cbind( sexo = m_a_p_e[ , sexo ], m_t )
setkey( m_t, NUMAFI )
gc()

# AUXILIAR PARA LAS TRANSICIONES SOLO LAS PRIMERAS SALIDAS----
y <- 2013
m <- 12
i <- 4
yf <- 2021
m_t_ax <- m_t[ , list( sexo, NUMAFI, `2013_12` ) ]
y <- n_y( y, m )
m <- n_m( m )
while ( y != yf ) {
  m_t_ax[ , paste0( y, "_", m ) := ( 1 - ( ( m_t[ , i:i ] >= 1 ) * 1 ) ) * 
            ( m_t[ , i:i ] - ( 1 - 1 * ( m_t[ ,(i-1):(i-1) ] > 0 ) ) * 
                m_t[ , (i-1):(i-1) ] ) ]
  y <- n_y( y, m )
  m <- n_m( m )
  i <- i + 1
}
setkey( m_t_ax, NUMAFI )
gc()
m_t <- m_t_ax
rm( m_t_ax )
gc()

# CAMBIO ESTRUCTURA MATRIZ DE SALIDAS----
y <- 2013
yf <- 2021
m <- 12
j <- 3
aux <- cbind( m_t[,list(sexo, NUMAFI )], m_t[ , j:j ]  )
setnames( aux, c('sexo', 'NUMAFI', 'edad'))
aux<-aux[ edad < 0 ]
salidas <- aux
y <- n_y( y, m )
m <- n_m( m )
j <- j + 1
while (y!=yf) {
  aux <- cbind( m_t[,list(sexo, NUMAFI )], m_t[ , j:j ]  )
  setnames( aux, c('sexo', 'NUMAFI', 'edad'))
  aux<-aux[ edad < 0 ]
  salidas <- rbind( salidas, aux )
  y <- n_y( y, m )
  m <- n_m( m )
  j <- j + 1
}
rm( aux )
gc()

# ELIMINO RETIROS POR VEJEZ O INVALIDEZ----
# # CARGO PENSIONISTAS
# pen <- fread('Y:/IESS_magisterio/Data/MG/POB_ROL_MAG.txt', sep = ';', key = 'CEDULA')
# pen[ SEXO == 'M', SEXO := 2 ]
# pen[ SEXO == 'H', SEXO := 1 ]
# pen$SEXO <- as.numeric( pen$SEXO )
# pen[ , cod := 1 ]
# pen <- pen[ , list( NUMAFI = CEDULA, cod ) ]
# 
# salidas <- merge( salidas, pen, all.x = TRUE, by = 'NUMAFI')
# salidas <- salidas[ is.na( cod ) ]
# salidas[ , cod := NULL ]
salidas[ , edad := -edad ]

salidas_acu <- salidas[ , .N, by = c( 'edad', 'sexo' ) ]
salidas_acu <- salidas_acu[ edad >= 15 & edad <=70 ]
rm( salidas )


# ELIMINO TABLA NO NECESARIAS----
rm( list = ls()[ !( ls() %in% c( 'salidas_acu','activos_acu' ) ) ] )
gc()
salidas_acu[ sexo == 'H', sexo := 1 ]
salidas_acu[ sexo == 'M', sexo := 2 ]

activos_acu[ sexo == 'H', sexo := 1 ]
activos_acu[ sexo == 'M', sexo := 2 ]

# CALCULANDO PROPORCION SALIDAS----
proporcion_1 <- merge( activos_acu[ sexo == 1 ][ , list( edad, act = N, N ) ], 
                       salidas_acu[ sexo == 1 ][ , list( edad, sal = N ) ],
                       all.x = TRUE, by = 'edad' )
proporcion_1[, pro := sal / act ]
proporcion_1 <- proporcion_1[ , list( edad, pro, N ) ]
# plot( proporcion_1[, list(edad, pro)] )

proporcion_2 <- merge( activos_acu[ sexo == 2 ][ , list( edad, act = N, N ) ], 
                       salidas_acu[ sexo == 2 ][ , list( edad, sal = N ) ],
                       all.x = TRUE, by = 'edad' )
proporcion_2[, pro := sal / act ]
proporcion_2 <- proporcion_2[, list(edad, pro, N ) ]
# proporcion_2[ pro > 1, pro := 0 ]
# plot( proporcion_2[, list(edad, pro)] )

# SUAVIZAMIENTO DE CURVA er Mujeres ----
modelo2 <- lm( pro ~ bs( edad, df = 4, degree = 2
                         #, knots = c( 22.5, 30.5, 40.5 )
                         #, knots = c( 20, 30, 40.5, 50, 60 )
                         #, knots = c( 18 )
                         )
               , weights = N
               , data = proporcion_2[ edad > 17 & edad < 70 ] 
               )
er2 <- data.table( edad = 15:70 )
er2[ , prob := predict( object = modelo2, newdata = er2 ) ]
er2
plot( er2 )
#####.
aux <- merge(er2, proporcion_2, by = 'edad', all.x = TRUE )
plot(aux$edad, aux$pro)
points(aux$edad, aux$prob, col = 2 )

# SUAVIZAMIENTO DE CURVA er Hombres ----
# proporcion_1[ edad ==58, N := 10000 ]
# proporcion_1[ edad ==49, N := 0 ]
modelo1 <- lm( pro ~ bs( edad, df = 4, degree = 3
                         # , knots = c( 22.5, 30.5, 60.5 ) 
                         # , knots = c( 20, 30, 40, 50, 60 )
                         # , knots = c( 20 )
                         )
               #, weights = N
               , data = proporcion_1[ edad >18 & edad < 70 ]
               )
er1 <- data.table( edad = 15:70 )
er1[ , prob := predict( object = modelo1, newdata = er1 ) ]
er1
plot( er1 )
#####.
aux <- merge(er1, proporcion_1, by = 'edad', all.x = TRUE )
plot(aux$edad, aux$pro)
points(aux$edad, aux$prob, col = 2 )

# Guardar ----
save( er1, er2, file = 'matriz_er_exterior.RData' )
# load('C:/Users/usuario01/Documents/PROYECTO/OIT/MATRICES 2020/Matrices_2020/matriz_er_tnrh.RData')
write.csv( er1, file="C:/Users/jendry.toapanta/Documents/PROYECTO/ACUMULADO 2021/OIT/MATRICES 2020/Matrices_2020/matriz_er1_exterior.csv")
write.csv( er2, file="C:/Users/jendry.toapanta/Documents/PROYECTO/ACUMULADO 2021/OIT/MATRICES 2020/Matrices_2020/matriz_er2_exterior.csv")

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

