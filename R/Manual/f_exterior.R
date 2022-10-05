library( tidyverse )
library( data.table )
library( lubridate )
library( RcppRoll )
library( splines )

# Prelectura ----
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
# APORTES HISTORICO ----
aux <- fread("Y:/IESS_AVE_repatriacion/Data/RPC/DNAC/Voluntarios Exterior_Planillas con fecnac.csv"
             , sep = ';', colClasses = colClasses )
aux <- as.data.table( pivot_wider( aux[, list( NUMAFI, VALSUE, ANIPER, MESPER ) ], 
                                   names_from =  c( ANIPER , MESPER ), 
                                   values_from = VALSUE,
                                   values_fill = 0 ) )

d_a <- aux[ , list ( NUMAFI, IMPOS = rowSums( aux[ , 2:466 ], dims = 1, na.rm = TRUE ) ) ]
rm( aux )
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
                                     values_fill = 0) )
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

# MATRIZ DE ACTIVOS POR EDAD----
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
rm( m_a_e, m_r_c, m_d_m, sex_dict )
gc()


# TRANSICIONES----
y <- 2013
m <- 12
i <- 14
yf <- 2021
m_t <- m_a_p_e[ , i:i ] - m_a_p_e[ , (i-12):(i-12) ]
y <- n_y( y, m )
m <- n_m( m )
while ( y != yf ) {
  m_t[ , paste0( y, "_", m ) := m_a_p_e[ , i:i ] - m_a_p_e[ , (i-12):(i-12) ] ]
  y <- n_y( y, m )
  m <- n_m( m )
  i <- i + 1
}
m_t <- cbind( NUMAFI = m_a_p_e[ , NUMAFI ], m_t )
m_t <- cbind( sexo = m_a_p_e[ , sexo ], m_t )
setkey( m_t, NUMAFI )
gc()

# AUXILIAR PARA LAS TRANSICIONES----
y <- 2013
m <- 12
i <- 3
yf <- 2021
m_t_ax <- m_t[ , list( sexo, NUMAFI ) ]
while ( y != yf ) {
  m_t_ax[ , paste0( y, "_", m ) := ( ( m_t[ , i:i ] > 1 ) * 1 ) * m_t[ , i:i ]  ]
  y <- n_y( y, m )
  m <- n_m( m )
  i <- i + 1
}
setkey( m_t_ax, NUMAFI )
gc()
m_t <- m_t_ax
gc()

y <- 2013
m <- 12
i <- 4
yf <- 2021
m_t_ax <- m_t[ , list( sexo, NUMAFI, `2013_12` ) ]
y <- n_y( y, m )
m <- n_m( m )
while ( y != yf ) {
  m_t_ax[ , paste0( y, "_", m ) := m_t[ , i:i ] - m_t[ , (i-1):(i-1) ]  ]
  y <- n_y( y, m )
  m <- n_m( m )
  i <- i + 1
}
setkey( m_t_ax, NUMAFI )
gc()
m_t <- m_t_ax
rm( m_t_ax )
gc()

# CAMBIO ESTRUCTURA MATRIZ DE TRANSICIONES----
y <- 2014 #2013
yf <- 2021
m <- 1 #12
j <- 4 #3
aux <- cbind( m_t[,list(sexo, NUMAFI )], m_t[ , j:j ]  )
setnames( aux, c('sexo', 'NUMAFI', 'edad'))
aux<-aux[ edad > 1 ]
entradas <- aux
y <- n_y( y, m )
m <- n_m( m )
j <- j + 1
while (y!=yf) {
  aux <- cbind( m_t[,list(sexo, NUMAFI )], m_t[ , j:j ]  )
  setnames( aux, c('sexo', 'NUMAFI', 'edad'))
  aux<-aux[ edad > 1 ]
  entradas <- rbind( entradas, aux )
  y <- n_y( y, m )
  m <- n_m( m )
  j <- j + 1
}
rm( aux )
gc()
entradas_acu <- entradas[ , .N, by = c( 'edad', 'sexo' ) ]
entradas_acu <- entradas_acu[ edad >= 15 & edad <=70 ]
entradas_acu[ sexo == 'H', sexo := 1 ]
entradas_acu[ sexo == 'M', sexo := 2 ]

# plot( entradas_acu[sexo==1][, list(edad,N)])
# SUAVIZAMIENTO DE CURVA f_HOMBRES ----
aux <- entradas_acu[ sexo == 1 ]
setorder( aux, edad )
aux[ , pro := N / sum( N ) ]
modelo1 <- lm( pro ~ bs( edad, df = 10, degree = 2
                         , knots = c(  52, 60 ) 
                         )
               # , weights = N
               ,data = aux
               )
f1 <- data.table( edad = 15:70 )
f1[ , prob := predict( object = modelo1, newdata = f1 ) ]
f1[ edad < 18, prob := f1[edad==18]$prob ]
f1$prob <- round( f1$prob, 4 )
f1[ edad == 69, prob := f1[ edad == 69 ]$prob + f1[ edad == 70 ]$prob ]
f1[ prob < 0, prob := 0 ]
f1$prob <- f1$prob / sum(f1$prob)
plot( f1 )
sum( f1$prob )
#####.
aux1 <- merge(f1, aux, by = 'edad', all.x = TRUE )
plot(aux1$edad, aux1$pro)
points(aux1$edad, aux1$prob, col = 2 )

# SUAVIZAMIENTO DE CURVA f_MUJERES ----
aux <- entradas_acu[ sexo == 2 ]
setorder( aux, edad )
aux[ , pro := N / sum( N ) ]
modelo2 <- lm( pro ~ bs( edad, df = 10, degree = 2
                         , knots = c( 29, 50, 60 ) 
                         )
               #, weights = N
               , data = aux[ edad < 70 ]
               )
f2 <- data.table( edad = 15:70 )
f2[ , prob := predict( object = modelo2, newdata = f2 ) ]

f2[ edad < 18, prob := f2[ edad == 18 ]$prob ]
f2$prob <- round( f2$prob, 4 )
f2$prob <- f2$prob / sum(f2$prob)

plot( f2 )
sum( f2$prob )
#####.
aux2 <- merge(f2, aux, by = 'edad', all.x = TRUE )
plot(aux2$edad, aux2$pro)
points(aux2$edad, aux2$prob, col = 2 )

# EXPORTO ----

save( f1, f2, file = 'matriz_f_exterior.RData' )
#load('C:/Users/usuario01/Documents/PROYECTO/OIT/MATRICES 2020/Matrices_2020/matriz_er.RData')
write.csv( f1, file="C:/Users/jendry.toapanta/Documents/PROYECTO/ACUMULADO 2021/OIT/MATRICES 2020/Matrices_2020/matriz_f1_exterior.csv")
write.csv( f2, file="C:/Users/jendry.toapanta/Documents/PROYECTO/ACUMULADO 2021/OIT/MATRICES 2020/Matrices_2020/matriz_f2_exterior.csv")



