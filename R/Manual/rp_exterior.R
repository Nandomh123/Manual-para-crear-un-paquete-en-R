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

# CREA IDENTIFICADOR DE NUEVAS ENTRADAS----
n_e <- m_d_m[ , list( NUMAFI ) ]
d_a <- d_a[ IMPOS > 0 ]
n_e <- merge( n_e, d_a[ , list( NUMAFI, IMPS = IMPOS ) ]
              , all.x = TRUE, by = 'NUMAFI' )
n_e[ is.na( IMPS ), IMPS := 0 ]
aux <- m_d_m[ , 442:( dim( m_d_m )[2] ) ]
aux[ aux < 0 ] <- 0
n_e[ , suma := rowSums( aux, dims = 1L, na.rm = TRUE ) ]
n_e[ suma < 0, suma := 0 ]
n_e[ , acu := IMPS + suma ]
y <- 2013
yf <- 2021
m <- 12
j <- 465
m_r <- n_e[ , list( NUMAFI, suma ) ]
setnames( m_r, c( 'NUMAFI', '2013_12'))
aux_2 <- m_d_m[ , j:j ]
setnames( aux_2, 'suma' )
aux_2[is.na( suma ), suma := 0 ]
aux_2[ suma < 0, suma := 0 ]
y <- n_y( y, m )
m <- n_m( m )
j <- j + 1 
while ( y != yf ) {
  aux_3 <- n_e[ , list( suma ) ] - aux_2
  m_r[ , paste0( y, "_", m ) := aux_3 ]
  aux_3 <- m_d_m[ , j:j ]
  setnames( aux_3, 'suma' )
  aux_3[is.na( suma ), suma := 0 ]
  aux_3[ suma < 0, suma := 0 ]
  aux_2 <- aux_2 + aux_3
  y <- n_y( y, m )
  m <- n_m( m )
  j <- j + 1 
}
rm( m_d_m, aux_2, aux_3 )
gc()

# NUEVAS ENTRADAS----
y <- 2013
yf <- 2021
m <- 12
j <- 2
m_n_e <- m_r[ , list( NUMAFI ) ]
while ( y != yf) {
  m_n_e[ , paste0( y, "_", m ) := ( ( m_r[ , j:j ] == n_e[ , acu ] ) * 1 ) ]
  j <- j + 1
  y <- n_y( y, m )
  m <- n_m( m )
}
rm( n_e, m_r )
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
rm( m_a_e, m_r_c, d_a )
gc()

# MATRIZ DE TRANSICIONES----
y <- 2013
m <- 12
i <- 14
yf <- 2021
m_t <- m_a_p_e[ , i:i ] - m_a_p_e[ , ( i-12 ):( i-12 ) ]
y <- n_y( y, m )
m <- n_m( m )
i <- i + 1 
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

# AUXILIAR PARA LAS TRANSICIONES SOLO LAS PRIMERAS----
y <- 2013
m <- 12
i <- 4
yf <- 2021
m_t_ax <- m_t[ , list( sexo, NUMAFI, `2013_12` ) ]
y <- n_y( y, m )
m <- n_m( m )
while ( y != yf ) {
  m_t_ax[ , paste0( y, "_", m ) := 
            #m_t[ , i:i ] - m_t[ , (i-1):(i-1) ]  
            ( 1 - 1*(m_t[ , (i-1):(i-1) ] < 0) ) * 
            ( 1 - 1*(m_t[ , i:i ]==0) ) *  
            ( m_t[ , i:i ] - m_t[ , (i-1):(i-1) ] )
  ]
  y <- n_y( y, m )
  m <- n_m( m )
  i <- i + 1
}
setkey( m_t_ax, NUMAFI )
gc()
m_t <- m_t_ax
rm(m_t_ax)
gc()

# NUEVAS ENTRADAS POR EDAD----
y <- 2013
yf <- 2021
m <- 12
j <- 3
m_t_e <- m_t[ , list( sexo, NUMAFI ) ]
while ( y != yf) {
  m_t_e[ , paste0( y, "_", m ) := ( m_t[ , j:j ] > 1 ) * m_t[ , j:j ] * m_n_e[ , ( j-1 ):( j-1 ) ] ]
  j <- j + 1
  y <- n_y( y, m )
  m <- n_m( m )
}
rm( m_n_e, m_a_p_e )
gc()

# ELIMINO TABLA NO NECESARIAS----
rm( list = ls()[ !( ls() %in% c( 'm_t','m_t_e','n_y', 'n_m' ) ) ] )
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

# CAMBIO ESTRUCTURA MATRIZ DE TRANSICIONES NUEVAS----
y <- 2014 #2013
yf <- 2021
m <- 1 #12
j <- 4 #3
aux <- cbind( m_t_e[,list(sexo, NUMAFI )], m_t_e[ , j:j ]  )
setnames( aux, c('sexo', 'NUMAFI', 'edad'))
aux<-aux[ edad > 1 ]
entra_nu <- aux
y <- n_y( y, m )
m <- n_m( m )
j <- j + 1
while (y!=yf) {
  aux <- cbind( m_t_e[,list(sexo, NUMAFI )], m_t_e[ , j:j ]  )
  setnames( aux, c('sexo', 'NUMAFI', 'edad'))
  aux<-aux[ edad > 1 ]
  entra_nu <- rbind( entra_nu, aux )
  y <- n_y( y, m )
  m <- n_m( m )
  j <- j + 1
}
rm( aux )
gc()
entra_nu_acu <- entra_nu[ , .N, by = c( 'edad', 'sexo' ) ]
entra_nu_acu <- entra_nu_acu[ edad >= 15 & edad <=70 ]

# PROPORCION NUEVAS ENTRADAS----
p1 <- merge(entradas_acu, entra_nu_acu, all.x = TRUE, by = c('edad','sexo') )
p1[, pro := N.y / N.x ]

p1[ sexo == 'H', sexo := 1 ]
p1[ sexo == 'M', sexo := 2 ]

pro_1 <- p1[ edad >= 15 & edad <= 70 & sexo == '1' ]
pro_1[ , sexo := NULL ]
pro_1[ , pro := 1 - pro ] # esto se comenta para python
pro_1 <- pro_1[ , list( edad, pro ) ]


pro_2 <- p1[ edad >= 15 & edad <= 70 & sexo == '2' ]
pro_2[ , sexo := NULL ]
pro_2[ , pro := 1 - pro ] # esto se comenta para python
pro_2 <- pro_2[ , list( edad, pro ) ]
plot( pro_2)
# SUAVIZAMIENTO DE CURVA rp  MUJERES----
modelo2 <- lm( pro ~ bs( edad, df = 4, degree = 2
                         #, knots = c( 18 )
                         )
               # , weights = N
               , data = pro_2[ edad < 70 ]
               )
er2 <- data.table( edad = 15:70 )
er2[ , prob := predict( object = modelo2, newdata = er2 ) ]
er2[ prob < 0, prob := 0 ]
plot( er2 )
#####
aux <- merge(er2, pro_2, by = 'edad', all.x = TRUE )
plot( aux$edad, aux$prob )
points(aux$edad, aux$pro, col = 2 )

# SUAVIZAMIENTO DE CURVA rp  HOMBRES----
modelo1 <- lm( pro ~ bs( edad, df = 6, degree = 2
                         # , knots = c( 20 )
                         )
               , data = pro_1[ edad < 70 ]
               )
er1 <- data.table( edad = 15:70 )
er1[ , prob := predict( object = modelo1, newdata = er1 ) ]
er1[ prob < 0, prob := 0 ]
plot( er1 )
#####.
aux <- merge(er1, pro_1, by = 'edad', all.x = TRUE )
plot( aux$edad, aux$prob )
points( aux$edad, aux$pro, col = 2 )
# FIN SUAVIZADO

save( pro_1, pro_2, file = 'matriz_rp_exterior.RData' )
# # PARA PYRHON
# #plot(pro_1)
# write.csv( pro_1, file="C:/Users/usuario01/pro_1_mag.csv")
# write.csv( pro_2, file="C:/Users/usuario01/pro_2_mag.csv")
# write.csv( entradas_acu, file="C:/Users/usuario01/entradas_acu_mag.csv")

write.csv( er1
           , file="C:/Users/jendry.toapanta/Documents/PROYECTO/ACUMULADO 2021/OIT/MATRICES 2020/Matrices_2020/matriz_rp1_exterior.csv")
write.csv( er2
           , file="C:/Users/jendry.toapanta/Documents/PROYECTO/ACUMULADO 2021/OIT/MATRICES 2020/Matrices_2020/matriz_rp2_exterior.csv")
