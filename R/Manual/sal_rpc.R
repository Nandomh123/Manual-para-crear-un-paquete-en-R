library(readxl)
SalariosExterior <- read_excel("C:/Users/jendry.toapanta/Downloads/SalariosExterior.xlsx", 
                               sheet = "Hoja1")
View(SalariosExterior)

SalariosExterior <- as.data.table( SalariosExterior)

modelo1 <- lm( Feme ~ bs( Edad, df = 4, degree = 2, knots = c( 22.5, 50.5, 60.5 ) 
                         ), 
               data = SalariosExterior[ Edad<=70 & Edad >= 35] )
ret1 <- data.table( Edad = 15:70 )
ret1[ , sal := predict( object = modelo1, newdata = ret1 ) ]
plot( ret1 )
####
aux <- merge( ret1, SalariosExterior[ Edad<=70 & Edad >= 15][,list( Edad, Feme ) ], by = 'Edad', all.x = TRUE )
plot(aux$Edad, aux$Feme)
points(aux$Edad, aux$sal, col = 2 )
# MUJERES
write.csv( ret1, file="C:/Users/jendry.toapanta/Downloads/sal_teorico_female_exterior.csv")
###################
modelo1 <- lm( Mascu ~ bs( Edad, df = 3, degree = 4#, knots = c(   50.5 ) 
                          ), 
               data = SalariosExterior[ Edad<=68 & Edad >= 20] )
ret1 <- data.table( Edad = 15:70 )
ret1[ , sal := predict( object = modelo1, newdata = ret1 ) ]
plot( ret1 )
####
aux <- merge( ret1, SalariosExterior[ Edad<=70 & Edad >= 15][,list( Edad, Mascu ) ], by = 'Edad', all.x = TRUE )
plot(aux$Edad, aux$Mascu )
points(aux$Edad, aux$sal, col = 2 )
# MUJERES
write.csv( ret1, file="C:/Users/jendry.toapanta/Downloads/sal_teorico_male_exterior.csv")

