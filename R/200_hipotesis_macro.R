message(paste(rep("-", 100), collapse = ""))

#Paquetes-------------------------------------------------------------------------------------------
require(tidyverse)
require(tidymodels)
require(data.table)
require(tidyposterior)
require(ggfortify)  #for plotting timeseries
require(chron)
require(lubridate)
require(zoo)
require(lmtest)
require(MTS)
require(lattice)
require(grid)
require(fUnitRoots)
#---------------------------------------------------------------------------------------------------
message("\tCargando tasas macro")
load(paste0(parametros$RData, "IESS_tasas_macro_historico.RData"))


#Preparando data por años---------------------------------------------------------------------------
tasas_macro <- tasas_macro %>%
  dplyr::filter(anio>=2006) %>%
  group_by( anio ) %>%
  mutate( tasa_activa = mean( tasa_activa, na.rm= TRUE),
          tasa_pasiva = mean( tasa_pasiva, na.rm= TRUE),
          pib = sum( pib_int, na.rm = TRUE),
          sbu = mean( sbu, na.rm = TRUE),
          sal = sum( sal_prom, na.rm = TRUE),
          inflacion = mean( inflacion_anual, na.rm = TRUE) ) %>%
  mutate( sal = ifelse(sal>0, sal, NA)) %>%
  ungroup() %>%
  distinct( anio, .keep_all = TRUE) %>%
  dplyr::select( anio, pib, tasa_activa, tasa_pasiva, sal, sbu, inflacion)

tasas_macro[which(tasas_macro$anio==2021),]$sal <- 8874.73315
tasas_macro[which(tasas_macro$anio==2022),]$sbu <- 407.75


#Calibración modelo---------------------------------------------------------------------------------


c_pib = 1.9 
c_a = 1
c_p = 1
c_s = 0.66 #0.66
c_su = 0.078
c_i = 1

pi =  c_pib * sqrt(var(tasas_macro$pib, na.rm = TRUE)); #10 #inv
i1 = c_i * sqrt(var(tasas_macro$inflacion, na.rm = TRUE)); #4 #inv
i2 = 2
s = c_s * sqrt(var(tasas_macro$sal, na.rm = TRUE));  #100 #inv
a = c_a * sqrt(var(tasas_macro$tasa_activa, na.rm = TRUE)); #120 #prop
p = c_p * sqrt(var(tasas_macro$tasa_pasiva, na.rm = TRUE)); #200 #prop
su = c_su * sqrt(var(tasas_macro$sbu, na.rm = TRUE)) ; #1.8 #inv max 50


#Diferenciación de la series------------------------------------------------------------------------
data <- tasas_macro %>%
  dplyr::filter(anio>=2006) %>%
  mutate(diff_pib = c(NA, diff(pib,1))/pi,
         diff_tasa_activa = c(NA,diff(tasa_activa,1))/a,
         diff_tasa_pasiva = c(NA,diff(tasa_pasiva,1))/p,
         diff_sal_prom = c(NA,diff(sal,1))/s,
         diff_sbu = c(NA,diff(sbu,1))/su,
         diff_inflacion = c(NA,base::diff((inflacion),1))/i1) %>%
  dplyr::select(-pib, -tasa_activa, -tasa_pasiva, -sal, -anio, -sbu, -inflacion)
data <- data[-1,]

train <- na.omit(data)

#Pruebas de estacionariedad de Dickey-Fuller--------------------------------------------------------
#Ho: No hay raíz unitaria presente
adfTest(train$diff_pib)
adfTest(train$diff_tasa_pasiva)
adfTest(train$diff_tasa_activa)
adfTest(train$diff_sal_prom)
adfTest(train$diff_sbu)
adfTest(train$diff_inflacion)


#Estimación del modelo VAR(1)-----------------------------------------------------------------------
m1<-VAR(train, include.mean = F, p=1)
m2=refVAR(m1,  thres = 1.96)
m2 = m2
#m2$coef
variables <- data.frame(var=c('PIB',
                              'Tasa pasiva',
                              'Tasa activa',
                              'Salario',
                              'SBU',
                              'Inflacion'))
coeficientes <- cbind(variables,
                      t(as.data.frame(m2$coef)) )

#Prueba de indepencia de residuos-------------------------------------------------------------------

#función mq aumentada-------------------------------------------------------------------------------
mq_aumentada <- function (x, lag = 24, adj = 0) 
{
  if (!is.matrix(x)) 
    x = as.matrix(x)
  nr = nrow(x)
  nc = ncol(x)
  g0 = var(x)
  ginv = solve(g0)
  qm = 0
  QM = NULL
  df = 0
  for (i in 1:lag) {
    x1 = x[(i + 1):nr, ]
    x2 = x[1:(nr - i), ]
    g = cov(x1, x2)
    g = g * (nr - i - 1)/(nr - 1)
    h = t(g) %*% ginv %*% g %*% ginv
    qm = qm + nr * nr * sum(diag(h))/(nr - i)
    df = df + nc * nc
    dff = df - adj
    mindeg = nc^2 - 1
    pv = 1
    if (dff > mindeg) 
      pv = 1 - pchisq(qm, dff)
    QM = rbind(QM, c(i, qm, dff, pv))
  }
  pvs = QM[, 4]
  dimnames(QM) = list(names(pvs), c("  m  ", "    Q(m) ", 
                                    "   df  ", " p-value"))
  cat("Ljung-Box Statistics: ", "\n")
  printCoefmat(QM, digits = 3)
  return(QM)
}

residuos = m2$residuals
residuos <- as.data.frame(residuos)
colnames(residuos) <- c("diff_pib",
                        "diff_tasa_activa",
                        "diff_tasa_pasiva",
                        "diff_sal_prom",
                        "diff_sbu",
                        "diff_inflacion")


#H0: Independencia de los residuos
#p>0.05 no se rechaza la H0

#png(file = paste0( parametros$resultado_graficos, 'iess_test_modelo', parametros$graf_ext ) )
box_ljung <- mq_aumentada(residuos, lag = 12)
#dev.off()

#No se rechaza la HO para todo retardo menor a 10
#No se rechaza la HO para todo retardo al nivel de significacia del 2%

#Prueba de nulidad de los coeficientes del VAR(1)---------------------------------------------------
#H0: Nulidad de los coeficientes
m3=VARchi(data[c(1:14),],p=1, thres=1.96)


#Prueba de normalidad de los residuos---------------------------------------------------------------

#H0: La variable sigue una distribución normal
#p>0.05 no se rechaza la H0
shapiro.test(residuos$diff_pib)
shapiro.test(residuos$diff_tasa_activa)
shapiro.test(residuos$diff_tasa_pasiva)
shapiro.test(residuos$diff_sal_prom)
shapiro.test(residuos$diff_sbu)
shapiro.test(residuos$diff_inflacion)

#Pruebas de homocedasticidad------------------------------------------------------------------------
#H0: Las desviaciones no tienen heterocedasticidad condicional
#p>0.05 no se rechaza la H0
MarchTest(residuos)


#Prueba de multicolinealidad------------------------------------------------------------------------
library(ppcor)
pcor(na.omit(data)[1:14,], method = "pearson")


#Predicciones del modelo----------------------------------------------------------------------------
#Predicción hasta 12/2026 por datos perdidos
data[is.na(data)]<-0

for (i in seq(16,16,1)) {
  aux <- Vpmiss(data, m2$Phi, m2$Sigma, tmiss=i, c(1,0,0,0,1,1), m2$cnst)
  data[i,2] <- aux[1]
  data[i,3] <- aux[2]
  data[i,4] <- abs(aux[3])
}

for (i in seq(17,20,1)) {
  aux <- Vpmiss(data, m2$Phi, m2$Sigma, tmiss=i, c(1,0,0,0,0,1), m2$cnst)
  data[i,2] <- aux[1]
  data[i,3] <- aux[2]
  data[i,4] <- abs(aux[3])
  data[i,5] <- abs(aux[4])
}

#Predicciones de 2027  a 2060-----------------------------------------------------------------------
b<-as.matrix(data[c(6:20),])
m2$data<-as.matrix(data[c(6:20),])
pred <- VARpred(m2, h = 34, orig = 0, output = TRUE)
data <-rbind( data, pred$pred)

fechas <- crossing(anio = 2006:2060)
aux <- as.data.frame(rbind(c(NA),data))
pred <- cbind(fechas, aux)


#Transformación a la serie original-----------------------------------------------------------------
pred[is.na(pred)] <-0
aux <- cumsum(c(46802044,pi*pred$diff_pib))
aux <- aux[-1]
pred['pib'] <- aux


aux <- c(pred$diff_tasa_activa)
aux <- diffinv(aux[-1]*a, lag = 1, xi=c(0.08845000))
pred['ta'] <- aux


aux <- c(pred$diff_tasa_pasiva)
aux <- diffinv(aux[-1]*p, lag = 1, xi=c(0.04348333))
pred['tp'] <- aux


aux <- c(pred$diff_sal_prom)
aux <- diffinv(aux[-1]*s, lag = 1, xi=c(4040.464))
pred['sal'] <- aux


aux <- c(pred$diff_sbu)
aux <- diffinv(aux[-1]*su, lag = 1, xi=c(160))
pred['sbu'] <- aux


aux <- c(pred$diff_inflacion)
aux <- diffinv(aux[-1]*i1, lag = 1, xi=c(0.033008495))
pred['inf'] <- aux



#Gráficos de las series-----------------------------------------------------------------------------
g <- pred %>%
  dplyr::select(pib,ta,tp,sal,inf, sbu)

autoplot(ts(g$pib,
            start = c(2006,1),
            frequency = 1), type = "l", ylab="pib") #+ ggtitle("Time Series ")

autoplot(ts(g$sal,
            start = c(2006,1),
            frequency = 1) , type = "l", ylab="salario anual") #+ ggtitle("Time Series ")

autoplot(ts(g$sbu,
            start = c(2006,1),
            frequency = 1) , type = "l",ylab = "sbu") #+ ggtitle("Time Series ")

autoplot(ts(g$ta,
            start = c(2006,1),
            frequency = 1), type = "l", ylab = "tasa activa") #+ ggtitle("Time Series ")

autoplot(ts(g$tp,
            start = c(2006,1),
            frequency = 1), type = "l", ylab = "tasa pasiva") #+ ggtitle("Time Series ")

autoplot(ts(g$inf,
            start = c(2006,1),
            frequency = 1), type = "l", ylab = "inflación promedio" ) #+ ggtitle("Time Series ")


#Calculo de las tasas de crecimiento promedio-------------------------------------------------------
aux <-pred %>%
  dplyr::filter(anio>=2021, anio<=2060) %>%
  dplyr::distinct(anio,.keep_all = TRUE) %>%
  dplyr::mutate( t_pib = (pib - dplyr::lag(pib))/dplyr::lag(pib) ) %>%
  dplyr::mutate(t_sal = (sal - dplyr::lag(sal))/dplyr::lag(sal) ) %>%
  dplyr::mutate(t_sbu = (sbu - dplyr::lag(sbu))/dplyr::lag(sbu) ) %>%
  dplyr::select( anio, t_pib, t_sal, t_sbu, ta, tp, inf)

(t_pib = mean(aux$t_pib, na.rm = TRUE))
(t_sal = mean(aux$t_sal, na.rm = TRUE))
(t_sbu = mean(aux$t_sbu, na.rm = TRUE))
(t_ta = mean(aux$ta, na.rm = TRUE))
(t_tp = mean(aux$tp, na.rm = TRUE))
(t_inf = mean(aux$inf, na.rm = TRUE))

tasas_macro_crec <- aux

#Tabla resumen de parámetros------------------------------------------------------------------------

hip_macro_resumen <- tasas_macro_crec %>%
  dplyr::filter(anio>=2021, anio<=2060) %>%
  dplyr::mutate(t_pib = 100 * mean( t_pib,   na.rm = TRUE),
                t_sal = 100 * mean( t_sal,   na.rm = TRUE),
                t_sbu = 100 * mean( t_sbu,   na.rm = TRUE),
                ta = 100 * mean( ta,   na.rm = TRUE),
                tp = 100 * mean( tp,   na.rm = TRUE),
                inf = 100 * mean( inf,   na.rm = TRUE) ) %>%
  distinct( t_pib, .keep_all = TRUE) %>%
  dplyr::select( -anio ) %>%
  gather(., key = 'hipotesis', value = 'tasas') %>%
  mutate( hipotesis = c('Crecimiento del PIB',
                        'Crecimiento Salarial',
                        'Crecimiento del SBU',
                        'Tasa Activa',
                        'Tasa Pasiva',
                        'Inflación promedio'))


#Guardar en un RData--------------------------------------------------------------------------------
message( '\tGuardando tasas macro' )

tasas_macro_pred <- pred %>%
  dplyr::select(anio,
                pib_nominal:= pib,
                tasa_activa:= ta,
                tasa_pasiva:= tp,
                sal_anual:=sal,
                sbu,
                inflación_prom:=inf)

save( tasas_macro_pred,
      tasas_macro_crec,
      hip_macro_resumen,
      coeficientes,
      box_ljung,
      file = paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )

#Exportar a excel-----------------------------------------------------------------------------------
write.xlsx(tasas_macro_pred,
           file = paste0( parametros$resultado_seguro , 'tasas_macro_predicciones.xlsx' ),
           sheetName = "hipotesis", 
           col.names = TRUE,
           row.names = FALSE, 
           append = FALSE)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()