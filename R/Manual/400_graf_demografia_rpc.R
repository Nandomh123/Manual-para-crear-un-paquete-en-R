message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGraficos demograficos' )
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
 
load( paste0( parametros$RData_seg, 'IESS_AVE_tablas_demografico.RData' ) )
# Llamado a alas datas ----
load(paste0( parametros$RData_seg, 'IESS_AVE_analisis_demografico.RData' )) # 

# Afiliados historicos ----
aux <- copy( afi_histo )
aux$Anio <- as.integer( aux$Anio )

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 17500)
y_brk <- seq( y_lim[1], y_lim[2], by = 5000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_pob_afi_hist_rpc <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Masculino, colour = "Afiliados Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = Anio, 
                  y = Femenino, colour = "Afiliados Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = Anio, 
                  y = Genero, colour = "Afiliados" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c("Afiliados" ,"Afiliados Masculino","Afiliados Femenino"),
                       values = c( "Afiliados Masculino" = parametros$male,
                                   "Afiliados Femenino" =  parametros$female,
                                   "Afiliados" = parametros$iess_green) ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Afiliados voluntarios en el exterior' ) +
  theme( legend.position = c( 0.5, 0.98 ), #"top", 
         legend.direction = "horizontal",
         axis.text.x = element_text( angle = 0, hjust = 1), 
         legend.text = element_text(size = 6, colour = "black"))+
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 

ggsave( plot = iess_pob_afi_hist_rpc,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_afi_hist_rpc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Afiliados fallecidos ----
aux1 <- copy( afili_fall )
aux1$ANIO_FALLECIMIENTO <- as.integer( aux1$ANIO_FALLECIMIENTO )

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 600)
y_brk <- seq( y_lim[1], y_lim[2], by = 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_pob_afi_falle_rpc <- ggplot( data = aux1 ) +
  geom_line( aes( x = ANIO_FALLECIMIENTO,
                  y = H, colour = "Afiliados Fallecidos Masculino" ), size = graf_line_size  ) +
  geom_line( aes( x = ANIO_FALLECIMIENTO,
                  y = M, colour = "Afiliados Fallecidos Femenino" ), size = graf_line_size  ) +
  geom_line( aes( x = ANIO_FALLECIMIENTO,
                  y = Total, colour = "Afiliados Fallecidos" ), size = graf_line_size  ) +
  scale_colour_manual( "",
                       breaks = c("Afiliados Fallecidos" ,"Afiliados Fallecidos Masculino",
                                  "Afiliados Fallecidos Femenino"),
                       values = c( "Afiliados Fallecidos Masculino" = parametros$male,
                                   "Afiliados Fallecidos Femenino" =  parametros$female,
                                   "Afiliados Fallecidos" = parametros$iess_green) ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Afiliados voluntarios fallecidos en el exterior' ) +
  theme( legend.position = c( 0.5, 0.93 ), #"top",
         legend.direction = "horizontal",
         axis.text.x = element_text( angle = 0, hjust = 1),
         legend.text = element_text(size = 6, colour = "black"))+

  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim )

ggsave( plot = iess_pob_afi_falle_rpc,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_afi_falle_rpc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Afiliados por edad y sexo 2020 ----
message( '\tGraficando población afiliada activa inicial por edad y sexo' )

aux <- copy( edad_sexo_perso )
max_edad <- 80
min_edad <- 18
aux <- aux[ edad >= min_edad & edad <= max_edad ]
setnames( aux, c('edad', 'sexo', 'n' ) )
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- as.numeric( (aux[,sum(n,na.rm = TRUE)]) )  # número total por sexo

aux[ sexo == "H", n:=-n ]
aux[ , n:= n/N ]

salto_y<-5
salto_x<-0.01
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')
limite <- max(abs(aux$n))+0.05*max(abs(aux$n))

iess_pir_afiliados_rpc <- ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = c( -limite, limite ) ) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x ) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_afiliados_rpc,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados_rpc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Fallecidos por edad y sexo 2020 ----
message( '\tGraficando población afiliada fallecida por edad y sexo' )

aux <- copy( muertos_edad_sexo )
max_edad <- 90
min_edad <- 18
aux <- aux[ EDAD_FALL >= min_edad & EDAD_FALL <= max_edad ]
setnames( aux, c('edad', 'sexo', 'n' ) )
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- as.numeric( (aux[,sum(n,na.rm = TRUE)]) )  # número total por sexo

aux[ sexo == "H", n:=-n ]
aux[ , n:= n/N ]

salto_y<-5
salto_x<-0.01
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')
limite <- max(abs(aux$n))+0.05*max(abs(aux$n))

iess_pir_afiliados_falle_rpc <- ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = c( -limite, limite )) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x ) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_afiliados_falle_rpc,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados_falle_rpc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGrafico poblacion afiliada' )


# Creción de la tabla ----
base_tact <- merge( Mujeres_tact, Hombres_tact, by = c('Anio'))
base_tact <- base_tact[ Anio > 2020 & Anio <= 2060]
base_tact <- base_tact[ , Total := mujeres+hombres]
setnames(base_tact, c('Anio','Femenino','Masculino','Genero'))
base_tact <- rbind( afi_histo, base_tact) 


# Creción de gráficos ----

aux <- copy( base_tact )
aux$Anio <- as.integer( aux$Anio )
x_lim <- c( 2010, 2060 )
x_brk <- seq( x_lim[1], x_lim[2], by = 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
y_lim <- c( 0, 30000)
y_brk <- seq( y_lim[1], y_lim[2], by = 5000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pob_base_tact_rpc <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Masculino, colour = "Afiliados Masculino" )  ) + 
  geom_line( aes( x = Anio, 
                  y = Femenino, colour = "Afiliados Femenino" ) ) + 
  geom_line( aes( x = Anio, 
                  y = Genero, colour = "Afiliados" ) ) + 
  scale_colour_manual( "",
                       breaks = c("Afiliados" ,"Afiliados Masculino","Afiliados Femenino"),
                       values = c( "Afiliados Masculino" = parametros$male,
                                   "Afiliados Femenino" =  parametros$female,
                                   "Afiliados" = parametros$iess_green) ) +
  theme_bw()  +
  labs(  x = 'Año', y = 'Número de personas' ) +
  theme( legend.position = c( 0.5, 0.98 ), #"top", 
         legend.direction = "horizontal",
         axis.text.x = element_text( angle = 0, hjust = 0.5,), 
         legend.text = element_text(size = 8, colour = "black"))+
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 

ggsave( plot = iess_pob_base_tact_rpc,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_base_tact_rpc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# ------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Evolucion de las personas en el exterior

# Hombres ------
# Creación de tabla
Hombres_actgx <- Hombres_actgx[ , !c(43:82)] # Eliminamos la primera comuna
# Creación de gráficos 
Hombres_actgx <- melt(Hombres_actgx, id.vars = 'Edad')
aux <- copy( Hombres_actgx )
setnames(aux, c('Edad','Año','value'))
aux$Edad <- as.integer( aux$Edad )
aux$Año <- as.character(aux$Año)

x_lim <- c( 15, 70 )
x_brk <- seq( x_lim[1], x_lim[2], by = 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', )
y_lim <- c( 0, 500)
y_brk <- seq( y_lim[1], y_lim[2], by = 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pob_hombres_actgx_rpc <- ggplot(data = aux, aes(x = Edad, y = value, fill = Año)) +
  geom_line(aes(color = as.integer(Año))) +
  theme_bw()  +
  labs(  x = 'Edad', y = 'Número de Hombres') +
  theme(axis.text.x = element_text( angle = 0, hjust = 0.5),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title.align = 0.5,
        # legend.key.size = unit(1.5, 'cm'),
        legend.key.height = unit(1.5, 'cm')) +
  guides(fill = guide_legend(title = "Año")) +
  scale_color_viridis(option = 'D')+
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim )

iess_pob_hombres_actgx_rpc

# Guardamos --------------
ggsave( plot = iess_pob_hombres_actgx_rpc,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_hombres_actgx_rpc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#-------------------------------------------------------------------------------
# Mujeres ------
# Creación de tabla
Mujeres_actgx <- Mujeres_actgx[ , !c(43:82)] # Eliminamos la primera comuna
# Creación de gráficos 
Mujeres_actgx <- melt(Mujeres_actgx, id.vars = 'Edad')
aux <- copy( Mujeres_actgx )
setnames(aux, c('Edad','Año','value'))
aux$Edad <- as.integer( aux$Edad )
aux$Año <- as.character(aux$Año)

x_lim <- c( 15, 70 )
x_brk <- seq( x_lim[1], x_lim[2], by = 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', )
y_lim <- c( 0, 650)
y_brk <- seq( y_lim[1], y_lim[2], by = 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pob_mujeres_actgx_rpc <- ggplot(data = aux, aes(x = Edad, y = value, fill = Año)) +
  geom_line(aes(color = as.integer(Año)),size = 0) + 
  theme_bw()  +
  labs(  x = 'Edad', y = 'Número de Mujeres') +
  theme(axis.text.x = element_text( angle = 0, hjust = 0.5),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title.align = 0.5,
        # legend.key.size = unit(1.5, 'cm'),
        legend.key.height = unit(1.5, 'cm')) +
  guides(fill = guide_legend(title = "Año")) +
  scale_color_viridis(option = 'D')+
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim )

iess_pob_mujeres_actgx_rpc
 
# Guardamos ---------
ggsave( plot = iess_pob_mujeres_actgx_rpc,
        filename = paste0( parametros$resultado_graficos, 'iess_pob_mujeres_actgx_rpc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# ----------------------------------------------------------------------

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()


