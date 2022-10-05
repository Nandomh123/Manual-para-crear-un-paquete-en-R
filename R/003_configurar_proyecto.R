# Parámetros globales R ----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tConfiguración global de R' )

options( scipen = 99 )
setNumericRounding( 2 )
options( stringsAsFactors = FALSE )

# Parámetros ---------------------------------------------------------------------------------------
message( '\tCreando entorno de parámetros' )

# Entorno con parámetros
parametros <- new.env()

# User name
parametros$user <- Sys.getenv( 'USER' )

parametros$fec_eje <- Sys.Date()

# Operating system name
parametros$opsys <- Sys.info()[[1]]

# Hostname
parametros$hostname <- Sys.info()[[4]]

#Servidor de datos
parametros$data_server <- 'C:/Users/Usuario01/Documents/Pasantes/Jonathan_Pallasco/Manual-para-crear-un-paquete-en-R/Manual/'

# local
#parametros$data_server <- paste0( getwd(), '/' )


# Directorio de trabajo
parametros$work_dir <- paste0( getwd(), '/' )

# Setting Time Zone
parametros$time_zone <- "America/Guayaquil"

# Colores IESS
parametros$iess_blue <- rgb( 0, 63, 138, maxColorValue = 255 )
parametros$iess_green <- rgb(206, 33, 8, maxColorValue = 255)
parametros$iess_total <- rgb(128, 128, 128, maxColorValue = 255  )
parametros$female <- rgb(128, 128, 128, maxColorValue = 255  )
parametros$male <- rgb( 0, 139, 139, maxColorValue = 255 )
parametros$epn <- rgb(206, 33, 8, maxColorValue = 255)
parametros$ciencias <- rgb( 0, 63, 138, maxColorValue = 255 )
parametros$ciencias1 <- rgb(128, 128, 128, maxColorValue = 255  )

# Calcular balance
# parametros$calcular_balance <- FALSE

parametros$mont_prop_afi <- 0.1275

# Direcciones globables  ---------------------------------------------------------------------------
message( '\tEstableciendo directorios globales' )
parametros$empresa <- 'EPN'

message( '\tConfiguración seguro' )

# Parametro realizar análisis demográfico
parametros$hacer_ana_dem <- FALSE
parametros$calcular_balance <- FALSE



# Configuraciones particulares por seguro ----------------------------------------------------------
parametros$fec_fin <- ymd( '2020-12-31' )
parametros$anio_ini <- 2020
parametros$anio <- 2021 # Año del estudio
parametros$edad_max <- 105

parametros$horizonte <- 40 # en años
parametros$fec_ini <- ymd( '2013-01-01' ) # fecha inicio del periodo de observación

parametros$seguro <- 'Manual'

# Variables automáticas ----------------------------------------------------------------------------
parametros$Data <- paste0( parametros$data_server, 'Data/' )
parametros$RData <- paste0( parametros$data_server, 'RData/' )

parametros$reportes <- paste0( parametros$work_dir, 'Reportes/' )
parametros$resultados <- paste0( parametros$work_dir, 'Resultados/' )
parametros$reporte_seguro <- paste0( parametros$work_dir, 'Reportes/Reporte_',
                                     parametros$seguro, '/' )

parametros$reporte_genera <- paste0( parametros$work_dir, 'R/Manual/600_reporte_latex.R' )

parametros$reporte_script <- paste0( parametros$reporte_seguro, 'reporte.R' )
parametros$reporte_nombre <- paste0( parametros$empresa, '_',
                                     parametros$seguro, '_crear_paquete' )
parametros$reporte_latex <- paste0( parametros$reporte_nombre, '.tex' )
parametros$resultado_seguro <- paste0( parametros$resultados, parametros$reporte_nombre, '_',
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/' )
parametros$resultado_tablas <- paste0( parametros$resultados, parametros$reporte_nombre, '_',
                                       format( parametros$fec_eje, '%Y_%m_%d' ), '/tablas/' )
parametros$resultado_graficos <- paste0( parametros$resultados, parametros$reporte_nombre, '_',
                                         format( parametros$fec_eje, '%Y_%m_%d' ), '/graficos/' )

parametros$graf_modelo_1 <- 'R/401_graf_plantilla.R'
parametros$graf_ext <- '.png'

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()

