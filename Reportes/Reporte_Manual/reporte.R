# Script generado para compilar el informe
message( paste( rep( '-', 100 ), collapse = '' ) )

# Carga información --------------------------------------------------------------------------------
message('\tCargando información' )
setwd( parametros$work_dir )

# Control ------------------------------------------------------------------------------------------
REP_copy_final <- FALSE
REP_knit_quiet <- TRUE
REP_hacer_graficos <- TRUE
REP_hacer_tablas <- TRUE
REP_latex_clean <- TRUE
REP_latex_aux_clean <- FALSE
REP_latex_quiet <- TRUE

# Parámetros ---------------------------------------------------------------------------------------
message('\tEstableciendo parámetros')
REP_rep_nom <- parametros$reporte_nombre
REP_empresa <- parametros$empresa
REP_fec_eje <- format( parametros$fec_eje, '%Y_%m_%d' )
REP_rep_dir <- parametros$resultado_seguro
REP_rep_tab <- parametros$resultado_tablas
REP_rep_gra <- parametros$resultado_graficos
REP_rep_latex <- parametros$reporte_latex
REP_hor <- parametros$horizonte
REP_style <- 'style.tex'
REP_bib_lib <- 'bibliografia_libros.bib'
REP_bib_art <- 'bibliografia_articulos.bib'
REP_bib_ley <- 'bibliografia_leyes.bib'

REP_tit <- 'Manual para crear un nuevo paquete en R'
REP_nom_seg <- 'Paso a paso de la creación de un nuevo paquete en R'
REP_seg <- 'Nuevo paquete R'

paste( 'Paquete R', parametros$seguro )
REP_fec_fin <- format( parametros$fec_fin, '%Y-%m-%d' )
REP_fec_val <- format( ymd( '2019-09-16' ), '%Y-%m-%d' )
REP_watermark <- paste0( 'Manual Nuevo Paquete R'
                         # , parametros$fec_eje, ' ', format( Sys.time(), '%H:%M:%S' )
)
REP_version <- digest( paste0( 'EPN', format( Sys.time(), '%Y%m%d%H' ) ), algo = 'sha256', file = FALSE )

# Copia de resultados  -----------------------------------------------------------------------------
REP_file_latex_org <- c( paste( parametros$work_dir, 'Reportes/bibliografia_libros.bib', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/bibliografia_articulos.bib', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/bibliografia_leyes.bib', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/style.tex', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/ciencias.png', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/EPN.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/logo_iess_azul.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/PaqueteR.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/NuevoPaqueteR.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/git.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/funciones.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/pruebafunciones.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/existe.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/check.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/familia.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/pruebadata.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/urls.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/github.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/DESCRIPTION.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/Archivos_Rd.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/NAMESPACE.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/doc_data.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/pkgdown.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/website.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/check_data_funciones.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/urlwebsite.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/github_1.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/github_2.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/github_3.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/github_4.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/github_5.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/minty.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/varbslib.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/tema.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/ws-barra-nave.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/t-f.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/footer.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/linkautor.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/README.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/SW-dev.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/web-readme.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/logo.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/weblogo.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/ord-func.png', sep = '')
)

REP_file_latex_des <- c( paste( REP_rep_dir, 'bibliografia_libros.bib', sep = '' ),
                         paste( REP_rep_dir, 'bibliografia_articulos.bib', sep = '' ),
                         paste( REP_rep_dir, 'bibliografia_leyes.bib', sep = '' ),
                         paste( REP_rep_dir, 'style.tex', sep = '' ),
                         paste( REP_rep_dir, 'graficos/ciencias.png', sep = '' ),
                         paste( REP_rep_dir, 'graficos/EPN.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/logo_iess_azul.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/PaqueteR.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/NuevoPaqueteR.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/git.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/funciones.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/pruebafunciones.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/existe.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/check.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/familia.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/pruebadata.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/urls.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/github.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/DESCRIPTION.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/Archivos_Rd.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/NAMESPACE.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/doc_data.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/pkgdown.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/website.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/check_data_funciones.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/urlwebsite.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/github_1.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/github_2.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/github_3.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/github_4.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/github_5.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/minty.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/varbslib.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/tema.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/ws-barra-nave.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/t-f.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/footer.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/linkautor.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/README.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/SW-dev.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/web-readme.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/logo.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/weblogo.png', sep = ''),
                         paste( REP_rep_dir, 'graficos/ord-func.png', sep = '')
)

REP_file_latex_clean <- c( paste( REP_rep_dir, 'bibliografia_libros.bib', sep = '' ),
                           paste( REP_rep_dir, 'bibliografia_articulos.bib', sep = '' ),
                           paste( REP_rep_dir, 'bibliografia_leyes.bib', sep = '' ),
                           paste( REP_rep_dir, 'style.tex', sep = '' ) )

file.copy( REP_file_latex_org, REP_file_latex_des, overwrite = TRUE  )

# Compilación reporte ------------------------------------------------------------------------------
message('\tInicio compilación')

# Genera información automática --------------------------------------------------------------------
source( paste0( parametros$reporte_seguro, 'auto_informacion.R' ), encoding = 'UTF-8', echo = FALSE )

# Kniting reporte ----------------------------------------------------------------------------------
setwd( parametros$reporte_seguro )
knit( input = "reporte.Rnw",
      output = paste0( REP_rep_dir, REP_rep_latex ),
      quiet = REP_knit_quiet, encoding = 'utf8' )

# Compilacion LaTeX --------------------------------------------------------------------------------
message('\tInicio compilación LaTeX')
setwd( REP_rep_dir )
tools::texi2pdf( REP_rep_latex, quiet = REP_latex_quiet, clean = REP_latex_clean )
setwd( parametros$work_dir )
message('\tFin compilación LaTeX')

if( REP_latex_aux_clean ) {
  unlink( REP_file_latex_clean, recursive = TRUE )
}

message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

