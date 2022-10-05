# Creación estructura reporte ----------------------------------------------------------------------
if ( dir.exists( parametros$resultado_seguro ) ) {
  unlink( parametros$resultado_tablas, recursive = TRUE, force = TRUE )
  unlink( parametros$resultado_graficos, recursive = TRUE, force = TRUE )
  unlink( parametros$resultado_seguro, recursive = TRUE, force = TRUE )

}
dir.create( parametros$resultado_seguro )
dir.create( parametros$resultado_tablas )
dir.create( parametros$resultado_graficos )


REP_file_latex_org <- c( paste( parametros$work_dir, 'Reportes/hombre.png', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/mujer.png', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/Ciencias.png', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/EPN.png', sep = '' ),
                         paste( parametros$work_dir, 'Reportes/PaqueteR.png', sep = ''),
                         paste( parametros$work_dir, 'Reportes/NuevoPaqueteR.png', sep = '')
                         )

REP_file_latex_des <- c( paste( parametros$resultado_seguro, 'graficos/hombre.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/mujer.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/Ciencias.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/EPN.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/PaqueteR.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/NuevoPaqueteR.png', sep = '')
                         )

file.copy( REP_file_latex_org, REP_file_latex_des, overwrite = TRUE  )

