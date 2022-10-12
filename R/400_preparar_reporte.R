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
                         paste( parametros$work_dir, 'Reportes/doc_data.png', sep = '')
)

REP_file_latex_des <- c( paste( parametros$resultado_seguro, 'graficos/hombre.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/mujer.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/Ciencias.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/EPN.png', sep = '' ),
                         paste( parametros$resultado_seguro, 'graficos/PaqueteR.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/NuevoPaqueteR.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/git.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/funciones.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/pruebafunciones.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/existe.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/check.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/familia.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/pruebadata.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/urls.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/github.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/DESCRIPTION.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/Archivos_Rd.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/NAMESPACE.png', sep = ''),
                         paste( parametros$resultado_seguro, 'graficos/doc_data.png', sep = '')
)

file.copy( REP_file_latex_org, REP_file_latex_des, overwrite = TRUE  )
