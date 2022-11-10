#-------------------------------------------------------------------#
# #
# Nuevo Paquete R Base Familia #
# #
# Written by: Jonathan Pallasco #
# Escuela Politecnica Nacional, Ecuador #
# jonathan.pallasco@epn.edu.ec #
# #
#-------------------------------------------------------------------#

##' @title Number of People in a Family
##' @description
##' Data set corresponding to the surveys of the number of people
##' who live in 200 families in Ecuador. Especially in poor areas
##' of the country.
##' @name Familia
##' @docType data
##' @format A data frame with 200 observations composed of the
##' following 3 variables:
##' \describe{
##' \item{Id}{Id of the surveyed families.}
##' \item{Persona}{Number of people in the family.}
##' \item{Tipo}{Type of Family according to the number of people
##' that make it up (Normal Family, Average Family, Large Family).}
##' }
##' @keywords datasets
##'
##' @examples
##' library(NuevoPaqueteR)
##' data(Familia)
##' summary(Familia)
##' attach(Familia)
##' str(Familia)
##'
NULL
