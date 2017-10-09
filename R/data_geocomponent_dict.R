#' List of geographic components and codes
#'
#' @description This dataset contains the geographic components and codes used in
#' Census 2010 summary file 1 (with urban/rural update). Search geographic components
#' with function \code{\link{search_geoheader}}.
#'
#' @docType data
#'
#' @usage data("dict_geocomp")
#'
#' @format A data.table with 114 rows and 2 variables:
#' \describe{
#'   \item{code}{code for the geocomponent, such as "01" and "M3"}
#'   \item{geo_component}{description of the geographic component}
#' }
#'
#' @keywords datasets
#'
#' @source  2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 6-15
#'
#'
"dict_geocomp"
