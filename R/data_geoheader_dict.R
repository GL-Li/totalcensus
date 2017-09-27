#' Complete list of geographic Header Record in Summary File 1
#'
#' This data serves as a dictionary for geographic header references and their
#' discription.
#'
#' @docType data
#'
#' @usage data("dict_geoheader")
#'
#' @format A data.table with 101 rows and 4 variables
#' \describe{
#'   \item{reference}{reference of the geoheader record}
#'   \item{field}{description of the geoheader record field}
#'   \item{start}{starting position of the geoheader in the record}
#'   \item{end}{ending position of the geoheader in the record}
#' }
#'
#' @keywords datasets
#'
#' @references 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 2-8
#'
#' @seealso \code{\link{search_geoheader}}
#'
"dict_geoheader"
