#' Complete FIPS code as of 2016 in the US
#'
#' This data contains a complete list of fips as of 2016
#'
#' @docType data
#'
#' @usage data("dict_fips")
#'
#' @format A data.table with 43934 rows and 9 variables:
#' \describe{
#'   \item{state_full}{full name of a state such as "Alabama"}
#'   \item{state}{abbreviation of a state such as "AL"}
#'   \item{STATE}{FIPS code of the state}
#'   \item{SUMLEV}{summary level of  the entry in the row}
#'   \item{COUNTY}{FIPS code of county}
#'   \item{CUSUB}{FIPS of COUnty SUBdivision}
#'   \item{PLACE}{FIPS code of place}
#'   \item{CONCIT}{FIPS code of CONsolidated CITy}
#'   \item{NAME}{name of the entry in the row}
#' }
#'
#' @keywords datasets
#'
#' @references
#' \href{https://www.census.gov/geographies/reference-files/2016/demo/popest/2016-fips.html}{list of all fips as of 2016}
#'
#' @seealso \code{\link{search_fips}}
#'
"dict_fips"
