#' List FIPS code as of 2016 in the US
#'
#' @description This dataset contains a complete list of FIPS of states, counties,
#' county subdivisions, places, consolidated cities, and their names and summary
#' levels as well as full name and abbreviation of state. Search for
#' FIPS with function \code{\link{search_fips}}.
#'
#' @docType data
#'
#' @usage data("dict_fips")
#'
#' @format A data.table with 43934 rows and 9 variables:
#' \describe{
#'   \item{state_full}{full name of a state such as "Alabama"}
#'   \item{state_abbr}{abbreviation of a state such as "AL"}
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
#' @source
#' \href{https://www.census.gov/geographies/reference-files/2016/demo/popest/2016-fips.html}{List of FIPS as of 2016}
#'
#'
"dict_fips"
