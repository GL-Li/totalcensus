# fips ========================================================================
#' List of FIPS code as of 2016 in the US
#'
#' @description This dataset contains a list of FIPS of states, counties,
#' county subdivisions, places, consolidated cities, and their names and summary
#' levels as well as full name and abbreviation of state. It does NOT contain
#' FIPS of many small areas. Search for FIPS with function
#' \code{\link{search_fips}}.
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


# cbas ========================================================================
#' List CBSA code of Metropolitan Statistical Area/Micropolitan Statistical Area
#'
#' @description This dataset contains Metropolitan Statistical Area/Micropolitan
#' Statistical Area CBSA code and title, plus associated metrodivision, CSA, state,
#' and county code. Search for CBSA with function \code{\link{search_cbsa}}.
#'
#' @docType data
#'
#' @usage data("dict_cbsa")
#'
#' @format A data.table with 1882 rows and 12 variables:
#' \describe{
#'   \item{CBSA}{CBSA code}
#'   \item{CBSA_title}{CBSA title}
#'   \item{state_full}{full name of the state. A cbsa could include multiple states}
#'   \item{county}{county or county equivalent}
#'   \item{CSA}{code of the CSA to which the CBSA belongs}
#'   \item{CSA_title}{CSA title}
#'   \item{METDIV}{metro division code}
#'   \item{METDIV_title}{metro division title}
#'   \item{metro_micro}{is the CBSA a metropolitan or a micropolitan statistic area}
#'   \item{STATE}{FIPS of the state}
#'   \item{COUNTY}{FIPS of the county}
#'   \item{central_outlying}{is the counry a central or outlying county in the CBSA}
#' }
#'
#' @keywords datasets
#'
#' @source
#' \href{http://www.nber.org/data/cbsa-fips-county-crosswalk.html}{List of CBSA}
#'
#'
"dict_cbsa"

