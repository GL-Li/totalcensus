#' List of geographic header record of ACS
#'
#' @description  This dataset has the complete list of geographic header
#' references and their discription used in ACS 1-year and 5-year summary file.
#' Search the dataset with function \code{\link{search_acs_geoheader}}.
#'
#' @docType data
#'
#' @usage data("dict_acs_geoheader")
#'
#' @format A data.table with 53 rows and 4 variables
#' \describe{
#'   \item{reference}{reference of the geoheader record}
#'   \item{field}{description of the geoheader record field}
#'   \item{start}{starting position of the geoheader in the record}
#'   \item{end}{ending position of the geoheader in the record}
#' }
#'
#' @keywords datasets
#'
#' @source 2016 ACS Summary File
#' \href{https://www2.census.gov/programs-surveys/acs/summary_file/2016/documentation/tech_docs/2016_SummaryFile_Tech_Doc.pdf}{technical documentation},
#' page 10 - 11.
#'
#'
"dict_acs_geoheader"
