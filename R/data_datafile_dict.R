#' Components of data files
#'
#' This data includes all data fields in data files in census 2010 summary file 1 with urban/rural update.
#'
#'
#'
#' @docType data
#'
#' @usage data("dict_datafile")
#'
#' @format A data.table with 9199 rows and 6 variables:
#' \describe{
#'   \item{file}{sequence number of the data file, from 1 to 48}
#'   \item{field}{description of field in a census table}
#'   \item{reference}{reference of the field, such as "LOGRECNO", "PCT0240019"}
#'   \item{table_num}{table number such as "H1", "PCT22G"}
#'   \item{table}{description of the table, which has many fields}
#'   \item{universe}{the unit of the census data}
#' }
#'
#' @keywords datasets
#'
#' @references 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' all across chapter 6.
#'
#' @seealso \code{\link{search_datafile}}
#'

"dict_datafile"
