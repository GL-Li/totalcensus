#' Contents of data files
#'
#' @description This dataset includes all data fields of data files in census 2010
#' summary file 1 (with urban/rural update). Fucntion \code{\link{search_datafile}}
#' searches content in this dataset.
#'
#'
#' @docType data
#'
#' @usage data("dict_datafile")
#'
#' @format A data.table with 9199 rows and 6 variables:
#' \describe{
#'   \item{file_segment}{sequence number of segment data files, from 1 to 48}
#'   \item{table_content}{description of content in a census table}
#'   \item{reference}{reference of the field, such as "LOGRECNO", "PCT0240019"}
#'   \item{table_number}{table number such as "H1", "PCT22G"}
#'   \item{table_name}{description of table, which has many table_content}
#'   \item{universe}{the unit of the census data}
#' }
#'
#' @keywords datasets
#'
#' @source 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' all across chapter 6.
#'
#'

"dict_datafile"
