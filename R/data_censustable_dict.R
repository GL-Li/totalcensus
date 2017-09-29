#' Complete list of census tables
#'
#' All census tables in census 2010 summary file 1 with urban/rural update
#'
#'
#' @docType data
#'
#' @usage data("dict_censustable")
#'
#' @format A data.table with 333 rows and 4 variables:
#' \describe{
#'   \item{table_ref}{reference code such as "H0010", "PCT022G"}
#'   \item{table_number}{table number such as "H1", "PCT22G"}
#'   \item{table_title}{description of the table}
#'   \item{universe}{unit of the table}
#' }

#'
#' @keywords datasets
#'
#' @references 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' all across chapter 5.
#'
#' @seealso \code{\link{search_table}}
#'
"dict_censustable"
