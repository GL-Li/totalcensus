#' All tables in census 2010 summary file 1 with urban/rural update
#'
#' This data serves as a dictionary to search for census tables.
#'
#' @docType data
#'
#' @usage data("dict_censustable")
#'
#' @format data.table
#' \describe{
#'   \item{table_ref}{reference code such as "H0010", "PCT022G"}
#'   \item{table_num}{table number such as "H1", "PCT22G"}
#'   \item{table}{description of the table}
#'   \item{universe}{unit of the table}
#' }

#'
#' @keywords datasets
#'
#' @references 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' all across chapter 6.
#'
#'
"dict_censustable"
