#' Summary levels and codes for state files
#'
#' @description  This data contains summary levels and codes used in state files of
#'  summary file 1 (with urban/rural update). Search with function \code{\link{search_sumlev}}.
#'  This dataset is different from that of national files, \code{\link{dict_summarylevel_US}},
#'  which is searched with \code{\link{search_sumlev_US}}.
#'
#' @docType data
#'
#' @usage data("dict_summarylevel")
#'
#' @format A data.table with 99 rows and 2 variables
#' \describe{
#'   \item{code}{code of summary level}
#'   \item{summary_level}{description of summary level}
#' }
#'
#' @keywords datasets
#'
#' @source 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 4-16 state summary file with urban/rural update
#'
#'
#'

"dict_summarylevel"
