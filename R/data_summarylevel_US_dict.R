#' Summary levels and codes for national files
#'
#' @description  This data contains summary levels and codes used in national files of
#'  summary file 1 (with urban/rural update). Search with function \code{\link{search_sumlev_US}}.
#'  This dataset is different from that of state files, \code{\link{dict_summarylevel}},
#'  which is searched with \code{\link{search_sumlev}}.
#'
#'
#' @docType data
#'
#' @usage data("dict_summarylevel_US")
#'
#' @format A data.table with 77 rows and 2 variables:
#' \describe{
#'   \item{code}{code of summary level}
#'   \item{summary_level}{description of summary level}
#' }
#'
#' @keywords datasets
#'
#' @references 2010 Census Summary File 1
#' \href{https://www.census.gov/prod/cen2010/doc/sf1.pdf}{technical documentation}
#' page 4-25 state summary file with urban/rural update
#'
#'
"dict_summarylevel_US"
