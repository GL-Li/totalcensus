#' Search summary levels of national files
#'
#' Search the code or description of summary levels of national files
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @return A data.table of summary levels match the search
#'
#' @seealso \code{\link{dict_summarylevel_US}} lists all geocomponents and codes
#'
#' @examples
#' # search summary level of geocomponent contains "block"
#' search_sumlev_US("block")
#'
#' # search summary level with code 40
#' search_sumlev_US("40")
#' # or
#' search_sumlev_US(40)
#'
#' @export
#' @import data.table
#' @import magrittr
#'


search_sumlev_US <- function(keyword, view = TRUE){
    dt1 <- dict_summarylevel_US[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict_summarylevel_US[grepl(tolower(keyword), tolower(description))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(description, code)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
