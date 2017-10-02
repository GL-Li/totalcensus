#' Search summary levels of state files
#'
#' Search the code or description of summary levels of state files.
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @return A data.table of searched results.
#'
#' @seealso \code{\link{dict_summarylevel}} lists all geocomponents and codes
#'
#' @examples
#' \dontrun{
#'   # search summary levels of geocomponent contains "block"
#'   search_sumlev("block")
#'
#'   # search summary levels of code 40
#'   search_sumlev("40")
#'   # or
#'   search_sumlev(40)
#' }
#'
#' @export
#' @import data.table
#' @import magrittr


search_sumlev <- function(keyword, view = TRUE){
    dt1 <- dict_summarylevel[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict_summarylevel[grepl(tolower(keyword), tolower(summary_level))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(summary_level, code)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
