#' Search summary level
#'
#' search the code or description of summary level
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @export
#'
#' @seealso \code{\link{dict_summarylevel}} lists all geocomponents and codes
#'
#' @examples
#' # search geocomponent contains "block"
#' search_sumlev("block")
#'
#' # search geocomponent with code 40
#' search_sumlev("40")
#' # or
#' search_sumlev(40)


search_sumlev <- function(keyword, view = TRUE){
    dt1 <- dict_summarylevel[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict_summarylevel[grepl(tolower(keyword), tolower(description))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(description, code)] %>%
        unique()

    if (view) View(dt)

    return(dt)
}
