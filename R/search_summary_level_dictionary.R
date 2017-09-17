#' Search summary level
#'
#' search the code or description of summary level
#'
#' @param keyword keyword to search in code or description
#'
#' @export
#'
#' @seealso \code{\link{summarylevel_dict}} lists all geocomponents and codes
#'
#' @examples
#' # search geocomponent contains "block"
#' search_sumlev("block")
#'
#' # search geocomponent with code 40
#' search_sumlev("40")
#' # or
#' search_sumlev(40)


search_sumlev <- function(keyword){
    dt1 <- summarylevel_dict[grepl(tolower(keyword), tolower(code))]
    dt2 <- summarylevel_dict[grepl(tolower(keyword), tolower(description))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(description, code)] %>%
        unique()
    return(dt)
}
