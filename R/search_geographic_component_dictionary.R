#' Search geographic component
#'
#' search the code or description of geographic component
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @export
#'
#' @seealso \code{\link{dict_geocomp}} lists all geocomponents and codes
#'
#' @examples
#' # search geocomponent contains "urban cluster"
#' search_geocomp("urban cluster")
#'
#' # search geocomponent with code 43
#' search_geocomp("43")
#' # or
#' search_geocomp(43)


search_geocomp <- function(keyword, view = TRUE){
    dt1 <- dict_geocomp[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict_geocomp[grepl(tolower(keyword), tolower(description))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(description, code)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
