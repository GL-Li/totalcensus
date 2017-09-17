#' Search geographic component
#'
#' search the code or description of geographic component
#'
#' @param keyword keyword to search in code or description
#'
#' @export
#'
#' @seealso \code{\link{geocomp_dict}} lists all geocomponents and codes
#'
#' @examples
#' # search geocomponent contains "urban cluster"
#' search_geocomp("urban cluster")
#'
#' # search geocomponent with code 43
#' search_geocomp("43")
#' # or
#' search_geocomp(43)


search_geocomp <- function(keyword){
    dt1 <- geocomp_dict[grepl(tolower(keyword), tolower(code))]
    dt2 <- geocomp_dict[grepl(tolower(keyword), tolower(description))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(description, code)] %>%
        unique()
    return(dt)
}
