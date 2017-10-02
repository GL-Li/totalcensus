#' Search geographic components
#'
#' Search the code or description of geographic component
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#' @seealso \code{\link{dict_geocomp}} lists all geocomponents and codes
#'
#' @examples
#' \dontrun{
#'   # search geocomponents containing "urban cluster"
#'   search_geocomp("urban cluster")
#'
#'   # search geocomponents with code 43
#'   search_geocomp("43")
#'   # or
#'   search_geocomp(43)
#' }
#'
#' @export
#' @import data.table
#' @import magrittr


search_geocomp <- function(keyword, view = TRUE){
    dt1 <- dict_geocomp[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict_geocomp[grepl(tolower(keyword), tolower(geo_component))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(geo_component, code)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
