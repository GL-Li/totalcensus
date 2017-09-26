#' Search geographic header
#'
#' Search in field reference or description with a keyword
#'
#'
#' @param keyword keyword in description or reference
#' @param view display the search result with View if TRUE
#'
#' @return data.table match the search criteria
#'
#' @examples
#' # search geoheader that contains keyword "india"
#' search_geoheader("india")
#'
#' # search geoheader of reference containing "INTPT"
#' search_geoheader("INTPT")
#'
#' @seealso \code{\link{dict_geoheader}}, which lists all the geographic headers
#'
#' @export
#'
search_geoheader <- function(keyword, view = TRUE) {
    dt1 <- dict_geoheader[grepl(tolower(keyword), tolower(reference))]
    dt2 <- dict_geoheader[grepl(tolower(keyword), tolower(description))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(description, reference)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
