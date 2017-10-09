#' Search geographic headers
#'
#' @description Search in field reference or description of geographic header
#' with keyword in dataset \code{\link{dict_geoheader}}.
#'
#'
#' @param keyword keyword in description or reference
#' @param view display the search result with View if TRUE
#'
#' @return data.table matching the search criteria
#'
#' @examples
#' \dontrun{
#'   # search geoheader that contains keyword "india"
#'   search_geoheader("india")
#'
#'   # search for lattitude
#'   search_geoheader("latitu")
#'
#'   # search geoheader of reference containing "INTPT"
#'   search_geoheader("INTPT")
#' }
#'
#'
#' @export
#' @import data.table
#' @import magrittr
#'
search_geoheader <- function(keyword, view = TRUE) {
    dt1 <- dict_geoheader[grepl(tolower(keyword), tolower(reference))]
    dt2 <- dict_geoheader[grepl(tolower(keyword), tolower(field))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(field, reference)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
