#' Search geographic header
#'
#' Search in field reference or description with a keyword
#'
#'
#' @param keyword keyword in description or reference
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
#' @seealso \code{\link{geoheader_dict}}, which lists all the geographic headers
#'
#' @export
#'
search_geoheader <- function(keyword) {
    # for geoheader_dict dataset to work properly as a data.table, have to place
    # data.table in Depends in DESCRPTION file!!!!
    dt1 <- geoheader_dict[grepl(tolower(keyword), tolower(reference))]
    dt2 <- geoheader_dict[grepl(tolower(keyword), tolower(description))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(description, reference)] %>%
        unique()
    return(dt)
}
