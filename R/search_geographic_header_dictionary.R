#' Search in field reference or description with a keyword
#'
#' @param keyword keyword in description or reference
#'
#' @return data.table match the search criteria
#'
#' @examples
#'
#' @export
search_geoheader_dict <- function(keyword) {
    # for geo_dict dataset to work properly as a data.table, have to place
    # data.table in Depends in DESCRPTION file!!!!
    dt1 <- geo_dict[tolower(reference) == tolower(keyword)]
    dt2 <- geo_dict[grepl(tolower(keyword), tolower(description))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(description, reference)] %>%
        unique()
    return(dt)
}
