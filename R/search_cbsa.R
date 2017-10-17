#' Search CBSA code and title
#'
#' @description  Search CBSA code of Core Based Statistical Area in dataset \code{\link{dict_cbsa}}.
#' The search also returns which CSA (Combined Statistical Area) that contains
#' the CBSA. If the CBSA contains multiple counties, each county is returned as
#' a row.
#'
#' @details Quite often, multiple rows are returned. It is necessary
#' to hand pick the right one you are really looking for.
#'
#' @param keyword keyword to be searched in CBSA or CBSA title.
#' @param view display the search result with View if TRUE.
#'
#' @return A data.table
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom stringr str_split
#'
#'

search_cbsa <- function(keyword, view = TRUE) {
    dt <- dict_cbsa

    # step 1: search in NAMEs or FIPS code
    keywords <- unlist(str_split(tolower(keyword), " "))
    for (kw in keywords){
        # combine all rows to form a new column for search
        dt <- dt[, comb := apply(dt[, c(1, 2)], 1, paste, collapse = " ")] %>%
            .[grepl(kw, tolower(comb))] %>%
            .[, comb := NULL]
    }

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
