#' Search county fips
#'
#' search census tables by keyword in table numbers or table descriptions
#'
#' @param keyword keyword to search in fips or county name. To search for fips
#'     contains both words "abc" and "defg", the keyword is simply a single string
#'     of "abc defg".
#' @param view display the search result with View if TRUE
#'
#' @export
#'
#' @seealso \code{\link{dict_countyfips}} lists all county fips
#'
#' @examples
#'


search_countyfips <- function(keyword, view = TRUE){
    # search rows that contains ALL keywords, NOT any
    dt <- dict_countyfips
    keyword <- tolower(keyword)
    keywords <- unlist(str_split(keyword, " "))
    for (kw in keywords) {
        # combine all rows to form a new column for search
        dt <- dt[, comb := apply(dt, 1, paste, collapse = " ")] %>%
            .[grepl(kw, tolower(comb))] %>%
            .[, comb := NULL]
    }

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
