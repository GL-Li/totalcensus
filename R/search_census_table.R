#' Search census tables
#'
#' search census tables by keyword in table numbers or table descriptions
#'
#' @param keyword keyword to search in code or description. To search for a table
#'     contains two words "abc" and "defg", the keyword is simply a single string
#'     of "abc defg".
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#'
#' @examples
#' \dontrun{
#'   # search census table contains "occupancy"
#'   search_table("occupancy")
#'
#'   # search census table with table number "H5"
#'   search_table("H5")
#' }
#'
#' @seealso \code{\link{dict_censustable}} lists all geocomponents and codes
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom stringr str_split
#'

search_table <- function(keyword, view = TRUE){
    # search rows that contains ALL keywords, NOT any
    dt <- dict_censustable
    keywords <- unlist(str_split(tolower(keyword), " "))
    for (kw in keywords) {
        # combine all rows to form a new column for search
        dt <- dt[, comb := apply(dt, 1, paste, collapse = " ")] %>%
            .[grepl(kw, tolower(comb))] %>%
            .[, comb := NULL]
    }

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
