#' Search FIPS
#'
#'
#' @param keyword keyword to be searched
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#' @examples
#' \dontrun{
#'   # search fips of Lincoln in Rhode Island
#'   search_fips("rhode island lincoln")
#'
#'   # list fips of all counties in Massachusetts, even cannot spell correctly
#'   search_fips("massachu county")
#' }
#' @seealso \code{\link{dict_fips}}
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom stringr str_split
#'
#'

search_fips <- function(keyword, view = TRUE) {
    dt <- dict_fips
    keywords <- unlist(str_split(tolower(keyword), " "))
    for (kw in keywords){
        # combine all rows to form a new column for search
        dt <- dt[, comb := apply(dt, 1, paste, collapse = " ")] %>%
            .[grepl(kw, tolower(comb))] %>%
            .[, comb := NULL]
    }

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}
