#' Search fips
#'
#'
#' @param keyword keyword to be searched
#' @param view display the search result with View if TRUE
#'
#' @seealso \code{\link{dict_fips}}
#'
#' @export
#'
#' @examples
#'
#'

search_fips <- function(keyword, table_only = FALSE, view = TRUE) {
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
