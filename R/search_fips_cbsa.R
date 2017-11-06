#' Search FIPS
#'
#' @description  Search FIPS of a state, county, county subdivision, place, or
#' consolidated city in dataset \code{\link{dict_fips}}. The search also returns
#' summary levels.
#'
#' @details Quite often, multiple rows are returned. It is necessary
#' to hand pick the right one you are really looking for.
#'
#' The function \code{\link{search_fips}} has changed summary level 061 to 060, and
#' 162 to 160 in search results.
#' The summary levels in \code{\link{dict_fips}} are 010, 040, 050, 061, 162, and 170.
#' The level 061 is for Minor Civil Division (MCD)/Census County Division (CCD) (10,000+). It
#' does not appear in \code{\link{dict_summarylevel}}, which instead has 060 for County Subdivision.
#' Level 061 is part of 060 and is replaced with 060 in order to use the census data. Similarly,
#' both level 162 in \code{\link{dict_fips}} and l60 in \code{\link{dict_summarylevel}} are for
#' State-Place. Always use 160 in census data.
#'
#' @param keyword keyword to be searched in NAMES or FIPS.
#' @param state abbreviation of a state.
#' @param view display the search result with View if TRUE.
#'
#' @return A data.table
#'
#' @examples
#' \dontrun{
#'   # search fips of Lincoln in Rhode Island
#'   search_fips("lincoln", "RI")
#'   search_fips("lincoln", "rhode")
#'
#'   # list fips of all counties in Massachusetts, even cannot spell correctly
#'   search_fips("county", "massa")
#'
#'   # search FIPS number
#'   search_fips("08375")
#' }
#' @seealso \code{\link{dict_fips}}
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom stringr str_split
#'
#'

search_fips <- function(keyword, state = NULL, view = TRUE) {

    if (is.null(state)) state <- "*"
    dt <- dict_fips[state_abbr %like% toupper(state)]

    keywords <- unlist(str_split(tolower(keyword), " "))
    for (kw in keywords){
        # combine all rows to form a new column for search
        dt <- dt[, comb := apply(dt[, c(1, 3:9)], 1, paste, collapse = " ")] %>%
            .[grepl(kw, tolower(comb))] %>%
            .[, comb := NULL]
    }

    # change to match those in census summary files
    dt[SUMLEV == "061", SUMLEV := "060"][SUMLEV == "162", SUMLEV := "160"]

    if (view) View(dt, paste(keyword, "found"))

    return(dt)
}



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
