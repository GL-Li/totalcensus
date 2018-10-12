#' Search FIPS Codes
#'
#' @description  Search FIPS code of a states, counties, county subdivisions, places, or
#' consolidated cities in dataset \code{\link{dict_fips}}. The search also returns
#' summary levels.
#'
#' @details Quite often, multiple rows are returned. It is necessary
#' to hand pick the right one you are really looking for.
#'
#' The function \code{\link{search_fips}} has changed summary level 061 to 060, and
#' 162 to 160 in search results.
#' The summary levels in \code{\link{dict_fips}} are 010, 040, 050, 061, 162, and 170.
#' The level 061 is for Minor Civil Division (MCD)/Census County Division (CCD) (10,000+). It
#' does not appear in those used in decennial census and ACS surveys,
#' which instead have 060 for County Subdivision.
#' Level 061 is part of 060 and is replaced with 060 in order to use the census data. Similarly,
#' 162 is replaced with 160.
#'
#' @param keywords keyword to be searched in NAMES or FIPS.
#' @param state abbreviation of a state.
#' @param view display the search result with View if TRUE.
#'
#' @return A data.table
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.table.
#'
#' # Search fips of Lincoln in Rhode Island.
#' aaa <- search_fips("lincoln", "RI", view = FALSE)
#'
#' # search FIPS number in all states
#' bbb <- search_fips("08375", view = FALSE)
#'
#' \dontrun{
#'   # view all fips code
#'   search_fips()
#' }
#'
#' @export
#'
#'

search_fips <- function(keywords = NULL, state = NULL, view = TRUE) {

    if (is.null(state)) state <- "*"
    dt <- dict_fips[state_abbr %like% toupper(state)]

    if (is.null(keywords)) keywords <- "*"
    kws <- unlist(str_split(tolower(keywords), " "))
    for (kw in kws){
        # combine all rows to form a new column for search
        dt <- dt[, comb := apply(dt[, c(1, 3:9)], 1, paste, collapse = " ")] %>%
            .[grepl(kw, tolower(comb))] %>%
            .[, comb := NULL]
    }

    # change to match those in census summary files
    dt[SUMLEV == "061", SUMLEV := "060"][SUMLEV == "162", SUMLEV := "160"]

    if (view) View(dt, paste(keywords, "found"))

    return(dt)
}



#' Search Core Based Statistical Area (CBSA)
#'
#' @description  Search CBSA code of Core Based Statistical Area in dataset \code{\link{dict_cbsa}}.
#' The search also returns which CSA (Combined Statistical Area) that contains
#' the CBSA. If the CBSA contains multiple counties, each county is returned as
#' a row.
#'
#' @details Quite often, multiple rows are returned. It is necessary
#' to hand pick the right one you are really looking for.
#'
#' @param keywords keywords to be searched.
#' @param view display the search result with View if TRUE.
#'
#' @return A data.table
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' aaa <- search_cbsa("providence", view = FALSE)
#'
#' bbb <- search_cbsa("new york", view = FALSE)
#'
#' \dontrun{
#'   # view all CBSA code
#'   search_cbsa()
#' }
#'
#'
#' @export
#'
#'

search_cbsa <- function(keywords = NULL, view = TRUE) {
    dt <- dict_cbsa

    if (is.null(keywords)) keywords <- "*"
    kws <- unlist(str_split(tolower(keywords), " "))
    for (kw in kws){
        # combine all rows to form a new column for search
        dt <- dt[, comb := apply(dt[, c(1, 2, 3, 4)], 1, paste, collapse = " ")] %>%
            .[grepl(kw, tolower(comb))] %>%
            .[, comb := NULL]
    }

    if (view) View(dt, paste(keywords, "found"))

    return(dt)
}
