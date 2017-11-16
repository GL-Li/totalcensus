#' Search geographic headers
#'
#' @description Search in field reference or description of geographic header
#' with keyword in dataset \code{\link{dict_census_geoheader}} or
#' \code{\link{dict_acs_geoheader}}.
#'
#' @param survey type of survey, either "census" or "acs".
#' @param keyword keyword in description or reference. The default "*" includes
#' all geoheaders.
#' @param view display the search result with View if TRUE
#'
#' @return data.table matching the search criteria
#'
#' @examples
#' \dontrun{
#'   # search geoheader that contains keyword "india" in Census 2010
#'   search_geoheaders("census", "india")
#'
#'   # search for lattitude
#'   search_geoheaders("census", "latitu")
#'
#'   # browse all geoheaders in ACS
#'   search_geoheaders("acs")
#' }
#'
#' @export
#'
search_geoheaders <- function(survey, keyword = "*", view = TRUE) {

    dict <- get(paste0("dict_", survey, "_geoheader"))

    dt1 <- dict[grepl(tolower(keyword), tolower(reference))]
    dt2 <- dict[grepl(tolower(keyword), tolower(field))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        .[, .(field, reference)] %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(invisible(dt))
}




#' Search table contents in data files
#'
#' @description Search in lookup datasets of each survey to find references of
#' for table_contents argument in function \code{\link{read_census2010}},
#' \code{\link{read_acs1year}}, and \code{\link{read_acs5year}}.
#'
#' @param survey either "census" for decenial or "acs" or American Community Survey.
#' @param keyword keyword to be searched
#' @param year ending year of the survey
#'
#' @return A data.table
#'
#'
#' @examples
#' \dontrun{
#'   # search exactly table "p3"
#'   search_datafile("p3", table_only = TRUE)
#'
#'   # search reference "p0030002"
#'   search_datafile("p0030002")
#'
#'   # search for file 03
#'   search_datafile("03", file_only = TRUE)
#'
#'   # search keyword "federal prision population".
#'   search_datafile("federal prison population")
#' }
#'
#' @export
#' @import data.table
#' @import magrittr
#' @importFrom purrr map reduce
#' @importFrom stringr str_split

search_tablecontents <- function(survey, keyword, year = NULL, view = TRUE) {

    if (survey == "census") dt <- generate_census_tablecontents()
    if (survey == "acs") dt <- generate_acs_tablecontents()

    keywords <- unlist(str_split(tolower(keyword), " "))
    for (kw in keywords){
        # combine all rows to form a new column for search
        dt <- dt[, comb := apply(dt, 1, paste, collapse = " ")] %>%
            .[grepl(kw, tolower(comb))] %>%
            .[, comb := NULL]
    }

    if (!is.null(year)){
        dt <- dt[, c("reference", "table_content", "table_name",
                     names(dt)[names(dt) %like% year]), with = FALSE]
    }

    if (view) View(dt, paste(keyword, "found"))

    return(invisible(dt))
}




#' Search summary levels of state files
#'
#' @description Search code or description of summary levels used in state files.
#' The summary levels are stored in dataset \code{\link{dict_summarylevel}}.
#' This dataset is different from that for national files,
#' \code{\link{dict_summarylevel_US}}. Function \code{\link{search_sumlev_US}}
#' searches summary levels in national files.
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @return A data.table of searched results.
#'
#'
#' @examples
#' \dontrun{
#'   # search summary levels of geocomponent contains "block"
#'   search_sumlev("block")
#'
#'   # search summary levels of code 40
#'   search_sumlev("40")
#' }
#'
#' @export
#' @import data.table
#' @import magrittr


search_summarylevels <- function(survey, keyword = "*", view = TRUE){

    dict <- get(paste0("dict_", survey, "_summarylevel"))

    dt1 <- dict[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict[grepl(tolower(keyword), tolower(summary_level))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(invisible(dt))
}




#' Search geographic components
#'
#' @description Search the code or content of geographic components in dataset
#' \code{\link{dict_geocomp}}.
#'
#' @details The most frequently used geographic components are:
#'
#' 00 : all geographic component
#'
#' 01 : urban
#'
#' 43 : rural
#'
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#' @seealso \code{\link{dict_geocomp}} lists all geocomponents and codes
#'
#' @examples
#' \dontrun{
#'   # search geocomponents containing "urban cluster"
#'   search_geocomp("urban cluster")
#'
#'   # search geocomponents with code 43
#'   search_geocomp("43")
#'   # or
#'   search_geocomp(43)
#' }
#'
#' @export
#' @import data.table
#' @import magrittr


search_geocomponents <- function(survey, keyword = "*", view = TRUE){

    dict <- get(paste0("dict_", survey, "_geocomponent"))

    dt1 <- dict[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict[grepl(tolower(keyword), tolower(geo_component))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(invisible(dt))
}




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

search_table <- function(survey, keyword, view = TRUE){
    # search rows that contains ALL keywords, NOT any
    dt <- get(paste0("dict_", survey, "_table"))
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

