# user functions =======================================================
#' Search geographic headers
#'
#' @description Search in field reference or description of geographic header
#' with keyword in dataset \code{\link{dict_decennial_geoheader}} or
#' \code{\link{dict_acs_geoheader}}.
#'
#' @param survey type of survey, either "decennial" or "acs".
#' @param keyword keyword in description or reference. The default "*" includes
#' all geoheaders.
#' @param view display the search result with View() if TRUE
#'
#' @return data.table matching the search criteria
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' # search geoheader that contains keyword "india" in decennial 2010
#' aaa <- search_geoheaders("decennial", "india", view = FALSE)
#'
#' # search for lattitude
#' bbb <- search_geoheaders("decennial", "latitu", view = FALSE)
#'
#'
#' \dontrun{
#'   # browse all geoheaders in ACS in View()
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
#' for table_contents argument in function \code{\link{read_decennial}},
#' \code{\link{read_acs1year}}, and \code{\link{read_acs5year}}.
#'
#' @param survey either "decennial" for decenial or "acs" or American Community Survey.
#' @param keyword keyword to be searched
#' @param year ending year of the survey
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' # search by what you want
#' aaa <- search_tablecontents("decennial", "federal prison", view = FALSE)
#'
#' # search by table reference
#' bbb <- search_tablecontents("acs", "B02003", view = FALSE)
#'
#' \dontrun{
#'   # view all decennial census table contents
#'   search_tablecontents("decennial")
#'
#'   # view all ACS table contents
#'   search_tablecontents("acs")
#' }
#'
#' @export
#'

search_tablecontents <- function(survey, keyword = "*", year = NULL, view = TRUE) {

    if (survey == "decennial") dt <- generate_decennial_tablecontents()
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




#' Search summary levels
#'
#' @description Search code or description of summary levels
#'
#' @param survey "decennial" or "acs"
#' @param keyword keyword to search in code or description
#' @param view display the search result with View if TRUE
#'
#' @return A data.table of searched results.
#'
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' aaa = search_summarylevels("decennial", "block", view = FALSE)
#' bbb <- search_summarylevels("acs", "40", view = FALSE)
#'
#' \dontrun{
#'   # view all summary levels
#'   search_summarylevels("decennial")
#'   search_summarylevels("acs")
#' }
#'
#' @export
#'


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
#' @description Search the code or content of geographic components
#'
#' @details The most frequently used geographic components are:
#'
#' 00 : all geographic component
#'
#' 01 : urban
#'
#' 43 : rural
#'
#' @param survey "decennial" or "acs"
#' @param keyword keyword to search in code or description, "*" for any words.
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' aaa <- search_geocomponents("decennial", "urban", view = FALSE)
#' bbb <- search_geocomponents("acs", "43", view = FALSE)
#'
#' \dontrun{
#'   # view all geocomponents
#'   search_geocomponents("decennial")
#'   search_geocomponents("acs")
#' }
#'
#' @export
#'


search_geocomponents <- function(survey, keyword = "*", view = TRUE){

    dict <- get(paste0("dict_", survey, "_geocomponent"))

    dt1 <- dict[grepl(tolower(keyword), tolower(code))]
    dt2 <- dict[grepl(tolower(keyword), tolower(geo_component))]

    dt <- rbindlist(list(dt1, dt2)) %>%
        unique()

    if (view) View(dt, paste(keyword, "found"))

    return(invisible(dt))
}




#'
#' search decennial and acs tables by keyword in table numbers or table descriptions
#'
#' @param survey "decennial" or "acs"
#' @param keyword keyword to search in code or description.
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' aaa <- search_tables("decennial", "occupancy", view = FALSE)
#' bbb <- search_tables("acs", "detailed race", view = FALSE)
#'
#' \dontrun{
#'   # view all tables
#'   search_tables("decennial")
#'   search_tables("acs")
#' }
#'
#'
#' @export
#'

search_tables <- function(survey, keyword = '*', view = TRUE){
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



# internal functions ===========================================================
# This funciton generates datasets used in the package but we do not want to save
# the datasets for users. When used just call the functions.


# do NOT save as dataset, but use as a function
# the reference are reserved over years, however the table_content and table_name has slight
# difference which gives trouble in merge data. We will only use reference for
# data merge and keep only one table content and table name.

generate_acs_tablecontents <- function(){
    acs1_2014 <- lookup_acs1year_2014[, .(reference,
                                          content_acs1_2014 = table_content,
                                          name_acs1_2014 = table_name,
                                          universe_acs1_2014 = universe,
                                          acs1_2014 = restriction)] %>%
        .[is.na(acs1_2014), acs1_2014 := "yes"] %>%
        setkey(reference)

    acs1_2015 <- lookup_acs1year_2015[, .(reference,
                                          content_acs1_2015 = table_content,
                                          name_acs1_2015 = table_name,
                                          universe_acs1_2015 = universe,
                                          acs1_2015 = restriction)] %>%
        .[is.na(acs1_2015), acs1_2015 := "yes"] %>%
        setkey(reference)

    acs1_2016 <- lookup_acs1year_2016[, .(reference,
                                          content_acs1_2016 = table_content,
                                          name_acs1_2016 = table_name,
                                          universe_acs1_2016 = universe,
                                          acs1_2016 = restriction)] %>%
        .[is.na(acs1_2016), acs1_2016 := "yes"] %>%
        setkey(reference)


    acs5_2015 <- lookup_acs5year_2015[, .(reference,
                                          content_acs5_2015 = table_content,
                                          name_acs5_2015 = table_name,
                                          universe_acs5_2015 = universe,
                                          acs5_2015 = restriction)] %>%
        .[is.na(acs5_2015), acs5_2015 := "yes"] %>%
        setkey(reference)

    acs5_2016 <- lookup_acs5year_2016[, .(reference,
                                          content_acs5_2016 = table_content,
                                          name_acs5_2016 = table_name,
                                          universe_acs5_2016 = universe,
                                          acs5_2016 = restriction)] %>%
        .[is.na(acs5_2016), acs5_2016 := "yes"] %>%
        setkey(reference)


    dict_acs_tablecontent <- reduce(list(acs1_2014, acs1_2015, acs1_2016, acs5_2015, acs5_2016), merge, all = TRUE) %>%
        # consolidate table_names
        .[!is.na(acs1_2014), ":=" (table_content = content_acs1_2014,
                                   table_name = name_acs1_2014,
                                   universe = universe_acs1_2014)] %>%
        .[!is.na(acs1_2015), ":=" (table_content = content_acs1_2015,
                                   table_name = name_acs1_2015,
                                   universe = universe_acs1_2015)] %>%
        .[!is.na(acs1_2016), ":=" (table_content = content_acs1_2016,
                                   table_name = name_acs1_2016,
                                   universe = universe_acs1_2016)] %>%
        .[!is.na(acs5_2015), ":=" (table_content = content_acs5_2015,
                                   table_name = name_acs5_2015,
                                   universe = universe_acs5_2015)] %>%
        .[!is.na(acs5_2016), ":=" (table_content = content_acs5_2016,
                                   table_name = name_acs5_2016,
                                   universe = universe_acs5_2016)] %>%
        .[, .(reference, table_content, table_name, acs5_2016, acs5_2015, acs1_2016, acs1_2015, acs1_2014, universe)] %>%
        .[is.na(acs5_2016), acs5_2016 := "-"] %>%
        .[is.na(acs5_2015), acs5_2015 := "-"] %>%
        .[is.na(acs1_2016), acs1_2016 := "-"] %>%
        .[is.na(acs1_2015), acs1_2015 := "-"] %>%
        .[is.na(acs1_2014), acs1_2014 := "-"]
}


generate_decennial_tablecontents <- function(){
    # will add decennial 2020 when available
    decennial_2010 <- lookup_decennial_2010[, .(reference, table_content, table_name, universe)] %>%
        .[, Census2010 := "yes"] %>%
        .[, .(reference, table_content, table_name, Census2010, universe)] %>%
        .[!is.na(table_name)]
}

