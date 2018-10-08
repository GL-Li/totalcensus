# user functions =======================================================
#' Search geographic headers
#'
#' @description Search in field reference or description of geographic header
#' records.
#'
#' @param survey type of survey, taking values of "dec", "decennial", "acs5", or
#' "acs1".
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
#' bbb <- search_geoheaders("dec", "latitu", view = FALSE)
#'
#'
#' \dontrun{
#'   # browse all geoheaders in ACS in View()
#'   search_geoheaders("acs1")
#' }
#'
#' @export
#'
search_geoheaders <- function(survey, keyword = NULL, view = TRUE) {
    if (survey %in% c("dec", "decennial")){
        dict <- generate_decennial_geoheaders_()
    } else if (survey == "acs5"){
        dict <- generate_acs5_geoheaders_()
    } else if (survey == "acs1"){
        cat(paste0("Some of 2005 fields are mofified to be consistent with ",
                   "later years. Run View(dict_acs_geoheader_2005_1year) ",
                   "to check for original field description."))
        dict <- generate_acs1_geoheaders_()

    } else {
        message('Survey must be "dec", "decennial", "acs5", or "acs1".')
        return(NULL)
    }

    if (!is.null(keyword)){
        dt1 <- dict[grepl(tolower(keyword), tolower(reference))]
        dt2 <- dict[grepl(tolower(keyword), tolower(field))]
        dt <- rbindlist(list(dt1, dt2)) %>%
            unique()
    } else {
        dt <- dict
    }

    if (view) View(dt, paste(keyword, "found"))

    return(invisible(dt))
}




#' Search table contents in data files
#'
#' @description Search in lookup datasets of each survey to find references of
#' for table_contents argument in function \code{\link{read_decennial}},
#' \code{\link{read_acs1year}}, and \code{\link{read_acs5year}}.
#'
#' @param survey type of survey, taking values of "dec", "decennial", "acs5", or
#' "acs1".
#' @param keyword keyword to be searched
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

search_tablecontents <- function(survey, year = NULL, keyword = NULL, view = TRUE) {

    if (survey %in% c("decennial", "dec")){
        cat(paste0("Be aware that the same reference may point to different ",
                   "table content in census 2000 and 2010."))
        dt <- generate_decennial_tablecontents_()
    } else if (survey == "acs5"){
        dt <- generate_acs5_tablecontents_()
    } else if (survey == "acs1"){
        dt <- generate_acs1_tablecontents_()
    } else {
        message('Survey must be "dec", "decennial", "acs5", or "acs1".')
        return(NULL)
    }

    if (!is.null(keyword)){
        keywords <- unlist(str_split(tolower(keyword), " "))
        for (kw in keywords){
            # combine all rows to form a new column for search
            dt <- dt[, comb := apply(dt, 1, paste, collapse = " ")] %>%
                .[grepl(kw, tolower(comb))] %>%
                .[, comb := NULL]
        }
    }

    if (!is.null(year)){
        year_arg <- year   # special for data.table year == year not working
        if (survey %in% c("decennial", "dec")){
            dt <- dt[year == year_arg]
        } else if (survey %in% c("acs5", "acs1")){
            dt <- dt[, c("reference", "table_content", "table_name",
                         names(dt)[grep(year_arg, names(dt))], "universe"),
                     with = FALSE]
        }
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


search_summarylevels <- function(survey, keyword = NULL, view = TRUE){

    if (survey %in% c("decennial", "dec")){
        dt <- generate_decennial_summary_level_()
    } else if (survey == "acs5"){
        dt <- dict_acs5_summarylevel
    } else if (survey == "acs1"){
        dt <- dict_acs1_summarylevel
    } else {
        message('Survey must be "dec", "decennial", "acs5", or "acs1".')
        return(NULL)
    }

    if (!is.null(keyword)){
        dt1 <- dt[grepl(tolower(keyword), tolower(code))]
        dt2 <- dt[grepl(tolower(keyword), tolower(summary_level))]

        dt <- rbindlist(list(dt1, dt2)) %>%
            unique()
    }

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
# This function generates datasets used in the package but we do not want to save
# the datasets for users. When used just call the functions.


# do NOT save as dataset, but use as a function
# the reference are reserved over years, however the table_content and table_name has slight
# difference which gives trouble in merge data. We will only use reference for
# data merge and keep only one table content and table name.

generate_decennial_geoheaders_ <- function(){
    # generate dict_decennial_geoheaders for all years
    dict_2010 <- dict_decennial_geoheader_2010 %>%
        .[, .(reference, field)] %>%
        .[, census_2010 := "yes"]
    dict_2000 <- dict_decennial_geoheader_2000 %>%
        .[, .(reference, field)] %>%
        .[, census_2000 := "yes"]
    dict <- merge(dict_2010, dict_2000, by = c("reference", "field"), all = TRUE)

    return(dict)
}


generate_acs5_geoheaders_ <- function(){
    dict_2009 <- dict_acs_geoheader_2009_5year %>%
        .[, .(reference, field)] %>%
        .[, acs5_2009 := "yes"] %>%
        .[reference != "BLANK"]

    # reference and field are the same for non-BLANKS after 2010
    # dict_2010 <- dict_acs_geoheader_2010 %>%
    #     .[, .(reference, field)] %>%
    #     .[, acs5_2010 := "yes"] %>%
    #     .[reference != "BLANK"]
    dict_2011_now <- dict_acs_geoheader_2011_now %>%
        .[, .(reference, field)] %>%
        .[, acs5_2010_to_now := "yes"] %>%
        .[reference != "BLANK"]
    dict <- purrr::reduce(
        list(dict_2011_now, dict_2009),
        merge, by = c("reference", "field"), all = TRUE
    )

    return(dict)
}

generate_acs1_geoheaders_ <- function(){
    dict_2005 <- dict_acs_geoheader_2005_1year %>%
        .[, .(reference, field)] %>%
        .[, acs1_2005 := "yes"] %>%
        .[reference != "BLANK"] %>%
        .[reference == "COUSUB", field := "County Subdivision (FIPS)"] %>%
        .[reference == "GEOID", field := "Geographic Identifier"] %>%
        .[reference == "MEMI", field := "Metropolitan/Micropolitan Indicator Flag"] %>%
        .[reference == "METDIV", field := "Metropolitan Statistical AreaMetropolitan Division"] %>%
        .[reference == "PLACE", field := "Place (FIPS Code)"] %>%
        .[reference == "PUMA5", field := "Public Use Microdata Area - 5% File"] %>%
        .[reference == "SDELM", field := "State-School District (Elementary)"] %>%
        .[reference == "SDSEC", field := "State-School District (Secondary)"] %>%
        .[reference == "SDUNI", field := "State-School District (Unified)"] %>%
        .[reference == "STUSAB", field := "State Postal Abbreviation"] %>%
        .[reference == "SUMLEV", field := "Summary Level"] %>%
        .[reference == "NAME", field := "Area Name"] %>%
        .[reference == "US", field := "US"]
    dict_2006_2008 <- dict_acs_geoheader_2006_2008_1year %>%
        .[, .(reference, field)] %>%
        .[, acs1_2006_to_2008 := "yes"] %>%
        .[reference != "BLANK"]
    dict_2009 <- dict_acs_geoheader_2009_1year %>%
        .[, .(reference, field)] %>%
        .[, acs1_2009 := "yes"] %>%
        .[reference != "BLANK"]

    # reference and field are the same for non-BLANKS after 2010
    # dict_2010 <- dict_acs_geoheader_2010 %>%
    #     .[, .(reference, field)] %>%
    #     .[, acs1_2010 := "yes"] %>%
    #     .[reference != "BLANK"]
    dict_2011_now <- dict_acs_geoheader_2011_now %>%
        .[, .(reference, field)] %>%
        .[, acs1_2010_to_now := "yes"] %>%
        .[reference != "BLANK"]
    dict <- purrr::reduce(
        list(dict_2011_now, dict_2009, dict_2006_2008, dict_2005),
        merge, by = c("reference", "field"), all = TRUE
    )

    return(dict)
}

modify_lookup_table_ <- function(period, year){
    p_y <- paste0("acs", period, "_", year)

    dt <- get(paste0("lookup_acs", period, "year_", year)) %>%
        .[, .(reference, table_content, table_name, universe, restriction)] %>%
        setnames(c("reference",
                   paste0("content_", p_y),
                   paste0("name_", p_y),
                   paste0("universe_", p_y),
                   p_y)) %>%
        setkey(reference)

    if (year >= 2013){
        dt[is.na(get(p_y)), (p_y) := "no restriction"]
    }

    # remove duplicated row as some table may appear in multiple file segments
    return(unique(dt))
}



generate_decennial_tablecontents_ <- function(){
    L2010 = copy(lookup_decennial_2010)
    L2000 = copy(lookup_decennial_2000)
    lookup_decennial <- rbindlist(list(L2010[, year := 2010],
                                       L2000[, year := 2000])) %>%
        .[, .(year, reference, table_content, table_name, universe)] %>%
        .[!reference %in% c("FILEID", "STUSAB", "CHARITER", "CIFSN", "LOGRECNO")]
}


generate_acs5_tablecontents_ <- function(){
    acs5_2009 <- modify_lookup_table_(5, 2009)
    acs5_2010 <- modify_lookup_table_(5, 2010)
    acs5_2011 <- modify_lookup_table_(5, 2011)
    acs5_2012 <- modify_lookup_table_(5, 2012)
    acs5_2013 <- modify_lookup_table_(5, 2013)
    acs5_2014 <- modify_lookup_table_(5, 2014)
    acs5_2015 <- modify_lookup_table_(5, 2015)
    acs5_2016 <- modify_lookup_table_(5, 2016)

    dict_acs_tablecontent <- reduce(list(acs5_2009,
                                         acs5_2010,
                                         acs5_2011,
                                         acs5_2012,
                                         acs5_2013,
                                         acs5_2014,
                                         acs5_2015,
                                         acs5_2016),
                                    merge, by = "reference", all = TRUE) %>%

        # add the following lines for year since 2013
        .[!is.na(acs5_2013), ":=" (table_content = content_acs5_2013,
                                   table_name = name_acs5_2013,
                                   universe = universe_acs5_2013)] %>%
        .[!is.na(acs5_2014), ":=" (table_content = content_acs5_2014,
                                   table_name = name_acs5_2014,
                                   universe = universe_acs5_2014)] %>%
        .[!is.na(acs5_2015), ":=" (table_content = content_acs5_2015,
                                   table_name = name_acs5_2015,
                                   universe = universe_acs5_2015)] %>%
        .[!is.na(acs5_2016), ":=" (table_content = content_acs5_2016,
                                   table_name = name_acs5_2016,
                                   universe = universe_acs5_2016)] %>%

        # include all years and surveys
        .[, .(reference, table_content, table_name,
              acs5_2016, acs5_2015, acs5_2014, acs5_2013, acs5_2012, acs5_2011,
              acs5_2010, acs5_2009,
              universe)]
}

generate_acs1_tablecontents_ <- function(){
    acs1_2005 <- modify_lookup_table_(1, 2005)
    acs1_2006 <- modify_lookup_table_(1, 2006)
    acs1_2007 <- modify_lookup_table_(1, 2007)
    acs1_2008 <- modify_lookup_table_(1, 2008)
    acs1_2009 <- modify_lookup_table_(1, 2009)
    acs1_2010 <- modify_lookup_table_(1, 2010)
    acs1_2011 <- modify_lookup_table_(1, 2011)
    acs1_2012 <- modify_lookup_table_(1, 2012)
    acs1_2013 <- modify_lookup_table_(1, 2013)
    acs1_2014 <- modify_lookup_table_(1, 2014)
    acs1_2015 <- modify_lookup_table_(1, 2015)
    acs1_2016 <- modify_lookup_table_(1, 2016)
    acs1_2017 <- modify_lookup_table_(1, 2017)

    dict_acs_tablecontent <- reduce(list(acs1_2005,
                                         acs1_2006,
                                         acs1_2007,
                                         acs1_2008,
                                         acs1_2009,
                                         acs1_2010,
                                         acs1_2011,
                                         acs1_2012,
                                         acs1_2013,
                                         acs1_2014,
                                         acs1_2015,
                                         acs1_2016,
                                         acs1_2017),
                                    merge, by = "reference", all = TRUE) %>%

        # add the following lines for year since 2013
        .[!is.na(acs1_2013), ":=" (table_content = content_acs1_2013,
                                   table_name = name_acs1_2013,
                                   universe = universe_acs1_2013)] %>%
        .[!is.na(acs1_2014), ":=" (table_content = content_acs1_2014,
                                   table_name = name_acs1_2014,
                                   universe = universe_acs1_2014)] %>%
        .[!is.na(acs1_2015), ":=" (table_content = content_acs1_2015,
                                   table_name = name_acs1_2015,
                                   universe = universe_acs1_2015)] %>%
        .[!is.na(acs1_2016), ":=" (table_content = content_acs1_2016,
                                   table_name = name_acs1_2016,
                                   universe = universe_acs1_2016)] %>%
        .[!is.na(acs1_2017), ":=" (table_content = content_acs1_2017,
                                   table_name = name_acs1_2017,
                                   universe = universe_acs1_2017)] %>%

        # include all years and surveys
        .[, .(reference, table_content, table_name,
              acs1_2017, acs1_2016, acs1_2015, acs1_2014, acs1_2013, acs1_2012,
              acs1_2011, acs1_2010, acs1_2009, acs1_2008, acs1_2007, acs1_2006,
              acs1_2005,
              universe)]
}



generate_decennial_summary_level_ <- function(){
    S2000 <- copy(dict_decennial_summarylevel_2000) %>%
        .[, year := 2000]
    S2010 <- copy(dict_decennial_summarylevel_2010) %>%
        .[, year := 2010]

    dict <- rbindlist(list(S2010, S2000)) %>%
        .[, .(year, code, summary_level, in_state_file, in_US_file)]

    return(dict)
}

