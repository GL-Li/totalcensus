# user functions =======================================================


#' Search Table Contents
#'
#' @description Search in lookup datasets of each survey to find references of
#' table_contents argument in function \code{\link{read_decennial}},
#' \code{\link{read_acs1year}}, and \code{\link{read_acs5year}}.
#'
#' @param survey survey type, including "dec" (or "decennial"), "acs1" or "acs5".
#' @param years year or ending year of the survey, can be a single year such as
#' 2010 or a vector like 2014:2016.
#' @param keywords keyword to search in code or description, in the form like
#' "abc def dsdfsa". Rows with all words are returned.
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' # search by what you want
#' aaa <- search_tablecontents("dec", 2000, "federal prison", view = FALSE)
#'
#' \dontrun{
#'   # view all decennial census table contents
#'   search_tablecontents("dec")
#'
#'   # view all ACS 5 year table contents
#'   search_tablecontents("acs5")
#' }
#'
#' @export
#'

search_tablecontents <- function(survey, years = NULL, keywords = NULL, view = TRUE) {

    if (survey %in% c("decennial", "dec")){
        cat(paste0("Be aware that the same reference may point to different ",
                   "table content in census 2000 and 2010."))
        dt <- generate_decennial_tablecontents_()
        if (!is.null(years)){
            for (yr in years){
                if (!(yr %in% c(2000, 2010))){
                    message("Only 2000 and 2010 are available for decennial census. ")
                    return(NULL)
                }
            }
            dt <- dt[year %in% years]
        }

    } else if (survey == "acs5"){
        cat(paste0("Restrictions of table content in each year. ",
                   "NA means data not collected."))
        dt <- generate_acs5_tablecontents_()
        if (!is.null(years)){
            if (min(years) < 2009 | max(years) > 2019){
                message("Only 2009 - 2019 are available for acs 5 year surveys.")
                return(NULL)
            }
            selected_cols <- select_columns(dt, years)
            dt <- dt[, c("reference", "table_content", "table_name", selected_cols, "universe"), with = FALSE]
        }

    } else if (survey == "acs1"){
        cat(paste0("Restrictions of table content in each year. ",
                   "NA means data not collected."))
        dt <- table_content_acs1year_all_years # generate_acs1_tablecontents_()
        if (!is.null(years)){
            if (min(years) < 2005 | max(years) > 2019){
                message("Only 2005 - 2019 are available for acs 1 year surveys.")
                return(NULL)
            }
            selected_cols <- select_columns(dt, years)
            dt <- dt[, c("reference", "table_content", "table_name", selected_cols, "universe"), with = FALSE]
        }

    } else {
        message('Survey must be "dec", "decennial", "acs5", or "acs1".')
        return(NULL)
    }

    if (!is.null(keywords)){
        keywords <- str_trim(keywords) %>%
            tolower()

        dt[, comb := str_c(reference, table_content, table_name, universe, sep = " ")]

        kws <- unlist(str_split(keywords, " "))
        for (kw in kws) {
            dt <- dt[str_detect(tolower(comb), kw)]
        }

        dt[, comb := NULL]
        # keywords <- unlist(str_split(tolower(keyword), " "))
        # for (kw in keywords){
        #     # combine all rows to form a new column for search
        #     dt <- dt[, comb := apply(dt, 1, paste, collapse = " ")] %>%
        #         .[grepl(kw, tolower(comb))] %>%
        #         .[, comb := NULL]
        # }
    }

    if (view) View(dt, paste(keywords, "found"))

    return(invisible(dt))
}



#' Search Geographic Headers
#'
#' @description Search in field reference or description of geographic header
#' records to find the reference of "geo_headers" argument in function \code{\link{read_decennial}},
#' \code{\link{read_acs1year}}, and \code{\link{read_acs5year}}.
#'
#' @param survey survey type, including "dec" (or "decennial"), "acs1" or "acs5".
#' @param years year or ending year of the survey, can be a single year such as
#' 2010 or a vector like 2014:2016.
#' @param keywords keyword to search in code or description, in the form like
#' "abc def dsdfsa". Rows with all words are returned.
#' @param view display the search result with View if TRUE
#'
#' @return data.table matching the search criteria
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' # search geoheader that contains keyword "india" in decennial 2010
#' aaa <- search_geoheaders("decennial", 2000, "india", view = FALSE)
#'
#' # search for lattitude
#' bbb <- search_geoheaders("dec", 2010, "latitu", view = FALSE)
#'
#'
#' \dontrun{
#'   # browse all geoheaders in ACS i year in View()
#'   search_geoheaders("acs1")
#' }
#'
#' @export
#'
search_geoheaders <- function(survey, years = NULL, keywords = NULL, view = TRUE) {
    if (survey %in% c("decennial", "dec")){
        dt <- generate_decennial_geoheaders_()
        if (!is.null(years)){
            for (yr in years){
                if (!(yr %in% c(2000, 2010))){
                    message("Only 2000 and 2010 are available for decennial census. ")
                    return(NULL)
                }
            }
            selected_cols <- select_columns(dt, years)
            dt <- dt[, c("reference", "field", selected_cols), with = FALSE]
        }

    } else if (survey == "acs5"){
        dt <- generate_acs5_geoheaders_()
        if (!is.null(years)){
            if (min(years) < 2009 | max(years) > 2019){
                message("Only 2009 - 2019 are available for acs 5 year surveys.")
                return(NULL)
            }

            selected_cols <- c()
            for (yr in years){
                if (yr >= 2010){
                    selected_cols <- c(selected_cols, "acs5_2010_to_now")
                } else if (yr == 2009){
                    selected_cols <- c(selected_cols, "acs5_2009")
                }
            }
            selected_cols <- sort(unique(selected_cols), decreasing = TRUE)
            dt <- dt[, c("reference", "field", selected_cols), with = FALSE]
        }

    } else if (survey == "acs1"){
        dt <- generate_acs1_geoheaders_()
        if (!is.null(years)){
            if (min(years) < 2005 | max(years) > 2019){
                message("Only 2005 - 2019 are available for acs 1 year surveys.")
                return(NULL)
            }

            selected_cols <- c()
            for (yr in years){
                if (yr >= 2010){
                    selected_cols <- c(selected_cols, "acs1_2010_to_now")
                } else if (yr == 2009){
                    selected_cols <- c(selected_cols, "acs1_2009")
                } else if (yr %in% 2006:2008){
                    selected_cols <- c(selected_cols, "acs1_2006_to_2008")
                } else if (yr == 2005){
                    selected_cols <- c(selected_cols, "acs1_2005")
                }
            }

            selected_cols <- sort(unique(selected_cols), decreasing = TRUE)
            dt <- dt[, c("reference", "field", selected_cols), with = FALSE]
        }

    } else {
        message('Survey must be "dec", "decennial", "acs5", or "acs1".')
        return(NULL)
    }

    if (!is.null(keywords)){
        keywords <- str_trim(keywords) %>%
            tolower()

        dt[, comb := str_c(reference, field, sep = " ")]

        kws <- unlist(str_split(keywords, " "))
        for (kw in kws) {
            dt <- dt[str_detect(tolower(comb), kw)]
        }

        dt[, comb := NULL]
    }

    if (view) View(dt, paste(keywords, "found"))

    return(invisible(dt))
}





#' Search Summary Levels
#'
#' @description Search code or description of summary levels for summary_level
#' argument in  function \code{\link{read_decennial}},
#' \code{\link{read_acs1year}}, and \code{\link{read_acs5year}}.
#'
#' @param survey survey type, including "dec" (or "decennial"), "acs1" or "acs5".
#' @param years year or ending year of the survey, can be a single year such as
#' 2010 or a vector like 2014:2016.
#' @param keywords keyword to search in code or description, in the form like
#' "abc def dsdfsa". Rows with all words are returned.
#' @param view display the search result with View if TRUE
#'
#' @return A data.table of searched results.
#'
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' aaa = search_summarylevels("decennial", 2010, "block", view = FALSE)
#' bbb <- search_summarylevels("acs5", 2009:2010, "40", view = FALSE)
#'
#' \dontrun{
#'   # view all summary levels
#'   search_summarylevels("decennial")
#'   search_summarylevels("acs1")
#' }
#'
#' @export
#'


search_summarylevels <- function(survey, years=NULL, keywords = NULL, view = TRUE){

    if (survey %in% c("decennial", "dec")){
        dt <- generate_decennial_summary_level_()

        if (!is.null(years)){
            for (yr in years){
                if (!(yr %in% c(2000, 2010))){
                    message("Only 2000 and 2010 are available for decennial census. ")
                    return(NULL)
                }
            }
            dt <- dt[year %in% years]
        }

    } else if (survey == "acs5"){
        dt <- dict_acs5_summarylevel
        if (!is.null(years)){
            if (min(years) < 2009 | max(years) > 2019){
                message("Only 2009 - 2019 are available for acs 5 year surveys.")
                return(NULL)
            }

            selected_cols <- c()
            for (yr in years){
                if (yr >= 2013){
                    selected_cols <- c(selected_cols, "state_2013_to_now", "US_2011_to_now")
                } else if (yr == 2012){
                    selected_cols <- c(selected_cols, "state_2012", "US_2011_to_now")
                } else if (yr == 2011){
                    selected_cols <- c(selected_cols, "state_2009_to_2011", "US_2011_to_now")
                } else if (yr == 2010){
                    selected_cols <- c(selected_cols, "state_2009_to_2011", "US_2010")
                } else if (yr == 2009){
                    selected_cols <- c(selected_cols, "state_2009_to_2011", "US_2009")
                }
            }

            state_cols <- selected_cols[str_detect(selected_cols, "state")] %>%
                unique() %>%
                sort(decreasing = TRUE)
            US_cols <- selected_cols[str_detect(selected_cols, "US")] %>%
                unique() %>%
                sort(decreasing = TRUE)

            selected_cols <- c(state_cols, US_cols)

            dt <- dt[, c("code", "summary_level", selected_cols), with = FALSE]
        }

    } else if (survey == "acs1"){
        dt <- dict_acs1_summarylevel
        if (!is.null(years)){
            if (min(years) < 2005 | max(years) > 2017){
                message("Only 2005 - 2017 are available for acs 1 year surveys.")
                return(NULL)
            }
        }

    } else {
        message('Survey must be "dec", "decennial", "acs5", or "acs1".')
        return(NULL)
    }

    if (!is.null(keywords)){
        keywords <- tolower(keywords) %>%
            str_trim()
        dt <- dt[, comb := str_c(tolower(code), tolower(summary_level), sep = " ")]

        kws <- unlist(str_split(tolower(keywords), " "))
        for (kw in kws) {
            dt <- dt[str_detect(tolower(comb), kw)]
        }

        dt[, comb := NULL]

        # dt1 <- dt[str_detect(tolower(code),
        #                      str_replace_all(keywords, " +", "|"))]
        # dt2 <- dt[str_detect(tolower(geo_component),
        #                      str_replace_all(keywords, " +", "|"))]
        #
        # dt <- rbindlist(list(dt1, dt2)) %>%
        #     unique()
    }

    if (view) View(dt, paste(keywords, "found"))

    return(invisible(dt))
}




#' Search Geographic Components
#'
#' @description Search the code or content of geographic components for geo_comp
#' argument in function \code{\link{read_decennial}},
#' \code{\link{read_acs1year}}, and \code{\link{read_acs5year}}.
#'
#' @details The most frequently used geographic components are:
#'
#' 00 : all geographic component
#' 01 : urban
#' 43 : rural
#'
#' @param survey survey type, including "dec" (or "decennial"), "acs1" or "acs5".
#' @param years year or ending year of the survey, can be a single year such as
#' 2010 or a vector like 2014:2016.
#' @param keywords keyword to search in code or description, in the form like
#' "abc def dsdfsa". Rows with all words are returned.
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' aaa <- search_geocomponents("decennial", 2010, "urban", view = FALSE)
#' bbb <- search_geocomponents("acs5", 2011:2015, "43", view = FALSE)
#'
#' \dontrun{
#'   # view all geocomponents
#'   search_geocomponents("dec")
#'   search_geocomponents("acs5")
#' }
#'
#' @export
#'


search_geocomponents <- function(survey, years = NULL, keywords = NULL, view = TRUE){

    if (survey %in% c("decennial", "dec")){
        dt <- generate_decennial_geocomponent_()

        if (!is.null(years)){
            for (yr in years){
                if (!(yr %in% c(2000, 2010))){
                    message("Only 2000 and 2010 are available for decennial census. ")
                    return(NULL)
                }
            }
            dt <- dt[year %in% years]
        }

    } else if (survey == "acs5"){
        if (!is.null(years)){
            if (min(years) < 2009 | max(years) > 2019){
                message("Only 2009 - 2019 are available for acs 5 year surveys.")
                return(NULL)
            }
        }
        dt <- dict_acs5_geocomponent

    } else if (survey == "acs1"){
        dt <- dict_acs1_geocomponent
        if (!is.null(years)){
            if (min(years) < 2005 | max(years) > 2019){
                message("Only 2005 - 2019 are available for acs 1 year surveys.")
                return(NULL)
            }

            selected_cols <- c()
            for (yr in years){
                if (yr >= 2009){
                    selected_cols <- c(selected_cols, "state_2009_to_now", "US_2009_to_now")
                } else if (yr %in% 2007:2008){
                    selected_cols <- c(selected_cols, "state_2007_2008", "US_2007_2008")
                } else if (yr == 2006){
                    selected_cols <- c(selected_cols, "state_2005_2006", "US_2006")
                } else if (yr == 2005){
                    selected_cols <- c(selected_cols, "state_2005_2006", "US_2005")
                }
            }

            state_cols <- selected_cols[str_detect(selected_cols, "state")] %>%
                unique() %>%
                sort(decreasing = TRUE)
            US_cols <- selected_cols[str_detect(selected_cols, "US")] %>%
                unique() %>%
                sort(decreasing = TRUE)

            selected_cols <- c(state_cols, US_cols)
            dt <- dt[, c("code", "geo_component", selected_cols), with = FALSE]
        }

    } else {
        message('Survey must be "dec", "decennial", "acs5", or "acs1".')
        return(NULL)
    }

    if (!is.null(keywords)){
        keywords <- tolower(keywords) %>%
            str_trim()
        dt <- dt[, comb := str_c(tolower(code), tolower(geo_component), sep = " ")]

        kws <- unlist(str_split(tolower(keywords), " "))
        for (kw in kws) {
            dt <- dt[str_detect(tolower(comb), kw)]
        }

        dt[, comb := NULL]

        # dt1 <- dt[str_detect(tolower(code),
        #                      str_replace_all(keywords, " +", "|"))]
        # dt2 <- dt[str_detect(tolower(geo_component),
        #                      str_replace_all(keywords, " +", "|"))]
        #
        # dt <- rbindlist(list(dt1, dt2)) %>%
        #     unique()
    }

    if (view) View(dt, paste(keywords, "found"))

    return(invisible(dt))
}



#'
#' Search Tables
#'
#' @description Search table numbers and description.
#'
#' @param survey survey type, including "dec" (or "decennial"), "acs1" or "acs5".
#' @param years year or ending year of the survey, can be a single year such as
#' 2010 or a vector like 2014:2016.
#' @param keywords keyword to search in code or description, in the form like
#' "abc def dsdfsa". Rows with all words are returned.
#' @param view display the search result with View if TRUE
#'
#' @return A data.table
#'
#'
#'
#' @examples
#' # Change view = TRUE (default) to View the returned data.
#' aaa <- search_tables("dec", 2010, "occupancy", view = FALSE)
#' bbb <- search_tables("acs5", 2014:2016, "detailed race", view = FALSE)
#'
#' \dontrun{
#'   # view all tables
#'   search_tables("dec")
#'   search_tables("acs1")
#' }
#'
#'
#' @export
#'

search_tables <- function(survey, years = NULL, keywords = NULL, view = TRUE){

    if (survey %in% c("decennial", "dec")){
        dt <- generate_decennial_table_()
        if (!is.null(years)){
            for (yr in years){
                if (!(yr %in% c(2000, 2010))){
                    message("Only 2000 and 2010 are available for decennial census. ")
                    return(NULL)
                }
            }
            dt <- dt[year %in% years]
        }

    } else if (survey == "acs5"){
        # generated in data_raw/acs with read_acsxyear()
        dt <- dict_acs5_table
        if (!is.null(years)){
            if (min(years) < 2009 | max(years) > 2019){
                message("Only 2009 - 2019 are available for acs 5 year surveys.")
                return(NULL)
            }

            selected_cols <- select_columns(dt, years)
            dt <- dt[, c("table_number", "table_name", selected_cols, "universe"), with = FALSE]
        }

    } else if (survey == "acs1"){
        dt <- dict_acs1_table
        if (!is.null(years)){
            if (min(years) < 2005 | max(years) > 2019){
                message("Only 2005 - 2019 are available for acs 1 year surveys.")
                return(NULL)
            }

            selected_cols <- select_columns(dt, years)
            dt <- dt[, c("table_number", "table_name", selected_cols, "universe"), with = FALSE]
        }

    } else {
        message('survey must be "dec", "decennial", "acs5", or "acs1".')
        return(NULL)
    }

    if (!is.null(keywords)){
        keywords <- tolower(keywords) %>%
            str_trim()
        dt <- dt[, comb := apply(dt, 1, paste, collapse = " ")]

        kws <- unlist(str_split(tolower(keywords), " "))
        for (kw in kws) {
            dt <- dt[str_detect(tolower(comb), kw)]
        }

        dt[, comb := NULL]
    }

    if (view) View(dt, paste(keywords, "found"))

    return(invisible(dt))
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
    acs5_2017 <- modify_lookup_table_(5, 2017)
    acs5_2018 <- modify_lookup_table_(5, 2018)
    acs5_2019 <- modify_lookup_table_(5, 2019)

    dict_acs_tablecontent <- reduce(list(acs5_2009,
                                         acs5_2010,
                                         acs5_2011,
                                         acs5_2012,
                                         acs5_2013,
                                         acs5_2014,
                                         acs5_2015,
                                         acs5_2016,
                                         acs5_2017,
                                         acs5_2018,
                                         acs5_2019),
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
        .[!is.na(acs5_2017), ":=" (table_content = content_acs5_2017,
                                   table_name = name_acs5_2017,
                                   universe = universe_acs5_2017)] %>%
        .[!is.na(acs5_2018), ":=" (table_content = content_acs5_2018,
                                   table_name = name_acs5_2018,
                                   universe = universe_acs5_2018)] %>%
        .[!is.na(acs5_2019), ":=" (table_content = content_acs5_2019,
                                   table_name = name_acs5_2019,
                                   universe = universe_acs5_2019)] %>%

        # include all years and surveys
        .[, .(reference, table_content, table_name,
              acs5_2019, acs5_2018, acs5_2017, acs5_2016, acs5_2015,
              acs5_2014, acs5_2013, acs5_2012, acs5_2011,
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
    acs1_2018 <- modify_lookup_table_(1, 2018)
    acs1_2019 <- modify_lookup_table_(1, 2019)

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
                                         acs1_2017,
                                         acs1_2018,
                                         acs1_2019),
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
        .[!is.na(acs1_2018), ":=" (table_content = content_acs1_2018,
                                   table_name = name_acs1_2018,
                                   universe = universe_acs1_2018)] %>%
        .[!is.na(acs1_2019), ":=" (table_content = content_acs1_2019,
                                   table_name = name_acs1_2019,
                                   universe = universe_acs1_2019)] %>%

        # include all years and surveys
        .[, .(reference, table_content, table_name,
              acs1_2019, acs1_2018,
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


generate_decennial_geocomponent_ <- function(){
    G2010 <- copy(dict_decennial_geocomponent_2010) %>%
        .[, year := 2010]
    G2000 <- copy(dict_decennial_geocomponent_2000) %>%
        .[, year := 2000]

    dict <- rbindlist(list(G2010, G2000)) %>%
        .[, .(year, code, geo_component,
              in_state_file = state_file,
              in_US_file = US_file)]

    return(dict)
}

generate_decennial_table_ <- function(){
    T2010 <- copy(dict_decennial_table_2010) %>%
        .[, .(table_number, table_name, universe, table_ref)] %>%
        .[, year := 2010]
    T2000 <- copy(dict_decennial_table_2000) %>%
        .[, .(table_number, table_name, universe, table_ref)] %>%
        .[, year := 2000]
    table <- rbindlist(list(T2010, T2000)) %>%
        .[, .(year, table_ref, table_number, table_name, universe)]

    return(table)
}

