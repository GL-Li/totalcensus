# 11/06/2017 : need work on docs

# package functions ============================================================

#' Read decennial census data
#'
#' @description  Read geographic header record file and data files of states and return
#' selected geoheaders, state abbreviation, and table contents of selected summary
#' level and geographic component. These table contents can be from different tables, for example,
#' c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138").
#' The rows of short contents that have no corresponding LOGRECNOs are filled with
#' NAs. To find the references of table contents of interest, search with function
#' \code{\link{search_datafile}}. To find geographic headers, browse
#' \code{\link{dict_geoheader}} or search with function \code{\link{search_geoheader}}.
#'
#' @param year year of the decennial census
#' @param states vector of state abbreviations, for example "IN" or c("MA", "RI").
#' @param table_contents selected references of contents in census tables.
#' @param geo_headers vector of references of selected geographci headers to be included in the return.
#' @param summary_level select which summary level to keep, "*" to keep all. It takes strings
#'        including "state", "county", "county_subdivision", "place", "tract", "block_group",
#'        and "block" for the most common levels. It also take code for level. Search all codes with
#'        \code{\link{search_sumlev}} for state files or \code{\link{search_sumlev_US}} for national files.
#' @param geo_comp select which geographic component to keep, "*" to keep all.
#'        Summary level of a state contains all geographic component. County
#'        subdivision and higher level have all area ("00"), urban ("01"), and rural ("43").
#'        census tract and lower level have no geographic component, all are "00". Browse
#'        \code{\link{dict_geocomp}} for other geographic components.
#' @param show_progress show progress of file reading if TRUE. Turn off if FALSE, which
#'     is useful in RMarkdown output.
#'
#' @return A data.table whose columns include the selected geoheaders and
#' table contents plus SUMLEV, GEOCOMP, and state.
#'
#'
#' @examples
#' \dontrun{
#' # read one table and one area from one state
#' aaa = read_decennial(year = 2010,
#'                      states = "ut",
#'                      table_contents = c("P0150008"),
#'                      geo_headers = c("COUSUB", "CBSA"),
#'                      summary_level = "block")
#'
#'
#' # read multiple table contents and areas from multiple states
#' bbb = read_decennial(year = 2010,
#'                      states = c("ut", "ri"),
#'                      table_contents = c("P0150008", "P0030001", "P0030003",
#'                                         "P0080036", "PCT012G002", "PCT012G181"),
#'                      areas = c("place = ut62360",
#'                                      "Providence city, RI",
#'                                      "cousub = ri41500",
#'                                      "cbsa = 39300"),
#'                      summary_level = "block")
#'
#'
#' # read table contents of all county subdivisions in Providence metro
#' ccc <- read_decennial(2010, "US",
#'                       table_contents = c("P0080036", "PCT012G002"),
#'                       geo_headers = c("name", "cbsa = 39300"),
#'                       summary_level = "county subdivision",
#'                       geo_comp = "00")
#' }
#'
#' @export
#'

read_decennial <- function(year,
                          states,
                          table_contents = NULL,
                          areas = NULL,
                          geo_headers = NULL,
                          summary_level = "*",
                          geo_comp = "*",
                          show_progress = TRUE){

    if (is.null(areas) + is.null(geo_headers) == 0){
        stop("Must keep at least one of arguments areas and geo_headers NULL")
    }

    # turn off warning, fread() gives warnings when read non-scii characters.
    options(warn = -1)

    if (!is.null(areas)){
        dt <- read_decennial_areas_(
            year, states, table_contents, areas, summary_level, geo_comp,
            show_progress
        )
    } else {
        dt <- read_decennial_geoheaders_(
            year, states, table_contents, geo_headers, summary_level, geo_comp,
            show_progress
        )
    }

    options(warn = 0)
    return(dt)
}




# internal functions ============================================================

read_decennial_areas_ <- function(year,
                                  states,
                                  table_contents = NULL,
                                  areas,
                                  summary_level = "*",
                                  geo_comp = "*",
                                  show_progress = TRUE){
    # read decennial census data of selected areas
    #
    # Args_____
    # year :  end year of the 5-year survey
    # states : vector of abbreviations of states such as c("MA", "RI")
    # table_contents :  vector of reference of available table contents
    # areas : For metro area, in the format like "New York metro".
    #      For county, city, or town, must use the exact name as those in
    #      \code{\link{dict_fips}} in the format like "kent county, RI",
    #     "Boston city, MA", and "Lincoln town, RI". And special examples like
    #     "Salt Lake City city, UT" must keep the "city" after "City".
    # summary_level : summary level like "050"
    # geo_comp : geographic component such as "00", "01", and "43"
    # show_progress : whether to show progress in fread()n
    #
    # Return_____
    # A data.table
    #
    # Examples_____
    # aaa = totalcensus:::read_decennial_areas_(
    #     year = 2010,
    #     states = c("ri", "MA"),
    #     table_contents = c("P0030003", "P0080036", "PCT012G002", "PCT012G181"),
    #     areas = c("Lincoln town, ri", "PLACE = RI59000", "providence metro"),
    #     summary_level = "block"
    # )
    #
    # bbb <- totalcensus:::read_decennial_areas_(
    #     year = 2010,
    #     states = c("ut", "ri"),
    #     table_contents = c("P0030003", "P0080036", "PCT012G002", "PCT012G181"),
    #     areas = c("Lincoln Town, RI",
    #               "Kent county, RI",
    #               "Salt Lake City city, UT",
    #               "Salt Lake metro",
    #               "Providence city, RI"),
    #     summary_level = "block group"
    # )

    #=== prepare arguments ===

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # convert areas to the form of data.table
    #    geoheader  code state                    name
    # 1:     PLACE 62360    UT     Providence city, UT
    # 2:    COUNTY   005    RI      Newport County, RI
    dt_areas <- convert_areas(areas)


    states <- toupper(states)
    # toupper(NULL) ---> character(0) will cause trouble
    if (!is.null(table_contents)) table_contents <- toupper(table_contents)

    # this is used to extract geographic headers
    if (!is.null(areas)) geo_headers <- unique(dt_areas[, geoheader])

    # switch summary level and geocomponent
    summary_level <- switch_summarylevel(summary_level)
    geo_comp <- switch_geocomp(geo_comp)


    # lookup of the year
    lookup <- get(paste0("lookup_census_", year))

    for (content in table_contents) {
        if (!tolower(content) %in% tolower(lookup$reference)){
            stop(paste("The table content reference", content, "does not exist."))
        }
    }

    # === read files ===

    lst_state <- list()
    for (state in states) {
        # read geography. do NOT read geo_headers from decennial data, instead read
        # from GEOID_coord_XX later on, which is generated from Census 2010 and
        # has much more geo_header data
        geo <- read_decennial_geo_(year, state,
                                   c(geo_headers, "STATE", "INTPTLON", "INTPTLAT"),
                                   show_progress = TRUE) %>%
            setnames(c("INTPTLON", "INTPTLAT"), c("lon", "lat")) %>%
            # convert STATE fips to state abbreviation
            .[, state := convert_fips_to_names(STATE)] %>%
            setkey(LOGRECNO)


        # add population data of each geographic area
        popul <- read_decennial_tablecontents_(year, state,
                                               "P0010001", show_progress) %>%
            .[, .(LOGRECNO, population = P0010001)]
        geo <- geo[popul]

        # read data from each file
        if(!is.null(table_contents)){
            # get files for table contents, follow the notation of read_tablecontent.R
            dt <- read_decennial_tablecontents_(year, state, table_contents,
                                                show_progress)
            decennial <- merge(geo, dt)
        } else {
            decennial <- geo
        }

        # To determine what PLACE or COUSUB a tract or block group (partially)
        # blongs, replace PLACE and COUSUB with those obtained from census 2010
        # in generate_geoid_coordinate.R
        decennial <- add_geoheader(decennial, state, geo_headers, summary_level,
                                   survey = "decennial")

        lst_state[[state]] <- decennial[SUMLEV %like% summary_level & GEOCOMP %like% geo_comp]

    }

    combined <- rbindlist(lst_state) %>%
        .[, ":=" (LOGRECNO = NULL, STATE = NULL)] %>%
        convert_geocomp_name()


    # select data for argument areas
    selected <- map(
        1:nrow(dt_areas),
        function(x) combined[get(dt_areas[x, geoheader]) %like% dt_areas[x, code] &
                                 state %like% dt_areas[x, state]] %>%
            .[, area := dt_areas[x, name]]
    ) %>%
        rbindlist() %>%
        # no use of the geoheaders
        .[, unique(dt_areas[, geoheader]) := NULL] %>%
        convert_geocomp_name()

    # reorder columns
    begin <- c("area", "lon", "lat")
    end <- c("GEOCOMP", "SUMLEV")
    setcolorder(selected, c(begin, setdiff(names(selected), c(begin, end)), end))

    return(selected)
}



read_decennial_geoheaders_ <- function(year,
                                  states,
                                  table_contents = NULL,
                                  geo_headers = NULL,
                                  summary_level = "*",
                                  geo_comp = "*",
                                  show_progress = TRUE){
    # read decennial census data of selected areas
    #
    # Args_____
    # year :  end year of the 5-year survey
    # states : vector of abbreviations of states such as c("MA", "RI")
    # table_contents :  vector of reference of available table contents
    # geo_headers : vetor of geographic header such as c("PLACE", "CBSA")
    # summary_level : summary level like "050"
    # geo_comp : geographic component such as "00", "01", and "43"
    # show_progress : whether to show progress in fread()n
    #
    # Return_____
    # A data.table
    #
    # Examples_____
    # aaa = totalcensus:::read_decennial_geoheaders_(
    #     year = 2010,
    #     states = c("ri", "MA"),
    #     table_contents = c("P0030003", "P0080036", "PCT012G002", "PCT012G181"),
    #     geo_headers = c("COUSUB", "PLACE", "CBSA"),
    #     summary_level = "block"
    # )
    #
    # bbb <- totalcensus:::read_decennial_geoheaders_(
    #     year = 2010,
    #     states = c("ut", "ri"),
    #     table_contents = c("P0030003", "P0080036", "PCT012G002", "PCT012G181"),
    #     geo_headers = c("COUSUB", "PLACE", "CBSA"),
    #     summary_level = "block group"
    # )

    #=== prepare arguments ===

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    states <- toupper(states)
    # toupper(NULL) ---> character(0) will cause trouble
    if (!is.null(table_contents)) table_contents <- toupper(table_contents)
    if (!is.null(geo_headers)) geo_headers <- toupper(geo_headers)

    # switch summary level to code
    summary_level <- switch_summarylevel(summary_level)
    geo_comp <- switch_geocomp(geo_comp)


    # lookup of the year
    lookup <- get(paste0("lookup_census_", year))

    for (content in table_contents) {
        if (!tolower(content) %in% tolower(lookup$reference)){
            stop(paste("The table content reference", content, "does not exist."))
        }
    }

    # === read files ===

    lst_state <- list()
    for (state in states) {
        # read geography. do NOT read geo_headers from decennial data, instead read
        # from GEOID_coord_XX later on, which is generated from Census 2010 and
        # has much more geo_header data
        geo <- read_decennial_geo_(year, state,
                                   c(geo_headers, "STATE", "INTPTLON", "INTPTLAT"),
                                   show_progress = TRUE) %>%
            setnames(c("INTPTLON", "INTPTLAT"), c("lon", "lat")) %>%
            # convert STATE fips to state abbreviation
            .[, state := convert_fips_to_names(STATE)] %>%
            setkey(LOGRECNO)


        # add population data of each geographic area
        popul <- read_decennial_tablecontents_(year, state,
                                               "P0010001", show_progress) %>%
            .[, .(LOGRECNO, population = P0010001)]
        geo <- geo[popul]

        # read data from each file
        if(!is.null(table_contents)){
            # get files for table contents, follow the notation of read_tablecontent.R
            dt <- read_decennial_tablecontents_(year, state, table_contents,
                                                show_progress)
            decennial <- merge(geo, dt)
        } else {
            decennial <- geo
        }

        # To determine what PLACE or COUSUB a tract or block group (partially)
        # blongs, replace PLACE and COUSUB with those obtained from census 2010
        # in generate_geoid_coordinate.R
        decennial <- add_geoheader(decennial, state, geo_headers, summary_level,
                                   survey = "decennial")

        lst_state[[state]] <- decennial[SUMLEV %like% summary_level & GEOCOMP %like% geo_comp]

    }

    combined <- rbindlist(lst_state) %>%
        .[, ":=" (LOGRECNO = NULL, STATE = NULL)] %>%
        convert_geocomp_name()


    if (length(geo_headers) == 1 &&
        geo_headers %in% c("STATE", "COUNTY", "PLACE", "COUNTY", "CBSA")){
            combined[, area := convert_fips_to_names(get(geo_headers), state, geo_headers)]
    }


    # reorder columns
    if (length(geo_headers) == 1 &&
        geo_headers %in% c("STATE", "COUNTY", "PLACE", "COUNTY", "CBSA")){
            begin <- c("area", "lon", "lat")
    } else {
        begin <- c("lon", "lat")
    }
    end <- c("GEOCOMP", "SUMLEV")
    setcolorder(combined, c(begin, setdiff(names(combined), c(begin, end)), end))

    return(combined)
}




# Read geographic header record file of ONE state

read_decennial_geo_ <- function(year,
                                state,
                                geo_headers = NULL,
                                show_progress = TRUE) {
    # This function reads the geographic record file of one state and returns a
    # data.table with columns of LOGRECNO, SUNLEV, GEOCOMP plus geoheaders
    # given in argument geo_headers of the

    # Args_____
    # year : integrer, year of the decennial census, such as 2010
    # state : string, abbreviation of a state, such as "IN"
    # geo_headers : string vector of geoheaders, such as c("CBSA", "PLACE")
    # show_progress : wheather to show progress in fread()
    #
    # Return_____
    # a data.table keyed with LOGRECNO


    #=== prepare arguments ===

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    if (show_progress) {
        cat(paste("Reading", state, "geographic header record file\n"))
    }


    #=== read files and select columns ===

    file <- paste0(path_to_census, "/census", year, "/", state, "/", tolower(state),
                   "geo", year, ".ur1")
    # use "Latin-1" for encoding special spanish latters such as ñ in Cañada
    geo <- fread(file, header = FALSE, sep = "\n", encoding = "Latin-1" ,
                 showProgress = show_progress)

    # always keep the following geoheaders in the output data
    dt <- geo[, .(LOGRECNO = as.numeric(str_sub(V1, 19, 25)),
                  SUMLEV = str_sub(V1, 9, 11),
                  GEOCOMP = str_sub(V1, 12, 13))]

    # add all selected fields to output data
    if (!is.null(geo_headers)) {
        for (ref in geo_headers) {
            # identify numeric geohearders
            if (ref %in% c("INTPTLAT", "INTPTLON", "AREALAND", "AREAWATR",
                           "POP100", "HU100")) {
                # place variable in () to add new columns
                dt[, (ref) := as.numeric(str_sub(
                    geo[, V1],
                    dict_census_geoheader[reference == ref, start],
                    dict_census_geoheader[reference == ref, end]
                ))]
            } else if (ref %in% c("LOGRECNO", "SUMLEV", "GEOCOMP")) {
                message(paste(ref, "is already included in return by default.\n"))
            } else {
                dt[, (ref) := str_trim(str_sub(
                    geo[, V1],
                    dict_census_geoheader[reference == ref, start],
                    dict_census_geoheader[reference == ref, end]
                ))]
            }
        }
    }

    # use key LOGRECNO for joining with data
    setkey(dt, LOGRECNO)

    return(dt)
}


# read selected table content of a state
# These contents can be from different tables, for example,
# c("PCT012F139", "P0030008", "P0100059", "P0150008", "H0070016", "PCT012F138").
# The rows of short contents that have no corresponding LOGRECNOs are filled with
# NAs.
read_decennial_1_file_tablecontents_ <- function(year,
                                                 state,
                                                 file_seg,
                                                 table_contents = NULL,
                                                 show_progress = TRUE){
    # Read selected table content in one data file of a state and return a
    # data.table, which has columns of LOGRECNO and selected table contents

    # Args_____
    # year : integrer, year of the decennial census, such as 2010
    # state : string, abbreviation of a state, such as "IN"
    # filg_seg : segment number of file such as "01" and "34"
    # table_contents : vector of references of table contents in the same file
    #     segment, for example c("P0030001", "P0030008")
    # show_progress : wheather to show progress in fread()
    #
    # Return_____
    # a data.table keyed with LOGRECNO


    #=== prepare arguments ===

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    #=== read data ===

    if (year == 2010){
        lookup_census <- lookup_census_2010
    }

    # determine location of the flds in file_seg
    all_contents <- lookup_census[file_segment == file_seg, reference]
    loc <- which(all_contents %in% table_contents)
    cols <- paste0("V", loc)

    file <- paste0(path_to_census, "/census", year, "/", state, "/", tolower(state),
                   "000", file_seg, year, ".ur1")

    if (show_progress) {
        cat(paste("Reading", state, "file", file_seg, "\n"))
    }

    # fread assigns column names as "V1", "V2", ... when header = FALSE
    dt <- fread(file, header = FALSE, select = c("V5", cols), showProgress = show_progress) %>%
        set_colnames(c("LOGRECNO", table_contents)) %>%
        setkey(LOGRECNO)

    return(dt)
}



read_decennial_tablecontents_ <- function(year,
                                          state,
                                          table_contents = NULL,
                                          show_progress = TRUE){
    # Read table_contents from multiple files of a state.

    # Args_____
    # year : integrer, year of the decennial census, such as 2010
    # state : string, abbreviation of a state, such as "IN"
    # table_contents : vector of references of table contents can be in different
    # file segment, for example c("P0030001", "P0030008")
    # show_progress : wheather to show progress in fread()
    #
    # Return_____
    # a data.table keyed with LOGRECNO
    #
    # Example_____
    # read_decennial_tablecontents_(
    #     2010, "RI",
    #     table_contents = c("P0150008", "P0030001",
    #                        "P0030003", "P0080036",
    #                        "PCT012G002", "PCT012G181")
    # )
    #        LOGRECNO P0030001 P0030003 P0080036 P0150008 PCT012G002 PCT012G181
    # 1:            1  1052567    60189       71     2988      16942         32
    # 2:            2   955043    59629       70     2955      16340         31
    # 3:            3   952101    59613       70     2955      16315         31
    # 4:            4     2942       16        0        0         25          0
    # 5:            5    97524      560        1       33        602          1
    # ---
    # 31758:    31758    29191      635        3       16        343          0
    # 31759:    31759    22787      222        0        5        232          0
    # 31760:    31760    41186     2621        6       41        889          1
    # 31761:    31761        0        0        0        0          0          0
    # 31762:    31762    35809      242        0       12        225          0

    # locate data files for the content
    lookup <- get(paste0("lookup_census_", year))
    file_content <- lookup_tablecontents(table_contents, lookup)

    # file_content <- lookup[reference %in% table_contents,
    #                               .(file_seg = file_segment,
    #                                 content = reference)] %>%
    #     .[, paste(content, collapse = ","), by = file_seg] %>%
    #     .[, table_contents := str_split(V1, ",")] %>%
    #     .[, V1 := NULL]

    dt <- purrr::map2(file_content[, file_seg],
                      file_content[, table_contents],
                      function(x, y) read_decennial_1_file_tablecontents_(
                          year, state, file_seg = x, table_contents = y,
                          show_progress = show_progress
                      )) %>%
        purrr::reduce(merge, all = TRUE)

    return(dt)

}
