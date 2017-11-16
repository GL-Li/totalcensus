# package function =======================================================
#' Read ACS 1-year survey
#'
#' @description Read ACS 1-year survey of selected geography, estimate, and margin
#' of selected states.
#'
#' @param year  year of the 1-year survey
#' @param states  vector of abbreviations of states such as c("MA", "RI")
#' @param table_contents  vector of reference of available table contents
#' @param geo_headers  vector of geographic headers like c("NAME", "PLACE")
#' @param summary_level  summary level like "050"
#' @param geo_comp  geographic component such as "00", "01", and "43"
#' @param with_margin  read also margin of error in addition to estimate
#' @param with_coord  whether to include longitude and latitude in return
#' @param with_population whether to include total population in return
#' @param with_acsgeoheaders whether to include geographic headers from ACS data
#' @param show_progress  whether to show progress in fread()n
#'
#' @return A data.table of selected data.
#'
#' @examples
#' \dontrun{
#' # The lowest summary level in ACS 1-year is county subdivision. There is no
#' # need to run fine search in geo_headers but just use NULL geo_headers.
#' # The example get data of all counties in UT and RI.
#' aaa <- read_acs1year(2015, c("ut", "ri"),
#'                       table_contents = c("B01001_009", "B00001_001", "B10001_002"),
#'                       summary_level = "county")
#' }
#'
#'
#' @export
#'

read_acs1year <- function(year,
                          states,
                          table_contents = NULL,
                          geo_headers = NULL,
                          summary_level = "*",
                          geo_comp = "*",
                          with_margin = FALSE,
                          with_coord = TRUE,
                          with_population = TRUE,
                          with_acsgeoheaders = FALSE,
                          show_progress = TRUE){

    # turn off warning, fread() gives warnings when read non-scii characters.
    options(warn = -1)

    #=== prepare arguments ===
    if (summary_level %in% c("tract", "block group", "block")) {
        stop("ACS 1-year surveys only have summary level above 070")
    }

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")
    geo_headers_0 <- geo_headers  # keep a copy for latter use
    states <- toupper(states)
    if (!is.null(table_contents)) table_contents <- toupper(table_contents)
    if (!is.null(geo_headers)) geo_headers <- toupper(geo_headers)

    # extract information from argument geo_headers
    dt_geo <- organize_geoheaders(geo_headers)

    # this is used to extract geoheaders
    if (!is.null(geo_headers)) geo_headers <- unique(dt_geo[, geoheader])

    # switch summary level to code

    if (summary_level %in% c("state", "county", "county subdivision", "place",
                             "tract", "block group")){
        summary_level <- switch(summary_level,
                                "state" = "040",
                                "county" = "050",
                                "county subdivision" = "060",
                                "place" = "160",
                                "tract" = "140",
                                "block group" = "150")
    }

    # lookup of the year
    lookup <- get(paste0("lookup_acs1year_", year))

    for (content in table_contents) {
        if (!tolower(content) %in% tolower(lookup$reference)){
            stop(paste("The table content reference", content, "does not exist."))
        }
    }

    # === read files ===

    lst_state <- list()
    for (state in states) {
        # read geography. do NOT read geo_headers from ACS data, instead read
        # from GEOID_coord_XX later on, which is generated from Census 2010 and
        # has much more geo_header data
        if (with_acsgeoheaders){
            geo <- read_acs1year_geo_(year, state, c(geo_headers, "STATE"),
                                      show_progress = TRUE) %>%
                # convert STATE fips to state abbreviation
                .[, state := convert_fips_to_names(STATE)] %>%
                setnames(geo_headers, paste0("acs_", geo_headers)) %>%
                setkey(LOGRECNO)
        }else {
            geo <- read_acs1year_geo_(year, state, "STATE",
                                      show_progress = TRUE) %>%
                # convert STATE fips to state abbreviation
                .[, state := convert_fips_to_names(STATE)] %>%
                setkey(LOGRECNO)
        }


        if (with_population){
            popul <- read_acs1year_tablecontents_(year, state,
                                                  "B01003_001", "e", show_progress) %>%
                .[, .(LOGRECNO, population = B01003_001_e)]
            geo <- geo[popul]
        }

        # read estimate and margin from each file
        if(!is.null(table_contents)){
            # get files for table contents, follow the notation of read_tablecontent.R
                dt <- read_acs1year_tablecontents_(year, state, table_contents,
                                                   "e", show_progress)
                if (with_margin) {
                    margin <- read_acs1year_tablecontents_(state, year, table_contents,
                                                        "m", show_progress)

                    dt <- merge(dt, margin)
                }

            acs <- merge(geo, dt)
        } else {
            acs <- geo
        }

        # add coordinates
        if (with_coord) {
            coord <- fread(paste0(path_to_census, "/generated_data/geoid_coord/geoid_coord_", state, ".csv"),
                           select = c("GEOID", "lon", "lat", geo_headers),
                           colClasses = "character") %>%
                .[, lon := as.numeric(lon)] %>%
                .[, lat := as.numeric(lat)]

            acs <- coord[acs, on = .(GEOID)]
        }

        lst_state[[state]] <- acs[SUMLEV %like% summary_level & GEOCOMP %like% geo_comp]

    }

    combined <- rbindlist(lst_state) %>%
        .[, ":=" (LOGRECNO = NULL, STATE = NULL)]


    # turn on warning
    options(warn = 0)

    # select data for argument geo_headers
    if (is.null(geo_headers_0)) {
        selected <- combined
    } else {
        selected <- map(1:nrow(dt_geo),
                        function(x) combined[get(dt_geo[x, geoheader]) %like% dt_geo[x, code] &
                                                 state %like% dt_geo[x, state]]) %>%
            rbindlist()
    }

    return(selected)
}


# internal functions ===========================================================

read_acs1year_geo_ <- function(year,
                               state,
                               geo_headers = NULL,
                               show_progress = TRUE) {
    # Read geography file of one state of ACS 1-year survey and return a
    # data.table of

    # Args_____
    # year : integer, year of 1-year census
    # state : string, state abbreviation such as "MA"
    # geo_headers : string vector of geographic headers such as c("PLACE", "CBSA")
    # show_progress : wheather to show the progress of fread()
    #
    # Return_____
    # a data.table with key of LOGRECNO


    #=== prepare arguments ===

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # allow lowercase input for state and geo_headers
    state <- tolower(state)
    geo_headers <- toupper(geo_headers)

    if (show_progress) {
        cat("Reading", toupper(state), year, "ACS 1-year survey geography file\n")
    }

    #=== read file ===

    file <- paste0(path_to_census, "/", "acs1year/", year, "/g", year, "1",
                   tolower(state), ".csv")

    # use "Latin-1" for encoding special spanish latters such as ñ in Cañada
    # read all columns and then select as the file is not as big as those in
    # decennial census.
    geo <- fread(file, header = FALSE, encoding = "Latin-1" ,
                 showProgress = show_progress, colClasses = "character") %>%
        setnames(dict_acs_geoheader$reference) %>%
        .[, c(c("GEOID", "NAME", "LOGRECNO", "SUMLEV", "GEOCOMP"), geo_headers), with = FALSE] %>%
        .[, LOGRECNO := as.numeric(LOGRECNO)] %>%
        setkey(LOGRECNO)

    return(geo)
}




read_acs1year_1_file_tablecontents_ <- function(year, state, file_seg, table_contents,
                                                est_marg = "e", show_progress = TRUE){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # get column names from file segment, then add six ommitted ones
    lookup <- get(paste0("lookup_acs1year_", year))
    col_names <- lookup[file_segment == file_seg] %>%
        # get rid of references ending with ".5", which are not in the file
        .[str_extract(reference, "..$") != ".5", reference]
    ommitted <- c("FILEID", "FILETYPE", "STUSAB", "CHARITER", "SEQUENCE", "LOGRECNO")
    col_names <- c(ommitted, col_names)

    file <- paste0(path_to_census, "/", "acs1year/", year, "/", est_marg, year, "1",
                   tolower(state), file_seg, "000.txt")

    dt <- fread(file, header = FALSE, showProgress = show_progress) %>%
        setnames(names(.), col_names) %>%
        .[, c("LOGRECNO", table_contents), with = FALSE] %>%
        # add "_e" or "_m" to show the data is estimate or margin
        setnames(table_contents, paste0(table_contents, "_", est_marg)) %>%
        setkey(LOGRECNO)

    return(dt)
}



# # example
# aaa <- totalcensus:::read_acs1year_tablecontents_(
#     year = 2015,
#     state = "ri",
#     table_contents = c("B01001_009", "B00001_001", "B10001_002"),
#     est_marg = "m"
# )


read_acs1year_tablecontents_ <- function(year, state, table_contents,
                                         est_marg = "e",
                                         show_progress = TRUE){
    # Read ACS table_contents from 1-year survey and return a data.table
    #
    # Args_____
    # year: integer, year of the survey
    # state: state abbreviation
    # table_contents: vector of the table content references
    # est_marg: stringe, read estimate data or margin of error data, takes value
    #     "e" for estimate and "m" for margin of error
    # show_progress: wheather to show progress of fread()
    #
    # Return_____
    # a data table keyed with LOGRECNO
    #
    # Examples_____
    # table_contents = c("B01001_009", "B00001_001", "B10001_002")
    # read_acs1year_tablecontents_(2015, "RI", table_contents)



    # locate data files for the content
    lookup <- get(paste0("lookup_acs1year_", year))
    file_content <- lookup_tablecontents(table_contents, lookup)
    # file_content <- lookup[reference %in% table_contents,
    #                        .(file_seg = file_segment,
    #                          content = reference)] %>%
    #     .[, paste(content, collapse = ","), by = file_seg] %>%
    #     .[, table_contents := str_split(V1, ",")] %>%
    #     .[, V1 := NULL]

    dt <- purrr::map2(file_content[, file_seg],
                      file_content[, table_contents],
                      function(x, y) read_acs1year_1_file_tablecontents_(
                          year, state, file_seg = x, table_contents = y,
                          est_marg = est_marg,
                          show_progress = show_progress
                      )) %>%
        purrr::reduce(merge, all = TRUE)

    return(dt)
}





