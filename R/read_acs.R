# read ACS 1-year survey =======================================================
#' Read ACS 1-year survey
#'
#' @description Read ACS 1-year survey of selected geography, estimate, and margin
#' of selected states.
#'
#' @param states  vector of abbrivations of states, for example, "IN" and c("MA", "RI").
#' @param year year of the survey
#' @param geo_headers geographic headers.
#' @param est_marg which data to read, "e" for estimate, "m" for margin
#'
#' @return A data.table of selected data.
#'
#' @examples
#' \dontrun{
#' # read Rhode Island data file 0004, replace the path with your local folder
#' ma_ri <- read_acs1year("~/census_data", c("ma", "ri"), 2015,
#'                     geo_headers = c("NAME", "GEOID"),
#'                     table_contents = c("B01001_009", "B00001_001", "B10001_002"),
#'                     summary_level = "place")
#' }
#'
#'
#' @export
#' @import data.table
#' @import magrittr
#'

read_acs1year <- function(states,
                          year,
                          geo_headers = NULL,
                          table_contents = NULL,
                          with_margin = TRUE,
                          with_coord = TRUE,
                          summary_level = "*",
                          geo_comp = "*",
                          show_progress = TRUE){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # switch summary level to code
    if (summary_level %in% c("state", "county", "county_subdivision", "place",
                             "tract", "block_group")){
        summary_level <- switch(summary_level,
                                "state" = "040",
                                "county" = "050",
                                "county_subdivision" = "060",
                                "place" = "160",
                                "tract" = "140",
                                "block_group" = "150")
    }

    # lookup of the year
    lookup <- get(paste0("lookup_acs1year_", year))

    for (content in table_contents) {
        if (!tolower(content) %in% tolower(lookup$reference)){
            stop(paste("The table content reference", content, "does not exist."))
        }
    }

    lst_state <- list()
    for (state in states) {
        # also accept lowercase input
        #bbstate <- tolower(state)

        # read geography
        geo <- read_acs1year_geo_(state, year,
                                  geo_headers = geo_headers, show_progress = TRUE) %>%
            .[, state := toupper(state)]

        # add coordinates
        if (with_coord) {
            coord <- fread(paste0(path_to_census, "/generated_data/geoid_coord.csv"))
            geo <- coord[geo, on = .(GEOID)]
        }

        # read estimate and margin from each file
        if(!is.null(table_contents)){
            table_contents <- toupper(table_contents)

            # get files for table contents, follow the notation of read_tablecontent.R
            file_content <- lookup[reference %in% table_contents] %>%
                .[, .(file_num = file_segment, content = reference)]

            lst = list()
            for (num in unique(file_content$file_num)){
                cont <- file_content[file_num == num, content]
                dt <- read_acs1year_estimate_margin_(state, year,
                                                     num, "e") %>%
                    .[, c("LOGRECNO", cont), with = FALSE] %>%
                    setnames(cont, paste0(cont, "_e")) %>%
                    setkey(LOGRECNO)
                if (with_margin) {
                    margin <- read_acs1year_estimate_margin_(state, year,
                                                             num, "m") %>%
                        .[, c("LOGRECNO", cont), with = FALSE] %>%
                        setnames(cont, paste0(cont, "_m")) %>%
                        setkey(LOGRECNO)

                    dt <- merge(dt, margin)
                }

                assign(paste0("dt_", num), dt)
                # list elements names must be character, not number
                lst[[num]] <- get(paste0("dt_", num))
            }

            # merge into a large data.table as return using key LOGRECNO, fill with NA
            # for short files
            est_marg <- Reduce(function(x, y) merge(x, y, all = TRUE), lst)

            combined <- merge(geo, est_marg)
        } else {
            combined <- geo
        }

        combined <- combined[SUMLEV %like% summary_level & GEOCOMP %like% geo_comp]

        lst_state[[state]] <- combined
    }

    return(rbindlist(lst_state))
}



# read ACS 5-year survey =======================================================
#' Read ACS 5-year survey
#'
#' @description Read ACS 5-year survey of selected geography, estimate, and margin
#' of selected states.
#'
#' @param path_to_census path to the directory of downloaded census data.
#' @param states  vector of abbrivations of states, for example, "IN" and c("MA", "RI").
#' @param year year of the survey
#' @param geo_headers geographic headers.
#' @param est_marg which data to read, "e" for estimate, "m" for margin
#'
#' @return A data.table of selected data.
#'
#' @examples
#' \dontrun{
#' # read Rhode Island data file 0004, replace the path with your local folder
#' ma_ri <- read_acs1year("~/census_data", c("ma", "ri"), 2015,
#'                     geo_headers = c("NAME", "GEOID"),
#'                     table_contents = c("B01001_009", "B00001_001", "B10001_002"),
#'                     summary_level = "place")
#' }
#'
#'
#' @export
#' @import data.table
#' @import magrittr
#' @import stringr
#'

read_acs5year <- function(states,
                          year,
                          geo_headers = NULL,
                          table_contents = NULL,
                          with_margin = TRUE,
                          with_coord = TRUE,
                          summary_level = "*",
                          geo_comp = "*",
                          show_progress = TRUE){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # switch summary level to code
    if (summary_level %in% c("state", "county", "county_subdivision", "place",
                             "tract", "block_group")){
        summary_level <- switch(summary_level,
                                "state" = "040",
                                "county" = "050",
                                "county_subdivision" = "060",
                                "place" = "160",
                                "tract" = "140",
                                "block_group" = "150")
    }

    # lookup of the year
    lookup <- get(paste0("lookup_acs1year_", year))

    for (content in table_contents) {
        if (!tolower(content) %in% tolower(lookup$reference)){
            stop(paste("The table content reference", content, "does not exist."))
        }
    }

    lst_state <- list()
    for (state in states) {
        cat("reading ", state, " ACS 5-year geography file\n")

        # also accept lowercase input
        #bbstate <- tolower(state)

        # read geography
        geo <- read_acs5year_geo_(state, year,
                                  geo_headers = geo_headers, show_progress = TRUE) %>%
            .[, state := toupper(state)]

        # add coordinates
        if (with_coord) {
            coord <- fread(paste0(path_to_census, "/generated_data/geoid_coord.csv"))
            geo <- coord[geo, on = .(GEOID)]
        }

        # read estimate and margin from each file
        if(!is.null(table_contents)){
            table_contents <- toupper(table_contents)

            # get files for table contents, follow the notation of read_tablecontent.R
            file_content <- lookup[reference %in% table_contents] %>%
                .[, .(file_num = file_segment, content = reference)]

            lst = list()
            for (num in unique(file_content$file_num)){
                cont <- file_content[file_num == num, content]
                dt <- read_acs5year_estimate_margin_(state, year,
                                                     num, "e") %>%
                    .[, c("LOGRECNO", cont), with = FALSE] %>%
                    setnames(cont, paste0(cont, "_e")) %>%
                    setkey(LOGRECNO)
                if (with_margin) {
                    margin <- read_acs5year_estimate_margin_(state, year,
                                                             num, "m") %>%
                        .[, c("LOGRECNO", cont), with = FALSE] %>%
                        setnames(cont, paste0(cont, "_m")) %>%
                        setkey(LOGRECNO)

                    dt <- merge(dt, margin)
                }

                assign(paste0("dt_", num), dt)
                # list elements names must be character, not number
                lst[[num]] <- get(paste0("dt_", num))
            }

            # merge into a large data.table as return using key LOGRECNO, fill with NA
            # for short files
            est_marg <- Reduce(function(x, y) merge(x, y, all = TRUE), lst)

            combined <- merge(geo, est_marg)
        } else {
            combined <- geo
        }

        combined <- combined[SUMLEV %like% summary_level & GEOCOMP %like% geo_comp]

        lst_state[[state]] <- combined
    }

    return(rbindlist(lst_state))
}
