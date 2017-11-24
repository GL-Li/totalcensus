# package function ============================================================

#' download census data
#'
#' @description Download census data from United States Census bureau. By default
#' it downloads summary files and extract summary files of Census 2010, ACS 5-year
#' survey of 2015, and ACS 1-year survey of 2016, 2015, and 2014.
#'
#' @param survey Which survey to download from, "decennial", "acs5year", or "acs1year"
#' @param year year or ending year of the survey
#' @param states vector of abbreviations of states such as c("MA", "RI")
#'
#' @export

download_census <- function(survey = NULL, year = NULL, states = c(states_DC, "US", "PR")){
    if (is.null(survey)){
        message("Need 200 GB free space. Run download_census() again to resume downloading in case downloading breaks.")
        download_decennial_(2010, states)
        download_acs5year_(2015, states)
        download_acs1year_(2016)
        download_acs1year_(2015)
        download_acs1year_(2014)
    } else {
        if (survey == "decennial"){
            download_decennial_(year, states)
        } else if (survey == "acs5year"){
            download_acs5year_(year, states)
        } else if (survey == "acs1year"){
            download_acs1year_(year)
        } else {
            message("Please select survey from decennial, acs5year, or acs1year.")
        }
    }
}


# internal functions ===========================================================

download_decennial_ <- function(year, states){

    states <- toupper(states)
    # temp folder to hold all downloaded data
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    path_to_decennial <- paste0(path_to_census, "census", year)

    if (!dir.exists(path_to_decennial)){
        dir.create(path_to_decennial)
    }

    i <- 0
    N <- length(states)
    for (st in states){
        i <- i + 1
        cat(paste0("Downloading ", i, " of ", N, " states.\n"))
        cat(paste0("Downloading ", st, " summary files of Census ", year, ".\n"))
        if (file.exists(paste0(path_to_decennial, "/download_record.csv"))){
            record <- fread(paste0(path_to_decennial, "/download_record.csv"))

            finished_states <- record[, state]

            if (st %in% finished_states){
                message(paste0(st, " data has already been downloaded and extracted.\n"))
            } else {
                download_decennial_1_state_(year, st)
                record <- rbindlist(list(record, data.table(state = st)))
                fwrite(record, file = paste0(path_to_decennial, "/download_record.csv"))
            }
        } else {
            download_decennial_1_state_(year, st)
            record <- data.table(state = st)
            fwrite(record, file = paste0(path_to_decennial, "/download_record.csv"))
        }
    }
}




download_decennial_1_state_ <- function(year, state){
    # download decennial census data from:
    # census 2010: https://www2.census.gov/census_2010/04-Summary_File_1/Urban_Rural_Update/
    #
    # Args_____
    # year : year of the survey
    # state : abbreviation of the state

    state <- toupper(state)

    # temp folder to hold all downloaded data
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")


    # construct right names for url
    state_names <- dict_fips[, .(full = state_full, abbr = state_abbr)] %>%
        unique() %>%
        .[, full := str_replace_all(full, " ", "_")] %>%
        # the US data is named as "National" in the download sites
        .[abbr == "US", full := "National"]

    full <- state_names[abbr == state, full]
    url <- paste0(
        "https://www2.census.gov/census_2010/04-Summary_File_1/Urban_Rural_Update/",
        full, "/", tolower(state), "2010.ur1.zip"
    )

    save_as <- paste0(path_to_census, tolower(state), ".zip")
    download.file(url, save_as, method = "auto")

    # unzip downloaded file
    cat(paste0("Unzipping downloaded zip file of ", state, "\n"))
    unzip(save_as, exdir = paste0(path_to_census, "census", year, "/", state))
    cat("File unzipped successfully\n")

    # delete downloaded file to save space
    cat("Delete downloaded zip file\n\n")
    file.remove(save_as)
}



download_acs5year_ <- function(year, states){
    states <- toupper(states)
    # temp folder to hold all downloaded data
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    path_to_acs5year <- paste0(
        path_to_census, "acs5year/", year
    )

    if (!dir.exists(path_to_acs5year)){
        dir.create(path_to_acs5year)
    }

    i <- 0
    N <- length(states)
    for (st in states){
        i <- i + 1
        cat(paste0("Downloading ", i, " of ", N, " states.\n"))
        cat(paste0("Downloading ", st, " summary files of ACS 5-year ending at ", year, ".\n"))
        if (file.exists(paste0(path_to_acs5year, "/download_record.csv"))){
            record <- fread(paste0(path_to_acs5year, "/download_record.csv"))

            finished_states <- record[, state]

            if (st %in% finished_states){
                message(paste0(st, " data has already been downloaded and extracted.\n"))
            } else {
                download_acs5year_1_state_(year, st)
                record <- rbindlist(list(record, data.table(state = st)))
                fwrite(record, file = paste0(path_to_acs5year, "/download_record.csv"))
            }
        } else {
            download_acs5year_1_state_(year, st)
            record <- data.table(state = st)
            fwrite(record, file = paste0(path_to_acs5year, "/download_record.csv"))
        }
    }
}



download_acs5year_1_state_ <- function(year, state){
    # download site at https://www2.census.gov/programs-surveys/acs/summary_file/
    #
    # Args_____
    # year : year of the survey
    # state : abbreviation of the state

    state <- toupper(state)

    # temp folder to hold all downloaded data
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")
    path_to_acs5year <- paste0(
        path_to_census, "acs5year/", year
    )


    # construct right names for url
    state_names <- dict_fips[, .(full = state_full, abbr = state_abbr)] %>%
        unique() %>%
        .[, full := str_replace_all(full, " ", "")] %>%
        .[abbr == "DC", full := "DistrictOfColumbia"]

    full <- state_names[abbr == state, full]

    # all geography not tracts and block groups
    url_1 <- paste0(
        "https://www2.census.gov/programs-surveys/acs/summary_file/",
        year, "/data/", "5_year_by_state/", full,
        "_All_Geographies_Not_Tracts_Block_Groups.zip"
    )

    # tracts and block groups only
    url_2 <- paste0(
        "https://www2.census.gov/programs-surveys/acs/summary_file/",
        year, "/data/", "5_year_by_state/", full,
        "_Tracts_Block_Groups_Only.zip"
    )

    for (i in c(1, 2)){
        url <- get(paste0("url_", i))
        save_as <- paste0(path_to_census, tolower(state), ".zip")

        cat(paste0("Downloading group ", i, " file of ", state, "\n"))
        download.file(url, save_as, method = "auto")

        # unzip downloaded file
        cat(paste0("Unzipping downloaded zip file of ", state, "\n"))
        unzip(save_as, exdir = paste0(path_to_acs5year, "/group", i))
        cat("File unzipped successfully\n")

        # delete downloaded file to save space
        cat("Delete downloaded zip file\n\n")
        file.remove(save_as)

        # move geography files out of group 1 and 2
        for (ext in c(".csv", ".txt")){
            from <- paste0(path_to_acs5year, "/group", i, "/g", year, "5",
                           tolower(state), ext)
            to <- paste0(path_to_acs5year, "/g", year, "5",
                         tolower(state), ext)
            file.rename(from, to)
        }
    }
}


download_acs1year_ <- function(year){
    # download all states' data which is not that big
    path_to_census <- "~/Downloads/tmp/" #Sys.getenv("PATH_TO_CENSUS")
    path_to_acs1year <- paste0(
        path_to_census, "acs1year/", year
    )

    cat(paste0("Downloading ", year, " acs 1-year file \n"))

    if (dir.exists(path_to_acs1year)){
        message("You already have acs 1-year data of ", year, ".\n")
    } else {
        url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/", year,
            "/data/1_year_entire_sf/All_Geographies.zip"
        )

        save_as <- paste0(path_to_census, "/acs1year", ".zip")

        download.file(url, save_as, method = "auto")

        # unzip downloaded file
        cat(paste0("Unzipping downloaded zip file of acs 1-year of", year, "\n"))
        unzip(save_as, exdir = path_to_acs1year)
        cat("File unzipped successfully\n")

        # delete downloaded file to save space
        cat("Delete downloaded zip file\n")
        file.remove(save_as)
    }
}
