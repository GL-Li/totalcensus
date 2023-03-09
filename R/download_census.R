# package user functions ============================================================

#' download census data
#'
#' @description Download decennial census and ACS 5-year and 1-year data from
#' United States Census bureau. It also download
#' generated data from Census 2010 if not exist.
#'
#' @param survey Which survey to download from, "decennial", "acs5year", ,
#' "acs1year", or "redistricting".
#' @param year year or ending year of the survey
#' @param states vector of abbreviations of states such as c("MA", "RI")
#'
#' @export
#'

download_census <- function(survey, year, states = c(states_DC, "US", "PR")) {

    options(timeout = 7200)

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # to work in Windows, do not end the path of a directory with "/"
    if (!file.exists(paste0(path_to_census, "/generated_data"))) {
        download_generated_data()
    }

    if (survey %in% c("decennial", "dec")) {
        download_decennial_(year, states)
    } else if (survey == "acs5") {
        download_acs5year_(year, states)
    } else if (survey == "acs1") {
        download_acs1year_(year, states)
    } else if (survey == "redistricting") {
        download_redistricting_(year, states)
    } else {
        message('Please select a survey from "dec" (or "decennial"), "acs5", "acs1", or "redistricting".')
    }

    options(timeout = 60)
}


#' Download data generated from Census 2010
#'
#' @description This function downloads data generated from Census 2010
#' from Census 2010.
#'
#' @export
#'

download_generated_data <- function() {
    # get user permission
    cat(paste(
        "Do you want to download data generated from decennial census 2010?",
        "This dataset is necessary for processing all summary files."
    ))
    continue <- switch(
        menu(c("yes", "no")),
        TRUE,
        FALSE
    )
    if (!continue) {
        stop("You choose not to download data.")
    }


    # total number of files expected in "generated_data/"
    total_files <- 426  # last version 424


    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    url <- "https://s3.amazonaws.com/gl-shared-data/generated_census_data_v060.zip"
    download.file(url, paste0(path_to_census, "/tmp.zip"))
    unzip(
        paste0(path_to_census, "/tmp.zip"),
        exdir = paste0(path_to_census, "/generated_data")
    )

    file.remove(paste0(path_to_census, "/tmp.zip"))

    n_files <- length(list.files(
        paste0(path_to_census, "/generated_data"),
        recursive = TRUE
    ))

    if (n_files == total_files) {
        cat("Extraction is successful\n\n")
    } else {
        cat("Last downloading or extraction has problem. Download and extract again.")
        download_generated_data()
    }
}




# internal functions ===========================================================

download_decennial_ <- function(year, states) {

    # states <- toupper(states)
    # path_to_census <- Sys.getenv("PATH_TO_CENSUS")
    #
    # path_to_year <- paste0(path_to_census, "/census", year)
    #
    #
    # if (!dir.exists(path_to_year)) {
    #     dir.create(path_to_year)
    # }

    i <- 0
    N <- length(states)
    for (st in states) {
        i <- i + 1
        cat(paste0("Downloading ", i, " of ", N, " states.\n"))
        cat(paste0("Downloading ", st, " summary files of Census ", year, ".\n"))
        download_decennial_1_state_(year, st)
        # if (file.exists(paste0(path_to_year, "/download_record.csv"))) {
        #     record <- fread(paste0(path_to_year, "/download_record.csv"))
        #
        #     finished_states <- record[, state]
        #
        #     if (st %in% finished_states) {
        #         n_files <- length(list.files(
        #             paste0(path_to_year, "/", toupper(st))
        #         ))
        #
        #         # for unknown reason RI has 54 files after extraction
        #         # force it to 50
        #         if (st == "RI" && n_files == 54) n_files <- 50
        #
        #         if (n_files == total_files) {
        #             message(paste0(st, " data has already been downloaded and extracted.\n"))
        #         } else {
        #             download_decennial_1_state_(year, st)
        #         }
        #
        #     } else {
        #         download_decennial_1_state_(year, st)
        #         record <- rbindlist(list(record, data.table(state = st)))
        #         fwrite(record, file = paste0(path_to_year, "/download_record.csv"))
        #     }
        # } else {
        #     download_decennial_1_state_(year, st)
        #     record <- data.table(state = st)
        #     fwrite(record, file = paste0(path_to_year, "/download_record.csv"))
        # }
    }
}




download_decennial_1_state_ <- function(year, state) {
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

    if (year == 2010) {
        url <- paste0(
            "https://www2.census.gov/census_2010/04-Summary_File_1/Urban_Rural_Update/",
            full, "/", tolower(state), "2010.ur1.zip"
        )

        save_as <- paste0(path_to_census, "/", tolower(state), ".zip")
        download.file(url, save_as, method = "auto")

        # unzip downloaded file
        cat(paste0("Unzipping downloaded zip file of ", state, "\n"))
        unzip(save_as, exdir = paste0(path_to_census, "/census", year, "/", state))
        cat("File unzipped successfully\n")

        # delete downloaded file to save space
        file.remove(save_as)
        cat("Deleted downloaded zip file\n\n")

    } else if (year == 2000) {
        if (tolower(state) == "us") {
            full <- "0Final_National"    # irregular directory
        }

        url_0 <- paste0(
            "https://www2.census.gov/census_2000/datasets/Summary_File_1/",
            full, "/"
        )
        save_as <- paste0(path_to_census, "/", tolower(state), ".zip")

        # there are 39 file segments
        file_segs <- c(paste0("0", 1:9), 10:39)
        for (fs in file_segs) {
            url <- paste0(url_0, tolower(state), "000", fs, "_uf1.zip")
            download.file(url, save_as, method = "auto")
            unzip(save_as, exdir = paste0(path_to_census, "/census", year, "/", state))
            file.remove(save_as)

            # change file name so the numbers in consistent with 2010 file name
            from <- paste0(path_to_census, "/census2000/", toupper(state),
                               "/", tolower(state), "000", fs, ".uf1")
            to <- paste0(path_to_census, "/census2000/", toupper(state),
                             "/", tolower(state), "000", fs, "2000.uf1")
            file.rename(from, to)
        }

        # geoheader file
        download.file(paste0(url_0, tolower(state), "geo_uf1.zip"), save_as, method = "auto")
        unzip(save_as, exdir = paste0(path_to_census, "/census", year, "/", state))
        file.remove(save_as)
        from <- paste0(path_to_census, "/census2000/", toupper(state),
                       "/", tolower(state), "geo.uf1")
        to <- paste0(path_to_census, "/census2000/", toupper(state),
                     "/", tolower(state), "geo2000.uf1")
        file.rename(from, to)

    }

}



download_acs5year_ <- function(year, states) {
    i <- 0
    N <- length(states)
    for (st in states) {
        i <- i + 1
        cat(paste0("Downloading ", i, " of ", N, " states.\n"))
        cat(paste0("Downloading ", st, " summary files of ACS 5-year ending at ", year, ".\n"))
        download_acs5year_1_state_(year, st)
    }
}



download_acs5year_1_state_ <- function(year, state) {
    # download site at https://www2.census.gov/programs-surveys/acs/summary_file/
    #
    # Args_____
    # year : year of the survey
    # state : abbreviation of the state

    state <- toupper(state)

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")
    path_to_year <- paste0(
        path_to_census, "/acs5year/", year
    )


    # construct right names for url
    state_names <- dict_fips[, .(full = state_full, abbr = state_abbr)] %>%
        unique() %>%
        .[, full := str_replace_all(full, " ", "")] %>%
        .[abbr == "DC", full := "DistrictOfColumbia"]

    full <- state_names[abbr == state, full]

    if (year == 2009 & toupper(state) == "DC") {
        full <- "DistrictofColumbia"  # all other years DistrictOfColumbia
    }

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

    # changes in 2021, and hope the same after
    if (year >= 2021) {
        url_1 <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/sequence-based-SF/data/", "5_year_by_state/", full,
            "_All_Geographies_Not_Tracts_Block_Groups.zip"
        )
        url_2 <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/sequence-based-SF/data/", "5_year_by_state/", full,
            "_Tracts_Block_Groups_Only.zip"
        )
    }



    for (i in c(1, 2)) {
        url <- get(paste0("url_", i))
        save_as <- paste0(path_to_census, "/", tolower(state), ".zip")

        cat(paste0("Downloading group ", i, " file of ", state, "\n"))
        download.file(url, save_as, method = "auto")

        # unzip downloaded file
        cat(paste0("Unzipping downloaded zip file of ", state, "\n"))
        unzip(save_as, exdir = paste0(path_to_year, "/group", i))
        cat("File unzipped successfully\n")

        # delete downloaded file to save space
        file.remove(save_as)
        cat("Deleted downloaded zip file\n\n")

        # move geography files out of group 1 and 2
        for (ext in c(".csv", ".txt")) {
            from <- paste0(path_to_year, "/group", i, "/g", year, "5",
                           tolower(state), ext)
            to <- paste0(path_to_year, "/g", year, "5",
                         tolower(state), ext)
            if (file.exists(from)) {
                file.rename(from, to)
            }
        }

        # convert geoheader record file from .txt to .csv if .csv file does
        # not exist
        csv_file <- paste0(path_to_year, "/g", year, "5",
                           tolower(state), ".csv")
        if (!file.exists(csv_file)) {
            txt_file <- paste0(path_to_year, "/g", year, "5",
                               tolower(state), ".txt")
            dt_csv <- convert_geo_txt2csv_acs5year_(txt_file, year)
            fwrite(dt_csv, file = csv_file, col.names = FALSE)
        }
    }
    return(NULL)
}



convert_geo_txt2csv_acs5year_ <- function(txt_file, year) {
    # In some years the acs5year data do not have geoheader record file in .csv
    # format but only .txt format. The txt format have all geo headers in one
    # line and can be split by positions. This function is to convert the txt
    # file into csv file.

    # year: year of the survey. Use different dict_acs_geoheader dataset for
    #    different years

    geo <- fread(txt_file, header = FALSE, sep = "\n", encoding = "Latin-1")

    if (year >= 2011) {
        dict <- dict_acs_geoheader_2011_now
    } else if (year == 2010) {
        dict <- dict_acs_geoheader_2010
    }else if (year == 2009) {
        dict <- dict_acs_geoheader_2009_5year
    }

    for (i in 1:nrow(dict)) {
        ref <- dict[i, reference]
        # avoid duplicated BLANK columns
        if (ref == "BLANK") {
            ref <- paste0(ref, "_", i)
        }
        geo[, (ref) := str_sub(V1, dict[i, start], dict[i, end])]
    }

    # only this is numerical
    geo[, V1 := NULL]
    geo[, LOGRECNO := as.numeric(LOGRECNO)]

    return(geo)
}


download_acs1year_ <- function(year, states) {
    i <- 0
    N <- length(states)
    for (st in states) {
        i <- i + 1
        cat(paste0("Downloading ", i, " of ", N, " states.\n"))
        cat(paste0("Downloading ", st, " summary files of ACS 1-year of ", year, ".\n"))
        download_acs1year_1_state_(year, st)
    }
}


download_acs1year_1_state_ <- function(year, state) {

    # Args_____
    # year : year of the survey
    # state : abbreviation of the state

    state <- toupper(state)

    # save final data to path_to_year
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")
    path_to_year <- paste0(path_to_census, "/acs1year/", year)


    # construct right state names for url
    state_names <- dict_fips[, .(full = state_full, abbr = state_abbr)] %>%
        unique() %>%
        .[, full := str_replace_all(full, " ", "")]

    full <- state_names[abbr == state, full]


    # construct url to data files.
    if (year >= 2010) {
        url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/data/1_year_by_state/", full, "_All_Geographies.zip"
        )
    }

    if (year == 2009) {
        url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/data/1_year_by_state/", full, ".zip"
        )
    }

    if (year %in% 2007:2008) {
        url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/data/1_year/",
            full, "/", "all_", tolower(state), ".zip"
        )
    }

    if (year == 2006) {
        url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/data/",
            full, "/", tolower(state), "_all_", year, ".zip"
        )
    }

    if (year == 2005) {
        if (toupper(state) == "US") {
            full <- "0UnitedStates"    # so irregular
        }
        url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/2005/data/",
            full, "/", "all_", tolower(state), ".zip"
        )
    }


    # download from url
    save_as <- paste0(path_to_census, "/", tolower(state), ".zip")
    download.file(url, save_as, method = "auto")

    # unzip downloaded file
    cat(paste0("Unzipping downloaded zip file of ", state, "\n"))

    unzip(save_as, exdir = path_to_year)


    # 2005 - 2008 file formats are not consistent. treat by each year
    if (year == 2008) {
        for (f in list.files(path_to_year, pattern = "*.zip")) {
            unzip(paste0(path_to_year, "/", f), exdir = path_to_year)
            file.remove(paste0(path_to_year, "/", f))
        }

    } else if (year == 2007) {
        # directory and format change every year !!!!
        directory <- paste0(path_to_year, "/prt03/sumfile/", full)
        for (f in list.files(directory, pattern = "*.zip")) {
            unzip(paste0(directory, "/", f), exdir = path_to_year)
        }

        # move out of the unzipped directory
        zip_dir <- paste0(path_to_year, "/tab4/sumfile/prod/2007/data")
        for (f in list.files(zip_dir, pattern = "*.txt")) {
            file.rename(paste0(zip_dir, "/", f), paste0(path_to_year, "/", f))
        }

        # delete the directory
        unlink(paste0(path_to_year, "/prt03"), recursive = TRUE)
        unlink(paste0(path_to_year, "/tab4"), recursive = TRUE)

        # have to download geography file seperately
        geo_url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/data/1_year/",
            full, "/g", year, "1", tolower(state), ".txt"
        )
        geo_save_as <- paste0(path_to_year, "/g", year, "1", tolower(state), ".txt")
        download.file(geo_url, geo_save_as, method = "auto")

    } else if (year == 2006) {
        # have to download geography file seperately
        geo_url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/data/",
            full, "/g", year, "1", tolower(state), ".txt"
        )
        geo_save_as <- paste0(path_to_year, "/g", year, "1", tolower(state), ".txt")
        download.file(geo_url, geo_save_as, method = "auto")

    } else if (year == 2005) {
        # directory and format change every year !!!!
        if (toupper(state) == "US") {
            full <- "UnitedStates"    # have to change back from www2 site
        }
        directory <- paste0(path_to_year, "/prt03/ftp2/sumfile/", full)
        for (f in list.files(directory, pattern = "*.zip")) {
            unzip(paste0(directory, "/", f), exdir = path_to_year)
        }

        # move out of the unzipped directory
        zip_dir <- paste0(path_to_year, "/tab4/sumfile/prod/data")
        for (f in list.files(zip_dir, pattern = "*")) {
            # in the format like m20061us0143000.txt
            f1 <- str_remove_all(f, "\\.|-") %>%
                str_remove("...$")
            yr <- str_extract(f1, "....$")  # year
            tp <- str_remove(f1, "....$") %>%    # type
                str_extract(".$")
            num <- str_remove(f1, ".....$") %>%   # file number
                str_extract("....$")
            st <- str_extract(f1, "^..")   # state
            new_name <- paste0(tp, yr, "1", st, num, "000.txt")

            file.rename(paste0(zip_dir, "/", f), paste0(path_to_year, "/", new_name))
        }

        # delete the directory
        unlink(paste0(path_to_year, "/prt03"), recursive = TRUE)
        unlink(paste0(path_to_year, "/tab4"), recursive = TRUE)

        # some stupid zip files left in the path_to_year directory, delete!!
        for (f in list.files(path_to_year, pattern = "*.zip")) {
            file.remove(paste0(path_to_year, "/", f))
        }

        # have to download geography file seperately
        if (toupper(state) == "US") {
            full <- "0UnitedStates"
        }
        geo_url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/2005/data/",
            full, "/", tolower(state), "geo.2005-1yr"
        )
        geo_save_as <- paste0(path_to_year, "/g", year, "1", tolower(state), ".txt")
        download.file(geo_url, geo_save_as, method = "auto")

    }


    # 2005 - 2009 only have geoheader record file in txt format.
    # convert geoheader record file from .txt to .csv if .csv file does
    # not exist.
    csv_file <- paste0(path_to_year, "/g", year, "1",
                       tolower(state), ".csv")
    if (!file.exists(csv_file)) {
        txt_file <- paste0(path_to_year, "/g", year, "1",
                           tolower(state), ".txt")
        dt_csv <- convert_geo_txt2csv_acs1year_(txt_file, year)
        fwrite(dt_csv, file = csv_file, col.names = FALSE)
    }

    cat("File unzipped and converted successfully\n")

    # delete downloaded file to save space
    file.remove(save_as)
    cat("Deleted downloaded zip file\n\n")

}


download_redistricting_ <- function(year, states) {

    i <- 0
    N <- length(states)
    for (st in states) {
        i <- i + 1
        cat(paste0("Downloading ", i, " of ", N, " states.\n"))
        cat(paste0("Downloading ", st, " summary files of Census ", year, ".\n"))
        download_redistricting_1_state_(year, st)
    }
}



download_redistricting_1_state_ <- function(year, state) {
    # download census 2020 redistricting data from:
    # https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/
    #
    # Args_____
    # year: census year
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

    if (year == 2020) {
        url <- paste0(
            "https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/",
            full, "/", tolower(state), "2020.pl.zip"
        )

        save_as <- paste0(path_to_census, "/", tolower(state), ".zip")
        download.file(url, save_as, method = "auto")

        # unzip downloaded file
        cat(paste0("Unzipping downloaded zip file of ", state, "\n"))
        unzip(save_as, exdir = paste0(path_to_census, "/redistricting", year, "/", state))
        cat("File unzipped successfully\n")

        # delete downloaded file to save space
        file.remove(save_as)
        cat("Deleted downloaded zip file\n\n")

    } else if (year == 2030) {
        # waiting for 10 years
    }

}










convert_geo_txt2csv_acs1year_ <- function(txt_file, year) {
    # In some years the acs5year data do not have geoheader record file in .csv
    # format but only .txt format. The txt format have all geo headers in one
    # line and can be split by positions. This function is to convert the txt
    # file into csv file.

    # year: year of the survey. Use different dict_acs_geoheader dataset for
    #    different years

    geo <- fread(txt_file, header = FALSE, sep = "\n", encoding = "Latin-1")

    if (year >= 2011) {
        dict <- dict_acs_geoheader_2011_now
    } else if (year == 2010) {
        dict <- dict_acs_geoheader_2010
    }else if (year == 2009) {
        dict <- dict_acs_geoheader_2009_1year
    } else if (year >= 2006 & year <= 2008) {
        dict <- dict_acs_geoheader_2006_2008_1year
    } else if (year == 2005) {
        dict <- dict_acs_geoheader_2005_1year
    }

    for (i in 1:nrow(dict)) {
        ref <- dict[i, reference]
        # avoid duplicated BLANK columns
        if (ref == "BLANK") {
            ref <- paste0(ref, "_", i)
        }
        geo[, (ref) := str_sub(V1, dict[i, start], dict[i, end])]
    }

    # only this is numerical
    geo[, V1 := NULL]
    geo[, LOGRECNO := as.numeric(LOGRECNO)]

    return(geo)
}



