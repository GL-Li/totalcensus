# package user functions ============================================================

#' download census data
#'
#' @description Download census data from United States Census bureau. By default
#' it downloads summary files and extract summary files of Census 2010, ACS 5-year
#' survey of 2015 and 2016, and ACS 1-year survey of 2016, 2015, and 2014. It also download
#' generated data from Census 2010 if not exist.
#'
#' @param survey Which survey to download from, "decennial", "acs5year", or "acs1year"
#' @param year year or ending year of the survey
#' @param states vector of abbreviations of states such as c("MA", "RI")
#'
#' @export
#'

download_census <- function(survey = NULL, year = NULL, states = c(states_DC, "US", "PR")){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # to work in Windows, do not end the path of a directory with "/"
    if (!file.exists(paste0(path_to_census, "/generated_data"))){
        download_generated_data()
    }

    if (is.null(survey)){
        cat(paste(
            "Do you want to download summary files of decennial census 2010,",
            "2016 ACS 5-year estimate, and 2016 ACS 1-year estimate?",
            "You need 200 GB free disc space. Run download_census() again to resume",
            "downloading in case downloading breaks."
        ))
        continue <- switch(
            menu(c("yes", "no")),
            TRUE,
            FALSE
        )
        if (!continue){
            stop("You choose not to download data.")
        }
        download_decennial_(2010, states)
        download_acs5year_(2016, states)
        download_acs1year_(2016)
    } else {
        if (survey == "decennial"){
            download_decennial_(year, states)
        } else if (survey == "acs5year"){
            download_acs5year_(year, states)
        } else if (survey == "acs1year"){
            download_acs1year_(year)
        } else {
            message('Please select a survey from "decennial", "acs5year", or "acs1year".')
        }
    }
}


#' Download data generated from Census 2010
#'
#' @description This function downloads data generated from Census 2010
#' from Census 2010.
#'
#' @export
#'

download_generated_data <- function(){
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
    if (!continue){
        stop("You choose not to download data.")
    }


    # total number of files expected in "generated_data/"
    total_files <- 424


    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    url <- "https://s3.amazonaws.com/gl-shared-data/generated_census_data.zip"
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

    if (n_files == total_files){
        cat("Extraction is successful\n\n")
    } else {
        cat("Last downloading or extraction has problem. Download and extract again.")
        download_generated_data()
    }
}




# internal functions ===========================================================

download_decennial_ <- function(year, states){

    # total number of files of each states expected right
    # after extraction
    total_files <- switch(
        as.character(year),
        "2010" = 50
    )

    states <- toupper(states)
    # temp folder to hold all downloaded data
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    path_to_decennial <- paste0(path_to_census, "/census", year)


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
                n_files <- length(list.files(
                    paste0(path_to_decennial, "/", toupper(st))
                ))

                # for unknown reason RI has 54 files after extraction
                # force it to 50
                if (st == "RI" && n_files == 54) n_files <- 50

                if (n_files == total_files){
                    message(paste0(st, " data has already been downloaded and extracted.\n"))
                } else {
                    download_decennial_1_state_(year, st)
                }

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

    if (year == 2010){
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

    } else if (year == 2000){
        if (tolower(state) == "us"){
            full <- "0Final_National"    # irregular directory
        }

        url_0 <- paste0(
            "https://www2.census.gov/census_2000/datasets/Summary_File_1/",
            full, "/"
        )
        save_as <- paste0(path_to_census, "/", tolower(state), ".zip")

        # there are 39 file segments
        file_segs <- c(paste0("0", 1:9), 10:39)
        for (fs in file_segs){
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



download_acs5year_ <- function(year, states){

    # # total number of files in group 1 or group 2 of each states expected right
    # # after extraction
    # total_files <- switch(
    #     as.character(year),
    #     "2015" = 490,
    #     "2016" = 490
    # )

    states <- toupper(states)
    # temp folder to hold all downloaded data
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    path_to_acs5 <- paste0(
        path_to_census, "/acs5year"
    )
    path_to_acs5year <- paste0(path_to_acs5, "/", year)

    # one time only create 1 layer of folders so have to do it twice
    if (!dir.exists(path_to_acs5)){
        dir.create(path_to_acs5)
    }
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
                # texas in big trouble as "txt" will be counted as "tx". Add "5"
                # to avoid confusion with other file names too
                st_5 <- paste0("5", tolower(st))
                n_files <- length(list.files(path_to_acs5year, st_5)) +
                    length(list.files(paste0(path_to_acs5year, "/group1"), st_5)) +
                    length(list.files(paste0(path_to_acs5year, "/group2"), st_5))
                if (n_files == total_files){
                    message(paste0(st, " data has already been downloaded and extracted.\n"))
                } else {
                    # download and extract again if anything is wrong
                    download_acs5year_1_state_(year, st)
                }
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
        path_to_census, "/acs5year/", year
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
        save_as <- paste0(path_to_census, "/", tolower(state), ".zip")

        cat(paste0("Downloading group ", i, " file of ", state, "\n"))
        download.file(url, save_as, method = "auto")

        # unzip downloaded file
        cat(paste0("Unzipping downloaded zip file of ", state, "\n"))
        unzip(save_as, exdir = paste0(path_to_acs5year, "/group", i))
        cat("File unzipped successfully\n")

        # delete downloaded file to save space
        file.remove(save_as)
        cat("Deleted downloaded zip file\n\n")

        # move geography files out of group 1 and 2
        for (ext in c(".csv", ".txt")){
            from <- paste0(path_to_acs5year, "/group", i, "/g", year, "5",
                           tolower(state), ext)
            to <- paste0(path_to_acs5year, "/g", year, "5",
                         tolower(state), ext)
            if(file.exists(from)){
                file.rename(from, to)
            }
        }

        # convert geoheader record file from .txt to .csv if .csv file does
        # not exist
        csv_file <- paste0(path_to_acs5year, "/g", year, "5",
                           tolower(state), ".csv")
        if (!file.exists(csv_file)){
            txt_file <- paste0(path_to_acs5year, "/g", year, "5",
                               tolower(state), ".txt")
            dt_csv <- convert_geo_txt2csv_acs5year_(txt_file, year)
            fwrite(dt_csv, file = csv_file, col.names = FALSE)
        }
    }
    return(NULL)
}



convert_geo_txt2csv_acs5year_ <- function(txt_file, year){
    # In some years the acs5year data do not have geoheader record file in .csv
    # format but only .txt format. The txt format have all geo headers in one
    # line and can be split by positions. This function is to convert the txt
    # file into csv file.

    # year: year of the survey. Use different dict_acs_geoheader dataset for
    #    different years

    geo <- fread(txt_file, header = FALSE, sep = "\n", encoding = "Latin-1")

    if (year >= 2011){
        dict <- dict_acs_geoheader
    } else if (year == 2010){
        dict <- dict_acs_geoheader_2010
    }else if (year == 2009){
        dict <- dict_acs_geoheader_2009_5year
    }

    for (i in 1:nrow(dict)){
        ref <- dict[i, reference]
        # avoid duplicated BLANK columns
        if (ref == "BLANK"){
            ref <- paste0(ref, "_", i)
        }
        geo[, (ref) := str_sub(V1, dict[i, start], dict[i, end])]
    }

    # only this is numerical
    geo[, V1 := NULL]
    geo[, LOGRECNO := as.numeric(LOGRECNO)]

    return(geo)
}


download_acs1year_ <- function(year){
    # # total number of files expected in each year's acs 1-year survey
    # # if not, there is a downloading or extraction problem.
    # total_files <- switch(
    #     as.character(year),
    #     '2017' = 18762,
    #     "2016" = 17702,
    #     "2015" = 17596,
    #     "2014" = 17596,
    #     "2010" = 19080
    # )

    # download all states' data which is not that big.
    # However, no single file before 2008
    if (year >= 2010){
        path_to_census <- Sys.getenv("PATH_TO_CENSUS")
        path_to_acs1year <- paste0(
            path_to_census, "/acs1year/", year
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
            cat(paste0("Unzipping downloaded zip file of acs 1-year of ", year, "\n"))
            unzip(save_as, exdir = path_to_acs1year)
            cat("File unzipped successfully\n")

            # # check extraction
            # n_files <- length(list.files(path_to_acs1year))
            # if (n_files == total_files){
            #     cat("File unzipped successfully\n")
            #     status <- "success"
            # } else {
            #     status <- "failure"
            # }

            # delete downloaded file to save space
            file.remove(save_as)
            cat("Deleted downloaded zip file\n")

            #return(invisible(status))
        }
    } else if (year == 2009){
        # 2009 has a single zip file to download but with different name
        # and geoheader file only in .txt format
        path_to_census <- Sys.getenv("PATH_TO_CENSUS")
        path_to_acs1year <- paste0(
            path_to_census, "/acs1year/", year
        )

        cat(paste0("Downloading ", year, " acs 1-year file \n"))

        if (dir.exists(path_to_acs1year)){
            message("You already have acs 1-year data of ", year, ".\n")
        } else {
            url <- paste0(
                "https://www2.census.gov/programs-surveys/acs/summary_file/", year,
                "/data/1_year_entire_sf/20091YRSF.zip"
            )

            save_as <- paste0(path_to_census, "/acs1year", ".zip")

            download.file(url, save_as, method = "auto")

            # unzip downloaded file
            cat(paste0("Unzipping downloaded zip file of acs 1-year of ", year, "\n"))
            unzip(save_as, exdir = path_to_acs1year)
            cat("File unzipped successfully\n")


            for (state in unique(dict_fips$state_abbr)){
                state = tolower(state)
                # convert geoheader record file from .txt to .csv if .csv file does
                # not exist.
                csv_file <- paste0(path_to_acs1year, "/g", year, "1",
                                   tolower(state), ".csv")
                if (!file.exists(csv_file)){
                    txt_file <- paste0(path_to_acs1year, "/g", year, "1",
                                       tolower(state), ".txt")
                    dt_csv <- convert_geo_txt2csv_acs1year_(txt_file, year)
                    fwrite(dt_csv, file = csv_file, col.names = FALSE)
                }

            }

            # # check extraction
            # n_files <- length(list.files(path_to_acs1year))
            # if (n_files == total_files){
            #     cat("File unzipped successfully\n")
            #     status <- "success"
            # } else {
            #     status <- "failure"
            # }

            # delete downloaded file to save space
            file.remove(save_as)
            cat("Deleted downloaded zip file\n")

            #return(invisible(status))
        }
    } else {
        for (st in unique(dict_fips$state_abbr)){
            download_acs1year_1_state_(year, st)
        }
    }

}


download_acs1year_1_state_ <- function(year, state){
    # before 2008 there is no single file that contains all data. We have to
    # download data by state. The downloaded data will be saved in the same
    # manner as other years.

    # Args_____
    # year : year of the survey
    # state : abbreviation of the state

    state <- toupper(state)

    # temp folder to hold all downloaded data
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")
    path_to_year <- paste0(path_to_census, "/acs1year/", year)

    geo_state <- paste0(path_to_year, "/g", year, "1", tolower(state), ".csv")
    if (file.exists(geo_state)){
        cat(paste0("Data of ", state, " already downloaded.\n"))
        return(NULL)
    }


    # construct right names for url
    state_names <- dict_fips[, .(full = state_full, abbr = state_abbr)] %>%
        unique() %>%
        .[, full := str_replace_all(full, " ", "")]

    full <- state_names[abbr == state, full]

    # download data files
    url <- paste0(
        "https://www2.census.gov/programs-surveys/acs/summary_file/",
        year, "/data/1_year/",
        full, "/", "all_", tolower(state), ".zip"
    )

    if (year == 2006){
        url <- paste0(
            "https://www2.census.gov/programs-surveys/acs/summary_file/",
            year, "/data/",
            full, "/", tolower(state), "_all_", year, ".zip"
        )
    }

    save_as <- paste0(path_to_census, "/", tolower(state), ".zip")
    download.file(url, save_as, method = "auto")

    # unzip downloaded file
    cat(paste0("Unzipping downloaded zip file of ", state, "\n"))

    unzip(save_as, exdir = path_to_year)

    if (year == 2008){
        for (f in list.files(path_to_year, pattern = "*.zip")){
            unzip(paste0(path_to_year, "/", f), exdir = path_to_year)
            file.remove(paste0(path_to_year, "/", f))
        }

    } else if (year == 2007){
        # directory and format change every year !!!!
        directory <- paste0(path_to_year, "/prt03/sumfile/", full)
        for (f in list.files(directory, pattern = "*.zip")){
            unzip(paste0(directory, "/", f), exdir = path_to_year)
        }

        # move out of the unzipped directory
        zip_dir <- paste0(path_to_year, "/tab4/sumfile/prod/2007/data")
        for (f in list.files(zip_dir, pattern = "*.txt")){
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
    } else if (year == 2005){

    }


    # convert geoheader record file from .txt to .csv if .csv file does
    # not exist.
    csv_file <- paste0(path_to_year, "/g", year, "1",
                       tolower(state), ".csv")
    if (!file.exists(csv_file)){
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

convert_geo_txt2csv_acs1year_ <- function(txt_file, year){
    # In some years the acs5year data do not have geoheader record file in .csv
    # format but only .txt format. The txt format have all geo headers in one
    # line and can be split by positions. This function is to convert the txt
    # file into csv file.

    # year: year of the survey. Use different dict_acs_geoheader dataset for
    #    different years

    geo <- fread(txt_file, header = FALSE, sep = "\n", encoding = "Latin-1")

    if (year >= 2011){
        dict <- dict_acs_geoheader
    } else if (year == 2010){
        dict <- dict_acs_geoheader_2010
    }else if (year == 2009){
        dict <- dict_acs_geoheader_2009_1year
    } else if (year >= 2006 & year <= 2008){
        dict <- dict_acs_geoheader_2006_2008_1year
    } else if (year == 2005){
        dict <- dict_acs_geoheader_2005_1year
    }

    for (i in 1:nrow(dict)){
        ref <- dict[i, reference]
        # avoid duplicated BLANK columns
        if (ref == "BLANK"){
            ref <- paste0(ref, "_", i)
        }
        geo[, (ref) := str_sub(V1, dict[i, start], dict[i, end])]
    }

    # only this is numerical
    geo[, V1 := NULL]
    geo[, LOGRECNO := as.numeric(LOGRECNO)]

    return(geo)
}

