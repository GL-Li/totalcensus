# read acs 1-year geography data ===============================================

read_acs1year_geo_ <- function(state,
                               year,
                               geo_headers = NULL,
                               show_progress = TRUE) {

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # allow lowercase input for state and geo_headers
    state <- tolower(state)
    geo_headers <- toupper(geo_headers)

    file <- paste0(path_to_census, "/", "acs1year/", year, "/g", year, "1",
                   tolower(state), ".csv")

    # use "Latin-1" for encoding special spanish latters such as ñ in Cañada
    geo <- fread(file, header = FALSE, encoding = "Latin-1" ,
                 showProgress = show_progress, colClasses = "character") %>%
        setnames(names(.), dict_acs_geoheader$reference) %>%
        .[, c(c("LOGRECNO", "SUMLEV", "GEOCOMP", "GEOID"), geo_headers), with = FALSE] %>%
        .[, LOGRECNO := as.numeric(LOGRECNO)]

    setkey(geo, LOGRECNO)

    return(geo)
}


# read acs 5-year geography data ===============================================

read_acs5year_geo_ <- function(state,
                               year,
                               geo_headers = NULL,
                               show_progress = TRUE) {

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # allow lowercase input for state and geo_headers
    state <- tolower(state)
    geo_headers <- toupper(geo_headers)

    file <- paste0(path_to_census, "/", "acs5year/", year, "/g", year, "5",
                   tolower(state), ".csv")

    # use "Latin-1" for encoding special spanish latters such as ñ in Cañada
    geo <- fread(file, header = FALSE, encoding = "Latin-1" ,
                 showProgress = show_progress, colClasses = "character") %>%
        setnames(names(.), dict_acs_geoheader$reference) %>%
        .[, c(c("LOGRECNO", "SUMLEV", "GEOCOMP", "GEOID"), geo_headers), with = FALSE] %>%
        .[, LOGRECNO := as.numeric(LOGRECNO)]

    setkey(geo, LOGRECNO)

    return(geo)
}

