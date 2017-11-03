# Read geographic header record file of ONE state

read_2010geoheader_ <- function(state,
                               geo_headers = NULL,
                               show_progress = TRUE) {

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    if (show_progress) {
        cat(paste("Reading", state, "geographic header record file\n"))
    }

    # allow lowercase input for state and geo_headers
    state <- toupper(state)
    geo_headers <- toupper(geo_headers)

    file <- paste0(path_to_census, "/census2010/", state, "/", tolower(state),
                   "geo2010.ur1")
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
            # identify numeric hearder
            if (ref %in% c("INTPTLAT", "INTPTLON", "AREALAND", "AREAWATR",
                           "POP100", "HU100")) {
                # place variable in () to add new columns
                dt[, (ref) := as.numeric(str_sub(
                    geo[, V1],
                    dict_census_geoheader[reference == ref, start],
                    dict_census_geoheader[reference == ref, end]
                ))]
            } else if (ref %in% c("LOGRECNO", "SUMLEV", "GEOCOMP")) {
                message(paste(ref, "is included in return by default.\n"))
            } else {
                dt[, (ref) := str_trim(str_sub(
                    geo[, V1],
                    dict_census_geoheader[reference == ref, start],
                    dict_census_geoheader[reference == ref, end]
                ))]
            }
        }
    }

    setkey(dt, LOGRECNO)

    return(dt)
}



