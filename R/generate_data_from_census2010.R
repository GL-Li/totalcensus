# 1/15/2018: change it to internal function from user function


# The generated data will be compressed and uploaded to AWS S3 for download
# under the generated data folder, run from terminal to compress all folders
# and file into generated_data.zip
#
# $ zip -r generated_census_data.zip ./
#
# upload the zip file to AWS S3 and remember to make it public and make sure the
# the url for the file is
# "https://s3.amazonaws.com/gl-shared-data/generated_census_data.zip"
#


# Generate additional geographic data from Census 2010.
#
# @description Decennial census has the most complete geographic header records,
# which can fill in many missing values in ACS data. This function is to generate
# such datasets. It creates a sub directory "generared_data/" under path_to_census
# if not exsist and store generated data.
#
# @param states vector of abbreviations of states, such as c("MA", "RI")
#
#



generate_census_data_ <- function(states =  c(states_DC, "PR", "US")){

    # By default generate GEOID coordinate for each states, DC, PR and US

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # create a subdirectory to hold all generated data
    if(!dir.exists(paste0(path_to_census, "/generated_data"))){
        dir.create(paste0(path_to_census, "/generated_data"))
        message('A subdirectory "generated_data/" is created under "',
                path_to_census, '" to store the generated data.')
    }


    # all summary levels used in ACS data
    acs_summarylevels <- c("040", "050", "060", "070",
                           "140", "150",  "155", "160", "170", "172",
                           "230", "250", "251", "252", "254", "256", "258", "260", "269",
                           "270", "280", "283", "286", "290", "291", "292", "293", "294",
                           "310", "311", "312", "313", "314", "315", "316", "320", "321",
                           "322", "323", "324", "330", "331", "332", "333", "335", "336",
                           "337", "338", "340", "341", "345", "346", "350", "351", "352",
                           "353", "354", "355", "356", "357", "358", "360", "361", "362",
                           "363", "364", "365", "366",
                           "400", "410", "430",
                           "500", "510", "550",
                           "610", "612", "620", "622",
                           "795", "860", "950", "960", "970")

    # generate all from census 2010 data
    geoheaders <- c("INTPTLON", "INTPTLAT", "STATE", "PLACE", "COUNTY", "COUSUB", "TRACT", "BLKGRP",
                    "BLOCK", "CD", "SLDU", "SLDL", "CBSA", "METDIV", "CSA", "CONCIT",
                    "ANRC", "AIANHH", "AIHHTLI", "AITSCE", "CNECTA", "NECTA",
                    "NECTADIV", "UA", "PUMA", "SDELM", "SDSEC", "SDUNI", "TTRACT",
                    "TBLKGRP", "ZCTA5", "NAME")

    i <- 0
    N <- length(states)
    for (st in states){
        i <- i + 1
        cat(paste("Reading", i, "of", N, "states geography.\n"))

        geo <- totalcensus:::read_decennial_geo_(
                               year = 2010,
                               state = st,
                               geo_headers = geoheaders
                           )

        # generate GEOIDs, coordinates and selected geoheaders for ACS =========

        geoid_coord <- geo %>%
            # only interest in these commona SUMLEV, see link below for the list
            # https://www.census.gov/geo/reference/geoidentifiers.html
            .[SUMLEV %in% acs_summarylevels] %>%
            # add GEOID for each summary levels
            .[SUMLEV == "040", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE)] %>%
            .[SUMLEV == "050", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY)] %>%
            .[SUMLEV == "060", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY, COUSUB)] %>%
            # 070 not in use any more
            .[SUMLEV == "070", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY, COUSUB, PLACE)] %>%
            .[SUMLEV == "140", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY, TRACT)] %>%
            .[SUMLEV == "150", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY, TRACT, BLKGRP)] %>%
            .[SUMLEV == "155", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, PLACE, COUNTY)] %>%
            .[SUMLEV == "160", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, PLACE)] %>%
            .[SUMLEV == "170", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CONCIT)] %>%
            .[SUMLEV == "172", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CONCIT, PLACE)] %>%
            .[SUMLEV == "230", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, ANRC)] %>%
            .[SUMLEV == "250", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH)] %>%
            .[SUMLEV == "251", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, AITSCE)] %>%
            # AIHHTLI takes "R" for level 252 and "T" for 254
            .[SUMLEV == "252", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, AIHHTLI)] %>%
            .[SUMLEV == "254", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, AIHHTLI)] %>%
            .[SUMLEV == "256", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, TTRACT)] %>%
            .[SUMLEV == "258", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, TTRACT, TBLKGRP)] %>%
            .[SUMLEV == "260", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, STATE)] %>%
            .[SUMLEV == "269", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, STATE, PLACE)] %>%
            .[SUMLEV == "270", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, STATE, COUNTY)] %>%
            .[SUMLEV == "280", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, AIANHH)] %>%
            # AIHHTLI takes "R" for level 283 and "T" for 286
            .[SUMLEV == "283", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, AIANHH, AIHHTLI)] %>%
            .[SUMLEV == "286", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, AIANHH, AIHHTLI)] %>%
            .[SUMLEV == "290", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, AITSCE, STATE)] %>%
            .[SUMLEV == "291", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, AIHHTLI, TTRACT)] %>%
            .[SUMLEV == "292", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, AIHHTLI, TTRACT)] %>%
            .[SUMLEV == "293", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, AIHHTLI, TTRACT, TBLKGRP)] %>%
            .[SUMLEV == "294", GEOID := paste0(SUMLEV, GEOCOMP, "US", AIANHH, AIHHTLI, TTRACT, TBLKGRP)] %>%
            .[SUMLEV == "310", GEOID := paste0(SUMLEV, GEOCOMP, "US", CBSA)] %>%
            .[SUMLEV == "311", GEOID := paste0(SUMLEV, GEOCOMP, "US", CBSA, STATE)] %>%
            .[SUMLEV == "312", GEOID := paste0(SUMLEV, GEOCOMP, "US", CBSA, STATE, PLACE)] %>%
            .[SUMLEV == "313", GEOID := paste0(SUMLEV, GEOCOMP, "US", CBSA, STATE, COUNTY)] %>%
            .[SUMLEV == "314", GEOID := paste0(SUMLEV, GEOCOMP, "US", CBSA, METDIV)] %>%
            .[SUMLEV == "315", GEOID := paste0(SUMLEV, GEOCOMP, "US", CBSA, METDIV, STATE)] %>%
            .[SUMLEV == "316", GEOID := paste0(SUMLEV, GEOCOMP, "US", CBSA, METDIV, STATE, COUNTY)] %>%
            .[SUMLEV == "320", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CBSA)] %>%
            .[SUMLEV == "321", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CBSA, PLACE)] %>%
            .[SUMLEV == "322", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CBSA, COUNTY)] %>%
            .[SUMLEV == "323", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CBSA, METDIV)] %>%
            .[SUMLEV == "324", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CBSA, METDIV, COUNTY)] %>%
            .[SUMLEV == "330", GEOID := paste0(SUMLEV, GEOCOMP, "US", CSA)] %>%
            .[SUMLEV == "331", GEOID := paste0(SUMLEV, GEOCOMP, "US", CSA, STATE)] %>%
            .[SUMLEV == "332", GEOID := paste0(SUMLEV, GEOCOMP, "US", CSA, CBSA)] %>%
            .[SUMLEV == "333", GEOID := paste0(SUMLEV, GEOCOMP, "US", CSA, CBSA, STATE)] %>%
            .[SUMLEV == "335", GEOID := paste0(SUMLEV, GEOCOMP, "US", CNECTA)] %>%
            .[SUMLEV == "336", GEOID := paste0(SUMLEV, GEOCOMP, "US", CNECTA, STATE)] %>%
            .[SUMLEV == "337", GEOID := paste0(SUMLEV, GEOCOMP, "US", CNECTA, NECTA)] %>%
            .[SUMLEV == "338", GEOID := paste0(SUMLEV, GEOCOMP, "US", CNECTA, NECTA, STATE)] %>%
            .[SUMLEV == "340", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CSA)] %>%
            .[SUMLEV == "341", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CSA, CBSA)] %>%
            .[SUMLEV == "345", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CNECTA)] %>%
            .[SUMLEV == "346", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CNECTA, NECTA)] %>%
            .[SUMLEV == "350", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA)] %>%
            .[SUMLEV == "351", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA, STATE)] %>%
            .[SUMLEV == "352", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA, STATE, PLACE)] %>%
            .[SUMLEV == "353", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA, STATE, COUNTY)] %>%
            .[SUMLEV == "354", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA, STATE, COUNTY, COUSUB)] %>%
            .[SUMLEV == "355", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA, NECTADIV)] %>%
            .[SUMLEV == "356", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA, NECTADIV, STATE)] %>%
            .[SUMLEV == "357", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA, NECTADIV, STATE, COUNTY)] %>%
            .[SUMLEV == "358", GEOID := paste0(SUMLEV, GEOCOMP, "US", NECTA, NECTADIV, STATE, COUNTY, COUSUB)] %>%
            .[SUMLEV == "360", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, NECTA)] %>%
            .[SUMLEV == "361", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, NECTA, PLACE)] %>%
            .[SUMLEV == "362", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, NECTA, COUNTY)] %>%
            .[SUMLEV == "363", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, NECTA, COUNTY, COUSUB)] %>%
            .[SUMLEV == "364", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, NECTA, NECTADIV)] %>%
            .[SUMLEV == "365", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, NECTA, NECTADIV, COUNTY)] %>%
            .[SUMLEV == "366", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, NECTA, NECTADIV, COUNTY, COUSUB)] %>%
            .[SUMLEV == "400", GEOID := paste0(SUMLEV, GEOCOMP, "US", UA)] %>%
            .[SUMLEV == "410", GEOID := paste0(SUMLEV, GEOCOMP, "US", UA, STATE)] %>%
            .[SUMLEV == "430", GEOID := paste0(SUMLEV, GEOCOMP, "US", UA, STATE, COUNTY)] %>%
            .[SUMLEV == "500", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CD)] %>%
            .[SUMLEV == "510", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CD, COUNTY)] %>%
            .[SUMLEV == "550", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CD, AIANHH)] %>%
            .[SUMLEV == "610", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SLDU)] %>%
            .[SUMLEV == "612", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SLDU, COUNTY)] %>%
            .[SUMLEV == "620", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SLDL)] %>%
            .[SUMLEV == "622", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SLDL, COUNTY)] %>%
            # summary level "795" is not in census 2010
            .[SUMLEV == "795", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, PUMA)] %>%
            .[SUMLEV == "860", GEOID := paste0(SUMLEV, GEOCOMP, "US", ZCTA5)] %>%
            .[SUMLEV == "950", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SDELM)] %>%
            .[SUMLEV == "960", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SDSEC)] %>%
            .[SUMLEV == "970", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SDUNI)] %>%
            setnames(c("INTPTLON", "INTPTLAT"), c("lon", "lat")) %>%
            # .[, LOGRECNO := NULL] %>%
            unique() %>%
            # CBSA of Los Angeles metro changed from 31100 to 31080 in 2013
            .[GEOID == "32000US0631100", GEOID := "32000US0631080"]%>%  # state
            .[GEOID == "31000US31100", GEOID := "31000US31080"]         # national


        # GEOID from acs 5-year estimate, which includes all GEOID in 1-year estimate
        geoid_acs5year <- read_acs5year_geo_(year = 2015, state = st) %>%
            # only interest in these SUMLEV
            .[SUMLEV %in% acs_summarylevels] %>%
            .[, .(NAME, GEOID)]

        # combine and reorder columns
        first4 <- c("GEOID", "lon", "lat", "NAME")
        GEOID_coordinate <- geoid_coord[geoid_acs5year, on = .(GEOID)] %>%
            setcolorder(c(first4, setdiff(names(.), first4)))

        # save to csv

        if(!dir.exists(paste0(path_to_census, "/generated_data/geoid_coord"))){
            dir.create(paste0(path_to_census, "/generated_data/geoid_coord"))
        }
        file_name <- paste0(path_to_census, "/generated_data/geoid_coord/geoid_coord_", st, ".csv")
        fwrite(GEOID_coordinate, file = file_name)



        # determine the PLACE code that a block group (partly) belongs to ======
        # no NA for any geoheaders at summary level "100", filled with "9" - "99999"
        # if not applicable
        blkgrp_geoid_place <- geo[SUMLEV == "100"] %>%
            # generate GEOID of the block group that contains the block,
            # summary level of block group is "150"
            .[, GEOID := paste0("150", GEOCOMP, "US", STATE, COUNTY, TRACT, BLKGRP)] %>%
            .[, .(GEOID, PLACE)] %>%
            # keep all blocks that does not belong to any PLACE
            #.[PLACE != "99999"] %>%
            .[, .(n_blocks = .N), by = .(GEOID, PLACE)] %>%
            # add back LOGRECNO in census 2010
            geoid_coord[, .(LOGRECNO, GEOID)][., on = .(GEOID)]

        if(!dir.exists(paste0(path_to_census, "/generated_data/blkgrp_geoid_place"))){
            dir.create(paste0(path_to_census, "/generated_data/blkgrp_geoid_place"))
        }
        file_name <- paste0(path_to_census, "/generated_data/blkgrp_geoid_place/blkgrp_geoid_place_", st, ".csv")
        fwrite(blkgrp_geoid_place, file = file_name)



        # determine the COUSUB code that a block group (partly) belongs to =====
        blkgrp_geoid_cousub <- geo[SUMLEV == "100"] %>%
            # generate GEOID of the block group that contains the block,
            # sumamry level of block group is "150"
            .[, GEOID := paste0("150", GEOCOMP, "US", STATE, COUNTY, TRACT, BLKGRP)] %>%
            .[, .(GEOID, COUSUB)] %>%
            .[, .(n_blocks = .N), by = .(GEOID, COUSUB)] %>%
            # add back LOGRECNO
            geoid_coord[, .(LOGRECNO, GEOID)][., on = .(GEOID)]

        if(!dir.exists(paste0(path_to_census, "/generated_data/blkgrp_geoid_cousub"))){
            dir.create(paste0(path_to_census, "/generated_data/blkgrp_geoid_cousub"))
        }
        file_name <- paste0(path_to_census, "/generated_data/blkgrp_geoid_cousub/blkgrp_geoid_cousub_", st, ".csv")
        fwrite(blkgrp_geoid_cousub, file = file_name)


        # determine the PLACE code that a tract (partly) belongs to ============
        tract_geoid_place <- geo[SUMLEV == "100"] %>%
            # generate GEOID of the tract that contains the block,
            # summary level of block group is "140"
            .[, GEOID := paste0("140", GEOCOMP, "US", STATE, COUNTY, TRACT)] %>%
            .[, .(GEOID, PLACE)] %>%
            #.[PLACE != "99999"] %>%
            .[, .(n_blocks = .N), by = .(GEOID, PLACE)] %>%
            # add back LOGRECNO
            geoid_coord[, .(LOGRECNO, GEOID)][., on = .(GEOID)]

        if(!dir.exists(paste0(path_to_census, "/generated_data/tract_geoid_place"))){
            dir.create(paste0(path_to_census, "/generated_data/tract_geoid_place"))
        }
        file_name <- paste0(path_to_census,
                            "/generated_data/tract_geoid_place/tract_geoid_place_", st, ".csv")
        fwrite(tract_geoid_place, file = file_name)



        # determine the COUSUB code that a tract (partly) belongs to ===========
        tract_geoid_cousub <- geo[SUMLEV == "100"] %>%
            # generate GEOID of the tract that contains the block,
            # sumamry level of block group is "140"
            .[, GEOID := paste0("140", GEOCOMP, "US", STATE, COUNTY, TRACT)] %>%
            .[, .(GEOID, COUSUB)] %>%
            .[, .(n_blocks = .N), by = .(GEOID, COUSUB)] %>%
            # add back LOGRECNO
            geoid_coord[, .(LOGRECNO, GEOID)][., on = .(GEOID)]

        if(!dir.exists(paste0(path_to_census, "/generated_data/tract_geoid_cousub"))){
            dir.create(paste0(path_to_census, "/generated_data/tract_geoid_cousub"))
        }
        file_name <- paste0(path_to_census,
                            "/generated_data/tract_geoid_cousub/tract_geoid_cousub_", st, ".csv")
        fwrite(tract_geoid_cousub, file = file_name)


        # generate fips for selected geoheaders   ============================================
        for (geoheader in c("CBSA", "PLACE", "COUSUB")){
            generate_fips_(geo_header = geoheader, st = st, geo = geo)
        }

    }
}

generate_fips_ <- function(geo_header, st, geo){
    # this function is to be called by generate_census_data()
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # get summary levels for each geoheader
    sumlev <- switch (geo_header,
        PLACE = "160",
        COUSUB = "060",
        CBSA = ifelse(st == "US", "310", "320")
    )

    fips <- geo[SUMLEV == sumlev & GEOCOMP == "00", .(STATE, get(geo_header), NAME)] %>%
        setnames("V2", geo_header) %>%
        .[, state := convert_fips_to_names(STATE)] %>%
        .[, STATE := NULL]

    geoheader <- tolower(geo_header)
    if(!dir.exists(paste0(path_to_census, "/generated_data/fips_", geoheader))){
        dir.create(paste0(path_to_census, "/generated_data/fips_", geoheader))
    }
    file_name <- paste0(path_to_census,
                        "/generated_data/fips_", geoheader, "/", geoheader, "_fips_", st, ".csv")
    fwrite(fips, file = file_name)
}
