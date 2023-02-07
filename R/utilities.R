
get_name_from_census2010 <- function(FIPs, geo_header, in_states){
    # this function is to be called in convert_fips_to_names()
    geoheader <- tolower(geo_header)

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    lst <- list()
    for (st in in_states){
        file <- paste0(path_to_census, "/generated_data/fips_", geoheader, "/",
                       geoheader, "_fips_", st, ".csv")
        lst[[st]] <- fread(file, colClasses = "character")
    }
    fips_geoheader <- rbindlist(lst) %>%
        .[, .(fips = get(geo_header), state, NAME)]

    if (geo_header == "CBSA" && in_states == "US"){
        names <- fips_geoheader[FIPs, on = .(fips)] %>%
            .[, NAME]
    } else {
        names <- fips_geoheader[FIPs, on = .(fips, state)] %>%
            .[, NAME]
    }


    return(names)
}




compress_datatable <- function(dt){
    # If a two-columns data.table dt has repeated element in first column, remove
    # duplication of that column and turn second column into rows of vectors

    # Example_____
    # dt <- data.table(x = c("A", "A", "B", "A"), y = letters[6:9])
    #    x y
    # 1: A f
    # 2: A g
    # 3: B h
    # 4: A i
    # compress_datatable(dt)
    #    x                  y
    # 1: A,  c("f", "g", "i")
    # 2: B                "h"
    #
    # dt2 <- data.table(x = c("A", "A", "A", "A"), y = letters[6:9])
    # compress_datatable(dt2)

    col_1 <- names(dt)[1]
    col_2 <- names(dt)[2]
    res <- dt %>%
        .[, paste(get(col_2), collapse = ","), by = .(get(col_1))] %>%
        .[, (col_1) := get] %>%
        # force to be a list after split, otherwise if there is only one list
        # element after split, it will be treated as vector and only the force
        # element of the vector will be taken.
        .[, (col_2) := list(str_split(V1, ","))] %>%
        .[, ":=" (get = NULL, V1 = NULL)]

    return(res)
}

lookup_tablecontents <- function(table_contents, lookup){
    # From a vector of table_contents find the file segments and table contents
    # for each segment. Used in function read_xxx_tablecontents_()

    # Args_____
    # table_contents: vector of table content references
    # lookup: the database where to find the match
    #
    # Return_____
    # A data.table with two columns, file_seg and table_contents
    #
    # Example_____
    # table_contents = c("P0150008", "P0030001", "P0030003",
    #                   "P0080036", "PCT012G002", "PCT012G181")
    # lookup <- lookup_decennial_2010
    # lookup_tablecontents(table_contents, lookup)
    #    file_seg                          table_contents
    # 1:       03   c("P0030001", "P0030003", "P0080036")
    # 2:       05                              "P0150008"
    # 3:       26           c("PCT012G002", "PCT012G181")

    # try to keep the order of input table_contents
    order_contents <- data.table(
        id = 1:length(table_contents),
        content = table_contents
    )

    file_content <- lookup[reference %in% table_contents,
                           .(file_seg = file_segment,
                             table_contents = reference)] %>%
        # if a table_content appears in multiple file_segs, keep only
        # the first one
        unique(by = "table_contents") %>%
        .[order_contents, on = .(table_contents = content)] %>%
        .[order(id)] %>%
        compress_datatable()

    return(file_content)
}


organize_geoheaders <- function(geo_headers) {
    # convert the argument geo_headers in read_xxx() into a data.table, which
    # has columns of geoheader, code, and state

    # Examples_____
    # geo_headers <- c("place = ut62360",
    #                  "place = 14140",
    #                  "cousub = ri41500",
    #                  "cbsa = *")
    # convert_geoheaders(geo_headers)
    #    geoheader  code state
    # 1:     PLACE 62360    UT
    # 2:     PLACE 14140
    # 3:    COUSUB 41500    RI
    # 4:      CBSA

    geo <- str_replace_all(geo_headers, " ", "") %>%
        toupper()
    res <- data.table(
        geoheader = str_extract(geo, "^[^=]*"),
        code = str_split(str_extract(geo, "[^=]*$"), ",")
    ) %>%
        .[, state := str_extract(code, "^[A-Z]*")] %>%
        .[, code := str_extract(code, "[0-9]*$")] %>%
        .[, name := pmap_chr(list(geoheader, code, state), get_name)]

    return(res)
}


organize_tablecontents <- function(table_contents) {
    # convert the argument table_contents in read_xxx() into a data.table, which
    # has columns of code and name. The names are given by users

    # Examples_____
    # table_contents <- c("aaa = C1234",
    #                  "bbb = B14140",
    #                  "A6432",  # no given name
    #                  "ddd = D222")
    # organize_tablecontents(table_contents)
    #     name   code
    # 1:   aaa  A1234
    # 2:   bbb B14140
    # 3: C6432  C6432
    # 4:   ddd   D222

    tc <- str_replace_all(table_contents, " ", "")
    res <- data.table(
        name = str_extract(tc, "^[^=]*"),
        reference = toupper(str_extract(tc, "[^=]*$"))
    )

    return(res)
}

get_fips <- function(area){
    # return fips code of a given area. Only provide One area

    # get fips code of a county, city or town in a state, or a metro area.
    # If a city or town has both PLACE and COUSUB, return PLACE only
    #
    # Args_____
    # area : for county, city, or town, in the format like "kent county, RI",
    #     "Boston city, MA", "Lincoln town, RI". For metro area, in the format
    #     like "New York metro".
    #
    # Return_____
    # string such as "CBSA = 39300", "PLACE = ma07000" of the area
    #
    # Examples_____
    # get_fips("Lincoln town, RI")
    # get_fips("Providence city, RI")
    # get_fips("Providence metro")
    # get_fips("Kent county, RI")
    # get_fips("Salt Lake City city, UT")
    # get_fips("COUSUB = RI41500")
    # get_fips("CBSA = 39300")

    area <- tolower(area)

    if (str_detect(area, "=")){
        geoheader <- area
        # } else if (str_detect(area, "\\*")){
        #     if (str_detect(area, ",")){
        #         state <- str_extract(area, "[^ ]*$")
        #         geography <- str_extract(area, "[^ ]*,") %>%
        #             str_replace(",", "")
        #     }
        #
        #     if (str_detect(area, "metro")){
        #         stop('Do not allow "* metro", please provide a specific metro.')
        #     }
        #
        #     if (geography == "county"){
        #         geoheader <- dict_fips[tolower(state_abbr) == state &
        #                               SUMLEV == "050"] %>%
        #             .[, .(COUNTY, state_abbr)] %>%
        #             .[, geoheader := paste0("COUNTY = ", state_abbr, COUNTY)] %>%
        #             .[, geoheader]
        #     }
        #
        #     if (geography == "city"){
        #         geoheader <- dict_fips
        #     }

    } else {
        if (str_detect(area, ",")){
            geography <- str_extract(area, "[^ ]*,") %>%
                str_replace(",", "")
        } else {
            geography <- "metro"
            area <- str_replace(area, "metro$", "") %>%
                str_trim()
        }


        if (geography == "metro"){
            geoheader <- get_cbsa(area)
        } else if (geography == "county"){
            state <- str_extract(area, "[^ ]*$")
            name <- str_extract(area, "^[^,]*")
            fips <- dict_fips[tolower(state_abbr) == state &
                                  tolower(NAME) == name &
                                  SUMLEV == "050"] %>%
                .[, COUNTY]
            if (length(fips) == 0){
                stop(paste0("No match found for ", name,
                            ". Please search a name with search_fips()."))
            } else {
                geoheader <- paste0("COUNTY = ", toupper(state), fips)
            }

        } else {
            state <- str_extract(area, "[^ ]*$")
            name = str_extract(area, "^[^,]*")

            if (nchar(state) == 2){
                fips <- dict_fips[tolower(state_abbr) == state]
            } else {
                fips <- dict_fips[tolower(state_full) %like% state]
            }

            fips <- fips[tolower(NAME) == name]

            if (nrow(fips) == 0){
                stop(paste0("No match found for ", area,
                            ". Please search with search_fips() for the name."))
            }

            if (any(fips[, PLACE] != "00000")){
                fips <- fips[PLACE != "00000", PLACE]
                geoheader <- paste0("PLACE = ", toupper(state), fips)
            } else {
                fips <- fips[COUSUB != 0, COUSUB]
                geoheader <- paste0("COUSUB = ", toupper(state), fips)
            }
        }
    }

    return(geoheader)

}


get_cbsa <- function(name){
    name <- tolower(name)
    cbsa <- dict_cbsa[tolower(CBSA_title) %like% name]

    if (nrow(cbsa) == 0){
        stop(paste0("No match found for ", name,
                    ". Please search with search_cbsa() for the right name."))
    } else {
        cbsa <- cbsa[, unique(CBSA)]
        if (length(cbsa) > 1){
            stop(paste0("Two many matches found for ", name,
                        ". Please search with search_cbsa() and provide a unique name."))
        } else {
            return(paste0("CBSA = ", cbsa))
        }
    }
}


get_name <- function(geoheader, fips, state = NULL){
    # get the name of a metro, city, county, or town from its fips code
    #
    # Args_____
    # state : state abbreviation such "IN"
    # geoheader : geographic header such as "CBSA", "COUNTY"
    # fips : fips code
    #
    # Return_____
    # name of the area

    state <- tolower(state)
    geoheader <- toupper(geoheader)

    if (geoheader %in% c("PLACE", "COUSUB")){
        name <- dict_fips[tolower(state_abbr) == state, c(geoheader, "NAME"), with = FALSE] %>%
            .[get(geoheader) == fips, NAME] %>%
            paste0(", ", toupper(state))
    }

    if (geoheader == "COUNTY"){
        name = dict_fips[tolower(state_abbr) == state & SUMLEV == "050"] %>%
            .[COUNTY == fips, NAME] %>%
            paste0(", ", toupper(state))
    }

    if (geoheader == "CBSA"){
        name <- dict_cbsa[CBSA == fips, CBSA_title] %>%
            unique() %>%
            str_extract("^[^-|,]*") %>%
            paste0(" metro")
    }

    return(name)
}



convert_areas <- function(areas) {
    # convert the argument areas in read_xxx() into a data.table, which
    # has columns of geoheader, code, and state.
    # The area is given by name or fips code and is coverted into a data.table
    # that has four columnes: geoheader, code, state, and name

    # Examples_____
    # areas <- c("PLACE = UT62360",
    #            "COUNTY = RI005",
    #            "COUSUB = RI41500",
    #            "CBSA = 39300",
    #            "Salt Lake City city, UT",
    #            "Bristol town, RI",
    #            "Salt Lake metro",
    #            "Kent county, RI")
    # convert_areas(areas)
    #    geoheader  code state
    #    geoheader  code state                    name
    # 1:     PLACE 62360    UT     Providence city, UT
    # 2:    COUNTY   005    RI      Newport County, RI
    # 3:    COUSUB 41500    RI        Lincoln town, RI
    # 4:      CBSA 39300              Providence metro
    # 5:     PLACE 67000    UT Salt Lake City city, UT
    # 6:    COUSUB 09280    RI        Bristol town, RI
    # 7:      CBSA 41620          Salt Lake City metro
    # 8:    COUNTY   003    RI         Kent County, RI


    # first convert all element in areas to format "geoheader = code"
    result <- map_chr(areas, get_fips) %>%
        organize_geoheaders()

    return(result)
}




add_geoheader <- function(dt, state, geo_headers, summary_level,
                          survey = "acs"){
    # Add codes of "PLACE" or "COUSUB" to data.table dt when summary level is
    # tract or block group.
    #
    # A tract or block group does not exclusively belong to a PLACE or COUSUB.
    # So in summary 1, the code of PALCE and COUSUB is not provided for them.
    # This function is to add the code using data from Census 2010. If a tract
    # or block group belongs to multiple PLACE or COUSUB, add additional rows
    # to shows the relationship.
    #
    # Args_____
    # dt : the data.table read from read_xxxx() functions.
    # state : state of the data.
    # geo_headers : argument of geo_headers in read_xxxx() functions.
    # summary_level : "140" for tract or "150" for block group.
    # survey : survey of dt, choose from "acs" and "decennial"


    # replace PLACE and COUSUB with those obtained from census 2010 with
    # generate_geoid_coordinate.R
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    if (survey == "acs"){

        if (summary_level %in% c("150", "*")){
            if ("PLACE" %in% geo_headers){
                file <- paste0(
                    path_to_census,
                    "/generated_data/blkgrp_geoid_place/blkgrp_geoid_place_",
                    state, ".csv"
                )
                blkgrp <- fread(file, colClasses = "character") %>%
                    .[, .(GEOID, PLACE_tmp = PLACE)]
                dt <- blkgrp[dt, on = .(GEOID), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "150", PLACE := PLACE_tmp] %>%
                    .[, PLACE_tmp := NULL]
            }

            if ("COUSUB" %in% geo_headers){
                file <- paste0(
                    path_to_census,
                    "/generated_data/blkgrp_geoid_cousub/blkgrp_geoid_cousub_",
                    state, ".csv"
                )
                blkgrp <- fread(file, colClasses = "character") %>%
                    .[, .(GEOID, COUSUB_tmp = COUSUB)]
                dt <- blkgrp[dt, on = "GEOID", allow.cartesian=TRUE] %>%
                    .[SUMLEV == "150", COUSUB := COUSUB_tmp] %>%
                    .[, COUSUB_tmp := NULL]
            }
        }
        if (summary_level %in% c("140", "*")){
            if ("PLACE" %in% geo_headers){
                file <- paste0(
                    path_to_census,
                    "/generated_data/tract_geoid_place/tract_geoid_place_",
                    state, ".csv"
                )
                tract <- fread(file, colClasses = "character") %>%
                    .[, .(GEOID, PLACE_tmp = PLACE)]
                dt <- tract[dt, on = .(GEOID), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "140", PLACE := PLACE_tmp] %>%
                    .[, PLACE_tmp := NULL]
            }

            if ("COUSUB" %in% geo_headers){
                file <- paste0(
                    path_to_census,
                    "/generated_data/tract_geoid_cousub/tract_geoid_cousub_",
                    state, ".csv"
                )
                tract <- fread(file, colClasses = "character") %>%
                    .[, .(GEOID, COUSUB_tmp = COUSUB)]
                dt <- tract[dt, on = .(GEOID), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "140", COUSUB := COUSUB_tmp] %>%
                    .[, COUSUB_tmp := NULL]
            }
        }
    }

    if (survey == "decennial"){
        if (summary_level %in% c("150", "*")){
            if ("PLACE" %in% geo_headers){
                file <- paste0(
                    path_to_census,
                    "/generated_data/blkgrp_geoid_place/blkgrp_geoid_place_",
                    state, ".csv"
                )
                blkgrp <- fread(file, colClasses = "character") %>%
                    .[, .(LOGRECNO, PLACE_tmp = PLACE)] %>%
                    .[, LOGRECNO := as.numeric(LOGRECNO)]
                dt <- blkgrp[dt, on = .(LOGRECNO), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "150", PLACE := PLACE_tmp] %>%
                    .[, PLACE_tmp := NULL]
            }

            if ("COUSUB" %in% geo_headers){
                file <- paste0(
                    path_to_census,
                    "/generated_data/blkgrp_geoid_cousub/blkgrp_geoid_cousub_",
                    state, ".csv"
                )
                blkgrp <- fread(file, colClasses = "character") %>%
                    .[, .(LOGRECNO, COUSUB_tmp = COUSUB)] %>%
                    .[, LOGRECNO := as.numeric(LOGRECNO)]
                dt <- blkgrp[dt, on = .(LOGRECNO), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "150", COUSUB := COUSUB_tmp] %>%
                    .[, COUSUB_tmp := NULL]
            }
        }

        if (summary_level %in% c("140", "*")){
            if ("PLACE" %in% geo_headers){
                file <- paste0(
                    path_to_census,
                    "/generated_data/tract_geoid_place/tract_geoid_place_",
                    state, ".csv"
                )
                tract <- fread(file, colClasses = "character") %>%
                    .[, .(LOGRECNO, PLACE_tmp = PLACE)] %>%
                    .[, LOGRECNO := as.numeric(LOGRECNO)]
                dt <- tract[dt, on = .(LOGRECNO), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "140", PLACE := PLACE_tmp] %>%
                    .[, PLACE_tmp := NULL]
            }

            if ("COUSUB" %in% geo_headers){
                file <- paste0(
                    path_to_census,
                    "/generated_data/tract_geoid_cousub/tract_geoid_cousub_",
                    state, ".csv"
                )
                tract <- fread(file, colClasses = "character") %>%
                    .[, .(LOGRECNO, COUSUB_tmp = COUSUB)] %>%
                    .[, LOGRECNO := as.numeric(LOGRECNO)]
                dt <- tract[dt, on = .(LOGRECNO), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "140", COUSUB := COUSUB_tmp] %>%
                    .[, COUSUB_tmp := NULL]
            }
        }
    }

    return(dt)
}




add_coord <- function(dt, state, geo_headers = NULL){
    # Add coordinates to dt read from ACS 1-year and ACS 5-year surveys.
    #
    # The summary file of ACS 1-year or 5-year surveys does not have
    # (lon, lat) of geographic area. In addition, it also has missing code of
    # geo_headers. This function adds coordinates and codes to dt using data
    # generated from Census 2010 summary file 1 based on GEOID. The data.table
    # dt can be any data that conitains a GEOID column.
    #
    # Args_____
    # dt : the data.table read from decennial census or ACS 1year and 5-year
    #     survey.
    # state : state of the data. The generated data are split by state in order
    #     save reading time.
    # geo_headers : argument of geo_headers in read_xxxx() functions.

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    file <- paste0(path_to_census, "/generated_data/geoid_coord/geoid_coord_",
                   state, ".csv")
    coord <- fread(file,
                   select = c("GEOID", "lon", "lat", geo_headers),
                   colClasses = "character") %>%
        .[, lon := as.numeric(lon)] %>%
        .[, lat := as.numeric(lat)]

    dt <- coord[dt, on = .(GEOID)]

    return(dt)
}



switch_summarylevel <- function(summary_level, year = NULL){
    # This function switch summary level from plain text to code
    common_level <- c("state", "county", "county subdivision", "place",
                      "tract", "block group", "block")

    if (summary_level %in% common_level){
        if (!is.null(year)){
            if (year == 2000){
                block_code <- "101"
            } else if (year == 2010){
                block_code <- "100"
            }
        }

        summary_level <- switch(summary_level,
                                "state" = "040",
                                "county" = "050",
                                "county subdivision" = "060",
                                "place" = "160",
                                "tract" = "140",
                                "block group" = "150",
                                "block" = block_code)
    }

    return(summary_level)
}


switch_geocomp <- function(geo_comp){
    # Switch only common geocomponent, leave others alone
    common_geo <- c("total", "urban", "urbanized area", "urban cluster", "rural")
    if (geo_comp %in% common_geo){
        geo_comp <- switch(
            geo_comp,
            "total" = "00",
            "urban" = "01",
            "urbanized area" = "04",
            "urban cluster" = "28",
            "rural" = "43"
        )
    }

    return(geo_comp)
}

convert_geocomp_name <- function(dt){
    # convert common geocomp from code to name
    # convert only the following common geocomp
    dt[GEOCOMP == "00", GEOCOMP := "total"] %>%
        .[GEOCOMP == "01", GEOCOMP := "urban"] %>%
        .[GEOCOMP == "04", GEOCOMP := "urbanized area"] %>%
        .[GEOCOMP == "28", GEOCOMP := "urban cluster"] %>%
        .[GEOCOMP == "43", GEOCOMP := "rural"]

    return(dt)
}


select_columns <- function(df, contains){
    # select column names that contain element in a vector contains

    # Args____
    # df: a data frame or data table
    # contains: such as 2010:2013 and c("aaa", "bbb")

    cols <- names(df)
    selected <- str_detect(cols, paste0(contains, collapse = "|"))
    selected_cols <- cols[selected]

    return(selected_cols)
}


# add GEOID ====================================================================
# The following functions help add GEOID to decennial census data, which is not
# provided in the summary file 1.

get_geoheaders_of_summarylevel <- function(summary_level){
    # get geoheaders that required to generate GEOID of a given summary level

    if (summary_level == "010") {
        geoheaders <- c("GEOCOMP")
    } else if (summary_level == "020"){
        geoheaders <- c("GEOCOMP", "REGION")
    } else if (summary_level == "030"){
        geoheaders <- c("GEOCOMP", "DIVISION")
    } else if (summary_level == "040"){
        geoheaders <- c("GEOCOMP", "STATE")
    } else if (summary_level == "050"){
        geoheaders <- c("GEOCOMP", "STATE", "COUNTY")
    } else if (summary_level == "060"){
        geoheaders <- c("GEOCOMP", "STATE", "COUNTY", "COUSUB")
    } else if (summary_level == "070"){
        geoheaders <- c("GEOCOMP", "STATE", "COUNTY", "COUSUB", "PLACE")
    } else if (summary_level == "140"){
        geoheaders <- c("GEOCOMP", "STATE", "COUNTY", "TRACT")
    } else if (summary_level == "150"){
        geoheaders <- c("GEOCOMP", "STATE", "COUNTY", "TRACT", "BLKGRP")
    } else if (summary_level %in% c("100", "101")){
        # first digit of BLOCK is BLKGRP
        geoheaders <- c("GEOCOMP", "STATE", "COUNTY", "TRACT", "BLOCK")
    } else if (summary_level == "155"){
        geoheaders <- c("GEOCOMP", "STATE", "PLACE", "COUNTY")
    } else if (summary_level == "160"){
        geoheaders <- c("GEOCOMP", "STATE", "PLACE")
    } else if (summary_level == "170"){
        geoheaders <- c("GEOCOMP", "STATE", "CONCIT")
    } else if (summary_level == "172"){
        geoheaders <- c("GEOCOMP", "STATE", "CONCIT", "PLACE")
    } else if (summary_level == "230"){
        geoheaders <- c("GEOCOMP", "STATE", "ANRC")
    } else if (summary_level == "250"){
        geoheaders <- c("GEOCOMP", "AIANHH")
    } else if (summary_level == "251"){
        geoheaders <- c("GEOCOMP", "AIANHH", "AITSCE")
    } else if (summary_level == "252"){
        geoheaders <- c("GEOCOMP", "AIANHH", "AIHHTLI")
    } else if (summary_level == "254"){
        # same as 252, one is R one is T
        geoheaders <- c("GEOCOMP", "AIANHH", "AIHHTLI")
    } else if (summary_level == "256"){
        geoheaders <- c("GEOCOMP", "AIANHH", "TTRACT")
    } else if (summary_level == "258"){
        geoheaders <- c("GEOCOMP", "AIANHH", "TTRACT", "TBLKGRP")
    } else if (summary_level == "260"){
        geoheaders <- c("GEOCOMP", "AIANHH", "STATE")
    } else if (summary_level == "269"){
        geoheaders <- c("GEOCOMP", "AIANHH", "STATE", "PLACE")
    } else if (summary_level == "270"){
        geoheaders <- c("GEOCOMP", "AIANHH", "STATE", "COUNTY")
    } else if (summary_level == "280"){
        geoheaders <- c("GEOCOMP", "STATE", "AIANHH")
    } else if (summary_level == "283"){
        geoheaders <- c("GEOCOMP", "STATE", "AIANHH", "AIHHTLI")
    } else if (summary_level == "286"){
        # same as 283
        geoheaders <- c("GEOCOMP", "STATE", "AIANHH", "AIHHTLI")
    } else if (summary_level == "290"){
        geoheaders <- c("GEOCOMP", "AIANHH", "AITSCE", "STATE")
    } else if (summary_level == "291"){
        geoheaders <- c("GEOCOMP", "AIANHH", "AIHHTLI", "TTRACT")
    } else if (summary_level == "292"){
        geoheaders <- c("GEOCOMP", "AIANHH", "AIHHTLI", "TTRACT")
    } else if (summary_level == "293"){
        geoheaders <- c("GEOCOMP", "AIANHH", "AIHHTLI", "TTRACT", "TBLKGRP")
    } else if (summary_level == "294"){
        geoheaders <- c("GEOCOMP", "AIANHH", "AIHHTLI", "TTRACT", "TBLKGRP")
    } else if (summary_level == "310"){
        geoheaders <- c("GEOCOMP", "CBSA")
    } else if (summary_level == "311"){
        geoheaders <- c("GEOCOMP", "CBSA", "STATE")
    } else if (summary_level == "312"){
        geoheaders <- c("GEOCOMP", "CBSA", "STATE", "PLACE")
    } else if (summary_level == "313"){
        geoheaders <- c("GEOCOMP", "CBSA", "STATE", "COUNTY")
    } else if (summary_level == "314"){
        geoheaders <- c("GEOCOMP", "CBSA", "METDIV")
    } else if (summary_level == "315"){
        geoheaders <- c("GEOCOMP", "CBSA", "METDIV", "STATE")
    } else if (summary_level == "316"){
        geoheaders <- c("GEOCOMP", "CBSA", "METDIV", "STATE", "COUNTY")
    } else if (summary_level == "320"){
        geoheaders <- c("GEOCOMP", "STATE", "CBSA")
    } else if (summary_level == "321"){
        geoheaders <- c("GEOCOMP", "STATE", "CBSA", "PLACE")
    } else if (summary_level == "322"){
        geoheaders <- c("GEOCOMP", "STATE", "CBSA", "COUNTY")
    } else if (summary_level == "323"){
        geoheaders <- c("GEOCOMP", "STATE", "CBSA", "METDIV")
    } else if (summary_level == "324"){
        geoheaders <- c("GEOCOMP", "STATE", "CBSA", "METDIV", "COUNTY")
    } else if (summary_level == "330"){
        geoheaders <- c("GEOCOMP", "CSA")
    } else if (summary_level == "331"){
        geoheaders <- c("GEOCOMP", "CSA", "STATE")
    } else if (summary_level == "332"){
        geoheaders <- c("GEOCOMP", "CSA", "CBSA")
    } else if (summary_level == "333"){
        geoheaders <- c("GEOCOMP", "CSA", "CBSA", "STATE")
    } else if (summary_level == "335"){
        geoheaders <- c("GEOCOMP", "CNECTA")
    } else if (summary_level == "336"){
        geoheaders <- c("GEOCOMP", "CNECTA", "STATE")
    } else if (summary_level == "337"){
        geoheaders <- c("GEOCOMP", "CNECTA", "NECTA")
    } else if (summary_level == "338"){
        geoheaders <- c("GEOCOMP", "CNECTA", "NECTA", "STATE")
    } else if (summary_level == "340"){
        geoheaders <- c("GEOCOMP", "STATE", "CSA")
    } else if (summary_level == "341"){
        geoheaders <- c("GEOCOMP", "STATE", "CSA", "CBSA")
    } else if (summary_level == "345"){
        geoheaders <- c("GEOCOMP", "STATE", "CNECTA")
    } else if (summary_level == "346"){
        geoheaders <- c("GEOCOMP", "STATE", "CNECTA", "NECTA")
    } else if (summary_level == "350"){
        geoheaders <- c("GEOCOMP", "NECTA")
    } else if (summary_level == "351"){
        geoheaders <- c("GEOCOMP", "NECTA", "STATE")
    } else if (summary_level == "352"){
        geoheaders <- c("GEOCOMP", "NECTA", "STATE", "PLACE")
    } else if (summary_level == "353"){
        geoheaders <- c("GEOCOMP", "NECTA", "STATE", "COUNTY")
    } else if (summary_level == "354"){
        geoheaders <- c("GEOCOMP", "NECTA", "STATE", "COUNTY", "COUSUB")
    } else if (summary_level == "355"){
        geoheaders <- c("GEOCOMP", "NECTA", "NECTADIV")
    } else if (summary_level == "356"){
        geoheaders <- c("GEOCOMP", "NECTA", "NECTADIV", "STATE")
    } else if (summary_level == "357"){
        geoheaders <- c("GEOCOMP", "NECTA", "NECTADIV", "STATE", "COUNTY")
    } else if (summary_level == "358"){
        geoheaders <- c("GEOCOMP", "NECTA", "NECTADIV", "STATE", "COUNTY", "COUSUB")
    } else if (summary_level == "360"){
        geoheaders <- c("GEOCOMP", "STATE", "NECTA")
    } else if (summary_level == "361"){
        geoheaders <- c("GEOCOMP", "STATE", "NECTA", "PLACE")
    } else if (summary_level == "362"){
        geoheaders <- c("GEOCOMP", "STATE", "NECTA", "COUNTY")
    } else if (summary_level == "363"){
        geoheaders <- c("GEOCOMP", "STATE", "NECTA", "COUNTY", "COUSUB")
    } else if (summary_level == "364"){
        geoheaders <- c("GEOCOMP", "STATE", "NECTA", "NECTADIV")
    } else if (summary_level == "365"){
        geoheaders <- c("GEOCOMP", "STATE", "NECTA", "NECTADIV", "COUNTY")
    } else if (summary_level == "366"){
        geoheaders <- c("GEOCOMP", "STATE", "NECTA", "NECTADIV", "COUNTY", "COUSUB")
    } else if (summary_level == "400"){
        geoheaders <- c("GEOCOMP", "UA")
    } else if (summary_level == "410"){
        geoheaders <- c("GEOCOMP", "UA", "STATE")
    } else if (summary_level == "430"){
        geoheaders <- c("GEOCOMP", "UA", "STATE", "COUNTY")
    } else if (summary_level == "500"){
        geoheaders <- c("GEOCOMP", "STATE", "CD")
    } else if (summary_level == "510"){
        geoheaders <- c("GEOCOMP", "STATE", "CD", "COUNTY")
    } else if (summary_level == "550"){
        geoheaders <- c("GEOCOMP", "STATE", "CD", "AIANHH")
    } else if (summary_level == "610"){
        geoheaders <- c("GEOCOMP", "STATE", "SLDU")
    } else if (summary_level == "612"){
        geoheaders <- c("GEOCOMP", "STATE", "SLDU", "COUNTY")
    } else if (summary_level == "620"){
        geoheaders <- c("GEOCOMP", "STATE", "SLDL")
    } else if (summary_level == "622"){
        geoheaders <- c("GEOCOMP", "STATE", "SLDL", "COUNTY")
    } else if (summary_level == "795"){
        geoheaders <- c("GEOCOMP", "STATE", "PUMA")
    } else if (summary_level == "860"){
        geoheaders <- c("GEOCOMP", "ZCTA5")
    } else if (summary_level == "950"){
        geoheaders <- c("GEOCOMP", "STATE", "SDELM")
    } else if (summary_level == "960"){
        geoheaders <- c("GEOCOMP", "STATE", "SDSEC")
    } else if (summary_level == "970"){
        geoheaders <- c("GEOCOMP", "STATE", "SDUNI")
    }

    return(geoheaders)
}

add_geoid <- function(dt, summary_level){
    # add GEOID to data.table dt that has geoheaders of summary_level
    geoheaders <- get_geoheaders_of_summarylevel(summary_level)
    # add first three element to GEOID
    dt[, GEOID := paste0(summary_level, get("GEOCOMP"), "US")]
    # add all others
    for (gh in geoheaders[2:length(geoheaders)]){
        dt[, GEOID := paste0(GEOID, get(gh))]
    }

    return(dt)
}


# add ACS names to decennial census data ======================================
add_acsname <- function(dt){
    # assign acs_NAME to decennial census data
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    file_name <- paste0(path_to_census,
                        "/generated_data/acs_geoid_name/acs_geoid_name.csv")
    generated <- fread(file_name) %>%
        .[, .(geoid_tmp = GEOID, acs_NAME)]

    # for block in dt, give the tract name from ACS
    dt[, geoid_tmp := GEOID]
    dt[GEOID %like% "10000US", geoid_tmp := paste0("14000US", str_sub(GEOID, 8, 18))]


    dt <- generated[dt, on = .(geoid_tmp)] %>%
        .[, geoid_tmp := NULL]
}
