convert_fips_to_names <- function(FIPs, states = NULL, geo_header = "STATE") {
    # convert fips codes to names of a geographies

    # Args_____
    # FIPs : string vector of fips code such as c("021", "002")
    # geo_header : string, taking values of "STATE", "COUNTY", "PLACE", "COUSUB"
    # or "CBSA"

    # Return_____
    # vector of state abbreviations such as c("RI", "MA")

    # Examples_____
    # totalcensus:::convert_fips_to_names(c("11", "44"))
    # [1] "DC" "RI"
    #
    # totalcensus:::convert_fips_to_names(FIPs <- c("14140", "76030"),
    #                                     states = c("RI", "MA"),
    #                                     geo_header = "PLACE")
    # [1] "Central Falls city" "Westfield city"
    #
    # totalcensus:::convert_fips_to_names(FIPs = c("39300", "46740"),
    #                                     geo_header = "CBSA")
    # [1] "metro: Providence-Warwick, RI-MA" "metro: Valley, AL"
    #

    states <- toupper(states)
    # make data.table for later join
    if (geo_header %in% c("STATE", "CBSA")){
        FIPs <- data.table(fips = FIPs)
    } else {
        FIPs <- data.table(fips = FIPs, state = states)
    }


    if (geo_header == "STATE"){
        fips_geo <- dict_fips[SUMLEV == "040", .(state = state_abbr, fips = STATE)]
        names <- fips_geo[FIPs, on = .(fips)] %>%
            .[, state]
    } else if (geo_header == "COUNTY"){
        fips_geo <- dict_fips[SUMLEV == "050",
                              .(state = state_abbr, county = NAME, fips = COUNTY)]
        names <- fips_geo[FIPs, on = .(fips, state)] %>%
            .[, county]
    } else if (geo_header == "PLACE"){
        message("Names of CDPs are set to NA.")
        fips_geo <- dict_fips[SUMLEV == "162",
                              .(state = state_abbr, place = NAME, fips = PLACE)]
        names <- fips_geo[FIPs, on = .(fips, state)] %>%
            .[, place]
    } else if (geo_header == "COUSUB"){
        fips_geo <- dict_fips[SUMLEV == "061",
                              .(state = state_abbr, cousub = NAME, fips = COUSUB)]
        names <- fips_geo[FIPs, on = .(fips, state)] %>%
            .[, cousub]
    } else if (geo_header == "CBSA"){
        names <- dict_cbsa[FIPs, on = .(CBSA = fips)] %>%
            .[, unique(CBSA_title)] %>%
            paste0("metro: ", .)
    } else if (geo_header == "COUNTY"){
        fips_geo <- dict_fips[SUMLEV == "050",
                              .(state = state_abbr, county = NAME, fips = COUNTY)]
        names <- fips_geo[FIPs, on = .(fips, state)] %>%
            .[, county]
    } else {
        message(paste('This version only provides names of major areas available in datasets',
                      'dict_fips and dict_cbsa for geographic headers',
                      'STATE, COUNTY, PLACE, COUNTY, and CBSA'))
        names <- "To be added"
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

    file_content <- lookup[reference %in% table_contents,
                           .(file_seg = file_segment,
                             table_contents = reference)] %>%
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

# convert area =================================================================
# The area is given by name or fips code and is coverted into a data.table
# that has four columnes: geoheader, code, state, and name

convert_areas <- function(areas) {
    # convert the argument areas in read_xxx() into a data.table, which
    # has columns of geoheader, code, and state

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




add_geoheader <- function(dt, state, geo_headers, summary_level, survey = "acs"){
    # When dt is a tract or block group level data.table obtained by reading
    # decennial census or ACS 5-year survey and the geoheader "PLACE" or "COUSUB"
    # is included in argument geo_headers, the codes
    # for "PLACE" or "OCUSUB" are usually not provided, as a tract or block group
    # may belong to multiple "PLACE" or "COUSUB". This function is to add them
    # using data from Census 2010. If a tract or block group belongs to multiple
    # PLACE or COUSUB, add additional rows to include the relationship.
    #
    # Args_____
    # dt : the data.table read from decennial census or ACS 5-year survey.
    # state : state of the data
    # geo_headers : argument of geo_headers in function read_acs5year() or
    #     read_decenial() that generated dt.
    # summary_level : "140" for tract or "150" for block group.
    # survey : survey of dt, choose from "acs" and "decennial"


    # replace PLACE and COUSUB with those obtained from census 2010 with
    # generate_geoid_coordinate.R
    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    if (survey == "acs"){
        if (summary_level %in% c("150", "*")){
            if ("PLACE" %in% geo_headers){
                file <- paste0(path_to_census,
                               "generated_data/blkgrp_geoid_place/blkgrp_geoid_place_",
                               state, ".csv")
                blkgrp <- fread(file, colClasses = "character") %>%
                    .[, .(GEOID, PLACE_tmp = PLACE)]
                dt <- blkgrp[dt, on = .(GEOID), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "150", PLACE := PLACE_tmp] %>%
                    .[, PLACE_tmp := NULL]
            }

            if ("COUSUB" %in% geo_headers){
                file <- paste0(path_to_census,
                               "generated_data/blkgrp_geoid_cousub/blkgrp_geoid_cousub_",
                               state, ".csv")
                blkgrp <- fread(file, colClasses = "character") %>%
                    .[, .(GEOID, COUSUB_tmp = COUSUB)]
                dt <- blkgrp[dt, on = "GEOID", allow.cartesian=TRUE] %>%
                    .[SUMLEV == "150", COUSUB := COUSUB_tmp] %>%
                    .[, COUSUB_tmp := NULL]
            }
        }
        if (summary_level %in% c("140", "*")){
            if ("PLACE" %in% geo_headers){
                file <- paste0(path_to_census,
                               "generated_data/tract_geoid_place/tract_geoid_place_",
                               state, ".csv")
                tract <- fread(file, colClasses = "character") %>%
                    .[, .(GEOID, PLACE_tmp = PLACE)]
                dt <- tract[dt, on = .(GEOID), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "140", PLACE := PLACE_tmp] %>%
                    .[, PLACE_tmp := NULL]
            }

            if ("COUSUB" %in% geo_headers){
                file <- paste0(path_to_census,
                               "generated_data/tract_geoid_cousub/tract_geoid_cousub_",
                               state, ".csv")
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
                file <- paste0(path_to_census,
                               "generated_data/blkgrp_geoid_place/blkgrp_geoid_place_",
                               state, ".csv")
                blkgrp <- fread(file, colClasses = "character") %>%
                    .[, .(LOGRECNO, PLACE_tmp = PLACE)] %>%
                    .[, LOGRECNO := as.numeric(LOGRECNO)]
                dt <- blkgrp[dt, on = .(LOGRECNO), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "150", PLACE := PLACE_tmp] %>%
                    .[, PLACE_tmp := NULL]
            }

            if ("COUSUB" %in% geo_headers){
                file <- paste0(path_to_census,
                               "generated_data/blkgrp_geoid_cousub/blkgrp_geoid_cousub_",
                               state, ".csv")
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
                file <- paste0(path_to_census,
                               "generated_data/tract_geoid_place/tract_geoid_place_",
                               state, ".csv")
                tract <- fread(file, colClasses = "character") %>%
                    .[, .(LOGRECNO, PLACE_tmp = PLACE)] %>%
                    .[, LOGRECNO := as.numeric(LOGRECNO)]
                dt <- tract[dt, on = .(LOGRECNO), allow.cartesian=TRUE] %>%
                    .[SUMLEV == "140", PLACE := PLACE_tmp] %>%
                    .[, PLACE_tmp := NULL]
            }

            if ("COUSUB" %in% geo_headers){
                file <- paste0(path_to_census,
                               "generated_data/tract_geoid_cousub/tract_geoid_cousub_",
                               state, ".csv")
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



add_coord <- function(dt, state, geo_headers){
    # When dt is read from ACS 1-year or 5-year survey, the coordinates are missing.
    # In addition, the code of geo_headers are not complete. This function add
    # coordinates and code to dt from Census 2010 data.
    #
    # Args_____
    # dt : the data.table read from decennial census or ACS 5-year survey.
    # state : state of the data
    # geo_headers : argument of geo_headers in function read_acs5year() or
    #     read_decenial() that generated dt.
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



switch_summarylevel <- function(summary_level){
    # This function switch summary level from plain text to code
    common_level <- c("state", "county", "county subdivision", "place",
                      "tract", "block group", "block")

    if (summary_level %in% common_level){
        summary_level <- switch(summary_level,
                                "state" = "040",
                                "county" = "050",
                                "county subdivision" = "060",
                                "place" = "160",
                                "tract" = "140",
                                "block group" = "150",
                                "block" = "100")
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
