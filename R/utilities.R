convert_fips_to_names <- function(FIPs, geography = "state") {
    # convert fips codes to names of a geographies

    # Args_____
    # FIPs : string vector of fips code such as c("021", "002")
    # geography : string, taking values of "state", "city", "county" or "metro"

    # Return_____
    # vector of state abbreviations such as c("RI", "MA")

    # Examples
    # convert_fips_to_names(c("11", "44"))
    # [1] "DC" "RI"

    # make data.table for later join
    FIPs <- data.table(fips = FIPs)

    if (geography == "state"){
        fips_geo <- dict_fips[SUMLEV == "040", .(state = state_abbr, fips = STATE)]
        names <- fips_geo[FIPs, on = .(fips)] %>%
            .[, state]
    }

    if (geography == "county"){
        NULL
    }

    if (geography == "city"){
        NULL
    }

    if (geography == "metro"){
        NULL
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

    col_1 <- names(dt)[1]
    col_2 <- names(dt)[2]
    res <- dt %>%
        .[, paste(get(col_2), collapse = ","), by = .(get(col_1))] %>%
        .[, (col_1) := get] %>%
        .[, (col_2) := str_split(V1, ",")] %>%
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
    # lookup <- lookup_census_2010
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
