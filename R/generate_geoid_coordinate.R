#' Generate coordinate for selected GEOIDs in ACS 5-year survey
#'
#' @description It creates a sub directory "generared_data/" under path_to_census
#' if not exsist and a file called geoid_coord.csv. The coordinates are generated
#' from census 2010 summary file 1 (with urban/rural update)
#'
#'
#' @export
#'


# geoid_coord <- read_census2010("~/census_data/", "ri",
#                                c("STATE", "PLACE", "COUNTY", "COUSUB", "TRACT", "BLKGRP",
#                                  "INTPTLON", "INTPTLAT"),
#                                geo_comp = "*")



generate_geoidcoord <- function(){

    path_to_census <- Sys.getenv("PATH_TO_CENSUS")

    # generate all from census 2010 data ===========================================
    geoid_coord <- read_census2010(states_DC,
                                   c("STATE", "PLACE", "COUNTY", "COUSUB", "TRACT", "BLKGRP",
                                     "CD", "SLDU", "SLDL", "INTPTLON", "INTPTLAT"),
                                   geo_comp = "*") %>%
        # only interest in these commona SUMLEV, see link below for the list
        # https://www.census.gov/geo/reference/geoidentifiers.html
        .[SUMLEV %in% c("040", "050", "060", "070", "140", "150", "155", "160",
                        "500", "610", "620")] %>%
        # add GEOID for selected summary level
        .[SUMLEV == "040", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE)] %>%
        .[SUMLEV == "050", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY)] %>%
        .[SUMLEV == "060", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY, COUSUB)] %>%
        # 070 not in use any more, the following combination is not right
        #.[SUMLEV == "070", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY, COUSUB, PLACE)] %>%
        .[SUMLEV == "140", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY, TRACT)] %>%
        .[SUMLEV == "150", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, COUNTY, TRACT, BLKGRP)] %>%
        .[SUMLEV == "155", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, PLACE, COUNTY)] %>%
        .[SUMLEV == "160", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, PLACE)] %>%
        .[SUMLEV == "500", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, CD)] %>%
        .[SUMLEV == "610", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SLDU)] %>%
        .[SUMLEV == "620", GEOID := paste0(SUMLEV, GEOCOMP, "US", STATE, SLDL)] %>%

        .[, .(GEOID, lon = INTPTLON, lat = INTPTLAT)]


    ## The following summary levels have GEOIDs
    # "040" "050" "060" "070" "140" "150" "155" "160" "311" "312" "313" "315" "316"
    # "320" "321" "322" "323" "324" "331" "333" "340" "341" "410" "430" "500" "510"
    # "610" "612" "795" "970" "260" "269" "270" "280" "283" "286" "550" "620" "622"
    # "170" "172" "950" "960" "290" "336" "338" "345" "346" "351" "352" "353" "354"
    # "360" "361" "362" "363" "356" "357" "358" "364" "365" "366" "230"

    # GEOID from acs 5-year survey
    geoid_acs5year <- read_acs5year(states_DC, 2015,
                                    c("NAME", "GEOID")) %>%
        # only interest in these SUMLEV
        .[SUMLEV %in% c("040", "160", "050", "060", "140", "150")] %>%
        .[, .(NAME, GEOID)]

    # combine
    GEOID_coordinate <- geoid_coord[geoid_acs5year, on = .(GEOID)]

    # save to csv
    if(!dir.exists(paste0(path_to_census, "/generated_data"))){
        dir.create(paste0(path_to_census, "/generated_data"))
    }
    file_name <- paste0(path_to_census, "/generated_data/geoid_coord.csv")
    fwrite(GEOID_coordinate, file = file_name)
}
