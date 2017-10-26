library(rawcensus)
library(data.table)
library(magrittr)

# generate all from census 2010 data ===========================================
geoid_coord <- read_census2010("~/census_data/", states_DC,
                               c("STATE", "PLACE", "COUNTY", "COUSUB", "TRACT", "BLKGRP",
                                 "INTPTLON", "INTPTLAT"),
                               geo_comp = "00") %>%
    # only interest in these SUMLEV
    .[SUMLEV %in% c("040", "160", "050", "060", "140", "150")] %>%
    # add GEOID for selected summary level
    .[SUMLEV == "040", GEOID := paste0("04000US", STATE)] %>%
    .[SUMLEV == "160", GEOID := paste0("16000US", STATE, PLACE)] %>%
    .[SUMLEV == "050", GEOID := paste0("05000US", STATE, COUNTY)] %>%
    .[SUMLEV == "060", GEOID := paste0("06000US", STATE, COUNTY, COUSUB)] %>%
    .[SUMLEV == "140", GEOID := paste0("14000US", STATE, COUNTY, TRACT)] %>%
    .[SUMLEV == "150", GEOID := paste0("15000US", STATE, COUNTY, TRACT, BLKGRP)] %>%
    .[, .(GEOID, lon = INTPTLON, lat = INTPTLAT)]


## The following summary levels have GEOIDs
# "040" "050" "060" "070" "140" "150" "155" "160" "311" "312" "313" "315" "316"
# "320" "321" "322" "323" "324" "331" "333" "340" "341" "410" "430" "500" "510"
# "610" "612" "795" "970" "260" "269" "270" "280" "283" "286" "550" "620" "622"
# "170" "172" "950" "960" "290" "336" "338" "345" "346" "351" "352" "353" "354"
# "360" "361" "362" "363" "356" "357" "358" "364" "365" "366" "230"

# GEOID from acs 5-year survey
geoid_acs5year <- read_acs5year("~/census_data/", states_DC, 2015,
                                c("NAME", "GEOID"), "B00001_001",
                                summary_level = "*",
                                geo_components = "00") %>%
    # only interest in these SUMLEV
    .[SUMLEV %in% c("040", "160", "050", "060", "140", "150")] %>%
    .[, .(NAME, GEOID)]

# combine
GEOID_coordinate <- geoid_coord[geoid_acs5year, on = .(GEOID)]

# 386 out of 359000 GEOID have no coord. They might be added after census 2010
GEOID_coordinate[is.na(lon)]
