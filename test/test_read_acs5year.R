library(totalcensus)

# read_acs1year_geo_() =========================================================
aaa <- totalcensus:::read_acs5year_geo_(
    year = 2009,
    state = "RI"
)
cols <- c("GEOID", "STUSAB", "NAME", "LOGRECNO", "SUMLEV", "GEOCOMP")
stopifnot(names(aaa) == cols)


aaa = totalcensus:::read_acs5year_geo_(
    2010, "RI", geo_headers =  c("STUSAB", "STATE", "CBSA")
)
stopifnot(dim(aaa) == c(1972, 8))


aaa <- totalcensus:::read_acs5year_geo_(
    year = 2009,
    state = "RI",
    geo_headers = dict_acs_geoheader_2009_5year$reference
)
cols <- c("GEOID", "STUSAB", "NAME", "LOGRECNO", "SUMLEV", "GEOCOMP",
          "FILEID", "US", "REGION", "DIVISION", "STATECE", "STATE", "COUNTY",
          "COUSUB", "PLACE", "TRACT", "BLKGRP", "CONCIT", "AIANHH", "AIANHHFP",
          "AIHHTLI", "AITSCE", "AITS", "ANRC", "CBSA", "CSA", "METDIV",
          "MACC", "MEMI", "NECTA", "CNECTA", "NECTADIV", "UA", "BLANK",
          "CDCURR", "SLDU", "SLDL", "ZCTA5", "SUBMCD", "SDELM", "SDSEC",
          "SDUNI", "UR", "PCI", "PUMA5")
stopifnot(names(aaa) == cols)


aaa <- totalcensus:::read_acs5year_geo_(
    year = 2016,
    state = "DC",
    geo_headers = dict_acs_geoheader_2011_now$reference
)
stopifnot(aaa[8, "PLACE"] == "50000")


aaa <- totalcensus:::read_acs5year_geo_(
    year = 2009,
    state = "US",
    geo_headers = dict_acs_geoheader_2009_5year$reference
)
stopifnot(dim(aaa) == c(7043, 45))


# read_acs1_geoheaders_() ======================================================
aaa <- totalcensus:::read_acs5year_geoheaders_(
    year = 2010,
    states = "RI",
    geo_headers =  c("STATE", "CBSA")
)
cols <- c("GEOID", "NAME", "STUSAB", "STATE", "CBSA", "GEOCOMP", "SUMLEV",
          "lon", "lat")
stopifnot(names(aaa) == cols)


aaa <- totalcensus:::read_acs5year_geoheaders_(
    year = 2016,
    states = "RI",
    table_contents = c("B01001_002", "B01001_026"),  # reference only
    geo_headers =  c("STATE", "COUNTY"),
    with_margin = TRUE
)
stopifnot(aaa[7, 6] == 385559)


# read_acs1year_areas_() =======================================================
aaa <- totalcensus:::read_acs5year_areas_(
    year = 2016,
    states = c("UT", "RI"),
    table_contents = c("B01001_002", "B01001_026"),
    areas = c("Salt Lake City city, UT",
              "Providence city, RI",
              "PLACE = RI19180"),
    summary_level = "place",
    with_margin = TRUE
)
stopifnot(aaa[3, 5] == 39609)


# read_acs1year() ==============================================================
# provide geo_headers
aaa <- read_acs5year(
    year = 2015,
    states = c("UT", "RI"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    geo_headers = c("COUNTY", "CBSA"),
    summary_level = "county",
    with_margin = TRUE
)
stopifnot(dim(aaa) == c(34, 15))


aaa <- read_acs5year(
    year = 2015,
    states = c("UT", "RI"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    geo_headers = c("PLACE", "COUSUB"),
    summary_level = "block group",
    with_margin = TRUE
)
stopifnot(aaa[1000, 10] == 358)


aaa <- read_acs5year(
    year = 2016,
    states = "CA",
    table_contents = c("male = B01001_002", "female = B01001_026"),
    geo_headers = "PLACE",
    summary_level = "place"
)
stopifnot(dim(aaa) == c(1522, 11))


# provided areas
aaa <- read_acs5year(
    year = 2016,
    states = c("UT", "RI"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    areas = c("Salt Lake City city, UT",
              "Providence city, RI",
              "PLACE = RI19180"),
    summary_level = "place",
    with_margin = TRUE
)
cols <- c("area", "GEOID", "NAME", "STUSAB", "population", "population_margin",
          "male", "male_margin", "female", "female_margin", "GEOCOMP",
          "SUMLEV", "lon", "lat")
stopifnot(names(aaa) == cols)


aaa <- read_acs5year(
    year = 2016,
    states = c("UT", "RI"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    areas = c("Salt Lake City city, UT",
              "Providence city, RI",
              "PLACE = RI19180"),
    summary_level = "block group"
)
stopifnot(dim(aaa) == c(370, 11))
