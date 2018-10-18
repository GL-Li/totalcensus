library(totalcensus)

# read_acs1year_geo_() =========================================================
aaa <- totalcensus:::read_acs1year_geo_(
    year = 2005,
    state = "RI"
)
cols <- c("GEOID", "STUSAB", "NAME", "LOGRECNO", "SUMLEV", "GEOCOMP")
stopifnot(names(aaa) == cols)


aaa = totalcensus:::read_acs1year_geo_(
    2010, "RI", geo_headers =  c("STUSAB", "STATE", "CBSA")
)
stopifnot(dim(aaa) == c(38, 8))


aaa <- totalcensus:::read_acs1year_geo_(
    year = 2005,
    state = "RI",
    geo_headers = dict_acs_geoheader_2005_1year$reference
)
cols <- c("GEOID", "STUSAB", "NAME", "LOGRECNO", "SUMLEV", "GEOCOMP",
          "FILEID", "US", "REGION", "DIVISION", "STATE", "COUNTY", "COUSUB",
          "PLACE", "AINDN", "CBSA", "METDIV", "CSA", "CNECTA", "NECTA",
          "NECTADIV", "UA", "CD2000", "BST", "PUMA5", "SDELM", "SDSEC",
          "SDUNI", "UR", "MEMI", "MEMIR", "PCI", "CD1990", "FIPSMCD", "FIPSPL")
stopifnot(names(aaa) == cols)


aaa <- totalcensus:::read_acs1year_geo_(
    year = 2017,
    state = "DC",
    geo_headers = dict_acs_geoheader_2011_now$reference
)
stopifnot(aaa[7, "PLACE"] == "50000")


aaa <- totalcensus:::read_acs1year_geo_(
    year = 2009,
    state = "US",
    geo_headers = dict_acs_geoheader_2009_1year$reference
)
stopifnot(dim(aaa) == c(1275, 45))


# read_acs1_geoheaders_() ======================================================
aaa <- totalcensus:::read_acs1year_geoheaders_(
    year = 2010,
    states = "RI",
    geo_headers =  c("STATE", "CBSA")
)
cols <- c("GEOID", "NAME", "STUSAB", "STATE", "CBSA", "GEOCOMP", "SUMLEV",
          "lon", "lat")
stopifnot(names(aaa) == cols)


aaa <- totalcensus:::read_acs1year_geoheaders_(
    year = 2016,
    states = "RI",
    table_contents = c("B01001_002", "B01001_026"),  # reference only
    geo_headers =  c("STATE", "COUNTY")
)
stopifnot(aaa[7, 6] == 386406)


# read_acs1year_areas_() =======================================================
aaa <- totalcensus:::read_acs1year_areas_(
    year = 2016,
    states = c("UT", "RI"),
    table_contents = c("B01001_002", "B01001_026"),
    areas = c("Salt Lake City city, UT",
              "Providence city, RI",
              "PLACE = RI19180"),
    summary_level = "place",
    with_margin = TRUE
)
stopifnot(aaa[3, 5] == 40454)


# read_acs1year() ==============================================================
# provide geo_headers
aaa <- read_acs1year(
    year = 2015,
    states = c("UT", "RI"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    geo_headers = c("COUNTY", "CBSA"),
    summary_level = "county",
    with_margin = TRUE
)
stopifnot(dim(aaa) == c(10, 15))

# provided areas
aaa <- read_acs1year(
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


aaa <- read_acs1year(
    year = 2017,
    states = "CA",
    table_contents = c("male = B01001_002", "female = B01001_026"),
    summary_level = "place"
)
stopifnot(dim(aaa) == c(142, 10))

