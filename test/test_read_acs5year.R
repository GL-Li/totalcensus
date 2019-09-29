library(totalcensus)


# read_acs5year_filesegment() ===================================================
aaa <- totalcensus:::read_acs5year_filesegment_(2009, "RI", "0003", 1)
bbb <- totalcensus:::read_acs5year_filesegment_(2009, "RI", "0003", 2)


# read_acs5year_1_file_tablecontents ===========================================
aaa <- totalcensus:::read_acs5year_1_file_tablecontents_(
    year = 2009,
    state = "RI",
    file_seg = "0003",
    group = 1,
    table_contents = c("B08519_022", "B08519_045")
)
bbb <- totalcensus:::read_acs5year_1_file_tablecontents_(
    year = 2009,
    state = "RI",
    file_seg = "0003",
    group = 2,
    table_contents = c("B08519_022", "B08519_045")
)


# read_acs5year_tablecontents_() ==============================================
aaa <- totalcensus:::read_acs5year_tablecontents_(
    2009, "RI", 1, c("B01001A_003", "B24124_024", "B08519_022", "B08519_045")
)
bbb <- totalcensus:::read_acs5year_tablecontents_(
    2009, "RI", 2, c("B01001A_003", "B24124_024", "B08519_022", "B08519_045")
)


# read_acs5year_geoheader_file_() ==============================================
aaa <- totalcensus:::read_acs5year_geoheader_file_(2009, "RI")



# read_acs5year_geo_() =========================================================
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


# read_acs5_geoheaders_() ======================================================
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
    table_contents = c("B01001_002", "B01001_026", "B07410_002"),  # reference only
    geo_headers =  c("STATE", "COUNTY"),
    with_margin = TRUE
)
stopifnot(aaa[7, 6] == 385559)


# read_acs5year_areas_() =======================================================
aaa <- totalcensus:::read_acs5year_areas_(
    year = 2016,
    states = c("UT", "RI"),
    table_contents = c("B01001_002", "B01001_026", "B07410_002"),
    areas = c("Salt Lake City city, UT",
              "Providence city, RI",
              "PLACE = RI19180"),
    summary_level = "block group",
    with_margin = TRUE
)
stopifnot(aaa[3, 6] == 685)


# read_acs5year() ==============================================================
# provide geo_headers
aaa <- read_acs5year(
    year = 2015,
    states = c("UT", "RI"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    geo_headers = c("COUNTY", "CBSA"),
    summary_level = "county",
    with_margin = TRUE,
    dec_fill = "dec2010"
)
stopifnot(aaa[2, 4] == 14940)


aaa <- read_acs5year(
    year = 2015,
    states = c("UT", "RI"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    geo_headers = c("PLACE", "COUSUB"),
    summary_level = "block group",
    with_margin = TRUE
)
stopifnot(aaa[1000, 9] == 358)


aaa <- read_acs5year(
    year = 2016,
    states = "CA",
    table_contents = c("male = B01001_002", "female = B01001_026"),
    geo_headers = "PLACE",
    summary_level = "place"
)
stopifnot(dim(aaa) == c(1522, 12))


# provided areas
aaa <- read_acs5year(
    year = 2017,
    states = c("UT", "RI"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    areas = c("Salt Lake City city, UT",
              "Providence city, RI",
              "PLACE = RI19180"),
    with_margin = TRUE
)
cols <- c("area", "GEOID", "NAME", "PLACE", "population", "population_margin",
          "male", "male_margin", "female", "female_margin", "GEOCOMP",
          "SUMLEV", "state", "STUSAB", "lon", "lat")
stopifnot(names(aaa) == cols)


aaa <- read_acs5year(
    year = 2017,
    states = c("RI", "MA"),
    table_contents = c("male = B01001_002", "female = B01001_026"),
    areas = "Providence metro",
    summary_level = "block group",
    dec_fill = "dec2010"
)
stopifnot(dim(aaa) == c(1205, 13))


aaa <- read_acs5year(
    2017, "RI",
    geo_headers = "PLACE",
    summary_level = "block group"
)


# check all file segments =====================================================
read_segment <- function(year, state, fs, group, est_marg){
    aaa <- totalcensus:::read_acs5year_filesegment_(
        year = year,
        state = state,
        file_seg = fs,
        group = group,
        est_marg = est_marg
    )
}

read_all_segment <- function(year, state = "RI", first_seg = 1, group = 1,
                             est_marg = "e"){
    look <- get(paste0("lookup_acs5year_", year))
    n_fs <<- max(as.integer(look$file_segment))
    for (i in first_seg:n_fs){

        if (i < 10){
            fs <- paste0("000", i)
        } else if (i < 100){
            fs <- paste0("00", i)
        } else {
            fs <- paste0("0", i)
        }

        #print(fs)

        read_segment(year, state, fs, group, est_marg)
    }
}

read_all_segment(2017, "RI", 1, 2, "e")


