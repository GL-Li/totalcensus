# This tests need downloaded data to run so are not included in building package
library(totalcensus)

test_search_xxx <- function(){
    ### search_geoheaders test, results will be different after adding new data

    geoheader_1 <- search_geoheaders("decennial", 2010, "india", view = FALSE)
    stopifnot(dim(geoheader_1) == c(10, 3))

    geo_header_2 <- search_geoheaders("decennial", 2010, "latitu", view = FALSE)
    stopifnot(dim(geo_header_2) == c(1, 3))


    # search_tablecontents test ====
    tablecontent_1 <- search_tablecontents("decennial", 2010, "federal prison", view = FALSE)
    stopifnot(dim(tablecontent_1) == c(16, 5))

    tablecontent_2 <- search_tablecontents("acs1", 2015, "B02003", view = FALSE)
    stopifnot(dim(tablecontent_2) == c(71, 5))

    # search_summurylevels test ====
    sumlev_1 = search_summarylevels("decennial", keywords = "block", view = FALSE)
    stopifnot(dim(sumlev_1) == c(17, 5))

    sumlev_2 <- search_summarylevels("acs1", keywords = "040", view = FALSE)
    stopifnot(dim(sumlev_2) == c(1, 5))

    # search_geocomponents test ====
    geocomp_1 <- search_geocomponents("decennial", 2010, "urban", view = FALSE)
    stopifnot(dim(geocomp_1) == c(30, 5))

    geocomp_2 <- search_geocomponents("acs5", 2013:2016, "43", view = FALSE)
    stopifnot(dim(geocomp_2) == c(1, 4))

    # search_tables test ====
    table_1 <- search_tables("decennial", 2000, "occupancy", view = FALSE)
    stopifnot(dim(table_1) == c(1, 5))

    table_2 <- search_tables("acs1", 2009:2013, "detailed race", view = FALSE)
    stopifnot(dim(table_2) == c(6, 8))

    # search_fips test ====
    fips_1 <- search_fips("lincoln", "RI", view = FALSE)
    stopifnot(dim(fips_1) == c(1, 9))

    fips_2 <- search_fips("08375", view = FALSE)
    stopifnot(dim(fips_2) == c(2, 9))

    # search_cbsa test ====
    cbsa_1 <- search_cbsa("providence", view = FALSE)
    stopifnot(dim(cbsa_1) == c(6, 12))

    cbsa_2 <- search_cbsa("new york", view = FALSE)
    stopifnot(dim(cbsa_2) == c(66, 12))

    message("=== passed all test for search_xxxx functions ===")
}


test_read_xxx <- function(){
    # read_decennial test ==============================================================
    # read one table and one area from one state
    dec_1 = read_decennial(
        year = 2010,
        states = "US",
        table_contents = c("urban = P0020002", "rural = P0020005"),
        geo_headers = "CBSA",
        summary_level = "county subdivision"
    )
    stopifnot(dim(dec_1) == c(36642, 10))
    stopifnot(sum(is.na(dec_1$area)) == 13737)


    dec_2 = read_decennial(
        year = 2000,
        states = "US",
        table_contents = c("urban = P0020002", "rural = P0020005"),
        geo_headers = "PLACE",
        summary_level = "*"
    )
    stopifnot(dim(dec_2) == c(280318, 10))
    stopifnot(sum(is.na(dec_2$area)) == 173022)


    # read multiple table contents and areas from multiple states
    dec_3 = read_decennial(
        year = 2010,
        states = c("UT", "RI"),
        table_contents = c("urban = P0020002", "rural = P0020005"),
        areas = c(
            "place = ut62360",
            "Providence city, RI",
            "cousub = ri41500",
            "cbsa = 39300"
        ),
        summary_level = "block"
    )
    stopifnot(dim(dec_3) == c(28981, 9))
    stopifnot(sum(dec_3$population) == 1258789)


    # read table contents of all county subdivisions in Providence metro
    library(data.table)
    library(magrittr)
    dec_4 <- read_decennial(
        year = 2010,
        states = "US",
        table_contents = c("urban = P0020002", "rural = P0020005"),
        geo_headers = c("NAME", "CBSA"),
        summary_level = "county subdivision",
        geo_comp = "*"
    ) %>%
        .[CBSA == "39300"]
    stopifnot(dim(dec_4) == c(183, 10))
    stopifnot(sum(dec_4$population) == 3201704)


    # read_acs5year test ==========================================================
    # read data using areas
    acs5_1 <- read_acs5year(
        year = 2015,
        states = c("UT", "RI"),
        table_contents = c(
            "white = B02001_002",
            "black = B02001_003",
            "asian = B02001_005"
        ),
        areas = c(
            "Lincoln town, RI",
            "Salt Lake City city, UT",
            "Salt Lake City metro",
            "Kent county, RI",
            "COUNTY = UT001",
            "PLACE = UT62360"
        ),
        summary_level = "block group",
        with_margin = TRUE
    )
    stopifnot(dim(acs5_1) == c(314, 16))
    stopifnot(sum(acs5_1$population_margin) == 92245)


    # read data using geoheaders
    acs5_2 <- read_acs5year(
        year = 2015,
        states = c("UT", "RI"),
        table_contents = c("male = B01001_002", "female = B01001_026"),
        geo_headers = "PLACE",
        summary_level = "block group"
    )
    stopifnot(dim(acs5_2) == c(3777, 11))


    test_vars <- c( "B01001_001", "B19049_001", "B19301_001", "B19001_001", "B19101_001")

    dc_test <- read_acs5year(
        year = 2016,
        states = c("VT", "DC"),
        table_contents = test_vars, #"B19301_001",
        summary_level = "tract"
    )

    # read_acs1year test ==========================================================
    # read summary data using areas of selected cities
    acs1_1 <- read_acs1year(
        year = 2016,
        states = c("UT", "RI"),
        table_contents = c("male = B01001_002", "female = B01001_026"),
        areas = c("Salt Lake City city, UT",
                  "Providence city, RI",
                  "PLACE = RI19180"),
        summary_level = "place",
        with_margin = TRUE
    )
    stopifnot(dim(acs1_1) == c(3, 14))
    stopifnot(sum(acs1_1$population_margin) == 196)


    # read data using geoheaders - all major counties
    acs1_2 <- read_acs1year(
        year = 2015,
        states = c("UT", "RI"),
        table_contents = c("male = B01001_002", "female = B01001_026"),
        geo_headers = c("COUNTY"),
        summary_level = "county",
        with_margin = TRUE
    )
    stopifnot(dim(acs1_2) == c(10, 14))
    stopifnot(sum(acs1_2$male) == 1760676)


    # national data
    acs1_3 <- read_acs1year(
        year = 2016,
        states = "US",
        table_contents = c("male = B01001_002", "female = B01001_026"),
        geo_headers = "CBSA",
        summary_level = "310",
        with_margin = TRUE
    )
    stopifnot(dim(acs1_3) == c(511, 14))
    stopifnot(sum(acs1_3$female) == 146881362)

    message("=== passed all test for read_xxx functions ===")
}



test_convert_functions <- function(){
    # convert_fips_to_names test =============
    ftn_1 <- convert_fips_to_names(c("11", "44"))
    stopifnot(ftn_1 == c( "DC", "RI"))

    ftn_2 <- convert_fips_to_names(
        FIPs = c("14140", "76030"),
        states = c("RI", "MA"),
        geo_header = "PLACE",
        in_states = c("RI", "MA")
    )
    stopifnot(ftn_2 == c("Central Falls city", "Westfield city"))


    ftn_3 <- convert_fips_to_names(
        FIPs = c("39300", "46740"),
        states = c(NA, NA),
        geo_header = "CBSA",
        in_states = "US"
    )
    stopifnot(ftn_3 == c("Providence-New Bedford-Fall River, RI-MA Metro Area",
                         "Valley, AL Micro Area"))

    ftn_4 <- convert_fips_to_names(c("001", "013"), states = c("RI", "MA"), geo_header = "COUNTY")
    stopifnot(ftn_4 == c( "Bristol County", "Hampden County"))

    message("=== passed all test for convert_xxx functions ===")

}


# . ===========================================================================
# run tests ====================================================================
test_search_xxx()
test_read_xxx()
test_convert_functions()
