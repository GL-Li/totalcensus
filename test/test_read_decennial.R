library(totalcensus)

# read_decennial_geo_ =========================================================
aaa <- totalcensus:::read_decennial_geo_(2010, "RI")


# read_decennial_geoheaders_ ==================================================
aaa <- totalcensus:::read_decennial_geoheaders_(2010, "US")

bbb <- totalcensus:::read_decennial_geoheaders_(
    2010, "US",
    table_contents = c("P0020002", "P0020005"),
    geo_headers = "CBSA"
)



# read_decennial_areas_ =======================================================
aaa <- totalcensus:::read_decennial_areas_(
    2010, c("UT", "RI"),
    areas = c(
        "place = ut62360",
        "Providence city, RI",
        "cousub = ri41500",
        "cbsa = 39300"
    ))


# read_decennial ==============================================================
aaa = read_decennial(
    year = 2010,
    states = "US",
    table_contents = c("urban = P0020002", "rural = P0020005"),
    geo_headers = "CBSA"
)
