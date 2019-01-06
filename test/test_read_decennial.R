library(totalcensus)

# read_decennial_geo_ =========================================================
aaa <- totalcensus:::read_decennial_geo_(2010, "RI", summary_level = "100")

bbb <- totalcensus:::read_decennial_geo_(2010, "RI")

ccc <- totalcensus:::read_decennial_geo_(
    2010, "RI", summary_level = "060",
    geo_headers = dict_decennial_geoheader_2010$reference
)

# read_decennial_geoheaders_ ==================================================
aaa <- totalcensus:::read_decennial_geoheaders_(
    2010, "US",
    geo_headers = "COUSUB"
)

bbb <- totalcensus:::read_decennial_geoheaders_(
    2010, "US",
    table_contents = c("P0020002", "P0020005"),
    geo_headers = "CBSA",
    summary_level = "place"
)

ccc <- totalcensus:::read_decennial_geoheaders_(
    2010, c("RI", "UT"),
    table_contents = c("P0020002", "P0020005"),
    geo_headers = c("CBSA", "COUSUB"),
    summary_level = "block"
)

ddd <- totalcensus:::read_decennial_geoheaders_(
    2010, "RI",
    table_contents = c("P0020002", "P0020005"),
    geo_headers = c("CBSA", "COUSUB"),
    summary_level = "block group"
)

# read_decennial_areas_ =======================================================
aaa <- totalcensus:::read_decennial_areas_(
    2010, c("UT", "RI"),
    areas = c("place = ut62360",
              "Providence city, RI",
              "cousub = ri41500",
              "cbsa = 39300")
)

bbb <- totalcensus:::read_decennial_areas_(
    2010, c("UT", "RI"),
    areas = c("place = ut62360",
              "Providence city, RI",
              "cousub = ri41500",
              "cbsa = 39300"),
    summary_level = "block"
)

ccc <- totalcensus:::read_decennial_areas_(
    2010, "US",
    areas = c("place = ut62360",
              "Providence city, RI",
              "cousub = ri41500",
              "cbsa = 39300"),
    summary_level = "county subdivision"
)

# read_decennial ==============================================================
aaa = read_decennial(
    year = 2010,
    states = "US",
    table_contents = c("urban = P0020002", "rural = P0020005"),
    geo_headers = "CBSA"
)

bbb = read_decennial(
    year = 2010,
    states = "US",
    table_contents = c("urban = P0020002", "rural = P0020005"),
    geo_headers = c("CBSA", "STATE"),
    summary_level = "county"
)

ccc <- read_decennial(
    2010, "US",
    areas = c("place = ut62360",
              "Providence city, RI",
              "cousub = ri41500",
              "cbsa = 39300"),
    summary_level = "county subdivision"
)
