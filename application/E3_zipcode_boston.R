library(totalcensus)
library(data.table)
library(magrittr)

zip_boston <- read_decennial(
    year = 2010,
    states = "MA",
    geo_headers = c("ZCTA5", "PLACE"),
    summary_level = "block"
) %>%
    # use search_fips("boston", "MA") to find its PLACE code is "07000"
    .[PLACE == "07000", unique(ZCTA5)]


