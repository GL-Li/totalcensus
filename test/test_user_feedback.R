library(totalcensus)
library(data.table)
library(magrittr)

home_value <- read_acs5year(
    year = 2016,
    states = "RI",
    table_contents = "home_value = B25077_001",
    areas = c("Providence county, RI",
              "washington county, RI"),
    summary_level = "tract"
)
stopifnot(is.numeric(home_value$home_value))


# issue #1 ======
test_vars <- c( "B01001_001", "B19049_001", "B19301_001", "B19001_001", "B19101_001")

dc_test <- read_acs5year(
    year = 2016,
    states = c("VT", "DC"),
    table_contents = test_vars,
    summary_level = "tract"
) %>%
    .[, test_vars, with = FALSE]
dt <-
stopifnot(all(sapply(dc_test, is.numeric)))

# issue #2, run without error ====
aaa <- read_acs5year(
    year = 2016,
    states = "MA",
    table_contents = "B01003_001",
    summary_level = "tract"
)

bbb <- read_acs1year(
    year = 2016,
    states = "MA",
    table_contents = "B01003_001",
    summary_level = "county"
)

ccc <-  read_acs5year(
    year = 2016,
    state = "MA",
    table_contents = c("B01003_001", "B00001_001", "B01002_002"),
    summary_level = "tract"
)

ddd <- read_decennial(
    year = 2010,
    states = "RI",
    table_contents = c("P0010001", "P0020001"),
    summary_level = "county"
)


# issue #3 ====


# issue #4 =====
tables <- "PCT12I"

NY_PCT12I <- read_decennial(
    year = 2010,
    states = "NY",
    table_contents = c("PCT012I001", "PCT012I009")
)
