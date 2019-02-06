library(tidycensus)
census_api_key(Sys.getenv("CENSUS_API"))

vars10 <- c("P005003", "P005004", "P005006", "P004003")
# example from ?get_decennial
system.time({
    il <- get_decennial(geography = "county", variables = vars10, year = 2010,
                        summary_var = "P001001", state = "IL")
})


system.time({
    il <- get_decennial(geography = "place", variables = vars10, year = 2010,
                        summary_var = "P001001", state = "IL")
})


system.time({
    RI <- get_decennial(geography = "tract", variables = vars10, year = 2010,
                        summary_var = "P001001", state = "RI", county = "Providence")
})

system.time({
    RI <- get_decennial(geography = "block", variables = vars10, year = 2010,
                        summary_var = "P001001", state = "RI", county = "Providence")
})

system.time({
    for (cou in c("Los Angeles", "Orange", "San Diego")){
        CA <- get_decennial(geography = "tract", variables = vars10, year = 2010,
                            summary_var = "P001001", state = "CA", county = cou)
        assign(cou, CA)
    }

})


vars10 <- c("P0010001")

# specify state and county to get block data
block <- get_decennial(geography = "block", variables = vars10, state = "RI", county = "Providence", year = 2010)

# specify state to get tract data
tract <- get_decennial(geography = "tract", variables = vars10, state = "RI", county = "Providence", year = 2010)

# how to get block data of a city with tidyverse?

# census API does not have data in urban/rural update. Request for tables below
# returns o
urban_rural <- c("P0020002", "P0020003", "P0020004", "P0020005")
pop_ur <- get_decennial(geography = "tract", variables = urban_rural, state = "RI", year = 2010)
