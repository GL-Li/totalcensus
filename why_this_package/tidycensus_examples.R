library(tidycensus)
census_api_key("ab664ab627f56ed01df0b97a25f6f473598a7fec")

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
