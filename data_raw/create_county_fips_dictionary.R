library(data.table)
library(magrittr)
library(stringr)

dict_countyfips <- fread("data_raw/county_fips.csv", colClasses = rep("character", 5)) %>%
    .[, fips := paste0(state_fips, "-", county_fips)]


save(dict_countyfips, file = "data/dict_county_fips.RData")
