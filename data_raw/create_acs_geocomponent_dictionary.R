# find the summary level has been used and merge with census summary level

library(data.table)
library(magrittr)
library(purrr)

state_acs5year <- read_acs5year(states_DC, 2015) %>%
    .[, .(code = unique(GEOCOMP))] %>%
    .[, state_acs5 := "yes"] %>%
    setkey(code)

us_acs5year <- read_acs5year("US", 2015) %>%
    .[, .(code = unique(GEOCOMP))] %>%
    .[, US_acs5 := "yes"] %>%
    setkey(code)

state_acs1year <- read_acs1year(states_DC, 2016) %>%
    .[, .(code = unique(GEOCOMP))] %>%
    .[, state_acs1 := "yes"] %>%
    setkey(code)

us_acs1year <- read_acs1year("US", 2016) %>%
    .[, .(code = unique(GEOCOMP))] %>%
    .[, US_acs1 := "yes"] %>%
    setkey(code)

dict_acs_geocomponent <- reduce(list(state_acs1year, us_acs1year,
                                     state_acs5year, us_acs5year),
                                merge, all = TRUE) %>%
    dict_all_geocomponent[, .(code, geo_component)][.] %>%
    .[is.na(state_acs1), state_acs1 := "-"] %>%
    .[is.na(US_acs1), US_acs1 := "-"] %>%
    .[is.na(state_acs5), state_acs5 := "-"] %>%
    .[is.na(US_acs5), US_acs5 := "-"]


save(dict_acs_geocomponent, file = "data/dict_acs_geocomponent.RData")

