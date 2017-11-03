# This file create dictionary of geographic component
library(data.table)
library(magrittr)
library(stringr)


# all available geographic component
dict_all_geocomponent <- fread("data_raw/geographic_component", sep = "\n", header = FALSE) %>%
    # first two letters are code and others are description
    .[, .(code = str_sub(V1, 1, 2),
          geo_component = str_sub(V1, 3, nchar(V1)))] %>%
    setkey(code)

save(dict_all_geocomponent, file = "data/dict_all_geocomponent.RData")


# geocomponent used in census 2010
make_census_component <- function(){
    state <- read_census2010(states_DC) %>%
        .[, .(code = unique(GEOCOMP))] %>%
        .[, in_state_file := "yes"] %>%
        setkey(code)
    us <- read_census2010("US") %>%
        .[, .(code = unique(GEOCOMP))] %>%
        .[, in_US_file := "yes"] %>%
        setkey(code)
    state_us <- merge(state, us, all = TRUE) %>%
        .[is.na(in_state_file), in_state_file := "-"] %>%
        .[is.na(in_US_file), in_US_file := "-"]
}

dict_census_geocomponent <- make_census_component() %>%
    dict_all_geocomponent[.]

save(dict_census_geocomponent, file = "data/dict_census_geocomponent.RData")

