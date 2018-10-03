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


# geocomponent used in decennial 2010
make_decennial_component <- function(){
    state <- read_decennial(2010, states_DC) %>%
        .[, .(code = unique(GEOCOMP))] %>%
        .[, state_file := "yes"] %>%
        setkey(code)
    us <- read_decennial(2010, "US") %>%
        .[, .(code = unique(GEOCOMP))] %>%
        .[, US_file := "yes"] %>%
        setkey(code)
    state_us <- merge(state, us, all = TRUE) %>%
        .[is.na(state_file), state_file := "-"] %>%
        .[is.na(US_file), US_file := "-"] %>%
        .[code == "total", code := "00"] %>%
        .[code == "urban", code := "01"] %>%
        .[code == "urbanized area", code := "04"] %>%
        .[code == "urban cluster", code := "28"] %>%
        .[code == "rural", code := "43"]
}

dict_decennial_geocomponent <- make_decennial_component() %>%
    dict_all_geocomponent[.] %>%
    .[order(code)]

save(dict_decennial_geocomponent, file = "data/dict_decennial_geocomponent.RData")

