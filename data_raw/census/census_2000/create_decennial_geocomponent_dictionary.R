# This file create dictionary of geographic component
library(data.table)
library(magrittr)
library(stringr)


# all available geographic component
dict_all_geocomponent_2000 <- fread("data_raw/census/census_2000/geographic_component.txt",
                                    sep = "\n", header = FALSE) %>%
    # first two letters are code and others are description
    .[, .(code = str_sub(V1, 1, 2),
          geo_component = str_sub(V1, 3, nchar(V1)))] %>%
    .[, geo_component := str_trim(geo_component)] %>%
    setkey(code)

save(dict_all_geocomponent_2000, file = "data/dict_all_geocomponent_2000.RData")


# geocomponent used in decennial 2010. This data is created after
# read_decennial() is created.
make_decennial_component <- function(){
    state <- read_decennial(2000, states_DC, geo_comp = "*") %>%
        .[, .(code = unique(GEOCOMP))] %>%
        .[, state_file := "yes"] %>%
        setkey(code)
    us <- read_decennial(2000, "US", geo_comp = "*") %>%
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

    return(state_us)
}

dict_tmp <- make_decennial_component()

dict_decennial_geocomponent_2000 <- dict_all_geocomponent_2000[dict_tmp] %>%
    .[order(code)]

save(dict_decennial_geocomponent_2000, file = "data/dict_decennial_geocomponent_2000.RData")

