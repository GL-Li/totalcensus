# find the geocomponents that have been used and merge with census summary level

# find the summary level that has been used and merge with census summary level

library(data.table)
library(magrittr)
library(purrr)

generate_geocomponent <- function(survey, year){
    if (survey %in% c("dec", "decennial")){
        state <- read_decennial(year, states_DC, geo_comp = "*")
        us <- read_decennial(year, "US", geo_comp = "*")
    } else if (survey == "acs5"){
        state <- read_acs5year(year, states_DC, geo_comp = "*")
        us <- read_acs5year(year, "US", geo_comp = "*")
    } else if (survey == "acs1"){
        state <- read_acs1year(year, states_DC, geo_comp = "*")
        us <- read_acs1year(year, "US", geo_comp = "*")
    }

    state <- state[, .(code = unique(GEOCOMP))] %>%
        .[, (paste0("state_", year)) := "yes"] %>%
        setkey(code)
    us <- us[, .(code = unique(GEOCOMP))] %>%
        .[, (paste0("US_", year)) := "yes"] %>%
        setkey(code)

    if (year <= 2008){
        all_geocomponent <- dict_all_geocomponent_2000
    } else if (year > 2008) {
        all_geocomponent <- dict_all_geocomponent_2010
    }

    dict <- merge(state, us, all = TRUE) %>%
        .[code == "total", code := "00"] %>%
        .[code == "urban", code := "01"] %>%
        .[code == "rural", code := "43"] %>%
        all_geocomponent[., on = .(code)] %>%
        .[order(code)]

    return(dict)
}

# acs1year summary level ======================================================
S2017 <- generate_geocomponent("acs1", 2017)
S2016 <- generate_geocomponent("acs1", 2016)
S2015 <- generate_geocomponent("acs1", 2015)
S2014 <- generate_geocomponent("acs1", 2014)
S2013 <- generate_geocomponent("acs1", 2013)
S2012 <- generate_geocomponent("acs1", 2012)
S2011 <- generate_geocomponent("acs1", 2011)
S2010 <- generate_geocomponent("acs1", 2010)
S2009 <- generate_geocomponent("acs1", 2009)
S2008 <- generate_geocomponent("acs1", 2008)
S2007 <- generate_geocomponent("acs1", 2007)
S2006 <- generate_geocomponent("acs1", 2006)
S2005 <- generate_geocomponent("acs1", 2005)


# same from 2009 to 2017, 2005, 6, 7, 8 are different
dict_acs1_geocomponent <- purrr::reduce(list(S2017, S2008, S2007, S2006, S2005),
                                        merge, by = c("code", "geo_component"),
                                        all = TRUE) %>%
    .[, .(code, geo_component,
          state_2009_to_now = state_2017,
          state_2007_2008 = state_2008,
          state_2005_2006 = state_2006,
          US_2009_to_now = US_2017,
          US_2007_2008 = US_2008,
          US_2006, US_2005)]

save(dict_acs1_geocomponent, file = "data/dict_acs1_geocomponent.RData")


# acs5year summary level ======================================================
S2016 <- generate_geocomponent("acs5", 2016)
S2015 <- generate_geocomponent("acs5", 2015)
S2014 <- generate_geocomponent("acs5", 2014)
S2013 <- generate_geocomponent("acs5", 2013)
S2012 <- generate_geocomponent("acs5", 2012)
S2011 <- generate_geocomponent("acs5", 2011)
S2010 <- generate_geocomponent("acs5", 2010)
S2009 <- generate_geocomponent("acs5", 2009)

dict_acs5_geocomponent <- purrr::reduce(list(S2016, S2012, S2011, S2010, S2009),
                                        merge, by = c("code", "geo_component"),
                                        all = TRUE) %>%
    .[, .(code, geo_component,
          state_2009_to_now = state_2016,
          US_2009_to_now = US_2016)]

save(dict_acs5_geocomponent, file = "data/dict_acs5_geocomponent.RData")



# library(data.table)
# library(magrittr)
# library(purrr)
#
# state_acs5year <- read_acs5year(states_DC, 2015) %>%
#     .[, .(code = unique(GEOCOMP))] %>%
#     .[, state_acs5 := "yes"] %>%
#     setkey(code)
#
# us_acs5year <- read_acs5year("US", 2015) %>%
#     .[, .(code = unique(GEOCOMP))] %>%
#     .[, US_acs5 := "yes"] %>%
#     setkey(code)
#
# state_acs1year <- read_acs1year(states_DC, 2016) %>%
#     .[, .(code = unique(GEOCOMP))] %>%
#     .[, state_acs1 := "yes"] %>%
#     setkey(code)
#
# us_acs1year <- read_acs1year("US", 2016) %>%
#     .[, .(code = unique(GEOCOMP))] %>%
#     .[, US_acs1 := "yes"] %>%
#     setkey(code)
#
# dict_acs_geocomponent <- reduce(list(state_acs1year, us_acs1year,
#                                      state_acs5year, us_acs5year),
#                                 merge, all = TRUE) %>%
#     dict_all_geocomponent[, .(code, geo_component)][.] %>%
#     .[is.na(state_acs1), state_acs1 := "-"] %>%
#     .[is.na(US_acs1), US_acs1 := "-"] %>%
#     .[is.na(state_acs5), state_acs5 := "-"] %>%
#     .[is.na(US_acs5), US_acs5 := "-"]
#
#
# save(dict_acs_geocomponent, file = "data/dict_acs_geocomponent.RData")
#
