# find the summary level that has been used and merge with census summary level

library(data.table)
library(magrittr)
library(purrr)

generate_summary_level <- function(survey, year){
    if (survey %in% c("dec", "decennial")){
        state <- read_decennial(year, states_DC)
        us <- read_decennial(year, "US")
    } else if (survey == "acs5"){
        state <- read_acs5year(year, states_DC)
        us <- read_acs5year(year, "US")
    } else if (survey == "acs1"){
        state <- read_acs1year(year, states_DC)
        us <- read_acs1year(year, "US")
    }

    state <- state[, .(code = unique(SUMLEV))] %>%
        .[, (paste0("state_", year)) := "yes"] %>%
        setkey(code)
    us <- us[, .(code = unique(SUMLEV))] %>%
        .[, (paste0("US_", year)) := "yes"] %>%
        setkey(code)

    dict <- merge(state, us, all = TRUE) %>%
        dict_all_summarylevel[., on = .(code)]

    return(dict)
}

# acs1year summary level ======================================================
# The same since 2006. Just check when new year added
S2019 <- generate_summary_level("acs1", 2019)
S2018 <- generate_summary_level("acs1", 2018)
S2017 <- generate_summary_level("acs1", 2017)
S2016 <- generate_summary_level("acs1", 2016)
S2015 <- generate_summary_level("acs1", 2015)
S2014 <- generate_summary_level("acs1", 2014)
S2013 <- generate_summary_level("acs1", 2013)
S2012 <- generate_summary_level("acs1", 2012)
S2011 <- generate_summary_level("acs1", 2011)
S2010 <- generate_summary_level("acs1", 2010)
S2009 <- generate_summary_level("acs1", 2009)
S2008 <- generate_summary_level("acs1", 2008)
S2007 <- generate_summary_level("acs1", 2007)
S2006 <- generate_summary_level("acs1", 2006)
S2005 <- generate_summary_level("acs1", 2005)


# same from 2006 to 2018, one row different from 2005
dict_acs1_summarylevel <- merge(S2017, S2005,
                                by = c("code", "summary_level"),
                                all = TRUE) %>%
    .[, .(code, summary_level,
          state_2006_to_now = state_2017, state_2005,
          US_2005_to_now = US_2017)]

save(dict_acs1_summarylevel, file = "data/dict_acs1_summarylevel.RData")


# acs5year summary level ======================================================
# The same since 2013. Just check when new year added
S2019 <- generate_summary_level("acs5", 2019)
S2018 <- generate_summary_level("acs5", 2018)
S2017 <- generate_summary_level("acs5", 2017)
S2016 <- generate_summary_level("acs5", 2016)
S2015 <- generate_summary_level("acs5", 2015)
S2014 <- generate_summary_level("acs5", 2014)
S2013 <- generate_summary_level("acs5", 2013)
S2012 <- generate_summary_level("acs5", 2012)
S2011 <- generate_summary_level("acs5", 2011)
S2010 <- generate_summary_level("acs5", 2010)
S2009 <- generate_summary_level("acs5", 2009)

dict_acs5_summarylevel <- purrr::reduce(list(S2019, S2018, S2017, S2016, S2015,
                                             S2014, S2013, S2012, S2011, S2010,
                                             S2009),
                                merge, by = c("code", "summary_level"),
                                all = TRUE) %>%
    .[, .(code, summary_level,
          state_2013_to_now = state_2019, state_2012, state_2009_to_2011 = state_2011,
          US_2011_to_now = US_2016, US_2010, US_2009)]

save(dict_acs5_summarylevel, file = "data/dict_acs5_summarylevel.RData")

