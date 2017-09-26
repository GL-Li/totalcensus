library(data.table)
library(magrittr)
library(stringr)
library(readxl)
library(rvest)
library(mytoolbox)


# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696
state_fips <- download_table("https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696",
                             '//*[@id="detail"]/table',
                             "data_raw/state_fips.csv") %>%
    setDT()  %>%
    setnames(1:3, c("state_full", "state", "STATE_tmp")) %>%
    .[, STATE := as.character(STATE_tmp)] %>%
    .[STATE_tmp < 10, STATE := paste0("0", STATE_tmp)] %>%
    .[, STATE_tmp := NULL]


dict_fips <- read_excel("data_raw/all-geocodes-v2016 .xlsx", skip = 4) %>%
    setDT() %>%
    setnames(1:7, c("SUMLEV", "STATE", "COUNTY", "COUSUB", "PLACE", "CONCIT", "NAME")) %>%
    # add state name
    state_fips[., on = .(STATE)] %>%
    # the United State is also in the dict, give it a state name as "US
    .[NAME == "United States", ":=" (state_full = "United States", state = "US")] %>%
    # DC has no state names
    .[STATE == "11", ":=" (state_full = "District of Columbia", state = "DC")]



save(dict_fips, file = "data/dict_fips.RData")
