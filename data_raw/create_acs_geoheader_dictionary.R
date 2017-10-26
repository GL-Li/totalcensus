library(data.table)
library(magrittr)
library(stringr)

dict_acs_geoheader <- fread("data_raw/acs_geoheader.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]


save(dict_acs_geoheader, file = "data/dict_acs_geoheader.RData")
