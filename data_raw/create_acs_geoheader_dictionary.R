### geoheader record files has changed over years.

library(data.table)
library(magrittr)
library(stringr)


# for recent acs surveys since 2011
dict_acs_geoheader <- fread("data_raw/acs_geoheader.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]


save(dict_acs_geoheader, file = "data/dict_acs_geoheader.RData")



# for old survey published in years 2010, 2009, 2008, 2007, and 2006
dict_acs_geoheader_2006_2010 <- fread("data_raw/acs_geoheader_2006_2010.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]


save(dict_acs_geoheader_2006_2010, file = "data/dict_acs_geoheader_2006_2010.RData")



# 2005, the earliest year available has different format
