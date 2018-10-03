### geoheader record files has changed over years.

library(data.table)
library(magrittr)
library(stringr)


# 2011 - current, 1 year and 5 year are the same
dict_acs_geoheader <- fread("data_raw/acs_geoheader.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]
save(dict_acs_geoheader, file = "data/dict_acs_geoheader.RData")

# 2010, 1 year and 5 year are the same but different from 2011
dict_acs_geoheader_2010 <- fread("data_raw/acs_geoheader_2010.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]
save(dict_acs_geoheader_2010, file = "data/dict_acs_geoheader_2010.RData")

# 2009, 1 year and 5 year are very different
dict_acs_geoheader_2009_1year <- fread("data_raw/acs_geoheader_2009_1year.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]
save(dict_acs_geoheader_2009_1year, file = "data/dict_acs_geoheader_2009_1year.RData")

dict_acs_geoheader_2009_5year <- fread("data_raw/acs_geoheader_2009_5year.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]
save(dict_acs_geoheader_2009_5year, file = "data/dict_acs_geoheader_2009_5year.RData")


# 2008, 2007, and 2006 only have 1 year data. They have the same geoheader file
dict_acs_geoheader_2006_2008_1year <- fread("data_raw/acs_geoheader_2006_2007_2008_1year.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]
save(dict_acs_geoheader_2006_2008_1year, file = "data/dict_acs_geoheader_2006_2008_1year.RData")


# 2005, the earliest year available has different format
dict_acs_geoheader_2005_1year <- fread("data_raw/acs_geoheader_2005_1year.csv") %>%
    .[, end := start + size - 1] %>%
    .[, size := NULL] %>%
    .[, type := NULL]
save(dict_acs_geoheader_2005_1year, file = "data/dict_acs_geoheader_2005_1year.RData")
