# This file create dictionary of geographic component
library(data.table)
library(magrittr)
library(stringr)

dict_geocomp <- fread("data_raw/geographic_component", sep = "\n", header = FALSE) %>%
    # first two letters are code and others are description
    .[, .(code = str_sub(V1, 1, 2),
          description = str_sub(V1, 3, nchar(V1)))]
save(dict_geocomp, file = "data/dict_geocomp.RData")
