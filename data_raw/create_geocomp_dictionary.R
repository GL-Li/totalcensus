# This file create dictionary of geographic component
library(data.table)
library(magrittr)
library(stringr)

geocomp_dict <- fread("data_raw/geographic_component", sep = "\n", header = FALSE) %>%
    # first two letters are code and others are description
    .[, .(code = str_sub(V1, 1, 2),
          description = str_sub(V1, 3, nchar(V1)))]
save(geocomp_dict, file = "data/geo_component.RData")
