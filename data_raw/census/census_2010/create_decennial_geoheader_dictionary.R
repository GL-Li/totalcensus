library(data.table)
library(magrittr)

# The csv file is cleaned version of the table from page 2-8 and 2-9 of the technical documentation
# of Census 2010 summary file 1 wit urban/rural update. Find the pdf at
# https://www.census.gov/prod/cen2010/doc/sf1.pdf
dict_decennial_geoheader_2010 <- read.csv("data_raw/census/census_2010/geographic_header_dictionary.csv", sep = ",",
                     stringsAsFactors = FALSE) %>%
    setDT() %>%
    .[, end := start + size -1] %>%
    .[, size := NULL] %>%
    .[, reference := trimws(reference)] %>%
    setcolorder(c("reference", "field", "start", "end"))

save(dict_decennial_geoheader_2010, file = "data/dict_decennial_geoheader_2010.RData")
