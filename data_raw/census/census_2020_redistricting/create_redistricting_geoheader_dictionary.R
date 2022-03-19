library(data.table)
library(magrittr)

# The csv file is cleaned version of the table from page 2-7 to 2-10 of the technical documentation
# of Census 2020 state redistricting data summary file.
dict_redistricting_geoheader_2020 <-
    read.csv("data_raw/census/census_2020_redistricting/geographic_header_dictionary.csv",
             sep = ",",
             stringsAsFactors = FALSE) %>%
    setDT() %>%
    .[, end := cumsum(size)] %>%
    .[, start := end - size + 1] %>%
    .[, end := start + size -1] %>%
    # .[, size := NULL] %>%
    .[, reference := trimws(reference)] %>%
    .[, .(reference, field, start, end)]

save(dict_redistricting_geoheader_2020,
     file = "data/dict_redistricting_2020.RData")
