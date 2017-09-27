library(data.table)
library(magrittr)
library(stringr)

## This file reads the text file copied from the technical documentation of the
## summary file 1 with urban/rural update (page 4-25) and save as a dataset in
## fold /data

dict_summarylevel_US <- fread("data_raw/national_summary_level_urban_rural_update",
                           header = FALSE, sep = "\n") %>%
    .[, .(code = str_sub(V1, 1, 3),
          summary_level = str_sub(V1, 5, nchar(V1)))]

save(dict_summarylevel_US, file = "data/dict_summarylevel_US.RData")



