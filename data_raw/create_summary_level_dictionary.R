library(data.table)
library(magrittr)
library(stringr)

## This file reads the text file copied from the technical documentation of the
## summary file 1 with urban/rural update (page 4-16) and save as a dataset in
## fold /data

dict_summarylevel <- fread("data_raw/state_summary_level_urban_rural_update",
                           header = FALSE, sep = "\n") %>%
    .[, .(code = str_sub(V1, 1, 3),
          description = str_sub(V1, 5, nchar(V1)))]

save(dict_summarylevel, file = "data/dict_summarylevel.RData")



