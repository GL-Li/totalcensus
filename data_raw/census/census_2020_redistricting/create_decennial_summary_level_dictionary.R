library(data.table)
library(magrittr)
library(stringr)

## This file reads the text file copied from the technical documentation of the
## summary file 1 with redistricting (page 4-5) and save as a dataset in
## fold /data

state <- fread("data_raw/census/census_2020/state_summary_level_redistricting",
                           header = FALSE, sep = "\n") %>%
    .[, .(code = str_sub(V1, 1, 3),
          summary_level = str_sub(V1, 5, nchar(V1)))] %>%
    .[, in_state_file := "yes"] %>%
    setkey(code, summary_level)


# ## This file reads the text file copied from the technical documentation of the
# ## summary file 1 with urban/rural update (page 4-25) and save as a dataset in
# ## fold /data
#
# national <- fread("data_raw/census/census_2010/national_summary_level_urban_rural_update",
#                               header = FALSE, sep = "\n") %>%
#     .[, .(code = str_sub(V1, 1, 3),
#           summary_level = str_sub(V1, 5, nchar(V1)))] %>%
#     .[, in_US_file := "yes"] %>%
#     setkey(code, summary_level)

dict_decennial_summarylevel_2010 <- merge(state, national, all = TRUE) %>%
    .[is.na(in_state_file), in_state_file := "-"] %>%
    .[is.na(in_US_file), in_US_file := "-"]


save(dict_decennial_summarylevel_2010, file = "data/dict_decennial_summarylevel_2010.RData")
