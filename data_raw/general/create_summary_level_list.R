library(data.table)
library(magrittr)
library(stringr)

dict_all_summarylevel <- fread("data_raw/general/summary_level_complete_list.txt", sep = "\n",
                header = FALSE, skip = 1) %>%
    .[, V1 := str_remove_all(V1, '"|,')] %>%
    .[, code := str_extract(V1, "^...")] %>%
    .[, summary_level := str_remove(V1, "^...")] %>%
    .[, .(code, summary_level)]


save(dict_all_summarylevel, file = "data/dict_all_summarylevel.RData")
