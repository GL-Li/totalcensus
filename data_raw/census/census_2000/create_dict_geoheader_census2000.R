library(data.table)
library(magrittr)
library(stringr)

dict_decennial_geoheader_2000 <- fread("data_raw/census/census_2000/geographic_header_recorder.txt",
                   header = FALSE, sep = "\n") %>%
    .[, row_number := 1:nrow(.)] %>%
    .[str_detect(V1, "[:digit:]+"), line := row_number] %>%
    .[, line := ifelse(is.na(line), shift(line, type = "lead"), line)] %>%
    .[, line := ifelse(is.na(line), shift(line, type = "lead"), line)] %>%
    .[, line := ifelse(is.na(line), shift(line, type = "lead"), line)] %>%
    .[, paste(V1, collapse = " "), by = line] %>%
    .[, field := str_extract(V1, "^[^\\.]+")] %>%
    .[, field := str_remove(field, " +$")] %>%
    .[, V1 := str_remove(V1, "^[^\\.]*\\.+ ")] %>%
    .[, V1 := str_remove(V1, "^[\\.]*(\\. )+")] %>%
    .[, reference := str_extract(V1, "^[^ ]+ ")] %>%
    .[, reference := str_remove_all(reference, " ")] %>%
    .[, V1 := str_remove(V1, "^[^ ]+ ")] %>%
    .[, size := as.integer(str_extract(V1, "^[:digit:]+"))] %>%
    .[, V1 := str_remove(V1, "^[:digit:]+ ")] %>%
    .[, start := as.integer(str_extract(V1, "^[:digit:]+"))] %>%
    .[, end := start + size -1] %>%
    .[, .(reference, field, start, end)]

save(dict_decennial_geoheader_2000, file = "data/dict_decennial_geoheader_2000.RData", compress = "xz", compression_level = 9)
