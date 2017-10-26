library(data.table)
library(magrittr)
library(stringr)

make_acs_data_dictionary <- function(){
    dt <- fread("data_raw/acs_datafile.csv", colClasses = "character", encoding = "Latin-1") %>%
        .[, c(2, 3, 4, 8), with = FALSE] %>%
        setnames(1:4, c("table_number", "file_segment", "reference", "table_name"))

    tabl <- dt[, .SD[1], by = .(table_number)] %>%
        .[, .(table_number, table_name)] %>%
        setkey(table_number)

    univ <- dt[, .SD[2], by = .(table_number)] %>%
        .[, .(table_number, universe = table_name)] %>%
        setkey(table_number)

    content <- dt[reference != ""] %>%
        setnames("table_name", "table_content") %>%
        # change the reference from 1, 12, ...  to 001, 012, ...
        .[, ref_tmp := as.numeric(reference)] %>%
        .[ref_tmp < 10, reference := paste0("00", reference)] %>%
        .[ref_tmp >9 & ref_tmp < 100, reference := paste0("0", reference)] %>%
        .[, reference := paste0(table_number, "_", reference)] %>%
        .[, ref_tmp := NULL] %>%
        setkey(table_number)

    dict <- tabl[content] %>%
        univ[.] %>%
        setcolorder(c("file_segment", "table_content", "reference",
                      "table_number", "table_name", "universe")) %>%
        .[order(file_segment)]
}

dict_acs_datafile <- make_acs_data_dictionary()

save(dict_acs_datafile, file = "data/dict_acs_datafile.RData",
     compress = "xz", compression_level = 9)
