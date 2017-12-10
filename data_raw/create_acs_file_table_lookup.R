library(data.table)
library(magrittr)
library(readxl)
library(stringr)

make_acs_lookup <- function(period, year){
    # period : 1 or 5 year
    # year : year of the survey
    file_lookup <- paste0(
        "data_raw/",
        "ACS_", period, "yr_Seq_Table_Number_Lookup_", year, ".txt"
    )
    file_restriction <- paste0(
        "data_raw/",
        "ACS_", year, "_SF_", period, "YR_Appendices.xls"
    )
    dt <- fread(file_lookup, colClasses = "character", encoding = "Latin-1") %>%
        .[, c(2, 3, 4, 8), with = FALSE] %>%
        setnames(1:4, c("table_number", "file_segment", "reference", "table_name"))

    restrict <- read_excel(file_restriction) %>%
        .[, c(1, 3)] %>%
        setDT() %>%
        setnames(c("table_number", "restriction")) %>%
        unique()

    tabl <- dt[, .SD[1], by = .(table_number)] %>%
        .[, .(table_number, table_name)] %>%
        restrict[., on = .(table_number)] %>%
        setkey(table_number)

    univ <- dt[, .SD[2], by = .(table_number)] %>%
        .[, .(table_number, universe = table_name)] %>%
        setkey(table_number)

    content <- dt[reference != ""] %>%
        setnames("table_name", "table_content") %>%
        # change the reference from 1, 12, ...  to 001, 012, ...
        .[str_length(reference) == 1, reference := paste0("00", reference)] %>%
        .[str_length(reference) == 2, reference := paste0("0", reference)] %>%
        .[, reference := paste0(table_number, "_", reference)] %>%
        # the 1-year 2014 lookup file has 1, 2, 3, ... as file_segment
        # change to 001, 002, ...
        .[str_length(file_segment) == 1, file_segment := paste0("000", file_segment)] %>%
        .[str_length(file_segment) == 2, file_segment := paste0("00", file_segment)] %>%
        .[str_length(file_segment) == 3, file_segment := paste0("0", file_segment)] %>%
        setkey(table_number)

    dict <- tabl[content] %>%
        univ[.] %>%
        setcolorder(c("file_segment", "table_content", "reference", "restriction",
                      "table_number", "table_name", "universe")) %>%
        .[order(file_segment)]
}



# ACS 5-year
lookup_acs5year_2016 <- make_acs_lookup(5, 2016)
lookup_acs5year_2015 <- make_acs_lookup(5, 2015)

# ACS 1-year
lookup_acs1year_2016 <- make_acs_lookup(1, 2016)
lookup_acs1year_2015 <- make_acs_lookup(1, 2015)
lookup_acs1year_2014 <- make_acs_lookup(1, 2014)

# save to data/
save(lookup_acs5year_2016, file = "data/lookup_acs5year_2016.RData",
     compress = "xz", compression_level = 9)
save(lookup_acs5year_2015, file = "data/lookup_acs5year_2015.RData",
     compress = "xz", compression_level = 9)
save(lookup_acs1year_2016, file = "data/lookup_acs1year_2016.RData",
     compress = "xz", compression_level = 9)
save(lookup_acs1year_2015, file = "data/lookup_acs1year_2015.RData",
     compress = "xz", compression_level = 9)
save(lookup_acs1year_2014, file = "data/lookup_acs1year_2014.RData",
     compress = "xz", compression_level = 9)
