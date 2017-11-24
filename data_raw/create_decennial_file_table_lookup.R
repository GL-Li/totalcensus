library(data.table)
library(magrittr)
library(stringr)
library(purrr)

# 2010 data file ==============================================================
make_decennial_lookup <- function(file_seg) {
    ## This function turns single csv file like "file_05.csv" into a data.table
    ##
    ## Args:
    ## file_seg: stringr
    ##     the segment number of the data file, such as 03, 15, ...
    ##
    ## Return:
    ## a data table sereve as dictionary of that data file

    file <- paste0("data_raw/file_", file_seg, ".csv")
    dt <- fread(file, header = FALSE) %>%
        setnames(1:4, c("table_content", "reference", "max_size", "data_type"))%>%
        .[, table_number := dict_decennial_table[substr(reference, 1, nchar(reference) -3), table_number]] %>%
        .[, table_name := dict_decennial_table[substr(reference, 1, nchar(reference) -3), table_name]] %>%
        .[, universe := dict_decennial_table[substr(reference, 1, nchar(reference) -3), universe]] %>%
        .[, file_segment := file_seg] %>%
        # "–" is non ascii character, replace with "-"
        .[, table_content := str_replace_all(table_content, "–", "-")] %>%
        .[, max_size := NULL] %>%
        .[, data_type := NULL] %>%
        setcolorder(c("file_segment", "table_content", "reference",
                      "table_number", "table_name", "universe"))
    return(dt)
}

# create dictionary for all columns of data files
file_segs <- map_chr(1:48, function(x) ifelse(x < 10, paste0("0", x), x))
lookup_decennial_2010 <- purrr::map(file_segs, make_decennial_lookup) %>%
    rbindlist()

# save data to package datasets
save(lookup_decennial_2010, file = "data/lookup_decennial_2010.RData", compress = "xz", compression_level = 9)
