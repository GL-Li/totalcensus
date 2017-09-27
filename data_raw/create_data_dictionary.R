library(data.table)
library(magrittr)
library(stringr)


make_file_datadict <- function(file_number) {
    ## This function turns single csv file like "file_05.csv" into a data.table
    ##
    ## Args:
    ## file_number: number
    ##     the segment number of the data file, such as 3, 15, ...
    ##
    ## Return:
    ## a data table sereve as dictionary of that data file

    if (file_number < 10) file_number <- paste0("0", file_number)
    file <- paste0("data_raw/file_", file_number, ".csv")
    dt <- fread(file, header = FALSE) %>%
        setnames(1:4, c("table_content", "reference", "max_size", "data_type"))%>%
        .[, table_number := dict_censustable[substr(reference, 1, nchar(reference) -3), table_number]] %>%
        .[, table_name := dict_censustable[substr(reference, 1, nchar(reference) -3), table_name]] %>%
        .[, universe := dict_censustable[substr(reference, 1, nchar(reference) -3), universe]] %>%
        .[, file_segment := as.numeric(file_number)] %>%
        .[, max_size := NULL] %>%
        .[, data_type := NULL] %>%
        setcolorder(c("file_segment", "table_content", "reference", "table_number", "table_name", "universe"))
    return(dt)
}



# create dictionary for all columns of data files =============================
dict_datafile <- data.table(file_segment = numeric(0),
                            table_content = character(0),
                            reference = character(0),
                            table_number = character(0),
                            table_name = character(0),
                            universe = character(0))
for (i in 1:48) {
    assign(paste0("a", i), make_file_datadict(i))
    dict_datafile <- rbindlist(list(dict_datafile, get(paste0("a", i))))
    print(paste0(i, "---", nrow(get(paste0("a", i))) - 5))
}

# save data to package datasets ================================================
save(dict_datafile, file = "data/dict_datafile.RData")
