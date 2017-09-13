library(data.table)
library(magrittr)
library(stringr)

get_table_list <- function(){
    ## This function read "table_list.csv" into data.table
    ##
    ## return:
    ##     data.table
    ##     of all tables in census 2010 summary file 1. It has three columns:
    ##     1) number used in reference in data files such as P023,
    ##     2) table number and name such as P4. HISPANIC OR LATINO ORIGIN,
    ##     3) universe, which tells what the numbers are about

    table_list <- fread("data_raw/table_list.csv") %>%
        .[, universe := sapply(universe, function(x) str_sub(x, 11, nchar(x)))] %>%
        setkey(number)
    return(table_list)
}


make_file_datadict <- function(file_number) {
    ## This function turns single csv file like "file_05.csv" into a data.table
    ##
    ## Args:
    ## file_number: number
    ##     the number of the data file, such as 3, 15, ...
    ##
    ## Return:
    ## a data table sereve as dictionary of that data file

    if (file_number < 10) file_number <- paste0("0", file_number)
    file <- paste0("data_raw/file_", file_number, ".csv")
    dt <- fread(file, header = FALSE) %>%
        setnames(1:4, c("field", "reference", "max_size", "data_type"))%>%
        .[, table := table_list[sapply(reference, function(x) substr(x, 1, nchar(x) -3)), field]] %>%
        .[, universe := table_list[sapply(reference, function(x) substr(x, 1, nchar(x) -3)), universe]] %>%
        .[, file := paste0("file_", file_number)] %>%
        .[, max_size := NULL] %>%
        setcolorder(c("file", "field", "reference", "table", "universe", "data_type"))
    return(dt)
}


table_list <- get_table_list()

# create dictionary for all columns of data files =============================
dict_datafile <- data.table(file = character(0),
                            field = character(0),
                            reference = character(0),
                            table = character(0),
                            universe = character(0),
                            data_type = character(0))
for (i in 1:48) {
    assign(paste0("a", i), make_file_datadict(i))
    dict_datafile <- rbindlist(list(dict_datafile, get(paste0("a", i))))
    print(paste0(i, "---", nrow(get(paste0("a", i))) - 5))
}

# save data to package datasets ================================================
dict_censustable <- get_table_list()
save(dict_censustable, file = "data/census_table_dictionary.RData")
save(dict_datafile, file = "data/data_file_dictionary.RData")
