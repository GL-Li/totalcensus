library(data.table)
library(magrittr)
library(stringr)

make_table_list <- function(){
    ## This function read "table_list.csv" into data.table
    ##
    ## return:
    ##     data.table
    ##     of all tables in census 2010 summary file 1. It has three columns:
    ##     1) number used in reference in data files such as P023,
    ##     2) table number and name such as P4. HISPANIC OR LATINO ORIGIN,
    ##     3) universe, which tells what the numbers are about

    table_list <- fread("data_raw/table_list.csv") %>%
        .[, universe := str_sub(universe, 11, nchar(universe))] %>%
        .[, table_num := str_extract(table, "^[^.]*")] %>%
        .[, table := str_replace(table, "^[^ ]*", "")] %>%
        setcolorder(c("table_ref", "table_num", "table", "universe")) %>%
        setkey(table_ref)
    return(table_list)
}


# save data to package datasets ================================================
dict_censustable <- make_table_list()
save(dict_censustable, file = "data/dict_censustable.RData")
