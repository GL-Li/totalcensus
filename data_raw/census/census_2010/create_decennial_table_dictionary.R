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

    table_list <- fread("data_raw/census/census_2010/table_list.csv") %>%
        .[, universe := str_sub(universe, 11, nchar(universe))] %>%
        .[, table_number := str_extract(table_name, "^[^.]*")] %>%
        .[, table_name := str_replace(table_name, "^[^ ]* ", "")] %>%
        .[, table_ref := {tmp1 = str_extract(table_number, "^[:upper:]+")
                          tmp2 = str_remove(table_number, "^[:upper:]+")
                          tmp3 = ifelse(str_length(tmp2) == 1, paste0("00", tmp2), paste0("0", tmp2))
                          tmp4 = paste0(tmp1, tmp3)
                          .(tmp4)}] %>%
        #.[, year := 2010] %>%
        setcolorder(c("table_number", "table_name", "universe", "table_ref"))
    return(table_list)
}


# save data to package datasets ================================================
dict_decennial_table_2010 <- make_table_list()
save(dict_decennial_table_2010, file = "data/dict_decennial_table_2010.RData")
