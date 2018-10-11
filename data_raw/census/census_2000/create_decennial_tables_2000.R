### reference is one digit shorter than that of 2010. For example, the total
### population is P001001 in census 2000, while P0010001 in 2010. To keep the
### consistency, add a "0" to the those in census 2000.

### data from techincal documentation
# https://www.census.gov/prod/cen2000/doc/sf1.pdf


library(data.table)
library(magrittr)
library(stringr)


# get table names and universe from technical documentation
# this file contain name and universe. Retrieve them seperately.

# get table name
table_number_name <- fread("data_raw/census/census_2000/census_2000_table_list.txt", sep = "\n", header = FALSE) %>%

    # stitch broken lines, # repeat in case a title covers 3 rows. table names
    # are in upper case while universe contains lower case
    .[, row_number := 1:nrow(.)] %>%
    .[!str_detect(V1, "[:lower:]+"), uppercase := row_number] %>%
    .[, uppercase := ifelse(!is.na(uppercase) & !is.na(shift(uppercase, type = "lead")),
                      shift(uppercase, type = "lead"), uppercase)] %>%
    .[, uppercase := ifelse(!is.na(uppercase) & !is.na(shift(uppercase, type = "lead")),
                            shift(uppercase, type = "lead"), uppercase)] %>%
    .[, uppercase := ifelse(!is.na(uppercase) & !is.na(shift(uppercase, type = "lead")),
                            shift(uppercase, type = "lead"), uppercase)] %>%
    .[!is.na(uppercase)] %>%    # keep only names
    .[, paste(V1, collapse = " "), by = uppercase] %>%

    # extract coumns
    .[, .(table_number = str_extract(V1, "^[^.]+"),
          table_name = str_remove(V1, "^[^.]+. "))]

# %>%
#     .[, starting_letter := str_extract(table_number, "^[:upper:]+")] %>%
#     .[, table_number := str_remove(table_number, "^[:upper:]+")] %>%
#     .[, digit := str_extract(table_number, "^[:digit:]+")] %>%
#     .[, table_number := str_remove(table_number, "^[:digit:]+")] %>%
#     .[as.integer(digit) < 10, digit := paste0("00", as.character(digit))]%>%
#     .[as.integer(digit) >= 10 & as.integer(digit) < 100, digit := paste0("0", as.character(digit))] %>%
#     .[, .(table_number = paste0(starting_letter, digit, table_number), table_name)]


# get table universe
table_univese <- fread("data_raw/census/census_2000/census_2000_table_list.txt", sep = "\n", header = FALSE) %>%
    .[, row_number := 1:nrow(.)] %>%
    .[str_detect(V1, "[:lower:]+"), lowercase := row_number] %>%
    .[, lowercase := ifelse(!is.na(lowercase) & !is.na(shift(lowercase, type = "lead")),
                            shift(lowercase, type = "lead"), lowercase)] %>%
    # repeat in case a title covers 3 lines
    .[, lowercase := ifelse(!is.na(lowercase) & !is.na(shift(lowercase, type = "lead")),
                            shift(lowercase, type = "lead"), lowercase)] %>%
    .[, lowercase := ifelse(!is.na(lowercase) & !is.na(shift(lowercase, type = "lead")),
                            shift(lowercase, type = "lead"), lowercase)] %>%
    # get universe
    .[!is.na(lowercase)] %>%
    .[, paste(V1, collapse = " "), by = lowercase] %>%
    .[, .(universe = str_remove(V1, "^[^:]+: "))] %>%
    .[, universe := str_remove(universe, "[\\. ]* [:digit:]+$")]


# combine table name and universe
dict_decennial_table_2000 <- copy(table_number_name) %>%
    .[, universe := table_univese$universe] %>%
    .[, table_ref := {tmp1 = str_extract(table_number, "^[:upper:]+")
                      tmp2 = str_remove(table_number, "^[:upper:]+")
                      tmp3 = ifelse(str_length(tmp2) == 1, paste0("00", tmp2), paste0("0", tmp2))
                      tmp4 = paste0(tmp1, tmp3)
                      .(tmp4)}]


save(dict_decennial_table_2000,
     file = "data/dict_decennial_table_2000.RData",
     compress = "xz", compression_level = 9)


