### reference is one digit shorter than that of 2010. For example, the total
### population is P001001 in census 2000, while P0010001 in 2010. To keep the
### consistency, add a "0" to the those in census 2000.

### data from techincal documentation
# https://www.census.gov/prod/cen2000/doc/sf1.pdf


library(data.table)
library(magrittr)
library(stringr)


# generate lookup_decennial_2000 data =========================================

# The first 5 rows of the regerence of all file segments are the same. Simply duplicate
# them to each file segment. Later on they will be combined with other references
# in each file segment in data dictionary.
first_5 <- fread("data_raw/census/census_2000/file_table_head.csv", header = F) %>%
    setnames(c("table_content", "reference"))

file_table_head <- data.table(file_segment = rep(1:39, each = 5),
                              table_content = first_5$table_content,
                              reference = first_5$reference,
                              table_number = NA,
                              table_name = NA,
                              universe = NA) %>%
    .[, file_segment := ifelse(file_segment < 10,
                               paste0("0", file_segment),
                               file_segment)]


# This is the main part of data dictionary. Note that the references are one digit
# short than those in census 2010. Add "0" to match.
census2000_lookup <- fread("data_raw/census/census_2000/census2000_file_table_lookup.txt", sep = "\n", header = FALSE) %>%
    .[, row_number := 1:nrow(.)] %>%

    # only row ending with " 9" is a true line. If not merge with row below.
    # repeat to remove all NAs in case a true line is broken into multiple rows
    .[str_extract(V1, "..$") == " 9", line_number := row_number] %>%
    .[, line_number := ifelse(is.na(line_number), shift(line_number, type = "lead"), line_number)] %>%
    .[, line_number := ifelse(is.na(line_number), shift(line_number, type = "lead"), line_number)] %>%
    .[, line_number := ifelse(is.na(line_number), shift(line_number, type = "lead"), line_number)] %>%
    .[, line_number := ifelse(is.na(line_number), shift(line_number, type = "lead"), line_number)] %>%
    .[, line_number := ifelse(is.na(line_number), shift(line_number, type = "lead"), line_number)] %>%
    .[, .(line = paste(V1, collapse = " ")), by = line_number] %>%

    # extract columns from each line
    .[, line := str_remove(line, " [^ ]+$")] %>%
    .[, file_segment := str_extract(line, "[^ ]+$")] %>%
    .[, line := str_remove(line, " [^ ]+$")] %>%
    .[, reference := str_extract(line, "[^ ]+$")] %>%
    .[, line := str_remove(line, " [^ ]+$")] %>%
    .[, table_content := str_remove_all(line, "[,:]*")] %>%
    .[, table_number := str_remove(reference, "...$")] %>%
    .[, .(file_segment, table_content, reference, table_number)] %>%

    # turn table_number from P001 to P1
    .[, table_number := {tmp1 = str_extract(table_number, "^[:upper:]+")
                         tmp2 = str_remove(table_number, tmp1)
                         tmp3 = str_remove(tmp2, "^0*")
                         tmp4 = paste0(tmp1, tmp3)
                         .(tmp4)}] %>%

    # insert "0" to reference so that P001001 becomes P0010001
    .[, reference := {tmp1 = str_remove(reference, "...$")
                tmp2 = str_extract(reference, "...$")
                tmp3 = paste0(tmp1, "0", tmp2)
                .(tmp3)}]


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
tables <- table_number_name %>%
    .[, universe := table_univese$universe]


# put all together to build lookup table
lookup_2000 <- tables[census2000_lookup, on = "table_number"] %>%
    setcolorder(c("file_segment", "table_content", "reference", "table_number",
                  "table_name", "universe"))
lookup_decennial_2000 <- rbind(file_table_head, lookup_2000) %>%
    .[order(file_segment)]

save(lookup_decennial_2000, file = "data/lookup_decennial_2000.RData", compress = "xz", compression_level = 9)



# compare to lookup_decennial_2010 ============================================
# conclusion: same reference number could point to different content in census
# 2000 and 2010. They should stay seperately.

# ignore fisrt 5
L2010 <- lookup_decennial_2010[reference %like% "0"] %>%
    .[, .(reference, table_name, table_content)]
L2000 <- lookup_decennial_2000[reference %like% "0"] %>%
    .[, .(reference, table_name, table_content)]

combined1 <- L2010[L2000, on = "reference"]
combined2 <- L2000[L2010, on = "reference"]

# compare tables
T2000 <- tables
T2010 = dict_decennial_table[, .(table_number, table_name, universe)] %>%
    unique()

T_combined_1 <- T2000[T2010, on = "table_number"]
T_combined_2 <- T2010[T2000, on = "table_number"]


T_number_shared <- intersect(T2000$table_number, T2010$table_number)   # 205
T_number_in_2000_only <- setdiff(T2000$table_number, T2010$table_number)    # 80
T_number_in_2010_only <- setdiff(T2010$table_number, T2000$table_number)    # 128
