library(data.table)
library(magrittr)
library(purrr)

# dict_acs_table ===========================================================
# the table_number are reserved across years, however the table_name has slight
# difference which gives trouble in merge data. We will only use table_number for
# data merge and keep only one table name.
acs1_2014 <- lookup_acs1year_2014[, .(table_number, name_acs1_2014 = table_name)] %>%
    unique() %>%
    .[, acs1_2014 := "yes"] %>%
    setkey(table_number)

acs1_2015 <- lookup_acs1year_2015[, .(table_number, name_acs1_2015 = table_name)] %>%
    unique() %>%
    .[, acs1_2015 := "yes"] %>%
    setkey(table_number)

acs1_2016 <- lookup_acs1year_2016[, .(table_number, name_acs1_2016 = table_name)] %>%
    unique() %>%
    .[, acs1_2016 := "yes"] %>%
    setkey(table_number)


acs5_2015 <- lookup_acs5year_2015[, .(table_number, name_acs5_2015 = table_name)] %>%
    unique() %>%
    .[, acs5_2015 := "yes"] %>%
    setkey(table_number)


dict_acs_table <- reduce(list(acs1_2014, acs1_2015, acs1_2016, acs5_2015), merge, all = TRUE) %>%
    # consolidate table_names
    .[acs1_2014 == "yes", table_name := name_acs1_2014] %>%
    .[acs1_2015 == "yes", table_name := name_acs1_2015] %>%
    .[acs1_2016 == "yes", table_name := name_acs1_2016] %>%
    .[acs5_2015 == "yes", table_name := name_acs5_2015] %>%
    .[, .(table_number, table_name, acs5_2015, acs1_2016, acs1_2015, acs1_2014)] %>%
    .[is.na(acs5_2015), acs5_2015 := "-"] %>%
    .[is.na(acs1_2016), acs1_2016 := "-"] %>%
    .[is.na(acs1_2015), acs1_2015 := "-"] %>%
    .[is.na(acs1_2014), acs1_2014 := "-"]

save(dict_acs_table, file = "data/dict_acs_table.RData")

