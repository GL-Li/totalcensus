library(data.table)
library(magrittr)
library(purrr)

# dict_acs_table
# the table_number are reserved across years, however the table_name has slight
# difference which gives trouble in merge data. We will only use table_number for
# data merge and keep only one table name.

make_acs_tables <- function(survey, year){
    lookup <- get(paste0("lookup_", survey, "year_", year))
    table <- lookup[, .(table_number, table_name, universe)] %>%
        .[, universe := str_remove(universe, "^Universe: ")] %>%
        setnames(c("table_name", "universe"),
                 c(paste0("name_", year), paste0("universe_", year))) %>%
        unique(by = "table_number") %>%
        .[, (paste0(survey, "_", year)) := "yes"]
}


# acs1 ====

latest_year <- 2019
# make tables of year 2005 - latest_year
for (year in 2005:latest_year){
    assign(paste0("acs1_", year), make_acs_tables("acs1", year))
}

# merge all tables, use newer names if possible
acs1_table <- reduce(lapply(paste0("acs1_", 2005:latest_year), get),
                           merge, by = "table_number", all = TRUE)

for (year in 2005:latest_year){
    acs1_table[get(paste0("acs1_", year)) == "yes", table_name := get(paste0("name_", year))]
    acs1_table[get(paste0("acs1_", year)) == "yes", universe := get(paste0("universe_", year))]
}

dict_acs1_table <- acs1_table[, c("table_number", "table_name", paste0("acs1_", latest_year:2005), "universe"),
                              with = FALSE]

save(dict_acs1_table, file = "data/dict_acs1_table.RData")



# acs5 ====
latest_year = 2021
for (year in 2009:latest_year){
    assign(paste0("acs5_", year), make_acs_tables("acs5", year))
}

# merge all tables, use newer names if possible
acs5_table <- reduce(lapply(paste0("acs5_", 2009:latest_year), get),
                     merge, by = "table_number", all = TRUE)

for (year in 2009:latest_year){
    acs5_table[get(paste0("acs5_", year)) == "yes", table_name := get(paste0("name_", year))]
    acs5_table[get(paste0("acs5_", year)) == "yes", universe := get(paste0("universe_", year))]
}

dict_acs5_table <- acs5_table[, c("table_number", "table_name", paste0("acs5_", latest_year:2009), "universe"),
                              with = FALSE]

save(dict_acs5_table, file = "data/dict_acs5_table.RData",
     compress = "xz", compression_level = 9)
