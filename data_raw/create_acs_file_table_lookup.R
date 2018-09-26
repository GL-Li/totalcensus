# lookup tables are downloaded from
# https://www.census.gov/programs-surveys/acs/technical-documentation/summary-file-documentation.2017.html
# more general, the lookup table are located in usertools subdirectory of ftp sites, for old years.
# https://www2.census.gov/programs-surveys/acs/summary_file/2010/documentation/5_year/user_tools/
# some named differently
# https://www2.census.gov/programs-surveys/acs/summary_file/2009/documentation/1_year/user_tools/merge_5_6.txt

# Appendices are available from 2013. Download them from
# https://www.census.gov/programs-surveys/acs/technical-documentation/summary-file-documentation.2017.html
# ignore if not available

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

    file_lookup_xls <- paste0(
        "data_raw/",
        "ACS_", period, "yr_Seq_Table_Number_Lookup_", year, ".xls"
    )

    if (file.exists(file_lookup)){
        dt <- fread(file_lookup, colClasses = "character", encoding = "Latin-1") %>%
            .[, c(2, 3, 4, 8), with = FALSE] %>%
            setnames(1:4, c("table_number", "file_segment", "reference", "table_name"))
    } else if (file.exists(file_lookup_xls)) {
        dt <- read_excel(file_lookup_xls, col_types = "text") %>%
            setDT() %>%
            .[, c(2, 3, 4, 8), with = FALSE] %>%
            setnames(1:4, c("table_number", "file_segment", "reference", "table_name"))
    } else {
        message("Please download the file sequence/table number lookup file in .txt or .xls format")
        return(NULL)
    }


    univ <- dt[, .SD[2], by = .(table_number)] %>%
        .[, .(table_number, universe = table_name)] %>%
        setkey(table_number)

    # some lookup file contain reference like "0.5", "2.7" that are not in the
    # raw data, remove them otherwise will cause column problem
    content <- dt[reference != "" & !grepl("\\.", reference)] %>%
        setnames("table_name", "table_content") %>%
        # change the reference from 1, 12, ...  to 001, 012, ...
        .[str_length(reference) == 1, reference := paste0("00", reference)] %>%
        .[str_length(reference) == 2, reference := paste0("0", reference)] %>%
        .[, reference := paste0(table_number, "_", reference)] %>%
        # the 1-year 2014 lookup file has 1, 2, 3, ... as file_segment
        # change to 0001, 0002, ...
        .[str_length(file_segment) == 1, file_segment := paste0("000", file_segment)] %>%
        .[str_length(file_segment) == 2, file_segment := paste0("00", file_segment)] %>%
        .[str_length(file_segment) == 3, file_segment := paste0("0", file_segment)] %>%
        setkey(table_number)

    # Appendices.xls files are not available for years earlier than 2013
    if (year >= 2013){
        file_restriction <- paste0(
            "data_raw/",
            "ACS_", year, "_SF_", period, "YR_Appendices.xls"
        )

        restrict <- read_excel(file_restriction) %>%
            .[, c(1, 3)] %>%
            setDT() %>%
            setnames(c("table_number", "restriction")) %>%
            unique()
    } else {
        # make a fake restriction (unknown restriction)
        restrict <- dt[, .(table_number)] %>%
            .[, restriction := "unknown"] %>%
            unique()
    }

    tabl <- dt[, .SD[1], by = .(table_number)] %>%
        .[, .(table_number, table_name)] %>%
        restrict[., on = .(table_number)] %>%
        setkey(table_number)

    dict <- tabl[content] %>%
        univ[.] %>%
        setcolorder(c("file_segment", "table_content", "reference", "restriction",
                      "table_number", "table_name", "universe")) %>%
        .[order(file_segment)]


    # save to R/data/
    dict_name <- paste0("lookup_acs", period, "year_", year)
    assign(dict_name, dict)
    save_as <- paste0("data/lookup_acs", period, "year_", year, ".RData" )
    save(list = dict_name, file = save_as,
         compress = "xz", compression_level = 9)

    return(dict)
}



# ACS 5-year
lookup_acs5year_2016 <- make_acs_lookup(5, 2016)
lookup_acs5year_2015 <- make_acs_lookup(5, 2015)
lookup_acs5year_2014 <- make_acs_lookup(5, 2014)
lookup_acs5year_2013 <- make_acs_lookup(5, 2013)
lookup_acs5year_2012 <- make_acs_lookup(5, 2012)
lookup_acs5year_2011 <- make_acs_lookup(5, 2011)
lookup_acs5year_2010 <- make_acs_lookup(5, 2010)
lookup_acs5year_2009 <- make_acs_lookup(5, 2009)

# ACS 1-year
lookup_acs1year_2017 <- make_acs_lookup(1, 2017)
lookup_acs1year_2016 <- make_acs_lookup(1, 2016)
lookup_acs1year_2015 <- make_acs_lookup(1, 2015)
lookup_acs1year_2014 <- make_acs_lookup(1, 2014)

lookup_acs1year_2010 <- make_acs_lookup(1, 2010)
lookup_acs1year_2008 <- make_acs_lookup(1, 2008)


# # save to data/
# save(lookup_acs5year_2016, file = "data/lookup_acs5year_2016.RData",
#      compress = "xz", compression_level = 9)
# save(lookup_acs5year_2015, file = "data/lookup_acs5year_2015.RData",
#      compress = "xz", compression_level = 9)
# save(lookup_acs5year_2010, file = "data/lookup_acs5year_2010.RData",
#      compress = "xz", compression_level = 9)
#
# save(lookup_acs1year_2017, file = "data/lookup_acs1year_2017.RData",
#      compress = "xz", compression_level = 9)
# save(lookup_acs1year_2016, file = "data/lookup_acs1year_2016.RData",
#      compress = "xz", compression_level = 9)
# save(lookup_acs1year_2015, file = "data/lookup_acs1year_2015.RData",
#      compress = "xz", compression_level = 9)
# save(lookup_acs1year_2014, file = "data/lookup_acs1year_2014.RData",
#      compress = "xz", compression_level = 9)
# save(lookup_acs1year_2010, file = "data/lookup_acs1year_2010.RData",
#      compress = "xz", compression_level = 9)
