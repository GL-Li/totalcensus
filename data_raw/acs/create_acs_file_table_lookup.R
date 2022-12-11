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
library(totalcensus)

make_acs_lookup <- function(period, year){
    # period : 1 or 5 year
    # year : year of the survey
    file_lookup <- paste0(
        "data_raw/acs/",
        "ACS_", period, "yr_Seq_Table_Number_Lookup_", year, ".txt"
    )

    file_lookup_xls <- paste0(
        "data_raw/acs/",
        "ACS_", period, "yr_Seq_Table_Number_Lookup_", year, ".xls"
    )

    if (file.exists(file_lookup)){
        dt <- fread(file_lookup, colClasses = "character", encoding = "Latin-1")
    } else if (file.exists(file_lookup_xls)) {
        dt <- read_excel(file_lookup_xls, col_types = "text")
    } else {
        message("Please download the file sequence/table number lookup file in .txt or .xls format")
        return(NULL)
    }

    if (year == 2005){
        table_shell <- read_excel("data_raw/acs/ACS_tables_Sum_file_shells_2005_1yr.xls", col_types = "text") %>%
            .[, 1:4] %>%
            setDT() %>%
            setnames(c("table_number", "table_seq", "table_content", "file_segment")) %>%
            .[!is.na(table_seq) & !str_detect(table_seq, "\\.")] %>%
            .[as.integer(table_seq) < 1000, reference := paste0(table_number, "_", table_seq)] %>%
            .[as.integer(table_seq) < 100, reference := paste0(table_number, "_","0", table_seq)] %>%
            .[as.integer(table_seq) < 10, reference := paste0(table_number, "_","00", table_seq)] %>%
            .[, table_seq := NULL]

        dt <- setDT(dt) %>%
            .[, c(2, 3, 7), with = FALSE] %>%
            setnames(c("table_number", "file_segment", "table_name"))

        name <- dt[!is.na(table_number)]
        universe <- dt[is.na(table_number), .(universe = table_name)] %>%
            .[, universe := str_remove(universe, "^Universe:  ")]
        dt <- cbind(name, universe)

        # a table content may appear in multiple file segment, join with file_seg too
        lookup_acs1year_2005 <- dt[table_shell, on = .(table_number, file_segment)] %>%
            .[, restriction := "unknown"] %>%
            setcolorder(c("file_segment", "table_content", "reference", "restriction",
                       "table_number", "table_name", "universe"))

        save(lookup_acs1year_2005, file = "data/lookup_acs1year_2005.RData",
             compress = "xz", compression_level = 9)

        return(lookup_acs1year_2005)

    } else {
        dt <- setDT(dt) %>%
            .[, c(2, 3, 4, 8), with = FALSE] %>%
            setnames(1:4, c("table_number", "file_segment", "reference", "table_name"))
    }


    univ <- dt[, .SD[2], by = .(table_number)] %>%
        .[, .(table_number, universe = table_name)] %>%
        setkey(table_number)

    # some lookup file contain reference like "0.5", "2.7" that are not in the
    # raw data, remove them otherwise will cause column problem.
    # in 2007 acs1year, reference of table_content == Universe: xxxx is not
    # empty but "0"
    content <- dt[reference != "" & reference != "0" & !grepl("\\.", reference)] %>%
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

        append_extension <- ifelse(year >= 2019, 'xlsx', 'xls')
        file_restriction <- paste0(
            "data_raw/acs/",
            "ACS_", year, "_SF_", period, "YR_Appendices.", append_extension
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

    if (year == 2005){
        # use 2006 table name to replace 2005 table name
        L2006 <- lookup_acs1year_2006[, .(reference, table_name)]
    }


    # save to R/data/
    dict_name <- paste0("lookup_acs", period, "year_", year)
    assign(dict_name, dict)
    save_as <- paste0("data/lookup_acs", period, "year_", year, ".RData" )
    save(list = dict_name, file = save_as,
         compress = "xz", compression_level = 9)

    return(dict)
}



# ACS 5-year =================================================================
lookup_acs5year_2021 <- make_acs_lookup(5, 2021)
# lookup_acs5year_2020 <- make_acs_lookup(5, 2020)
# lookup_acs5year_2019 <- make_acs_lookup(5, 2019)
# lookup_acs5year_2018 <- make_acs_lookup(5, 2018)
# lookup_acs5year_2017 <- make_acs_lookup(5, 2017)
# lookup_acs5year_2016 <- make_acs_lookup(5, 2016)
# lookup_acs5year_2015 <- make_acs_lookup(5, 2015)
# lookup_acs5year_2014 <- make_acs_lookup(5, 2014)
# lookup_acs5year_2013 <- make_acs_lookup(5, 2013)
# lookup_acs5year_2012 <- make_acs_lookup(5, 2012)
# lookup_acs5year_2011 <- make_acs_lookup(5, 2011)
# lookup_acs5year_2010 <- make_acs_lookup(5, 2010)
# lookup_acs5year_2009 <- make_acs_lookup(5, 2009)

lookup_acs5year_all_years <- totalcensus:::generate_acs5_tablecontents_()


# ACS 1-year =================================================================
lookup_acs1year_2019 <- make_acs_lookup(1, 2019)
lookup_acs1year_2018 <- make_acs_lookup(1, 2018)
lookup_acs1year_2017 <- make_acs_lookup(1, 2017)
lookup_acs1year_2016 <- make_acs_lookup(1, 2016)
lookup_acs1year_2015 <- make_acs_lookup(1, 2015)
lookup_acs1year_2014 <- make_acs_lookup(1, 2014)
lookup_acs1year_2013 <- make_acs_lookup(1, 2013)
lookup_acs1year_2012 <- make_acs_lookup(1, 2012)
lookup_acs1year_2011 <- make_acs_lookup(1, 2011)
lookup_acs1year_2010 <- make_acs_lookup(1, 2010)
lookup_acs1year_2009 <- make_acs_lookup(1, 2009)
lookup_acs1year_2008 <- make_acs_lookup(1, 2008)
lookup_acs1year_2007 <- make_acs_lookup(1, 2007)
lookup_acs1year_2006 <- make_acs_lookup(1, 2006)
lookup_acs1year_2005 <- make_acs_lookup(1, 2005)

table_content_acs1year_all_years <- totalcensus:::generate_acs1_tablecontents_()
save(table_content_acs1year_all_years,
     file = "data/table_content_acs1year_all_years.RData",
     compress = "xz", compression_level = 9)
for (yr in 2005:2019){
    tmp1 <- paste0("lookup_acs1year_", yr)
    tmp2 <- get(paste0("lookup_acs1year_", yr)) %>%
        .[, .(file_segment,  table_content, reference)]
    assign(tmp1, tmp2)
    file_name <- paste0("data/lookup_acs1year_", yr, ".RData")
    save(list = tmp1, file = file_name,
         compress = "xz", compression_level = 9)
}


# # save to data/
# save(lookup_acs5year_2019, file = "data/lookup_acs5year_2019.RData",
#      compress = "xz", compression_level = 9)
# save(lookup_acs5year_2018, file = "data/lookup_acs5year_2018.RData",
#      compress = "xz", compression_level = 9)
# save(lookup_acs5year_2017, file = "data/lookup_acs5year_2017.RData",
#      compress = "xz", compression_level = 9)
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
