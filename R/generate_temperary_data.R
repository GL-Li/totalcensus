# This funciton generates datasets used in the package but we do not want to save
# the datasets for users. When used just call the functions.


# do NOT save as dataset, but use as a function
# the reference are reserved over years, however the table_content and table_name has slight
# difference which gives trouble in merge data. We will only use reference for
# data merge and keep only one table content and table name.

generate_acs_tablecontents <- function(){
    acs1_2014 <- lookup_acs1year_2014[, .(reference,
                                          content_acs1_2014 = table_content,
                                          name_acs1_2014 = table_name)] %>%
        .[, acs1_2014 := "yes"] %>%
        setkey(reference)

    acs1_2015 <- lookup_acs1year_2015[, .(reference,
                                          content_acs1_2015 = table_content,
                                          name_acs1_2015 = table_name)] %>%
        .[, acs1_2015 := "yes"] %>%
        setkey(reference)

    acs1_2016 <- lookup_acs1year_2016[, .(reference,
                                          content_acs1_2016 = table_content,
                                          name_acs1_2016 = table_name)] %>%
        .[, acs1_2016 := "yes"] %>%
        setkey(reference)


    acs5_2015 <- lookup_acs5year_2015[, .(reference,
                                          content_acs5_2015 = table_content,
                                          name_acs5_2015 = table_name)] %>%
        .[, acs5_2015 := "yes"] %>%
        setkey(reference)


    dict_acs_tablecontent <- reduce(list(acs1_2014, acs1_2015, acs1_2016, acs5_2015), merge, all = TRUE) %>%
        # consolidate table_names
        .[acs1_2014 == "yes", ":=" (table_content = content_acs1_2014,
                                    table_name = name_acs1_2014)] %>%
        .[acs1_2015 == "yes", ":=" (table_content = content_acs1_2015,
                                    table_name = name_acs1_2015)] %>%
        .[acs1_2016 == "yes", ":=" (table_content = content_acs1_2016,
                                    table_name = name_acs1_2016)] %>%
        .[acs5_2015 == "yes", ":=" (table_content = content_acs5_2015,
                                    table_name = name_acs5_2015)] %>%
        .[, .(reference, table_content, table_name, acs5_2015, acs1_2016, acs1_2015, acs1_2014)] %>%
        .[is.na(acs5_2015), acs5_2015 := "-"] %>%
        .[is.na(acs1_2016), acs1_2016 := "-"] %>%
        .[is.na(acs1_2015), acs1_2015 := "-"] %>%
        .[is.na(acs1_2014), acs1_2014 := "-"]
}


generate_census_tablecontents <- function(){
    # will add census 2020 when available
    census_2010 <- lookup_census_2010[, .(reference, table_content, table_name)] %>%
        .[, census_2010 := "yes"] %>%
        .[!is.na(table_name)]
}
