### compare selected table content of each year by eyeballing the numbers.
### There must be something wrong if the numbers differ too much.
### Use data from the large city Chicago to make sure noise does not affect
### data.

library(totalcensus)
library(magrittr)
library(data.table)
library(ggplot2)

# compare acs1year over years ==================================================
read1 <- function(year, table_content){
    read_acs1year(
        year = year,
        states = "IL",
        table_contents = table_content,
        areas = "Chicago city, IL",
        summary_level = "place",
        with_margin = TRUE
    )
}

compare_acs1year <- function(table_content){
    dt <- rbind(read1(2005, table_content),
                read1(2006, table_content),
                read1(2007, table_content),
                read1(2008, table_content),
                read1(2009, table_content),
                read1(2010, table_content),
                read1(2011, table_content),
                read1(2012, table_content),
                read1(2013, table_content),
                read1(2014, table_content),
                read1(2015, table_content),
                read1(2016, table_content),
                read1(2017, table_content)) %>%
        .[, year := c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
                      2014, 2015, 2016, 2017)]
    ggplot <- ggplot(dt, aes_string("year", table_content)) +
        geom_point() +
        geom_line() +
        ylim(0, max(dt[, get(table_content)]))
    print(ggplot)
    return(dt)
}

B01001I_001 <- compare_acs1year("B01001I_001")
B12006_001 <- compare_acs1year("B12006_001")
B20005I_001 <- compare_acs1year("B20005I_001")
B24040_001 <- compare_acs1year("B24040_001")
B25068_001 <- compare_acs1year("B25068_001")
C01001A_001 <- compare_acs1year("C01001A_001")
C27014_001 <- compare_acs1year("C27014_001")


# compare acs5year over years ==================================================
read5 <- function(year, table_content){
    read_acs5year(
        year = year,
        states = "IL",
        table_contents = table_content,
        areas = "Chicago city, IL",
        summary_level = "place"
    )
}

compare_acs5year <- function(table_content){
    dt <- rbind(read5(2009, table_content),
                read5(2010, table_content),
                read5(2011, table_content),
                read5(2012, table_content),
                read5(2013, table_content),
                read5(2014, table_content),
                read5(2015, table_content),
                read5(2016, table_content)) %>%
        .[, year := c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)]
    ggplot <- ggplot(dt, aes_string("year", table_content)) +
        geom_point() +
        geom_line() +
        ylim(0, max(dt[, get(table_content)]))
    print(ggplot)
    return(dt)
}

B01001I_001 <- compare_acs5year("B01001I_001")
B11004_001 <- compare_acs5year("B11004_001")
B12006_001 <- compare_acs5year("B12006_001")
B19325_001 <- compare_acs5year("B19325_001")
B20005I_001 <- compare_acs5year("B20005I_001")
B25010_001 <- compare_acs5year("B25010_001")
B25068_001 <- compare_acs5year("B25068_001")
B99084_001 <- compare_acs5year("B99084_001")


# census 2000 =================================================================
aaa = read_decennial(
    year = 2000,
    states = "RI",
    table_contents = c("urban = P0020002", "rural = P0020005"),
    areas = c("Providence city, RI",
        "cousub = ri41500"),
    summary_level = "*"
)



