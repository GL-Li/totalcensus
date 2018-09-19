### compare selected table content of each year by eyeballing the numbers.
### There must be something wrong if the numbers are differ too much.
### Use data from the large city Chicago to make sure noise does not affect
### data.

library(totalcensus)
library(magrittr)
library(data.table)

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
    rbind(read1(2010, table_content),
          read1(2014, table_content),
          read1(2015, table_content),
          read1(2016, table_content),
          read1(2017, table_content))
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
        summary_level = "place",
        with_margin = TRUE
    )
}

compare_acs5year <- function(table_content){
    rbind(read5(2010, table_content),
          read5(2015, table_content),
          read5(2016, table_content))
}

B01001I_001 <- compare_acs1year("B01001I_001")
B12006_001 <- compare_acs1year("B12006_001")
B20005I_001 <- compare_acs1year("B20005I_001")
B24040_001 <- compare_acs1year("B24040_001")
B25068_001 <- compare_acs1year("B25068_001")
C01001A_001 <- compare_acs1year("C01001A_001")
C27014_001 <- compare_acs1year("C27014_001")
