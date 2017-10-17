# variable offered in API for 2010 census ======================================

# https://api.census.gov/data/2010/sf1/variables.html

library(mytoolbox)
library(magrittr)
library(data.table)
# large table, takes time to download
var <- download_table("https://api.census.gov/data/2010/sf1/variables.html",
                      '/html/body/table',
                      "./data_raw/API_variables_2010_census.csv") %>%
    # first row is strange, delete
    .[-1, 1:3] %>%
    setDT()


#=== does it have complete tables? ===#

tables <- var[Label != "GEO PLACE HOLDER"] %>%
    .[!Label %like% "API"] %>%
    .[!Label %like% "FIPS"] %>%
    .[!Concept %like% "Geographic Characteristics"]
# This tabel has 8912 rows of table contents while there are 8938 rows of table
# contents in dict_datafile.
dict_datafile[!is.na(table_name)]

# sf1 with urban/rural update adds two tables PCT23 and PCT24 which is not
# provided by the API
dict_datafile[table_number %like% "PCT23|PCT24"]


#=== what geographic headers included ===#

geoheaders <- var[Concept %like% "Geographic" | Label %like% "GEO"]
# It offers 56 geographic headers, much less than that used in sf1, which has
# nearly 100. In particular, it misses the important INTPTLON and INTPTLAT.

# These are not in API
setdiff(dict_geoheader$reference, geoheaders$Name)

