[![Build Status](https://travis-ci.org/GL-Li/rawcensus2010.svg?branch=master)](https://travis-ci.org/GL-Li/rawcensus2010)

# Working with Census 2010 raw data

This package extracts data from the raw data of Census 2010 summary file 1 (with urban/rural update) and returns a tidy data.table. The extracted data can be further processed with `data.table` or `dplyr` packages. Users will be able to retrieve any data from the summary file 1 conveniently from their local computers.


## Why another R census package




## Download raw data

In order to use this package, the raw data need to be stored in your local computer. The 2010 census summary file 1 (with urban/rural update) can be downloaded from [United States Census Bureau official site](https://www2.census.gov/census_2010/04-Summary_File_1/Urban_Rural_Update/). The data is split into sub-folders of 50 states and DC. An additional sub-folder called "National/" holds summary data for the United States. Inside a sub-folder, for example, "Indiana/", there is a file named _in2010.ur1.zip_. Download this file and unzip it to a folder named with Indiana's abbriation, “IN”. Do this for all other states and DC. For the "National/" data, unzip the file to folder "US". You can just download the sub-folders you need. If you want download all of them, make sure you have enough disc space as the total file size is about 140 GB. 


## Installation
Install `devtools` package if you do not have it:  
```r
install.packages("devtools")
```   

Install `rawcensus2010` package:   
```r
devtools::install_github("GL-Li/rawcensus2010")
```

## Use the package
### Read census data
The working horse of the package is function `read_2010census()`. It read selected geographical headers and table contents and returns a data.table. An example is shown below. The return includes a column of LOGRECNO, which is also set as the key of the data.table. The selected headers and table contents are shown as other columns of the data.table. The data.table contains all 331,556 rows in the raw data. Each row corresponds to a geographic entity. 

```r
library(rawcensus)
tmp <- read_2010census(path_to_census = "your_local_path_census_data",  # under it are state sub-folders
                       state = "IN", 
                       geoheaders = c("PLACE", "COUSUB", "NAME", "SUMLEV", "GEOCOMP"),
                       table_contents = c("PCT012F139", "H0070016"))
print(tmp)

    #         LOGRECNO PLACE COUSUB                                NAME SUMLEV GEOCOMP PCT012F139 H0070016
    #      1:        1                                          Indiana    040      00       1539    42507
    #      2:        2                                          Indiana    040      01       1446    39815
    #      3:        3                                          Indiana    040      04       1257    34693
    #      4:        4                                          Indiana    040      28        189     5122
    #      5:        5                                          Indiana    040      43         93     2692
    #     ---                                                                                             
    # 331552:   331552                      Westview School Corporation    970      00          1       19
    # 331553:   331553                              Whiting School City    970      00          6      205
    # 331554:   331554              Whitko Community School Corporation    970      00          0       12
    # 331555:   331555                      School District Not Defined    970      00          0        0
    # 331556:   331556              School District Not Defined (Water)    970      00          0        0

```
The data of geographic entities you are interesting in can be further selected from this data.table. As an example, I will show how to select the data of census blocks of South Bend city. South Bend is a PLACE with FIPS code "71000" and the summary level (SUMLEV) code of census block is "100". We can do the selection in `data.table` package or using `dplyr` package.

```r
# using data.table
library(data.table)
tmp[PLACE == "71000" & SUMLEV == "100"]

    #       LOGRECNO PLACE COUSUB       NAME SUMLEV GEOCOMP PCT012F139 H0070016
    #    1:   241626 71000  11890 Block 2018    100      00         NA        0
    #    2:   241627 71000  11890 Block 2019    100      00         NA        0
    #    3:   241628 71000  11890 Block 2022    100      00         NA        0
    #    4:   241629 71000  11890 Block 2023    100      00         NA        2
    #    5:   241630 71000  11890 Block 2024    100      00         NA        0
    #   ---                                                                    
    # 5002:   252566 71000  61128 Block 2058    100      00         NA        0
    # 5003:   252567 71000  61128 Block 2064    100      00         NA        0
    # 5004:   253091 71000  80180 Block 4039    100      00         NA        0
    # 5005:   253092 71000  80180 Block 4040    100      00         NA        0
    # 5006:   253093 71000  80180 Block 4094    100      00         NA        0

# or using dplyr to get the same result
library(dplyr)
filter(tmp, PLACE == "71000" & SUMLEV == "100")
```

### search the codes
In the above example, many special codes and references are used in census data. Strings of upper case letters are for geographic headers, strings of mixed uppder case letters and numbers for table contents, strings of integers for summary level (SUMLEV), geographic components (GEOCOMP) and FIPS numbers of place (PLACE) and county subdivision (COUSUB). There are many more other codes and references; they are nightmare for census data users.

The _rawcensus2010_ package provides a few functions to search for these codes. These functions are named as `search_xxxx()`. In above example, we used PLACE FIPS code and summary level code to select rows of census blocks of South Bend. The search functions can help to find the codes, as shown blow.

```r
# find the FIPS code of South Bend. PLACE == "71000" is for South Bend in Indiana
search_fips("indiana south bend")

    #    state_full state STATE SUMLEV COUNTY COUSUB PLACE CONCIT            NAME
    # 1:    Indiana    IN    18    162    000  00000 71000  00000 South Bend city
    
# find summary level code of census block. The third is the census block with code "100"
search_sumlev("block")

    #                                                                                       summary_level code
    # 1:                         State-County-County Subdivision-Place/Remainder-Census Tract-Block Group  091
    # 2:             State-County-County Subdivision-Place/Remainder-Census Tract-Block Group-Urban/Rural  090
    # 3:       State-County-County Subdivision-Place/Remainder-Census Tract-Block Group-Urban/Rural-Block  100
    # 4:                                                            State-County-Census Tract-Block Group  150
    # 5: State-County-Census Tract-Block Group-American Indian Area/Alaska Native Area/Hawaiian Home Land  154
```

The most useful search function is `search_datafile()`, which searchs for table contents to be used in `read_2010census()`. For example, we can search table contents for asian population in prison with `search_datafile("prison asian population")`. In the search results, the references of table contents, "PCT020D005" and "PCT020D006", are for asian population in federal and state prisons respectively. They can be fed to `read_2010census()` as argument `table_contents = c("PCT020D005", "PCT020D006")`. 

```r
search_datafile("prison asian population")

    #    file_segment               table_content  reference table_number                                                     table_name   universe
    # 1:           37 ----- Federal prisons (102) PCT020D005       PCT20D GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (ASIAN ALONE)    ... ...
    # 2:           37   ----- State prisons (103) PCT020D006       PCT20D GROUP QUARTERS POPULATION BY GROUP QUARTERS TYPE (ASIAN ALONE)    ... ...
```
