<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/GL-Li/totalcensus.svg?branch=master)](https://travis-ci.org/GL-Li/totalcensus)

Extract data from summary files of Decennial Census and American Community Survey
=================================================================================

The `totalcensus` package extracts any data available in the raw summary files of decennial census and American Community Survey (ACS) and returns a tidy data.table that can be easily processed with `data.table` or `dplyr` packages.

Update
------

**1/17/2018**: Please reinstall the updated version. You do not need to re-download census data, but need to update generated data by running

``` r
download_generated_data()
```

Why another R census package
----------------------------

The [census API](https://www.census.gov/data/developers/guidance/api-user-guide.Available_Data.html) offers most data in decennial census and ACS data for download and API-based packages such as `tidycensus`, `censusapi` and `acs` make the downloading very convenient in R. So why we need another package?

Package `totalcensus` makes census data extraction easier and gives you full access and control of all census data downloaded to you own computer. It is particularly convenient to extract higher spatial resolution data at census tract, block group, and block level.

As an extreme example, here is how we extract the median home values in **all** block groups in the United States from 2011-2015 ACS 5-year survey with this package. You simply need to call the function `read_acs5year()`. It takes 15 seconds for my 4-years old laptop to return the data of all 217,739 block groups. In addition to the table contents we request, we also get the population and coordinate of each block group.

``` r
library(totalcensus)
home_national <- read_acs5year(
    year = 2015,
    states = states_DC,   # all 50 states plus DC
    table_contents = "home_value = B25077_001",
    summary_level = "block group"
)
```

With the coordinates, we can visualize the data on US map with `ggplot2` and `ggmap`. Each data point in the figure below corresponds to a a block group, colored by median home value and sized by population. This plot not only displays the median home values, but also tells population densities on the map.

![](figures/home_value_national_blockgroup.png)

There are additional benefits of using this package:

-   You can get detailed urban/rural data from Census 2010. This package use summary file 1 with urban/rural update, while the census API only provide data in summary file 1 before urban/rural update.
-   You can get all block groups that belong or partially belong to a city. Original census data do not provide city information for a block group as a block group may not uniquely belong to a city. However, large cities have most block groups within their boundaries and only a small number of block groups run across the borders. The block group level data provide valuable spatial information of a city. This is particularly helpful for ACS 5-year surveys which cover data down to the level of block groups.
-   It provides longitude and latitude of the internal point of a geographic area for easy and quick mapping. You do not always need shape files to make nice maps, as in the map shown above.

The current version extract data from summary files of:

-   2010 decennial census
-   2012-2016 ACS 5-year survey
-   2011-2015 ACS 5-year survey
-   2016 ACS 1-year survey
-   2015 ACS 1-year survey
-   2014 ACS 1-year survey

More will be added in later version.

### More reading materials

-   [Extract US Census 2010 data with data.table and dplyr](https://gl-li.netlify.com/2017/08/29/process-2010-census-data-with-data-table/): this post explains how the summary files of decennial census 2010 are processed under the hood in `totalcensus` package.
-   [Proccess 5-digit ZIP Code Tabulation Area (ZCTA5) data with totalcensus package](https://gl-li.netlify.com/2017/12/23/census_data_zip_code/): an application example.
-   [Using totalcensus package to determine relationship between census geographic entities](https://gl-li.netlify.com/2017/12/28/use-totalcensus-package-to-determine-relationship-between-geographic-entities/); an application example.

Installation and setup
----------------------

### Installation

``` r
install.packages("devtools")

devtools::install_github("GL-Li/totalcensus")
```

### Setup

This package requires downloading census data and you need to create a folder to store the downloaded data. Let's call the folder `my_census_data` and assume the full path to this folder is `xxxxx/my_census_data`. Run the function below to set the path for the package.

``` r
set_path_to_census("xxxxx/my_census_data", install = TRUE)
```

Basic application
-----------------

The package has three functions to read decennial census, ACS 5-year survey, and ACS 1-year survey: `read_decennial()`, `read_acs5year()`, and `read_acs1year()`. They are similar but as these datasets are so different, we prefer to keep three separate functions, one for each.

The function arguments serve as filters to select the data you want:

-   year: the year or ending year of the decennial census or ACS survey.
-   states: the states of which you want read geography and data files. In addition to 50 states and "DC", you can choose from "PR" (Puerto Rico), plus a special one "US" for national files.
-   table\_contents: this parameter specifies which table contents you want to read. Population is always returned even if table\_contents is NULL. Users can name the table contents in the format such as `c("male = B01001_002", "female = B01001_026")`.
-   areas: if you know which metropolitan areas, counties, cities and towns you want to get data from, you can specify them here by name or FIPS code, for example, `c("New York metro", "PLACE = UT62360", "Salt Lake City city, UT")`.
-   geo\_headers: In case you do not know which areas to extract data, you can read all the geographic headers specified here and select areas after reading.
-   summary\_level: it determines which summary level data to extract. Common ones like "state", "county", "place", "county subdivision", "tract", "block group", and "block" can be input as plain text. Others have to be given by code.
-   geo\_comp: specifies data of which geographic component you want. Most common ones are "all", "urban", "urbanized area", "urban cluster", and "rural". Others are provided by code.

There are a family of `search_xxx()` functions to help find table contents, geoheaders, summary levels, geocomponents, FIPS codes and CBSA codes.

The following examples demonstrate how to use these `read_xxx()` and `search_xxx()` functions.

### Median gross rent in cities with population over 65000

A property management company wants to know the most recent rents in major cities in the US. How to get the data?

We first need to determine which survey to read. For most recent survey data, we want to read 2016 ACS 1-year surveys, which provide data for geographic areas with population over 65000.

We also need to determine which data files to read. We know summary level of cities is "160" or "place". Browsing with `search_summarylevels("acs")`, we see that this summary level is only in state files of ACS 1-year survey. So we have to read all of the state files.

Then we need to check if 2016 ACS 1-year survey has the rent data. We run `search_tablecontents("acs")` to open the dataset with `View()` in RStudio. You can provide keywords to search in the function but it is better to do the search in RStudio with filters. There are so many tables that contains string "rent". It takes some time to find the right one if you are not familiar with ACS tables. After some struggle, we think B25064\_001 is what we want.

We do not need to specify `areas` and `geo_headers` as we are extracting all geographic areas matches the conditions.

Below is the code that gives what we want. The first time you use `read_xxx` functions to read data files, you will be asked to download summary files required for this function call, in this case, 2016 ACS 1-year summary files. Choose 1 to continue. While you can download data during calling the `read_xxx` function, I recommend downloading all data you want with `download_census` function beforehand. The downloading takes time, which can be done when you are sleeping. The details is discussed in section of [Downloading data](#downloading-data).

``` r
rent <- read_acs1year(
    year = 2016,
    states = states_DC,
    table_contents = "rent = B25064_001",
    summary_level = "place",
    geo_comp = "all"
) 

# Fisrt 5 rows
  #             GEOID        lon      lat state population rent GEOCOMP SUMLEV                                  NAME
  # 1: 16000US1150000  -77.01709 38.90415    DC     681170 1376     all    160 Washington city, District of Columbia
  # 2: 16000US2829700  -89.07184 30.41606    MS      72077  774     all    160            Gulfport city, Mississippi
  # 3: 16000US2836000  -90.21282 32.31583    MS     169141  813     all    160             Jackson city, Mississippi
  # 4: 16000US2205000  -91.12590 30.44845    LA     227707  818     all    160           Baton Rouge city, Louisiana
  # 5: 16000US2208920  -93.65559 32.52366    LA      68485  959     all    160          Bossier City city, Louisiana
```

It is always nice to visualize them on a map.

``` r
library(ggplot2)
library(ggmap)
us_map <- get_map("united states", zoom = 4, color = "bw")

ggmap(us_map) + 
    geom_point(
        data = rent[order(-population)],
        aes(lon, lat, size = population/1e3, color = rent)
    ) +
    ylim(25, 50) +
    scale_size_area(breaks = c(100, 200, 500, 1000, 2000, 5000)) +
    scale_color_continuous(low = "green", high = "red") +
    labs(
        color = "monthly\nrent ($)",
        size = "total\npopulation\n(thousand)",
        title = "Monthly rent in cities over 65000 population",
        caption = "Source: 2016 ACS 1-year survey"
    ) +
    theme(
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        title = element_text(size = 14)
    )
```

![](figures/rent_cities_population_over_65000.png)

### Black communities in South Bend city, IN at census block level

Only the decennial census has data down to block level. The most recent one is Census 2010. We will use two method to extract the data.

#### Method 1: using argument `areas`

Knowing names of a city, county, metro area, or town, we can feed them directly to argument `areas`. The returned data.table contains the table contents we want to read as well as population and coordinates. The reading takes a few seconds.

``` r
# read data of black population in each block
black_popul <- read_decennial(
    year = 2010,
    states = "IN",
    table_contents = "black_popul = P0030003",
    areas = "South Bend city, IN",
    summary_level = "block"
)

# first 5 rows of black_popul:
   #                   area       lon      lat state population black_popul GEOCOMP SUMLEV
   # 1: South Bend city, IN -86.21864 41.63613    IN         28          10     all    100
   # 2: South Bend city, IN -86.21659 41.63670    IN          0           0     all    100
   # 3: South Bend city, IN -86.22172 41.63573    IN         52          16     all    100
   # 4: South Bend city, IN -86.22022 41.63182    IN        279          21     all    100
   # 5: South Bend city, IN -86.22093 41.63367    IN         42           1     all    100
```

It is better to separate data manipulation from reading to save reading time as you usually need to try multiple manipulation. Data manipulation can be done with `data.table` or `dplyr`.

``` r
# remove blocks where no people lives in and add a column of black percentage. 
black <- black_popul %>%
    .[population != 0] %>%
    # percentage of black population in each block
    .[, black_pct := round(100 * black_popul / population, 2)]
```

#### Method 2: use argument `geo_headers`

Argument `geo_headers` works for any geographic areas presented in census geography files, such as American Indian Area/Alaska Native Area/ Hawaiian Home Land and congressional district, which are not covered by argument `areas`.

Here is how we read black population in each block in South Bend city, using `geo_headers`. A city is a census place with geographic header "PLACE". We first read all block level data for all "PLACE":

``` r
black_popul_place <- read_decennial(
    year = 2010,
    states = "IN",
    table_contents = "black_popul = P0030003",  
    geo_headers = "PLACE",
    summary_level = "block"
)
```

We can then select rows of South Bend by the area name or by PLACE code. The PLACE code of South Bend can be searched with `search_fips("south bend")`, which is "71000".

``` r
# select by name
black_popul <- black_popul_place[area %like% "South Bend"]

# select by code
black_popul <- black_popul_place[PLACE == "71000"]
```

#### Visualize the data

Again we visualize percentage of black population on map with `ggplot2` and `ggmap`.

``` r
south_bend <- get_map("south bend, IN", zoom = 13, color = "bw")
ggmap(south_bend) +
    geom_point(
        data = black,
        aes(lon, lat, size = population, color = black_pct)
    ) +
    scale_size_area(breaks = c(10, 100, 200, 500)) +
    scale_color_continuous(low = "green", high = "red") +
    labs(
        color = "% black",
        size = "total\npopulation",
        title = "Black communities in South Bend city at block level",
        caption = "Source: Census 2010"
    ) +
    theme(
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        title = element_text(size = 14)
    )
```

![](figures/south_bend_block_black.png)

Downloading data
----------------

This package requires downloading census data to your local computer. You can choose to download only the data you want. For example, if you care only the 2015 ACS 5-year survey data in the states of Massachusetts and New Hampshire, you can just download data of these two states, which takes a couple of GB disc space. Or you can download data of all most recent decennial Census, most recent ACS 5-year survey, and most recent ACS 1-year survey. You need about 200 GB free disc space to download them. The downloaded data will be extracted automatically to the folder `my_census_data`.

It takes a long time to download and extract. You can stop downloading any time and then resume downloading by running the function again. I recommend downloading all recent decennial and ACS data when you are sleeping. A lot of fun to play with these data. Re-run the function to repair possible mistakes during download and extraction.

A set of data generated from Census 2010 will also be downloaded, which is used to fill missing geographic header records in ACS data.

Below are a few examples of data downloading.

``` r
# download 2016 ACS 5-year survey data of MA and NH, a couple of GB
download_census(survey = "acs5year", year = 2016, states = c("MA", "NH"))

# download 2016 ACS 5-year survey of all states, ~52 GB
download_census("acs5year", 2016)

# download 2016 ACS 1-year survey. It is only 1.5 GB, so download for all states
download_census("acs1year", 2016)

# download decennial census 2010, of state California, the largest state, 8.4 GB
download_census("decennial", 2010, "CA")

# download most recent datasets, ~200G in total, including 2010 decennial census
# 2016 ACS 5-year survey, 2016 ACS 1-year survey.
download_census()
```

The census data can be found on Census Bureau's website but you do not need to download them manually. Use the function above.

-   [Census 2010 summary file 1 with urban/rural update](https://www2.census.gov/census_2010/04-Summary_File_1/Urban_Rural_Update/)
-   [ACS summary files since 2005](https://www2.census.gov/programs-surveys/acs/summary_file/)
