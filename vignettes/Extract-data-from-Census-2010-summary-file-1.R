## ----eval=FALSE----------------------------------------------------------
#  library(rawcensus2010)
#  library(data.table)
#  library(magrittr)
#  library(ggplot2)
#  library(dplyr)
#  
#  # search for the table for race population
#  search_datafile("race population")

## ----eval=FALSE----------------------------------------------------------
#  # This is the path to the census data in my computer, change to yours
#  path <- "~/dropbox_datasets/US_2010_census/"
#  race_popl <- read_2010census(path, "US",
#                               c("STATE", "NAME", "SUMLEV", "GEOCOMP"),
#                               c("P0030002", "P0030003", "P0030005")) %>%
#      # rename the table contents to "white", "black", and "asian". From here the data
#      # can be processed with data.table or dplyr package
#      setnames(c("P0030002", "P0030003", "P0030005"), c("white", "black", "asian")) %>%
#      # summary level for state is "040" and geographic component for all area is "00"
#      .[SUMLEV == "040" & GEOCOMP == "00"] %>%
#      # keep columns we care
#      .[, .(NAME, white, black, asian)]
#  head(race_popl)

## ----eval=FALSE----------------------------------------------------------
#  race_popl <- read_2010census(path, "US",
#                               c("STATE", "NAME", "SUMLEV", "GEOCOMP"),
#                               c("P0030002", "P0030003", "P0030005")) %>%
#      rename(white = P0030002, black = P0030003, asian = P0030005) %>%
#      filter(SUMLEV == "040" & GEOCOMP == "00") %>%
#      select(c("NAME", "white", "black", "asian"))
#  

