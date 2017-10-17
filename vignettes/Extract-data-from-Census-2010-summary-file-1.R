## ----echo=FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(eval = FALSE)

## ------------------------------------------------------------------------
#  library(rawcensus2010)
#  library(data.table)
#  library(magrittr)
#  library(ggplot2)
#  library(dplyr)
#  
#  # search for the table for race population
#  search_datafile("race population")

## ------------------------------------------------------------------------
#  # This is the path to the census data in my computer, change to yours
#  path <- "~/dropbox_datasets/US_2010_census/"
#  race_popl <- read_2010census(path, "US",
#                               c("STATE", "NAME"),
#                               c("P0030002", "P0030003", "P0030005")) %>%
#      # rename the table contents to "white", "black", and "asian". From here the data
#      # can be processed with data.table or dplyr package
#      setnames(c("P0030002", "P0030003", "P0030005"), c("white", "black", "asian")) %>%
#      # summary level for state is "040" and geographic component for all area is "00"
#      .[SUMLEV == "040" & GEOCOMP == "00"] %>%
#      # keep columns we care
#      .[, .(NAME, white, black, asian)]
#  head(race_popl)

## ----eval----------------------------------------------------------------
#  race_popl <- read_2010census(path, "US",
#                               c("STATE", "NAME", "SUMLEV", "GEOCOMP"),
#                               c("P0030002", "P0030003", "P0030005")) %>%
#      rename(white = P0030002, black = P0030003, asian = P0030005) %>%
#      filter(SUMLEV == "040" & GEOCOMP == "00") %>%
#      select(c("NAME", "white", "black", "asian"))
#  

## ------------------------------------------------------------------------
#  search_datafile("prison population")

## ------------------------------------------------------------------------
#  prison <- read_2010census(path, "US",
#                            c("INTPTLON", "INTPTLAT"),
#                            c("PCT0200005", "PCT0200006")) %>%
#      setnames(c("INTPTLON", "INTPTLAT", "PCT0200005", "PCT0200006"),
#               c("lon", "lat", "federal", "state")) %>%
#      # summary level of country subdivision is "060", geocomponent of all is "00"
#      .[SUMLEV == "060" & GEOCOMP == "00"] %>%
#      # keep columns of interests
#      .[, .(lon, lat, federal, state)] %>%
#      # remove the places having no prison
#      .[federal != 0 | state != 0] %>%
#      .[federal == 0, federal := NA] %>%
#      .[state == 0, state := NA]
#  head(prison)

## ------------------------------------------------------------------------
#  ggplot() +
#      geom_path(data = map_data("world"), aes(long, lat, group = group), color = "grey", size=0.5) +
#      geom_path(data = map_data("state"), aes(long, lat, group = group), color = "grey", size=0.3) +
#      geom_point(data = prison,
#                 aes(lon, lat, size = state, color = "blue"),
#                 alpha = 0.5) +
#      geom_point(data = prison,
#                 aes(lon, lat, size = federal, color = "red"),
#                 alpha = 0.5) +
#      scale_size_area(max_size = 4, breaks = c(1000, 5000, 10000, 20000)) +
#      scale_color_identity(guide = "legend",
#                           breaks = c("red", "blue"),
#                           labels = c("federal", "state")) +
#      guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
#      labs(color = NULL, size = "inmates", x = NULL, y = NULL) +
#      xlim(-180, -60) +
#      ylim(15, 66) +
#      coord_map() +
#      theme(panel.grid = element_blank(),
#            axis.text = element_blank(),
#            axis.ticks = element_blank())

