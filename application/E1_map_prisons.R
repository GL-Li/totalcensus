# load packages
# follow instruction on https://github.com/GL-Li/totalcensus to setup
# totalcensus package
library(totalcensus)
library(tidyverse)
library(leaflet)

# federal and state prison location and population
# search tables for tables of prison population
# search_tablecontents("decennial")
# PCT0200005 for federal prison
# PCT0200006 for state prison
# PCT0200007 for local jail

# read prison population data and format data
pris_pop <- read_decennial(
    year = 2010,
    states = "US",
    table_contents = c(
        "total = PCT0200003",
        "fed_pris = PCT0200005",
        "state_pris = PCT0200006",
        "local_jail = PCT0200007"
    ),
    geo_headers = "NAME",
    summary_level = "county subdivision",
    show_progress = FALSE
) %>%
    # remove county subdivisions that has no prison popualation
    filter(fed_pris != 0 | state_pris != 0 | local_jail != 0) %>%
    mutate(fed_pris = ifelse(fed_pris == 0, NA, fed_pris)) %>%
    mutate(state_pris = ifelse(state_pris == 0, NA, state_pris)) %>%
    mutate(local_jail = ifelse(local_jail == 0, NA, local_jail)) %>%
    select(lon, lat, NAME, fed_pris, state_pris, local_jail) %>%
    gather(key = "type", value = "inmates", -c(lon, lat, NAME)) %>%
    filter(!is.na(inmates)) %>%
    mutate(type = factor(
        type, levels = c("local_jail", "state_pris", "fed_pris")
    )) %>%
    arrange(-inmates)

# format color for leaflet map
pal <- colorFactor(
    palette = c("green", "blue", "red"),
    domain = pris_pop$type
)

# make interactive map
leaflet(pris_pop) %>%
    fitBounds(-122, 25, -70, 50) %>%
    addTiles() %>%
    addCircles(lng = ~lon, lat = ~lat, radius = ~inmates,
               color = ~pal(type), popup = ~paste(inmates, "inmates,", NAME)) %>%
    addLegend(position = "bottomleft", pal = pal, values = ~type,
              title = "")
