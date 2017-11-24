library(data.table)
library(magrittr)
library(ggplot2)
library(ggmap)
library(rawcensus)

# example 1: population in the US ================================

pop_us <- read_decennial(
    year = 2010,
    states = "US",
    geo_headers = c("STATE", "COUNTY", "COUSUB", "PLACE", "NAME")
)

# total, urban, and rural population in each state
state_pop <- pop_us[SUMLEV == "040" & GEOCOMP %in% c("00", "01", "43")]

# sum by county and county subdivision are the same but more than total in the US
# which is 308745538
county <- pop_us[SUMLEV == "050" & GEOCOMP == "00"]
county_sub <- pop_us[SUMLEV == "060" & GEOCOMP == "00"]

county[, sum(population)]
    # 312471327
county_sub[, sum(population)]
    # 312471327

# Puerto Rico population not included as US population
pr <- pop_us[state == "PR"][1, population]
    # 3725789
# 308745538 + 3725789 = 312471327

# in pop_us, each county subdivision has three rows of values, corresponding to total, urban
# and rural
pop_total <- pop_us[GEOCOMP == "00" & SUMLEV == "060" & population > 0]
pop_urban <- pop_us[GEOCOMP == "01" & SUMLEV == "060" & population > 0]
pop_rural <- pop_us[GEOCOMP == "43" & SUMLEV == "060" & population > 0]


# where are these places and population, show only population > 1000
us_map <- get_map(zoom = 4, color = "bw")

ggmap(us_map) +
    geom_point(data = pop_rural,
               aes(lon, lat, size = population, color = "green"),
               alpha = 0.2) +
    geom_point(data = pop_urban,
               aes(lon, lat, size = population, color = "red"),
               alpha = 0.2) +
    ylim(25, 50) +
    scale_size_area(max_size = 4, breaks = c(1e3, 1e4, 1e5, 1e6, 3e6)) +
    scale_color_identity(guide = "legend", breaks = c("red", "green"), label = c("urban", "rural")) +
    guides(size = guide_legend(override.aes = list(alpha = 1)),
           color = guide_legend(override.aes = list(alpha = 1)))

ggsave(filename = "application/urban_rural_population_county_subdivision.png",
       width = 9, height = 6)


# Cities and towns named after Lincoln and their population ====================



# federal and state prison location and population =============================
# search tables for fedral prison
search_tablecontents("census", "prison")
    # PCT0200005 has the total population in federal prison
    # PCT0200006 has state prison population

# read prison data
pris_pop <- read_decennial(
    year = 2010,
    states = "US",
    table_contents = c("PCT0200005", "PCT0200006"),
    summary_level = "county subdivision",
    geo_comp = "00"
) %>%
    setnames(c("PCT0200005", "PCT0200006"), c("federal_pris", "state_pris")) %>%
    .[federal_pris != 0 | state_pris != 0] %>%
    .[federal_pris == 0, federal_pris := NA] %>%
    .[state_pris == 0, state_pris := NA]


ggmap(us_map) +
    geom_point(data = pris_pop,
               aes(lon, lat, size = state_pris, color = "green"),
               alpha = 0.5) +
    geom_point(data = pris_pop,
               aes(lon, lat, size = federal_pris, color = "red"),
               alpha = 0.5) +
    ylim(25, 50) +
    scale_size_area(breaks = c(1000, 5000, 10000, 20000)) +
    scale_color_identity(guide = "legend",
                         breaks = c("red", "green"),
                         labels = c("federal", "state")) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
    labs(color = NULL, size = "inmates") +
    theme(panel.background = element_blank())


ggplot() +
    geom_path(data = map_data("world"), aes(long, lat, group = group), color = "grey", size=0.5) +
    geom_path(data = map_data("state"), aes(long, lat, group = group), color = "grey", size=0.3) +
    geom_point(data = pris_pop,
               aes(lon, lat, size = state_pris, color = "green"),
               alpha = 0.5) +
    geom_point(data = pris_pop,
               aes(lon, lat, size = federal_pris, color = "red"),
               alpha = 0.5) +
    scale_size_area(max_size = 4, breaks = c(1000, 5000, 10000, 20000)) +
    scale_color_identity(guide = "legend",
                         breaks = c("red", "green"),
                         labels = c("federal", "state")) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
    labs(color = NULL, size = "inmates") +
    xlim(-180, -60) +
    ylim(15, 66) +
    coord_map()


# total fed prison population
pris[, sum(federal, na.rm = TRUE)]
    # 173314 which includes 1294 from Puerto Rico (STATE == "72") while pure
    # national count 172020 does not include it



# race in each state - a quick extraction =====================================
race <- read_decennial(
    year = 2010,
    states = "US",
    table_contents = c("P0030001", "P0030002", "P0030003"),
    summary_level = "state"
)  %>%
    setnames(c("P0030001", "P0030002", "P0030003"), c("total", "white", "black")) %>%
    # state race in all, urban and rural areas and black percentage
    .[GEOCOMP %in% c("00", "01", "43")] %>%
    .[, .(state, total, white, black, GEOCOMP)] %>%
    .[, perc_black := round(100 * black / total, 2)]

# race in selected cities =====================================================
city_race <- read_decennial(
    year = 2010,
    states = "US",
    table_contents = c("P0030001", "P0030002", "P0030003"),
    areas = c("New York city, NY",
              "Boston city, MA",
              "Chicago city, IL",
              "Houston city, TX",
              "Los Angeles city, CA",
              "Atlanta city, GA"),
    summary_level = "place",
    geo_comp = "00"
) %>%
    setnames(c("P0030001", "P0030002", "P0030003"),  c("total", "white", "black")) %>%
    .[, .(area, total, white, black)] %>%
    .[, perc_black := round(100 * black / total, 2)]



# compare cities and towns named after Lincoln and Washington ==================
get_city_town <- function(name){
    name <- tolower(name)
    us <- read_decennial(
        year = 2010,
        states = "US",
        geo_headers = c("NAME"),
        geo_comp = "00"
    )
    # both city and town can be in summary level "060" and "160", include all
    # and the remove duplicate
    city1 <-  us[SUMLEV == "160"] %>%
        .[tolower(NAME) %like% paste(name, "city")]

    city2 <-  us[SUMLEV == "060"] %>%
        .[tolower(NAME) %like% paste(name, "city")]

    town1 <- us[SUMLEV == "160"] %>%
        .[tolower(NAME) %like% paste(name, "town")]

    town2 <- us[SUMLEV == "060"] %>%
        .[tolower(NAME) %like% paste(name, "town")]

    city_town <- rbindlist(list(city1, city2, town1, town2)) %>%
        .[, ":=" (SUMLEV = NULL, GEOCOMP = NULL)] %>%
        unique() %>%
        .[tolower(NAME) %like% "city", type := "city"] %>%
        .[tolower(NAME) %like% "town", type := "town"] %>%
        .[tolower(NAME) %like% "township", type := "township"]

    return(city_town)
}


lincoln <- get_city_town("lincoln")
washington <- get_city_town("washington")

us_map <- get_map(zoom = 4, color = "bw")
ggmap(us_map) +
    geom_point(data = lincoln, aes(lon, lat, size = population, color = "red"), alpha = 0.6) +
    geom_point(data = washington, aes(lon, lat, size = population, color = "green"), alpha = 0.6) +
    scale_color_identity(guide = "legend", label = c("red" = "Lincoln", "green" = "Washington")) +
    ylim(25, 50) +
    labs(color = NULL)
