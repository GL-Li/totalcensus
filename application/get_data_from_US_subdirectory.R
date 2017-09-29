library(data.table)
library(magrittr)
library(ggplot2)
library(ggmap)
library(rawcensus2010)

# example 1: population in the US ================================

path <- "~/dropbox_datasets/US_2010_census/"

geo_us <- read_geoheader(path, "US", c("STATE", "COUSUB", "PLACE", "NAME", "SUMLEV", "INTPTLON", "INTPTLAT", "GEOCOMP"))

popl <- read_censustable(path, "US", "p1") %>%
    setnames("P0010001", "population")

pop_us <- geo_us[popl, on = .(LOGRECNO)]

# sum by county and sounty subdivision are the same but more than total in the US
# which is 308745538
county <- pop_us[SUMLEV == "050" & GEOCOMP == "00"]
county_sub <- pop_us[SUMLEV == "060" & GEOCOMP == "00"]

county[, sum(population)]
    # 312471327
county_sub[, sum(population)]
    # 312471327

# Puerto Rico population not included as US population
pr <- pop_us[STATE == "72"][1, population]
    # 3725789
# 308745538 + 3725789 = 312471327

# in pop_us, each place has three rows of values, corresponding to total, urban
# and rural
pop_total <- pop_us[GEOCOMP == "00"]
pop_urban <- pop_us[GEOCOMP == "01"]
pop_rural <- pop_us[GEOCOMP == "43"]

# "PLACE" (fips) is not unique to a place but "PLACENS" (ansi) is. So use PLACENS
# instead of PLACE
pop_total[, .N, by = PLACE][order(-N)]
pop_total[, .N, by = PLACENS][order(-N)]


# where are these places and population, show only population > 1000
us_map <- get_map(zoom = 4)
ggmap(us_map) +
    geom_point(data = pop_urban[population >= 1e3],
               aes(INTPTLON, INTPTLAT, size = population, color = "red"),
               alpha = 0.2) +
    geom_point(data = pop_rural[population >= 1e3],
               aes(INTPTLON, INTPTLAT, size = population, color = "blue"),
               alpha = 0.2) +
    scale_size_area(max_size = 4, breaks = c(1e3, 1e4, 1e5, 1e6, 8e6)) +
    scale_color_identity(guide = "legend", breaks = c("red", "blue"), label = c("urban", "rural")) +
    guides(size = guide_legend(override.aes = list(alpha = 1)),
           color = guide_legend(override.aes = list(alpha = 1)))



# Cities and towns named after Lincoln and their population ====================



# federal and state prison location and population =======================================
# search tables for fedral prison
search_datafile("prison")
    # PCT0200005 has the total population in federal prison
    # PCT0200006 has state prison population

# read prison data
pris_pop <- read_tablecontents(path, "US", c("PCT0200005", "PCT0200006")) %>%
    setnames(c("PCT0200005", "PCT0200006"), c("federal", "state")) %>%
    geo_us[.]

# selected summary levels
pris <- pris_pop %>%
    .[SUMLEV == "060"] %>%
    .[GEOCOMP == "00"] %>%
    .[federal != 0 | state != 0] %>%
    .[federal == 0, federal := NA] %>%
    .[state == 0, state := NA]

us_map <- get_map(zoom = 4)

ggmap(us_map) +
    geom_point(data = pris,
               aes(INTPTLON, INTPTLAT, size = federal, color = "red"),
               alpha = 0.5) +
    geom_point(data = pris,
               aes(INTPTLON, INTPTLAT, size = state, color = "blue"),
               alpha = 0.5) +
    scale_size_area(breaks = c(1000, 5000, 10000, 20000)) +
    scale_color_identity(guide = "legend",
                         breaks = c("red", "blue"),
                         labels = c("federal", "state")) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
    labs(color = NULL, size = "inmates")


ggplot() +
    geom_path(data = map_data("world"), aes(long, lat, group = group), color = "grey", size=0.5) +
    geom_path(data = map_data("state"), aes(long, lat, group = group), color = "grey", size=0.3) +
    geom_point(data = pris,
               aes(INTPTLON, INTPTLAT, size = state, color = "blue"),
               alpha = 0.5) +
    geom_point(data = pris,
               aes(INTPTLON, INTPTLAT, size = federal, color = "red"),
               alpha = 0.5) +
    scale_size_area(breaks = c(1000, 5000, 10000, 20000)) +
    scale_color_identity(guide = "legend",
                         breaks = c("red", "blue"),
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
race <- read_tablecontents(path, "US", c("P0030001", "P0030002", "P0030003")) %>%
    setnames(c("P0030001", "P0030002", "P0030003"), c("total", "white", "black")) %>%
    geo_us[.]

# state race in all, urban and rural areas and black percentage
state_race <- race[SUMLEV == "040" & GEOCOMP %in% c("00", "01", "43")] %>%
    .[, perc_black := round(100 * black / total, 2)]

# race in any city or town =====================================================
# lincoln town RI
race[STATE == "36" & PLACE == "51000" & SUMLEV == "160"]
