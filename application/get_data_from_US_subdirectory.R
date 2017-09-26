library(data.table)
library(magrittr)
library(ggplot2)
library(ggmap)

# example 1: population of all places in the US ================================

path <- "~/dropbox_datasets/US_2010_census/"

geo_us <- read_geoheader(path, "US", c("STATE", "PLACE", "NAME", "SUMLEV", "PLACENS", "INTPTLON", "INTPTLAT", "GEOCOMP"))

popl <- read_censustable(path, "US", "p1") %>%
    setnames("P0010001", "population")

# population in each place
pop_us <- geo_us[popl, on = .(LOGRECNO)] %>%
    .[SUMLEV == "160"]

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
