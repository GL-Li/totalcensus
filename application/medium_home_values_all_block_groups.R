library(rawcensus)
library(data.table)
library(magrittr)
library(ggplot2)
library(ggmap)


# national home value ==========================================================
home_national <- read_acs5year(states = states_DC,
                                   year = 2015,
                                   table_contents = c("B01003_001", "B25077_001"),
                                   summary_level = "block_group",
                                   with_margin = FALSE) %>%
    setnames(c("B01003_001_e", "B25077_001_e"), c("population", "value")) %>%
    # some missing value in home value shown as "." and so the whole column was
    # read into character. change column back to numeric and remove NAs
    .[, value := as.numeric(value)] %>%
    .[!is.na(value)] %>%
    .[order(value)]


us_map <- get_map("united states", zoom = 4, color = "bw")

ggmap(us_map) +
    geom_point(data = home_national,
               aes(lon, lat, size = population, color = value),
               alpha = 1) +
    ylim(25, 50) +
    scale_size_area(max_size = 0.1) +
    scale_color_continuous(low = "green", high = "red",
                           breaks = c(100000, 500000, 1000000, 1500000, 2000000),
                           labels = scales::unit_format("K", 1e-3)) +
    guides(size = FALSE) +
    labs(color = "value ($)",
         caption = "Data source: ACS 5-year survey 2011-2015",
         title = "Median Home Values in Each Block Group") +
    theme(panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.92, 0.2))

ggsave(filename = "application/national_home_value.png", width = 9, height = 6)


# New York metro home value ==========================================================
home_nymetro <- read_acs5year(states = c("NY", "NJ", "PA"),
                               year = 2015,
                              geo_headers = "CBSA",
                               table_contents = c("B01003_001", "B25077_001"),
                               #summary_level = "block_group",
                               with_margin = FALSE) %>%
    .[CBSA == "35620"] %>%
    setnames(c("B01003_001_e", "B25077_001_e"), c("population", "value")) %>%
    # some missing value in home value shown as "." and so the whole column was
    # read into character. change column back to numeric and remove NAs
    .[, value := as.numeric(value)] %>%
    .[!is.na(value)] %>%
    .[order(value)]


ny_map <- get_map("new york city", zoom = 9, color = "bw")

ggmap(ny_map) +
    geom_point(data = home_nymetro,
               aes(lon, lat, size = population, color = value),
               alpha = 1) +
    # ylim(40.5, 41) +
    # xlim(-74.6, -73.3) +
    scale_size_area(max_size = 0.5) +
    scale_color_continuous(low = "green", high = "red",
                           breaks = c(100000, 500000, 1000000, 1500000, 2000000),
                           labels = scales::unit_format("K", 1e-3)) +
    guides(size = FALSE) +
    labs(color = "value ($)",
         caption = "Data source: ACS 5-year survey 2011-2015",
         title = "Median Home Values in Each Block Group") +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.92, 0.2))

ggsave(filename = "application/nymetro_home_value.png", width = 9, height = 6)

