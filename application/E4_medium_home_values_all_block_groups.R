library(totalcensus)
library(data.table)
library(magrittr)
library(ggplot2)
library(ggmap)
goog_api <- Sys.getenv("GGMAP_GOOGLE_API_KEY")
register_google(goog_api)

# # national home value ==========================================================
home_national <- read_acs5year(
    year = 2018,
    states = states_DC,
    # all 50 states plus DC
    table_contents = "home_value = B25077_001",
    summary_level = "block group"
) %>%
    .[!is.na(home_value)] %>%
    .[order(home_value)]


us_map <- get_map("united states", zoom = 4, color = "bw")

ggmap(us_map) +
    geom_point(
        data = home_national,
        # displace higher values
        aes(lon, lat, size = population, color = home_value),
        alpha = 1
    ) +
    ylim(25, 49) +
    scale_size_area(max_size = 0.05) +
    scale_color_continuous(
        low = "#00CCFF",
        high = "red",
        breaks = c(100000, 500000, 1000000, 1500000, 2000000),
        labels = scales::unit_format(unit = "K", scale = 1e-3)
    ) +
    guides(size = FALSE) +
    labs(color = "value ($)",
         caption = "Source: 2014-2018 ACS 5-year estimate",
         title = "Median Home Values in Each Block Group") +
    theme(
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.928, 0.197)
    )

ggsave(filename = "application/national_home_value.png",
       width = 9, dpi = 600)


# New York home value ==========================================================
home_ny <- read_acs5year(
    year = 2016,
    states = "NY",
    table_contents = "home_value = B25077_001",
    areas = "New York city, NY",
    summary_level = "block group",
    with_margin = FALSE
) %>%
    .[!is.na(home_value)] %>%
    .[order(home_value)]


home_ny <- read_acs5year(
    year = 2016,
    states = "NY",
    table_contents = "home_value = B25077_001",
    areas = "New York metro",
    summary_level = "block group",
    with_margin = FALSE
) %>%
    .[!is.na(home_value)] %>%
    .[order(home_value)]


ny_map <- get_map("new york city", zoom = 11, color = "bw")

ggmap(ny_map) +
    geom_point(data = home_ny,
               aes(lon, lat, size = population, color = home_value),
               alpha = 1) +
    # ylim(40.5, 41) +
    # xlim(-74.6, -73.3) +
    scale_size_area(max_size = 0.5) +
    scale_color_continuous(
        low = "green",
        high = "red",
        breaks = c(100000, 500000, 1000000, 1500000, 2000000),
        labels = scales::unit_format(unit = "K", scale = 1e-3)
    ) +
    guides(size = FALSE) +
    labs(color = "value ($)",
         caption = "Source: 2016 ACS 5-year estimate",
         title = "Median Home Values in Each Block Group") +
    theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.92, 0.2)
    )

ggsave(filename = "application/nymetro_home_value.png",
       width = 9,
       height = 6)
