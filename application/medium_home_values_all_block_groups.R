library(totalcensus)
library(data.table)
library(magrittr)
library(ggplot2)
library(ggmap)


# national home value ==========================================================
home_national <- read_acs5year(
    year = 2015,
    states = states_DC,
    # all 50 states plus DC
    table_contents = "B25077_001",
    summary_level = "block group"
) %>%
    setnames("B25077_001", "home_value") %>%
    .[!is.na(home_value)]


us_map <- get_map("united states", zoom = 4, color = "bw")

ggmap(us_map) +
    geom_point(
        data = home_national[order(home_value)],
        # displace higher values
        aes(lon, lat, size = population, color = home_value),
        alpha = 1
    ) +
    ylim(25, 50) +
    scale_size_area(max_size = 0.1) +
    scale_color_continuous(
        low = "green",
        high = "red",
        breaks = c(100000, 500000, 1000000, 1500000, 2000000),
        labels = scales::unit_format("K", 1e-3)
    ) +
    guides(size = FALSE) +
    labs(color = "value ($)",
         caption = "Data source: ACS 5-year survey 2011-2015",
         title = "Median Home Values in Each Block Group") +
    theme(
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.92, 0.2)
    )

ggsave(filename = "application/national_home_value.png",
       width = 9,
       height = 6)


# New York metro home value ==========================================================
home_nymetro <- read_acs5year(
    year = 2015,
    states = c("NY", "NJ", "PA"),
    table_contents = "B25077_001",
    areas = "New York metro",
    summary_level = "block group",
    with_margin = FALSE
) %>%
    setnames("B25077_001", "home_value") %>%
    .[!is.na(home_value)] %>%
    .[order(home_value)]


ny_map <- get_map("new york city", zoom = 9, color = "bw")

ggmap(ny_map) +
    geom_point(data = home_nymetro,
               aes(lon, lat, size = population, color = home_value),
               alpha = 1) +
    # ylim(40.5, 41) +
    # xlim(-74.6, -73.3) +
    scale_size_area(max_size = 0.5) +
    scale_color_continuous(
        low = "green",
        high = "red",
        breaks = c(100000, 500000, 1000000, 1500000, 2000000),
        labels = scales::unit_format("K", 1e-3)
    ) +
    guides(size = FALSE) +
    labs(color = "value ($)",
         caption = "Data source: ACS 5-year survey 2011-2015",
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
