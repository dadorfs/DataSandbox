## -------------------------------------------------
##
## Script name: NOAA Atlantic Hurricanes 
##
## Purpose of script: Explore + visualize Atlantic storms from 1975-2021
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-08-18
##
## -------------------------------------------------
##
## Notes:
##   
##
## -------------------------------------------------


## Load packages

library(tidyverse)
library(gganimate)
library(sf)
library(rnaturalearth)
library(here)
library(glue)
library(wesanderson)


## -------------------------------------------------
## Load data

storm_dat <- dplyr::storms

## -------------------------------------------------
## Explore

# Named storms per year
named_storms_per_year <- 
  storm_dat %>%
  group_by(year) %>%
  summarize(named_storms = n_distinct(name))

ggplot(named_storms_per_year, aes(x = year, y = named_storms)) + 
  geom_col(fill = "royalblue", color = "black", alpha = 0.5) + 
  geom_smooth(method = lm, se = FALSE, color = "red", linetype = "dashed") +
  labs(x = "Year", y = "Named Storms") + 
  theme_classic()

# Named storms - filter peak status (i.e, depression, storm, hurricane) for each storm
storm_dat_max_status <- 
  storm_dat %>% 
  mutate(full_date = make_datetime(year, month, day, hour)) %>% 
  slice_max(by = c(name,year), order_by = tibble(wind, full_date)) %>% 
  filter(status %in% c("tropical depression", "tropical storm", "hurricane")) %>% 
  mutate(status = factor(status, levels = c("hurricane", "tropical storm", "tropical depression")))

ggplot(storm_dat_max_status, aes(x = year, fill = status)) +
  geom_bar(alpha = 0.5, color = "black") +
  labs(x = "Year") +
  scale_x_continuous(breaks = seq(1975, 2021, by=1)) + 
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.125, 0.9)
  )

## -------------------------------------------------
# Mapping storms

# Polygon-based 
world_coordinates <- map_data("world")

world_map <- ggplot() +
  geom_polygon(
    data = world_coordinates,
    aes(x = long, y = lat, group = group),
    fill = "white",
    color = "black",
    linewidth = 0.2
  ) +
  lims(x = c(-150, 0), y = c(0, 90)) +
  coord_quickmap() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank()
  )

# Draw 1992 storm trajectories onto map
# world_map +
#   geom_point(
#     data = filter(storm_dat, year == 1992),
#     aes(
#       x = long,
#       y = lat,
#       color = name,
#       size = wind
#     ),
#     alpha = 0.7
#   ) +
#   scale_size_continuous(range = c(0.1, 3))

# Simple features map
north_america <- ne_countries(continent = "North America", returnclass = "sf")

# Map background
north_america_plot <-
  ggplot() +
  geom_sf(data = north_america) +
  coord_sf(xlim = c(-110, 0),
           ylim = c(5, 65),
           expand = FALSE) +
  theme(panel.background = element_blank(),
        axis.title = element_blank())

# Draw 1992 storm trajectories onto map
# north_america_plot +
#   geom_path(
#     data = filter(storm_dat, year == 1992),
#     aes(
#       x = long,
#       y = lat,
#       group = name,
#       color = name,
#       linewidth = wind
#     ),
#     lineend = "round",
#     alpha = 0.7
#   ) 

## -------------------------------------------------
# Animated maps

# Generic function to animate named storm in a given year
storm.animation <- function(storm_name, storm_year) {
  tryCatch({
    
      storm_slct <- storm_dat %>%
      filter(year == storm_year & name == storm_name) %>%
      select(name, year, month, day, hour, lat, long, wind, status) %>%
      mutate(date = make_datetime(year, month, day, hour), .after = hour)
    
    if (nrow(storm_slct) == 0) {
      stop(glue(
        "No data found for storm '{storm_name}' in year {storm_year}."
      ))
    }
    
    max_status <- str_to_title(
      slice_max(storm_slct, wind, n = 1, with_ties = FALSE)[["status"]]
    )
      
    ggplot(data = storm_slct) +
      geom_sf(data = north_america,
              fill = "beige") +
      coord_sf(xlim = c(-110, 0),
               ylim = c(5, 65),
               expand = FALSE) +
      geom_path(aes(
        x = long,
        y = lat,
        color = wind,
        linewidth = wind
      ),
      lineend = "round") +
      labs(title = glue("{max_status} {storm_name}"),
           subtitle = "{frame_along}") +
      scale_color_gradient(low = "gold", high = "red") +
      scale_linewidth_continuous(name = "Windspeed (mph)", range = c(1, 4)) +
      theme(
        panel.background = element_rect(fill = "#7FCDFF"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = "none"
      ) +
      transition_reveal(date)
  }, error = function(err) {
    message("An error occurred: ", err)
  })
}

# Hurricane Andrew
andrew92 <- storm.animation(storm_name = "Andrew", storm_year = 1992)
andrew_gif <- animate(andrew92, width = 6, height = 5, units = "in", res = 200)

# Save
# anim_save("hurricane_andrew_1992.gif", andrew_gif)






                    
                  

