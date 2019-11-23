# source: https://ggplot2tutor.com/streetmaps/streetmaps/

library("tidyverse")
library("osmdata")

location_string <- "Newton Centre, Massachusetts, United States"

streets <- osmdata::getbb((location_string)) %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata::osmdata_sf()

small_streets <- osmdata::getbb((location_string)) %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(key = "highway",
                           value = c("residential", "living_street",
                                     "unclassified",
                                     "service", "footway")) %>%
  osmdata::osmdata_sf()

river  <- osmdata::getbb((location_string)) %>%
  osmdata::opq() %>%
  osmdata::add_osm_feature(key = "waterway",
                           value = c("river")) %>%
  osmdata::osmdata_sf()

light_streets <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .6) +
  # No rivers in Newton
  # geom_sf(data = river$osm_lines,
  #         inherit.aes = FALSE,
  #         color = "black",
  #         size = .2,
  #         alpha = .5)
  coord_sf(xlim = c(getbb(location_string)["x", "min"], getbb(location_string)["x", "max"]), 
           ylim = c(getbb(location_string)["y", "min"], getbb(location_string)["y", "max"])) +
  theme_void() +
  theme(panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Newton Centre",
       subtitle = getbb(location_string) %>% 
         as.data.frame() %>%
         transmute(avg = round((min + max) / 2, 3)) %>% 
         t() %>% 
         unlist() %>% 
         as.character() %>%
         paste0(collapse = ", "))

dark_streets <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#ffbe7f",
          size = .2,
          alpha = .6) +
  coord_sf(xlim = c(getbb(location_string)["x", "min"], getbb(location_string)["x", "max"]), 
           ylim = c(getbb(location_string)["y", "min"], getbb(location_string)["y", "max"])) +
  theme_void() +
  theme(panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent"),
        plot.background = element_rect(fill = "#282828"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(color = "white")) +
  labs(title = "Newton Centre",
       subtitle = getbb(location_string) %>% 
         as.data.frame() %>%
         transmute(avg = round((min + max) / 2, 3)) %>% 
         t() %>% 
         unlist() %>% 
         as.character() %>%
         paste0(collapse = ", "))

both_plots <- cowplot::plot_grid(light_streets, dark_streets)


ggsave(file.path("figs", "both_plots.jpg"), both_plots, width = 9, height = 6)
