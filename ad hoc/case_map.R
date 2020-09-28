# Case map #

library(tidyverse) ; library(lubridate) ; library(sf) ; library(ggspatial) ; library(shadowtext) ; library(ggtext)

# Retrieve data ----------------------------------------------------------------

# Postcodes
# Source: ONS Open Geography Portal
postcodes <- read_csv("../../data/geospatial/postcodes.csv")

# Confirmed cases: official sensitive data
# Source: PHE Covid-19 Situational Awareness Explorer
raw <- read_csv("../../data/data.csv") %>% 
  mutate(date = as.Date(`Specimen Date`, format = "%Y-%m-%d")) %>% 
  filter(date >= max(date)-days(13)) 

cases <- raw %>% 
  left_join(postcodes, by = c("Patient Postcode" = "postcode")) %>% 
  select(wd19nm, lon, lat)

# Trafford's wards
wards <- st_read("../../data/geospatial/wards.geojson")

# Identify repeat locations ----------------------------------------------------
rpt_locs <- cases %>% 
  group_by(lon, lat, wd19nm) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(desc(n)) %>% 
  st_as_sf(crs = 4326, coords = c("lon", "lat"))

# Plot map ---------------------------------------------------------------------
ggplot() +
  annotation_map_tile(type = "cartodark", zoomin = -0) +
  geom_sf(data = wards, fill = NA, colour = "#757575", size = 0.3) +
  geom_sf(data = rpt_locs, aes(size = n), shape = 21, colour = "#FFFFFF", fill = "#39809E", alpha = 0.5) +
  labs(x = NULL, y = NULL,
       title = "Confirmed coronavirus cases over the last 14 days",
       subtitle = paste("Trafford, up to", format(max(raw$date), "%d %B %Y")),
       caption = "Source: PHE Daily COVID-19 Surveillance Reports\n Contains Ordnance Survey data © Crown copyright and database right 2020",
       size = "Number of cases") +
  annotation_scale(location = "bl", style = "ticks", line_col = "#FFFFFF", text_col = "#FFFFFF") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        plot.title.position = "plot",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
        legend.position = "bottom", 
        legend.justification = "left",
        legend.direction = "horizontal",
        legend.text = element_text(size = 12))

# Write results ----------------------------------------------------------------
ggsave("case_map.png", dpi = 300, scale = 1)


