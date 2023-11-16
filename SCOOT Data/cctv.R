library(tidyverse)
library(data.table)
library(mapboxapi)
library(leaflet)
library(lubridate)
library(sf)

cctv_raw <- fread("20231112-ubdc-cctv-glasgow-yolo-records.csv")

cctv_raw %>% 
  as_tibble() %>% 
  mutate(timestamp = lubridate::as_datetime(timestamp),
         dt_hour = hour(timestamp),
         dt_day = day(timestamp),
         dt_month = month(timestamp),
         dt_year = year(timestamp)) %>% 
  filter(dt_month >= 2 & dt_month < 11) %>% 
  mutate(pre_LEZ = if_else(dt_month >= 2 & dt_month < 6, "PreLEZ", "PostLEZ")) -> cctv


cctv %>% 
  group_by(camera_id) %>% 
  slice(1) %>% 
  select(camera_id, camera_label, longitude, latitude) %>% 
  ungroup %>% 
  mutate(camera_id = factor(camera_id)) -> camera_locations



# Convert the data to an sf object
cctv_sf <- st_as_sf(camera_locations, coords = c("longitude", "latitude"), crs = 4326)



# Create the map
leaflet() %>% 
  addMapboxTiles(style_id = "light-v10",
                 #style_id = "light-v9",
                 username = "mapbox") %>% 
  addCircleMarkers(data = camera_locations,
                   label = camera_locations$camera_id,
                   radius = 5,
                   labelOptions = labelOptions(noHide = T)) 

##
lez_shp <- read_sf("Low_Emission_Zone(LEZ)/Low_Emission_Zone_(LEZ).shp")

cctv_lez <- st_intersection(cctv_sf, lez_shp)
cctv_lez_id <- cctv_lez %>% pull(camera_id)

cctv_lez_id

# Create the map
leaflet() %>% 
  addMapboxTiles(style_id = "light-v10",
                 #style_id = "light-v9",
                 username = "mapbox") %>% 
  addCircleMarkers(data = cctv_lez,
                   label = cctv_lez$camera_id,
                   radius = 5,
                   labelOptions = labelOptions(noHide = T)) 


#####

cctv %>% 
  filter(camera_id %in% cctv_lez_id) -> lez


#####
lez %>% 
  group_by(camera_id) %>% 
  select(6:11) %>% 
  summarise(cars = sum(cars)) %>% 
  arrange(desc(cars)) %>% 
  print(n = Inf) 


lez %>% 
  select(dt_month, pre_LEZ, cars, pedestrians, cyclists, buses, taxis, lorries) %>% 
  pivot_longer(!c(dt_month, pre_LEZ), names_to = "mode", values_to = "value") %>% 
  group_by(dt_month, mode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() -> transport_df 


transport_df %>% 
  pivot_wider(names_from = mode, values_from = value) 

transport_df %>% 
  filter(dt_month == 4) %>% 
  mutate(prop = value / sum(value) * 100) %>% 
  arrange(desc(value)) 

lez %>% 
  group_by(dt_month) %>% 
  summarise(cars = sum(cars)) %>% 
  ggplot(aes(dt_month, cars)) +
  geom_line()

#######################

lez %>% 
  select(dt_hour, cars, pedestrians, cyclists, buses, taxis, lorries) %>% 
  pivot_longer(!dt_hour, names_to = "mode", values_to = "value") %>% 
  group_by(dt_hour, mode) %>% 
  summarise(value = round(sum(value)/365, 0))  -> transport_df_hour


transport_df_hour %>% 
  ggplot(aes(dt_hour, value)) +
  geom_line(aes(colour = mode), size = 2) +
  labs(x = "Hour", y = "Counts") +
  facet_wrap(~mode, scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 20)) +
  scale_colour_manual(
    name = NULL,
    values = c("#1f83b4",  "#18a188", "#ffb022", "#6f63bb", "#de3e3e","#29a03c") 
  ) 

ggsave("hour.jpg", width = 6, height = 5)

#########
lez %>% 
  group_by(camera_id, dt_month) %>%  # Changed grouping order
  summarise(cars = sum(cars)) %>% 
  ungroup() %>% 
  ggplot(aes(dt_month, cars)) +
  geom_line(aes(group = camera_id)) +  # Adjusted grouping in geom_line
  labs(x = "2023", y = "Counts") +
  facet_wrap(~camera_id) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("by cars.jpg", width = 6, height = 5)


lez %>% 
  group_by(camera_id, dt_month) %>%  # Changed grouping order
  summarise(pedestrians = sum(pedestrians)) %>% 
  ungroup() %>% 
  ggplot(aes(dt_month, pedestrians)) +
  geom_line(aes(group = camera_id)) +  # Adjusted grouping in geom_line
  labs(x = "2023", y = "Counts") +
  facet_wrap(~camera_id) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("by pedestrians.jpg", width = 6, height = 5)


lez %>% 
  group_by(camera_id, dt_month) %>%  # Changed grouping order
  summarise(cyclists = sum(cyclists)) %>% 
  ungroup() %>% 
  ggplot(aes(dt_month, cyclists)) +
  geom_line(aes(group = camera_id)) +  # Adjusted grouping in geom_line
  labs(x = "2023", y = "Counts") +
  facet_wrap(~camera_id) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("by cyclists.jpg", width = 6, height = 5)



lez %>% 
  group_by(camera_id, dt_month) %>%  # Changed grouping order
  summarise(buses = sum(buses)) %>% 
  ungroup() %>% 
  ggplot(aes(dt_month, buses)) +
  geom_line(aes(group = camera_id)) +  # Adjusted grouping in geom_line
  labs(x = "2023", y = "Counts") +
  facet_wrap(~camera_id) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("by buses.jpg", width = 6, height = 5)



lez %>% 
  group_by(camera_id, dt_month) %>%  # Changed grouping order
  summarise(taxis = sum(taxis)) %>% 
  ungroup() %>% 
  ggplot(aes(dt_month, taxis)) +
  geom_line(aes(group = camera_id)) +  # Adjusted grouping in geom_line
  labs(x = "2023", y = "Counts") +
  facet_wrap(~camera_id) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("by taxis.jpg", width = 6, height = 5)


lez %>% 
  group_by(camera_id, dt_month) %>%  # Changed grouping order
  summarise(lorries = sum(lorries)) %>% 
  ungroup() %>% 
  ggplot(aes(dt_month, lorries)) +
  geom_line(aes(group = camera_id)) +  # Adjusted grouping in geom_line
  labs(x = "2023", y = "Counts") +
  facet_wrap(~camera_id) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("by lorries.jpg", width = 6, height = 5)

##########################

library(magrittr)
library(hrbrthemes)
library(waffle)
library(ggthemes)

transport_df

# Convert dt_month to month names and then to a factor with ordered levels
transport_df <- transport_df %>%
  mutate(month_name = factor(month.abb[dt_month], levels = month.abb))

# Create the plot
ggplot(transport_df, aes(fill = mode, values = value/1000)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~month_name, nrow = 1, strip.position = "bottom") + # Use month_name here
  scale_x_discrete() + 
  scale_y_continuous(expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Glasgow Traffic within the LEZ Boudnary",
    subtitle = "Using Glasgow CCTV Big Data by Monthly Counts",
    x = "2023",
    y = "Count by thousands"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line()) +
  guides(fill = guide_legend(reverse = TRUE))


ggsave("waffle.jpg", width = 8, height = 6)


# https://github.com/hrbrmstr/waffle
# https://geospock.com/en/resources/blog/using-data-visualisation-to-beat-gridlock/
# https://www.kaggle.com/code/ricardoxp1/seoul-cctv-cameras-spatial-analysis



ggplot(transport_df, aes(x = month_name, y = value, colour = mode, group = mode)) +
  geom_line() +
  geom_point(colour = "black") +
  facet_wrap(~mode, scales = "free") +
  labs(x = "2023", y = "Counts") +
  theme_bw()+
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  # Increase axis text size
        strip.text = element_text(size = 15)) # Increase facet strip text size

ggsave("line.jpg", width = 11, height = 6)
