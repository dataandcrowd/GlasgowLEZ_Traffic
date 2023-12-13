library(tidyverse)
library(data.table)
library(mapboxapi)
library(leaflet)
library(lubridate)
library(mapview)
library(sf)

cctv_raw <- fread("20231112-ubdc-cctv-glasgow-yolo-records.csv")

cctv_raw %>% 
  as_tibble() %>% 
  mutate(timestamp = lubridate::as_datetime(timestamp),
         dt_date = as_date(timestamp),
         dt_month = month(timestamp),
         dt_weekdays = weekdays(timestamp),
         week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends")) %>% 
  filter(dt_month >= 3 & dt_month < 9) %>% 
  mutate(LEZ = if_else(dt_month >= 3 & dt_month < 6, "Pre-LEZ", "Post-LEZ"),
         camera_id = as.factor(camera_id)) -> cctv

cctv$LEZ <- factor(cctv$LEZ, levels = c("Pre-LEZ", "Post-LEZ"))  


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

cctv_lez <- st_intersection(cctv_sf, lez_shp) %>% 
  filter(camera_id %in% c(40, 75, 431, 428))



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
cctv_lez %>% 
  left_join(cctv, by = "camera_id") %>% 
  select(camera_id, LEZ, cars, cyclists, pedestrians, vans, lorries, taxis, buses) %>% 
  group_by(camera_id, LEZ) %>% 
  summarise(cars = sum(cars)) 


cctv_lez %>% 
  left_join(cctv, by = "camera_id") %>% 
  st_drop_geometry() %>% 
  select(week_group, camera_id, dt_date, LEZ, cars, pedestrians, cyclists, buses, taxis, lorries) %>% 
  pivot_longer(!c(camera_id, week_group, dt_date, LEZ), names_to = "mode", values_to = "value") %>% 
  group_by(camera_id, LEZ, dt_date, week_group, mode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() -> transport_df 


################
library(rstatix)
library(ggpubr)

transport_df %>% 
  filter(mode == "cars") %>% 
  group_by(camera_id, mode, LEZ) %>% 
  summarise(number = sum(value)) %>% 
  pivot_wider(names_from = LEZ, values_from = number) 

transport_df %>% 
  filter(mode == "taxis") %>% 
  group_by(camera_id, mode, LEZ) %>% 
  summarise(number = sum(value)) %>% 
  pivot_wider(names_from = LEZ, values_from = number) 

transport_df %>% 
  filter(mode %in% c("buses", "lorries")) %>% 
  group_by(camera_id, mode, LEZ) %>% 
  summarise(number = sum(value)) %>% 
  pivot_wider(names_from = LEZ, values_from = number) %>% 
  arrange(mode)

transport_df %>% 
  filter(mode == "buses") %>% 
  group_by(camera_id, mode) %>% 
  anova_test(value ~ week_group + LEZ)
  
transport_df %>% 
  filter(mode == "lorries") %>% 
  group_by(camera_id, mode) %>% 
  anova_test(value ~ week_group + LEZ) 
  

transport_df%>% 
  filter(mode == "buses") %>% 
  ggboxplot(x = "week_group", y = "value", color = "LEZ",
          palette = c("#00AFBB", "#E7B800")) +
  labs(x = "", y = "Count") +
  facet_wrap(~camera_id+mode, scales = "free") 


transport_df%>% 
  filter(mode == "lorries") %>% 
  ggboxplot(x = "week_group", y = "value", color = "LEZ",
            palette = c("#00AFBB", "#E7B800")) +
  labs(x = "", y = "Count") +
  facet_wrap(~camera_id+mode, scales = "free") 

