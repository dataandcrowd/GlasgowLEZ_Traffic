library(arrow)
library(data.table)
library(tidyverse)
library(sf)
library(mapview)

df <- read_parquet("glasgow.parquet")
monitor <- read_sf("Mointor shp/useful_detector_530.shp")
#df %>% st_as_sf(coords = c("easting", "northing"), crs = 27700) -> df_sf

unique(df$siteId)


mapview(monitor)

df %>% 
  as_tibble() %>% 
  mutate(timestamp = lubridate::as_datetime(timeStamp),
         dt_date = date(timestamp),
         dt_hour = hour(timestamp),
         dt_day = day(timestamp),
         dt_month = month(timestamp),
         dt_weekdays = weekdays(timestamp),
         dt_year = year(timestamp),
         lez = ifelse(dt_month < 6, "Pre-LEZ", "Post-LEZ")) -> df1


df1 %>% 
  filter(description == "Cathcart Rd northbound to Preston St") %>% 
  group_by(dt_date) %>% 
  summarise(dailyflow = sum(flow)) %>% 
  ggplot(aes(dt_date, dailyflow)) +
  geom_line()
  

lez_shp <- read_sf("Low_Emission_Zone(LEZ)/Low_Emission_Zone_(LEZ).shp")

lez_shp %>% 
  st_transform(27700) %>% 
  st_buffer(100) -> lez_buff_500

mapview(lez_buff_500)

lez_monitor <- st_intersection(monitor, lez_buff_500)


mapview(lez_buff_500, col.regions = "steelblue") +
  mapview(lez_monitor %>% filter(siteId == "GA5351_T"))



df1 %>% 
  filter(siteId %in% lez_monitor$siteId) %>% 
  group_by(dt_hour, siteId) %>% 
  summarise(dailyflow = sum(flow)) %>% 
  ggplot(aes(dt_hour, dailyflow, colour = siteId)) +
  geom_line() +
  theme(legend.position = "bottom") -> gg

ggplotly(gg)


df1 %>% 
  filter(siteId %in% lez_monitor$siteId)  -> df2


library(rstatix)

df2 %>% 
  group_by(lez, dt_weekdays) %>% 
  summarise(weekdayflow = sum(flow)) %>% 
  ungroup() %>% 
  anova_test(weekdayflow ~ dt_weekdays + lez)
  
  
