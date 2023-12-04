library(arrow)
library(data.table)
library(tidyverse)
library(openair)
library(sf)
library(mapview)

df <- read_parquet("glasgow.parquet")
monitor <- read_sf("Mointor shp/useful_detector_530.shp")
options(scipen = 999)

lez_shp <- read_sf("Low_Emission_Zone(LEZ)/Low_Emission_Zone_(LEZ).shp")

lez_shp %>% 
  st_transform(27700) %>% 
  st_buffer(100) -> lez_buff_500

mapview(lez_buff_500)
lez_monitor <- st_intersection(monitor, lez_buff_500)

##
apmonitor <- importMeta(source = "aurn", all = FALSE, year = NA, duplicate = FALSE)

apmonitor %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(lez_shp) %>% 
  mapview()


apmonitor %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  filter(code %in% c("GLA4", "GLKP", "GHSR")) -> ap_glasgow

ap_glasgow %>% 
  st_transform(27700) %>% 
  st_buffer(200) %>% 
  mapview(col.regions = "red") +
  mapview(lez_monitor)

###

lez_monitor %>% 
  filter(siteId %in% c("GA1321_B", "GA1571_Q", "GA2401_D",   ## Near GLA4
                       "GG2001_S", "GA5301_B", "GG2021_A", "GA5371_C" ## Near GHSR
  )) -> lez_monitor_for_ap


######
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
  filter(siteId %in% lez_monitor_for_ap$siteId) %>% 
  group_by(dt_date, siteId) %>% 
  summarise(dailyflow = sum(flow)) %>% 
  ggplot(aes(dt_date, dailyflow, colour = siteId)) +
  geom_line() +
  theme(legend.position = "bottom") 


df1 %>% 
  filter(siteId %in% lez_monitor_for_ap$siteId) %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  group_by(siteId, week_group, lez, dt_date) %>% 
  summarise(total_flow = sum(flow)) %>% 
  ungroup() -> imsi

imsi$lez <- factor(imsi$lez, levels = c("Pre-LEZ", "Post-LEZ"))


library(rstatix)

imsi %>% 
  group_by(siteId) %>% 
  anova_test(total_flow ~ week_group + lez) 

# post-hoc test
imsi %>% 
  group_by(week_group, siteId) %>% 
  t_test(total_flow ~ lez) %>% View




library(ggpubr)
ggboxplot(imsi, x = "week_group", y = "total_flow", color = "lez",
          palette = c("#00AFBB", "#E7B800")) +
  labs(x = "") +
  facet_wrap(~siteId)



