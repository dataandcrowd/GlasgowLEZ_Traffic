library(arrow)
library(data.table)
library(tidyverse)
library(openair)
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
  drop_na() %>% 
  group_by(dt_month) %>% 
  summarise(flow = sum(flow)) %>% 
  ggplot(aes(dt_month, flow)) +
  geom_line(size = 2)

options(scipen = 999)


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
  mapview(lez_monitor)

mapview(lez_buff_500, col.regions = "steelblue") +
  mapview(lez_monitor %>% filter(siteId == "GA5351_T"))

########################################################################


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


########################################################################

library(plotly)

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
####################################################
df2 %>% 
  group_by(lez, dt_month, siteId) %>% 
  summarise(sumflow = sum(flow)) -> lez_traffic


lez_monitor %>% 
  left_join(lez_traffic, by = "siteId") %>% 
  select(siteId, dt_month, sumflow) %>% 
  group_by(siteId) %>% 
  summarise(sumflow = sum(sumflow)) -> lez_monitor_total

lez_monitor %>% 
  left_join(lez_traffic, by = "siteId") %>% 
  select(siteId, lez, sumflow) %>% 
  drop_na() %>% 
  group_by(siteId, lez) %>% 
  summarise(sumflow = sum(sumflow)) -> lez_stats

lez_stats$lez <- factor(lez_stats$lez, levels = c("Pre-LEZ", "Post-LEZ"))


lez_monitor %>% 
  left_join(lez_traffic, by = "siteId") %>% 
  st_drop_geometry() %>% 
  select(lez, sumflow) %>% 
  drop_na() %>% 
  group_by(lez) %>% 
  summarise(sumflow = sum(sumflow)) -> lez_stats1


# lez_monitor %>% 
#   left_join(lez_traffic, by = "siteId") %>% 
#   select(siteId, dt_month, sumflow) %>%  
#   pivot_wider(names_from = "dt_month", values_from = "sumflow") %>%  
#   rename(March = `3`, April = `4`, May = `5`, June = `6`, July = `7`, August = `8`) %>%
#   select(siteId, March, April, May, June, July, August) -> lez_monitor_month

lez_stats %>% 
    pivot_wider(names_from = "lez", values_from = "sumflow") -> lez_monitor_prepostlez
    

mapview(lez_monitor_total, zcol = "sumflow", at = seq(0, 650000, 100000), legend = TRUE) +
mapview(lez_monitor_prepostlez, zcol = "Pre-LEZ", at = seq(0, 200000, 50000), legend = TRUE) +
  mapview(lez_monitor_prepostlez, zcol = "Post-LEZ", at = seq(0, 200000, 50000), legend = TRUE) 



ggplot() +
  geom_sf(data = lez_shp %>% st_transform(crs = 27700), fill="grey", alpha=0.3) +
  geom_sf(data=lez_stats, aes(color=sumflow, size= sumflow)) +
  geom_sf_text(data = lez_stats, aes(label = sumflow), size = 3.5, 
               nudge_x = -30, nudge_y = -40) +
  viridis::scale_color_viridis() +
  labs(x = "", y = "") +
  scale_size(range = c(0, 10)) +
  theme_bw() + 
  facet_wrap(~lez) +
  coord_sf() 
  
ggsave("lez_traffic.jpg", width = 16, height = 8)



####################################################
library(rstatix)

## Hour
df2 %>% 
  group_by(lez, dt_hour) %>% 
  summarise(sumflow = sum(flow)) %>% 
  ungroup() %>% 
  anova_test(sumflow ~ dt_hour + lez)

## Date
df2 %>% 
  group_by(lez, dt_date) %>% 
  summarise(sumflow = sum(flow)) %>% 
  ungroup() %>% 
  anova_test(sumflow ~ dt_date + lez)

### Day of Week

df2 %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
                                )) %>% 
  group_by(lez, week_group, dt_date) %>% 
  summarise(sumflow = sum(flow)) %>% 
  ungroup() %>% 
  anova_test(sumflow ~ week_group + lez)


df2 %>% 
  mutate(dt_week = isoweek(timeStamp)) %>%
  group_by(lez, dt_week) %>% 
  summarise(sumflow = sum(flow)) %>% 
  ungroup() %>% 
  anova_test(sumflow ~ dt_week + lez)



library(ggpubr)
df2 %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  group_by(lez, week_group, dt_date) %>% 
  summarise(sumflow = sum(flow)) %>% 
  ungroup() %>% 
  select(-dt_date)-> imsi

imsi$lez <- factor(imsi$lez, levels = c("Pre-LEZ", "Post-LEZ"))

ggboxplot(imsi, x = "week_group", y = "sumflow", color = "lez",
            palette = c("#00AFBB", "#E7B800")) +
  labs(x = "")

ggsave("anova_weekdays.jpg", height = 3, width = 6)
