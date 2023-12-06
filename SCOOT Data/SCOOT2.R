options(scipen = 999)

library(arrow)
library(data.table)
library(tidyverse)
library(sf)
library(mapview)
library(openair)

####################
#---Data Import---##
####################

##-SCOOT DATA-##
df <- read_parquet("glasgow.parquet") ## SCOOT Data
monitor <- read_sf("Mointor shp/useful_detector_530.shp") ## SCOOT Monitor

lez_shp <- read_sf("Low_Emission_Zone(LEZ)/Low_Emission_Zone_(LEZ).shp") ## LEZ Boundary

# Wrap Buffer
lez_shp %>% 
  st_transform(27700) %>% 
  st_buffer(100) -> lez_buff_500
mapview(lez_buff_500)

# Filtering Monitor
lez_monitor <- st_intersection(monitor, lez_buff_500)
mapview(lez_monitor)

#################
##-Air Quality-##
#################
meta <- importMeta(source = "saqn", all = TRUE)

meta %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_intersection(lez_shp) %>% 
  mapview()


meta %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  filter(code %in% c("GLA4", "GLKP", "GHSR"),
         variable == "NO2",
         end_date == 'ongoing') -> ap_glasgow ## Filter Glasgow

ap_glasgow %>% 
  st_transform(27700) %>% 
  st_buffer(200) %>% 
  mapview(col.regions = "red") +
  mapview(lez_monitor) +
  mapview(ap_glasgow, col.regions = "black")



load('openair_glasgow23.RData')

no2_2023 %>% 
  mutate(dt_date = as_date(date)) %>% 
  filter(dt_date >= "2023-03-01" & dt_date <"2023-08-31") %>% 
  group_by(dt_date, code) %>% 
  summarise(no2_daily = mean(no2, na.rm = T)) -> no2_daily

#############
##-Weather-##
#############
pre <- read_csv("Dalmarnock_327234_Precipitation.csv") %>% 
  mutate(Timestamp = dmy_hm(Timestamp),
         dt_date = as_date(Timestamp)) %>% 
  select(dt_date, Precipitation)


######################################################
## Filtering the SCOOT Data for Glasgow LEZ Monitors #
######################################################
lez_monitor %>% 
  filter(siteId %in% c("GA1321_B", "GA1571_Q", "GA2401_D",   ## Near GLA4
                       "GG2001_S", "GA5301_B", "GG2021_A", "GA5371_C" ## Near GHSR
  )) -> lez_monitor_for_ap


## Distance 
st_distance(ap_glasgow %>% st_transform(27700) %>% filter(code == "GLA4"), lez_monitor_for_ap %>% filter(siteId %in% c("GA1321_B", "GA1571_Q", "GA2401_D"))) # Air pollution station GLA4 to the nearby monitoring points

st_distance(ap_glasgow %>% st_transform(27700) %>% filter(code == "GHSR"), lez_monitor_for_ap %>% filter(siteId %in% c("GG2001_S",  "GG2021_A", "GA5371_C"))) # Air pollution station GLA4 to the nearby monitoring points


df %>%  ## <- df is the raw SCOOT Data
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
  filter(siteId %in% unique(lez_monitor_for_ap$siteId)) %>% 
  group_by(dt_date, siteId) %>% 
  summarise(dailyflow = sum(flow)) %>% 
  ggplot(aes(dt_date, dailyflow, colour = siteId)) +
  geom_line() +
  theme(legend.position = "bottom") 


## Grouping Days of the Week into three categories
df1 %>% 
  filter(siteId %in% unique(lez_monitor_for_ap$siteId)) %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  group_by(siteId, week_group, lez, dt_date) %>% 
  summarise(total_flow = sum(flow)) %>% 
  ungroup() %>% 
  left_join(pre, by = "dt_date") -> df_for_analysis

df_for_analysis$lez <- factor(df_for_analysis$lez, levels = c("Pre-LEZ", "Post-LEZ"))
#df_for_analysis$no2_daily[is.nan(df_for_analysis$no2_daily)] <- NA ## convert NaN to NA


library(rstatix)

df_for_analysis %>% 
  group_by(siteId) %>% 
  anova_test(total_flow ~ week_group  + lez) -> result1

result1$p <- result1$p %>% round(3)

result1


# post-hoc test
df_for_analysis %>% 
  group_by(siteId, week_group) %>% 
  games_howell_test(total_flow ~ lez) #%>% View

df_for_analysis %>% 
  group_by(siteId, lez) %>% 
  games_howell_test(total_flow ~ week_group) #%>% View



df_for_analysis %>% 
  group_by(siteId) %>% 
  anova_test(total_flow ~ week_group + Precipitation + lez) -> result2

result2$p <- result2$p %>% round(3)

result2


library(ggpubr)
ggboxplot(df_for_analysis, x = "week_group", y = "total_flow", color = "lez",
          palette = c("#00AFBB", "#E7B800")) +
  labs(x = "") +
  facet_wrap(~siteId) 
  
ggsave("box.jpg", width = 7, height = 4)

#######
##-- Air Quality
no2_daily %>% 
  mutate(dt_weekdays = weekdays(dt_date),
         week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"),
         dt_month = month(dt_date),
         lez = ifelse(dt_month < 6, "Pre-LEZ", "Post-LEZ")) %>% 
  left_join(pre, by = "dt_date") -> no2_df


no2_df$lez <- factor(no2_df$lez, levels = c("Pre-LEZ", "Post-LEZ"))  

no2_df %>% 
  drop_na() %>% 
  group_by(code, week_group, lez) %>% 
  summarise(no2_daily = mean(no2_daily)) %>% 
  pivot_wider(names_from = lez, values_from = no2_daily)



  
no2_df %>% 
  drop_na() %>% 
  group_by(code) %>% 
  anova_test(no2_daily ~ week_group + lez) -> result3

result3$p <- result3$p %>% round(3)

result3


ggboxplot(no2_df, x = "week_group", y = "no2_daily", color = "lez",
          palette = c("#00AFBB", "#E7B800")) +
  labs(x = "", y = "Daily Mean NO2") +
  facet_wrap(~code) 


ggsave("box1.jpg", width = 7, height = 3)


# post-hoc test
no2_df %>% 
  group_by(code, week_group) %>% 
  games_howell_test(no2_daily ~ lez) %>% View


no2_df %>% 
  group_by(code, lez) %>% 
  games_howell_test(no2_daily ~ week_group) %>% View




no2_df %>% 
  drop_na() %>% 
  group_by(code) %>% 
  anova_test(no2_daily ~ week_group + Precipitation + lez) 

