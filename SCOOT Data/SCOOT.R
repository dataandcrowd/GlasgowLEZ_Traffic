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
df22 <- read_parquet("glasgow2022.parquet") ## SCOOT Data
df23 <- read_parquet("glasgow2023.parquet") ## SCOOT Data
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


## Distance 
st_distance(ap_glasgow %>% st_transform(27700) %>% filter(code == "GLA4"), 
            lez_monitor_for_ap %>% filter(siteId %in% c("GA1321_B", "GA1571_Q", "GA2401_D"))) # Air pollution station GLA4 to the nearby monitoring points

st_distance(ap_glasgow %>% st_transform(27700) %>% filter(code == "GHSR"), 
            lez_monitor_for_ap %>% filter(siteId %in% c("GG2001_S",  "GG2021_A", "GA5371_C"))) # Air pollution station GLA4 to the nearby monitoring points


df23 %>%  ## <- df is the raw SCOOT Data
  as_tibble() %>% 
  mutate(timestamp = lubridate::as_datetime(timeStamp),
         dt_date = date(timestamp),
         dt_hour = hour(timestamp),
         dt_day = day(timestamp),
         dt_month = month(timestamp),
         dt_weekdays = weekdays(timestamp),
         dt_year = year(timestamp),
         lez = ifelse(dt_month < 6, "Pre-LEZ", "Post-LEZ")) -> df23_dt

df23_dt %>% 
  filter(siteId %in% unique(lez_monitor_for_ap$siteId)) %>% 
  group_by(dt_date, siteId) %>% 
  summarise(dailyflow = sum(flow)) %>% 
  ggplot(aes(dt_date, dailyflow, colour = siteId)) +
  geom_line() +
  theme(legend.position = "bottom") 


## Grouping Days of the Week into three categories
df23_dt %>% 
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


###################
##--SCOOT 2022--###
###################

df22 %>%  ## <- df is the raw SCOOT Data
  as_tibble() %>% 
  mutate(timestamp = lubridate::as_datetime(dt),
         dt_date = date(timestamp),
         dt_hour = hour(timestamp),
         dt_day = day(timestamp),
         dt_month = month(timestamp),
         dt_weekdays = weekdays(timestamp),
         dt_year = year(timestamp),
         lez = ifelse(dt_month < 6, "Pre-June 2022", "Post-June 2022")) -> df22_dt

df22_dt %>% 
  filter(siteId %in% unique(lez_monitor_for_ap$siteId)) %>% 
  group_by(dt_date, siteId) %>% 
  summarise(dailyflow = sum(flow)) %>% 
  ggplot(aes(dt_date, dailyflow, colour = siteId)) +
  geom_line() +
  theme(legend.position = "bottom") 


## Grouping Days of the Week into three categories
df22_dt %>% 
  filter(siteId %in% unique(lez_monitor_for_ap$siteId)) %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  group_by(siteId, week_group, lez, dt_date) %>% 
  summarise(total_flow = sum(flow)) %>% 
  ungroup() %>% 
  left_join(pre, by = "dt_date") -> df_for_analysis_22

df_for_analysis_22$lez <- factor(df_for_analysis_22$lez, levels = c("Pre-June 2022", "Post-June 2022"))
#df_for_analysis_22$no2_daily[is.nan(df_for_analysis_22$no2_daily)] <- NA ## convert NaN to NA


library(rstatix)

df_for_analysis_22 %>% 
  group_by(siteId) %>% 
  anova_test(total_flow ~ week_group  + lez) -> anova_traffic_result

anova_traffic_result$p <- anova_traffic_result$p %>% round(3)

anova_traffic_result


# post-hoc test
df_for_analysis_22 %>% 
  group_by(siteId, week_group) %>% 
  games_howell_test(total_flow ~ lez) %>% arrange(p.adj.signif)

df_for_analysis_22 %>% 
  group_by(siteId, lez) %>% 
  games_howell_test(total_flow ~ week_group) %>% 
  View


# df_for_analysis_22 %>% 
#   group_by(siteId) %>% 
#   anova_test(total_flow ~ week_group + Precipitation + lez) -> result2
# 
# result2$p <- result2$p %>% round(3)
# 
# result2


library(ggpubr)
ggboxplot(df_for_analysis_22, x = "week_group", y = "total_flow", color = "lez",
          palette = c("#00AFBB", "#E7B800")) +
  labs(x = "") +
  facet_wrap(~siteId) 

ggsave("box22_traffic.jpg", width = 7, height = 4)

##################
df_for_analysis %>% 
  select(-Precipitation) %>% 
  group_by(siteId, week_group, lez) %>% 
  summarise(total_flow = mean(total_flow)) %>% 
  pivot_wider(names_from = lez, values_from = total_flow) %>% 
  mutate(`Diff_Per(%)` = -(1 - (`Post-LEZ` / `Pre-LEZ`))*100)


df_for_analysis_22 %>% 
  group_by(siteId, week_group, lez) %>% 
  summarise(total_flow = mean(total_flow)) %>% 
  pivot_wider(names_from = lez, values_from = total_flow) %>% 
  mutate(`Diff_Per(%)` = -(1 - (`Post-June 2022` / `Pre-June 2022`))*100)


