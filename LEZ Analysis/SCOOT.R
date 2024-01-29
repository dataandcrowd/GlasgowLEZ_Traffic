options(scipen = 999)

library(arrow)
library(data.table)
library(tidyverse)
library(sf)
library(mapview)
library(openair)
library(rstatix)
library(ggpubr)


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
  st_buffer(50) -> lez_buff_50

mapview(lez_buff_50)

# Filtering Monitor
lez_monitor <- st_intersection(monitor, lez_buff_50)
mapview(lez_monitor)


lez_monitor |> 
  filter(siteId %in% c("GA1571_Q", "GA2401_D",   ## Near GLA4
                       "GG2001_S", "GA5371_C" ## Near GHSR
  )) |> 
    mapview() +
  mapview(lez_buff_50)

######################################################
## Filtering the SCOOT Data for Glasgow LEZ Monitors #
######################################################
lez_monitor %>% 
  filter(siteId %in% c("GA1571_Q", "GA2401_D",   ## Near GLA4
                       "GG2001_S", "GA5371_C" ## Near GHSR
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




## SCOOT 2023
df23 %>%  ## <- df is the raw SCOOT Data
  as_tibble() %>% 
  mutate(timestamp = lubridate::as_datetime(timeStamp),
         dt_date = date(timestamp),
         dt_month = month(timestamp),
         dt_weekdays = weekdays(timestamp),
         dt_year = year(timestamp)) |> 
  filter(between(dt_month, 6, 8)) |> 
  select(siteId, dt_date, dt_year, dt_month, dt_weekdays, flow) -> df23_dt

## SCOOT 2022
df22 %>%  ## <- df is the raw SCOOT Data
  as_tibble() |>  
  mutate(timestamp = lubridate::as_datetime(dt),
         dt_date = date(timestamp),
         dt_month = month(timestamp),
         dt_weekdays = weekdays(timestamp),
         dt_year = year(timestamp))|> 
  filter(between(dt_month, 6, 8)) |> 
  select(siteId, dt_date, dt_year, dt_month, dt_weekdays, flow) -> df22_dt



df23_dt %>% 
  filter(siteId %in% unique(lez_monitor_for_ap$siteId)) %>% 
  group_by(dt_date, siteId) %>% 
  summarise(dailyflow = sum(flow)) %>% 
  ggplot(aes(dt_date, dailyflow, colour = siteId)) +
  geom_line() +
  theme(legend.position = "bottom") 
#--> data between mid and end July are missing.


df22_dt %>% 
  filter(siteId %in% unique(lez_monitor_for_ap$siteId)) %>% 
  group_by(dt_date, siteId) %>% 
  summarise(dailyflow = sum(flow)) %>% 
  ggplot(aes(dt_date, dailyflow, colour = siteId)) +
  geom_line() +
  theme(legend.position = "bottom") 



########
traffic_tbl <- 
  bind_rows(df22_dt, df23_dt) |> 
  mutate(location = if_else(siteId == "GA1571_Q" | siteId == "GA2401_D", "Hope St", "High St"))

## Grouping Days of the Week into three categories
traffic_tbl %>% 
  filter(siteId %in% unique(lez_monitor_for_ap$siteId)) %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  group_by(siteId, location,week_group, dt_year, dt_date) %>% 
  summarise(total_flow = sum(flow)) %>% 
  ungroup() -> df_for_analysis

df_for_analysis$dt_year <- factor(df_for_analysis$dt_year)


df_for_analysis |> 
  group_by(dt_date, siteId, location, dt_year) %>% 
  summarise(dailyflow = sum(total_flow)) %>% 
  ggplot(aes(dt_date, dailyflow, colour = siteId)) +
  geom_line() +
  facet_wrap(dt_year~. , scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") 
#* Missing data: 2023-07-12 ~ 2023-08-13 

df_for_analysis |> 
  group_by(dt_date, siteId, location, dt_year) %>% 
  summarise(dailyflow = sum(total_flow)) %>% 
  ggplot(aes(dt_date, dailyflow, colour = siteId)) +
  geom_smooth() +
  facet_wrap(dt_year~. , scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") 




######
library(rstatix)


df_for_analysis |> 
  group_by(siteId, week_group, dt_year) |> 
  summarise(mean_daily_flow = mean(total_flow)) |>  
  pivot_wider(names_from = dt_year, values_from = mean_daily_flow)


df_for_analysis |> 
  group_by(siteId, week_group, dt_year) |> 
  get_summary_stats(mean_daily_flow, type = "common") |> 
  print(n = 50)


df_for_analysis |> 
  mutate(siteId = case_when(siteId == "GA1571_Q" ~ "GA1571_Q (High St)",
                            siteId == "GA2401_D" ~ "GA2401_D (Hope St)",
                            siteId == "GG2001_S" ~ "GG2001_S (Hope St)",
                            siteId == "GA5371_C" ~ "GA5371_C (High St)")) |> 
  gghistogram(x = "total_flow",
              add = "mean", 
              rug = TRUE,
              fill = "dt_year",
              color = "dt_year", 
              palette = c("#00AFBB", "#E7B800")) +
  labs(x = "Averaged Daily Flow") +
  theme(legend.title = element_blank())



df_for_analysis |> 
  mutate(siteId = case_when(siteId == "GA1571_Q" ~ "GA1571_Q (High St)",
                            siteId == "GA2401_D" ~ "GA2401_D (Hope St)",
                            siteId == "GG2001_S" ~ "GG2001_S (Hope St)",
                            siteId == "GA5371_C" ~ "GA5371_C (High St)")) |> 
  gghistogram(x = "total_flow",
            rug = TRUE,
            fill = "dt_year",
            color = "dt_year", 
            palette = c("#00AFBB", "#E7B800")) +
  ylim(0,20) +
  facet_wrap(~siteId, scales = "free") +
  labs(x = "Averaged Daily Flow") +
  theme(legend.title = element_blank())



df_for_analysis |> 
  mutate(siteId = case_when(siteId == "GA1571_Q" ~ "GA1571_Q (High St)",
                            siteId == "GA2401_D" ~ "GA2401_D (Hope St)",
                            siteId == "GG2001_S" ~ "GG2001_S (Hope St)",
                            siteId == "GA5371_C" ~ "GA5371_C (High St)")) |> 
  ggboxplot(x = "week_group", y = "total_flow", color = "dt_year") +
  facet_wrap(~siteId, scales = "free") +
  labs(x = "", y = "Averaged Daily Flow")



df_for_analysis %>% 
  group_by(siteId, week_group, dt_year) %>% 
  shapiro_test(total_flow) |> 
  mutate(p1 = if_else(p > 0.05, "Normality", "Skewed"))


df_for_analysis %>% 
  group_by(siteId, week_group) %>% 
  wilcox_test(total_flow ~ dt_year) |> 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") 

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
  filter(code %in% c("GLA4", "GHSR"),
         variable == "NO2",
         end_date == 'ongoing') -> ap_glasgow ## Filter Glasgow

ap_glasgow %>% 
  st_transform(27700) %>% 
  st_buffer(200) %>% 
  mapview(col.regions = "red") +
  mapview(lez_monitor) +
  mapview(ap_glasgow, col.regions = "black")



load('openair_glasgow.RData')

no2_2023 %>% 
  mutate(dt_date = as_date(date)) %>% 
  filter(dt_date >= "2023-06-01" & dt_date <= "2023-08-31") %>% 
  group_by(code, dt_date) %>% 
  summarise(no2_daily = mean(no2, na.rm = T)) |> 
  ungroup() |> 
  mutate(no2_daily = ifelse(is.nan(no2_daily), NA, no2_daily),
         dt_year = "2023") -> no2_daily_23

no2_2022 %>% 
  mutate(dt_date = as_date(date)) %>% 
  filter(dt_date >= "2022-06-01" & dt_date <= "2022-08-31") %>% 
  group_by(code, dt_date) %>% 
  summarise(no2_daily = mean(no2, na.rm = T)) |> 
  ungroup() |> 
  mutate(no2_daily = ifelse(is.nan(no2_daily), NA, no2_daily),
         dt_year = "2022") -> no2_daily_22


## Clean data
no2 <- bind_rows(no2_daily_22, no2_daily_23) |> 
  mutate(dt_weekdays = weekdays(dt_date),
         week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"))


## Analysis

no2 %>% 
  drop_na(no2_daily) %>% 
  group_by(code, week_group, dt_year) %>% 
  summarise(no2_daily = mean(no2_daily)) %>% 
  pivot_wider(names_from = dt_year, values_from = no2_daily)


no2 |> 
  filter(code != "GLKP") |> 
  ggboxplot(x = "week_group", y = "no2_daily", color = "dt_year") +
  facet_wrap(~code, scales = "free") +
  labs(x = "", y = "Averaged NO2")



no2 |> 
  filter(code != "GLKP") |> 
  ggdensity(x = "no2_daily",
              rug = TRUE,
              fill = "dt_year",
              color = "dt_year", 
              palette = c("#00AFBB", "#E7B800")) +
  facet_wrap(~code, scales = "free") +
  labs(x = "Averaged NO2") +
  theme(legend.title = element_blank())

no2 |> 
  filter(code != "GLKP") |> 
  gghistogram(x = "no2_daily",
            rug = TRUE,
            fill = "dt_year",
            color = "dt_year", 
            palette = c("#00AFBB", "#E7B800")) +
  facet_wrap(~code, scales = "free") +
  labs(x = "Averaged NO2") +
  theme(legend.title = element_blank())


no2 %>% 
  group_by(code, week_group, dt_year) %>% 
  shapiro_test(no2_daily) |> 
  mutate(p1 = if_else(p > 0.05, "Normality", "Skewed"))


no2 %>% 
  group_by(code, week_group) %>% 
  wilcox_test(no2_daily ~ dt_year) 


