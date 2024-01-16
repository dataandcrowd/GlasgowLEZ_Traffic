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

lez_shp <- read_sf("Low_Emission_Zone(LEZ)/Low_Emission_Zone_(LEZ).shp") ## LEZ Boundary
monitor <- read_sf("Mointor shp/useful_detector_530.shp") ## SCOOT Monitor

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

#############
##-Weather-##
#############
pre <- read_csv("Dalmarnock_327234_Precipitation.csv") %>% 
  mutate(Timestamp = dmy_hm(Timestamp),
         dt_date = as_date(Timestamp)) %>% 
  select(dt_date, Precipitation)


###################
##-- Air Quality--#
###################
load('openair_glasgow.RData')

no2_2023 %>% 
  mutate(dt_date = as_date(date)) %>% 
  filter(dt_date >= "2023-03-01" & dt_date <= "2023-08-31") %>% 
  group_by(dt_date, code) %>% 
  summarise(no2_daily = mean(no2, na.rm = T)) -> no2_daily_23

no2_2022 %>% 
  mutate(dt_date = as_date(date)) %>% 
  filter(dt_date >= "2022-03-01" & dt_date <= "2022-08-31") %>% 
  group_by(dt_date, code) %>% 
  summarise(no2_daily = mean(no2, na.rm = T)) -> no2_daily_22

no2_2021 %>% 
  mutate(dt_date = as_date(date)) %>% 
  filter(dt_date >= "2021-03-01" & dt_date <= "2021-08-31") %>% 
  group_by(dt_date, code) %>% 
  summarise(no2_daily = mean(no2, na.rm = T)) -> no2_daily_21


no2_daily_23 %>% 
  mutate(dt_weekdays = weekdays(dt_date),
         week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"),
         dt_month = month(dt_date),
         lez = ifelse(dt_month < 6, "Pre-LEZ", "Post-LEZ")) %>% 
  left_join(pre, by = "dt_date") -> no2_df_23


no2_df_23$lez <- factor(no2_df_23$lez, levels = c("Pre-LEZ", "Post-LEZ"))  

no2_df_23 %>% 
  drop_na(no2_daily) %>% 
  group_by(code, week_group, lez) %>% 
  summarise(no2_daily = mean(no2_daily)) %>% 
  pivot_wider(names_from = lez, values_from = no2_daily) %>% 
  mutate(`Diff_Per(%)` = -(1 - (`Post-LEZ` / `Pre-LEZ`)) * 100)

  
no2_df_23 %>% 
  drop_na(no2_daily) %>% 
  group_by(code) %>% 
  anova_test(no2_daily ~ week_group + lez) -> result3

result3$p <- result3$p %>% round(3)

result3

no2_df_23 |> 
  filter(code != "GLKP") |> 
  ggboxplot(x = "week_group", y = "no2_daily", color = "lez",
          palette = c("#00AFBB", "#E7B800")) +
  labs(x = "", y = "Daily Mean NO2") +
  facet_wrap(~code) 


ggsave("box1.jpg", width = 5, height = 3)


# post-hoc test
no2_df_23 %>% 
  group_by(code, week_group) %>% 
  games_howell_test(no2_daily ~ lez) #%>% View


no2_df_23 %>% 
  group_by(code, lez) %>% 
  games_howell_test(no2_daily ~ week_group) #%>% View


no2_df_23 %>% 
  drop_na(no2_daily) %>% 
  group_by(code) %>% 
  anova_test(no2_daily ~ week_group + Precipitation + lez) 



################################

no2_daily_22 %>% 
  mutate(dt_weekdays = weekdays(dt_date),
         week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"),
         dt_month = month(dt_date),
         lez = ifelse(dt_month < 6, "Pre-June 2022", "Post-June 2022")) %>% 
  left_join(pre, by = "dt_date") -> no2_df_22


no2_df_22$lez <- factor(no2_df_22$lez, levels = c("Pre-June 2022", "Post-June 2022"))  

no2_df_22 %>% 
  drop_na(no2_daily) %>% 
  group_by(code, week_group, lez) %>% 
  summarise(no2_daily = mean(no2_daily)) %>% 
  pivot_wider(names_from = lez, values_from = no2_daily) %>% 
  mutate(`Diff_Per(%)` = -(1 - (`Post-June 2022` / `Pre-June 2022`))*100)


no2_df_22 %>% 
  drop_na(no2_daily) %>% 
  group_by(code) %>% 
  anova_test(no2_daily ~ week_group + lez) -> result3

result3$p <- result3$p %>% round(3)

result3

no2_df_23 |> 
  filter(code != "GLKP") |> 
ggboxplot(x = "week_group", y = "no2_daily", color = "lez",
          palette = c("#00AFBB", "#E7B800")) +
  labs(x = "", y = "Daily Mean NO2") +
  facet_wrap(~code) 


ggsave("box22_2.jpg", width = 5, height = 3)


# post-hoc test
no2_df_22 %>% 
  group_by(code, week_group) %>% 
  games_howell_test(no2_daily ~ lez) #%>% View


no2_df_22 %>% 
  group_by(code, lez) %>% 
  games_howell_test(no2_daily ~ week_group)# %>% View


no2_df_22 %>% 
  drop_na(no2_daily) %>% 
  group_by(code) %>% 
  anova_test(no2_daily ~ week_group + Precipitation + lez) 

###########################################################

no2_daily_21 %>% 
  mutate(dt_weekdays = weekdays(dt_date),
         week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"),
         dt_month = month(dt_date),
         lez = ifelse(dt_month < 6, "Pre-June 2021", "Post-June 2021")) -> no2_df_21


no2_df_21$lez <- factor(no2_df_21$lez, levels = c("Pre-June 2021", "Post-June 2021"))  

no2_df_21 %>% 
  drop_na(no2_daily) %>% 
  group_by(code, week_group, lez) %>% 
  summarise(no2_daily = mean(no2_daily)) %>% 
  pivot_wider(names_from = lez, values_from = no2_daily) %>% 
  mutate(`Diff_Per(%)` = -(1 - (`Post-June 2021` / `Pre-June 2021`))*100)
