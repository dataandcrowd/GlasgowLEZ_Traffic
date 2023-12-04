library(sf)
library(mapview)
library(openair)
library(tidyverse)

load("openair_glasgow.RData")


scotmonitor %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mapview()

unique(no2_2021$code)

no2_2023 %>% 
  filter(code == "GHSR") %>% 
  ggplot(aes(date, no2)) +
  geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-06-01")), linetype="dashed", 
             color = "red", linewidth=1) +
  theme_bw()


no2_2023 %>% 
  filter(code == "GLKP") %>% 
  ggplot(aes(date, no2)) +
  geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-06-01")), linetype="dashed", 
             color = "red", linewidth=1) +
  theme_bw()


no2_2023 %>% 
  filter(code == "GLA4") %>% 
  ggplot(aes(date, no2)) +
  geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-06-01")), linetype="dashed", 
             color = "red", linewidth=1) +
  theme_bw()


no2 <- rbind(no2_2021, no2_2022, no2_2023)

no2 %>% 
  ggplot(aes(date, no2)) +
  geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-06-01")), linetype="dashed", 
             color = "red", linewidth=1) +
  facet_grid(rows = vars(code)) +
  ylim(0,100) +
  labs(x = "", y = "NO2(ppb)", title = "NO2 in Glasgow Jan 2021- Nov 2023", 
       caption = "GHSR: Glasgow High Street (Traffic)\nGLA4: Glasgow Kerbside (Traffic)\nGLKP: Glasgow Townhead (Background)") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("no2_timeseries.jpg", width = 7, height = 5)  



no2 %>% 
  filter(date >= "2023-01-01 00:00:00") %>% 
  ggplot(aes(date, no2)) +
  geom_line() +
  geom_smooth() +
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-06-01")), linetype="dashed", 
             color = "red", linewidth=1) +
  facet_grid(rows = vars(code)) +
  ylim(0,80) +
  labs(x = "", y = "NO2(ppb)", title = "NO2 in Glasgow Jan - Nov 2023",
       caption = "GHSR: Glasgow High Street (Traffic)\nGLA4: Glasgow Kerbside (Traffic)\nGLKP: Glasgow Townhead (Background)") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("no2_timeseries_2023.jpg", width = 6, height = 5)  


no2 %>% 
  mutate(year_month = format(as.Date(date), "%Y.%m")) %>% 
  ggplot(aes(year_month, no2)) +
  geom_boxplot(aes(fill = code)) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2023-06-01")), linetype="dashed", 
             color = "red", linewidth=1) +
  facet_grid(rows = vars(code)) +
  ylim(0,100) +
  labs(x = "", y = "NO2(ppb)", title = "NO2 in Glasgow Jan 2021- Nov 2023", 
       caption = "GHSR: Glasgow High Street (Traffic)\nGLA4: Glasgow Kerbside (Traffic)\nGLKP: Glasgow Townhead (Background)") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 12),  
        strip.text = element_text(size = 15))

ggsave("no2_ts_box.jpg", width = 8, height = 5)  


##############

no2 %>% 
  mutate(hour = hour(date)) %>% 
  group_by(code, hour) %>% 
  summarise(no2_hour = mean(no2, na.rm = T)) %>% 
  pivot_wider(names_from = code, values_from = no2_hour) %>% 
  print(n = Inf)


no2 %>% 
  mutate(month = month(date)) %>% 
  group_by(code, month) %>% 
  summarise(no2_month = mean(no2, na.rm = T)) %>% 
  pivot_wider(names_from = code, values_from = no2_month) %>% 
  print(n = Inf)


no2 %>% 
  mutate(month_year = floor_date(as_date(date), "month")) %>% 
  group_by(code, month_year) %>% 
  summarise(no2_month = mean(no2, na.rm = T)) %>% 
  print(n = Inf)


no2 %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  filter(year == 2022) %>% 
  group_by(code, month) %>% 
  summarise(no2_month = mean(no2, na.rm = T)) %>% 
  pivot_wider(names_from = month, values_from = no2_month) 

#####
library(ggpubr)
library(rstatix)

no2 %>% 
  mutate(dt_date = date(date),
         dt_hour = hour(date),
         dt_day = day(date),
         dt_month = month(date),
         dt_weekdays = weekdays(date),
         dt_year = year(date),
         lez = ifelse(dt_month < 6, "Pre-LEZ", "Post-LEZ")) -> no2_df

no2_df$lez <- factor(no2_df$lez, levels = c("Pre-LEZ", "Post-LEZ")) 


###### GLA4


### Day of Week

# GLA4
no2_df %>% 
  filter(dt_year == 2023, code == "GLA4") %>% 
  filter(dt_month %in% c(3:8)) %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  drop_na %>% 
  anova_test(no2 ~ week_group + lez)

no2_df %>% 
  filter(dt_year == 2023, code == "GLA4") %>% 
  filter(dt_month %in% c(3:8)) %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  drop_na %>%  
  ggboxplot(x = "week_group", y = "no2", color = "lez",
            palette = c("#00AFBB", "#E7B800")) +
  labs(x = "", caption = "GLA4: Urban Traffic") +
  ylim(0,80) +
  stat_compare_means(method = "anova") 

ggsave("no2_GLA4.jpg", width = 6, height = 4)


# GLKP
no2_df %>% 
  filter(dt_year == 2023, code == "GLKP") %>% 
  filter(dt_month %in% c(3:8)) %>% 
  drop_na %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  drop_na %>% 
  anova_test(no2 ~ week_group + lez) 


no2_df %>% 
  filter(dt_year == 2023, code == "GLKP") %>% 
  filter(dt_month %in% c(3:8)) %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  drop_na %>%  
ggboxplot(x = "week_group", y = "no2", color = "lez",
          palette = c("#00AFBB", "#E7B800")) +
  labs(x = "", caption = "GLKP: Urban Background") +
  ylim(0,80) +
  stat_compare_means(method = "anova")  

ggsave("no2_GLKP.jpg", width = 6, height = 4)

## GHSR
no2_df %>% 
  filter(dt_year == 2023, code == "GHSR") %>% 
  filter(dt_month %in% c(3:8)) %>% 
  drop_na %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  drop_na %>% 
  anova_test(no2 ~ week_group + lez) 


no2_df %>% 
  filter(dt_year == 2023, code == "GHSR") %>% 
  filter(dt_month %in% c(3:8)) %>% 
  mutate(week_group = case_when(dt_weekdays %in% c("Tuesday", "Wednesday", "Thursday") ~ "Core Weekdays",
                                dt_weekdays %in% c("Monday", "Friday") ~ "Other Weekdays",
                                dt_weekdays %in% c("Saturday", "Sunday") ~ "Weekends"
  )) %>% 
  drop_na %>%  
  ggboxplot(x = "week_group", y = "no2", color = "lez",
            palette = c("#00AFBB", "#E7B800")) +
  labs(x = "", caption = "GHSR: Urban Traffic") +
  stat_compare_means(method = "anova")  

ggsave("no2_GHSR.jpg", width = 6, height = 4)
