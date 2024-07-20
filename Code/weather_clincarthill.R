options(scipen = 999)

library(arrow)
library(tidyverse)
library(sf)
library(mapview)
library(openair)
library(rstatix)
library(mgcv)

cc22_aug <- read_csv("clincarthill_22_aug.csv")
cc22_sep <- read_csv("clincarthill_22_sep.csv")
cc23_aug <- read_csv("clincarthill_23_aug.csv")
cc23_sep <- read_csv("clincarthill_23_sep.csv")

cc22 <- bind_rows(cc22_aug, cc22_sep)
cc23 <- bind_rows(cc23_aug, cc23_sep)


# Create a named vector mapping compass directions to degrees
compass_to_degrees <- c(
  N = 0, NNE = 22.5, NE = 45, ENE = 67.5,
  E = 90, ESE = 112.5, SE = 135, SSE = 157.5,
  S = 180, SSW = 202.5, SW = 225, WSW = 247.5,
  W = 270, WNW = 292.5, NW = 315, NNW = 337.5
)

cc22 |> 
  rename(dttm = `Report Date / Time`,
         temp = `Air Temperature`,
         hum = `Relative Humidity`,
         ws = `Wind Speed`,                                                     
         wd = `Wind Direction`) |> 
  select(dttm, temp, hum, ws, wd) |> 
  mutate(day_name = wday(dttm, label=TRUE),
         dt_date = as_date(dttm),
         week_group = case_when(day_name %in% c("Mon", "Fri") ~ "Other Weekdays",
                                day_name %in% c("Tue", "Wed", "Thu") ~ "Core Weekdays",
                                day_name %in% c("Sat", "Sun") ~ "Weekends")
  ) |> 
  group_by(dt_date, week_group) |> 
  summarise(ws = mean(ws, na.rm = T),
            wd  = mean(wd, na.rm = T),
            hum = mean(hum, na.rm = T),
            temp = mean(temp, na.rm = T)) |> 
  ungroup() -> weather_by_week


cc23 |> 
  rename(dttm = `Report Date / Time`,
         temp = `Air Temperature`,
         hum = `Relative Humidity`,
         ws = `Wind Speed`,                                                     
         wd = `Wind Direction`) |> 
  select(dttm, temp, hum, ws, wd) |> 
  mutate(day_name = wday(dttm, label=TRUE),
         dt_date = as_date(dttm),
         week_group = case_when(day_name %in% c("Mon", "Fri") ~ "Other Weekdays",
                                day_name %in% c("Tue", "Wed", "Thu") ~ "Core Weekdays",
                                day_name %in% c("Sat", "Sun") ~ "Weekends")
  ) |> 
  group_by(dt_date, week_group) |> 
  summarise(ws = mean(ws, na.rm = T),
            wd  = mean(wd, na.rm = T),
            hum = mean(hum, na.rm = T),
            temp = mean(temp, na.rm = T)) |> 
  ungroup() -> weather_by_week23

#$################$################$################
load("traffic_and_no2_glasgow.RData")

df_for_analysis |> 
  filter(dt_date >= "2022-08-01" & dt_date <= "2022-09-30") |> 
  select(siteId, dt_date, dt_year, week_group, total_flow) |> 
  arrange(dt_date) -> traffic_22

traffic_22

df_for_analysis |> 
  filter(dt_date >= "2023-08-01" & dt_date <= "2023-09-30") |> 
  select(siteId, dt_date, dt_year, week_group, total_flow)  |> 
  arrange(dt_date) -> traffic_23

traffic_23

no2 |> 
  filter(dt_date >= "2022-08-01" & dt_date <= "2022-09-30") |> 
  select(code, dt_date, week_group, no2_daily) |> 
  mutate(dt_year = as.factor(2022)) -> no2_22

no2_22


no2 |> 
  filter(dt_date >= "2023-08-01" & dt_date <= "2023-09-30") |> 
  select(code, dt_date, week_group, no2_daily) |> 
  mutate(dt_year = as.factor(2023)) -> no2_23

no2_23


#$################$################$################
# High St 2022
no2_22 |> 
  filter(code == "GHSR") |> 
  full_join(traffic_22 |> filter(siteId == "GG2001_S"), by = c("dt_date", "dt_year", "week_group")) |> 
  full_join(weather_by_week, by = c("dt_date", "week_group")) |> 
  mutate(north_south = ws * sin(wd * pi / 180),
         east_west = ws * cos(wd * pi / 180))  -> df_highst_22

df_highst_22

# High St 2023
no2_23 |> 
  filter(code == "GHSR") |> 
  full_join(traffic_23 |> filter(siteId == "GG2001_S"), by = c("dt_date", "dt_year", "week_group")) |> 
  full_join(weather_by_week23, by = c("dt_date", "week_group")) |> 
  mutate(north_south = ws * sin(wd * pi / 180),
         east_west = ws * cos(wd * pi / 180)) -> df_highst_23

df_highst_23
 
bind_rows(df_highst_22, df_highst_23) -> highst
highst

# A positive cosine wind is from the west. 
# A negative cosine wind is from the east. 
# The sine wind runs parallel to the y axis. 
# A positive sine wind is from the south, and a negative sine wind is from the north.

gam_model <- gam(no2_daily ~ s(ws) + s(north_south) + s(east_west) + s(temp) + s(hum), data = highst)
summary(gam_model)

highst$NO2_normalised <- residuals(gam_model)

ggplot(highst, aes(x = dt_date, y = NO2_normalised)) +
  geom_line() +
  facet_wrap(~dt_year, scales = "free_x") +
  labs(title = "Normalized NO2 Concentration",
       x = "",
       y = "Normalised NO2")


############
no2_22 |> 
  filter(code == "GLA4") |> 
  full_join(traffic_22 |> filter(siteId == "GA2401_D"), by = c("dt_date", "dt_year", "week_group")) |> 
  full_join(weather_by_week, by = c("dt_date", "week_group")) |> 
  mutate(north_south = ws * sin(wd * pi / 180),
         east_west = ws * cos(wd * pi / 180)) -> df_hopest22

df_hopest22

no2_23 |> 
  filter(code == "GLA4") |> 
  full_join(traffic_23 |> filter(siteId == "GA2401_D"), by = c("dt_date", "dt_year","week_group")) |> 
  full_join(weather_by_week23, by = c("dt_date", "week_group")) |> 
  mutate(north_south = ws * sin(wd * pi / 180),
         east_west = ws * cos(wd * pi / 180)) -> df_hopest23

df_hopest23


bind_rows(df_hopest22, df_hopest23) -> hopest


gam_model2 <- gam(no2_daily ~ s(ws) + s(north_south) + s(east_west) + s(temp) + s(hum), data = hopest)
summary(gam_model2)

hopest$NO2_normalised <- residuals(gam_model2)

ggplot(hopest, aes(x = dt_date, y = NO2_normalised)) +
  geom_line() +
  facet_wrap(~dt_year, scales = "free_x") +
  labs(title = "Normalised NO2 Concentration",
       x = "",
       y = expression("NO"[2] * " (Normalized)"))


##################
##--Statistics--##
##################


highst |> 
  group_by(week_group, dt_year) |> 
  summarise(traffic = mean(total_flow),
            traffic_sd = sd(total_flow),
            no2 = mean(no2_daily),
            no2_sd = sd(no2_daily))


hopest |> 
  group_by(week_group, dt_year) |> 
  summarise(traffic = mean(total_flow),
            traffic_sd = sd(total_flow),
            no2 = mean(no2_daily),
            no2_sd = sd(no2_daily))


## Wilcox Signed ranked test
# Traffic
highst |> 
  group_by(week_group) |> 
  wilcox_test(total_flow ~ dt_year)|> 
  as.data.frame()

hopest |> 
  group_by(week_group) |> 
  wilcox_test(total_flow ~ dt_year)|> 
  as.data.frame()

# Normalised NO2
highst |> 
  group_by(week_group) |> 
  wilcox_test(NO2_normalised ~ dt_year) |> 
  as.data.frame()

hopest |> 
  group_by(week_group) |> 
  wilcox_test(NO2_normalised ~ dt_year)|> 
  as.data.frame()



# Traffic Flow Comparison
ggplot(highst, aes(x = interaction(dt_year, week_group), y = total_flow, fill = dt_year)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier points
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="black", fill="black") +  # Add mean points
  labs(
    title = "Traffic Flow on High Street",
    subtitle = "Comparison between Aug-Sep 2022/2023",
    x = "Year and Week Group",
    y = "Total Traffic Flow",
    fill = "Year"
  ) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    #plot.title = element_text(hjust = 0.5),  # Center the plot title
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),  # Rotate x-axis labels and adjust justification
    legend.position = "top"  # Position the legend at the top
  ) +
  scale_x_discrete(labels = function(x) gsub("\\.", "\n", x))  # Replace dots with newlines in x-axis labels

ggsave("traffic_flow_highst.jpg", width = 6.5, height = 5.5)


ggplot(hopest, aes(x = interaction(dt_year, week_group), y = total_flow, fill = dt_year)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier points
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="black", fill="black") +  # Add mean points
  labs(
    title = "Traffic Flow on Hope Street",
    subtitle = "Comparison between Aug-Sep 2022/2023",
    x = "Year and Week Group",
    y = "Total Traffic Flow",
    fill = "Year"
  ) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    #plot.title = element_text(hjust = 0.5),  # Center the plot title
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),  # Rotate x-axis labels and adjust justification
    legend.position = "top"  # Position the legend at the top
  ) +
  scale_x_discrete(labels = function(x) gsub("\\.", "\n", x))  # Replace dots with newlines in x-axis labels

ggsave("traffic_flow_hopest.jpg", width = 6.5, height = 5.5)

# NO2 Normalized Comparison
combined_range <- range(c(highst$NO2_normalised, hopest$NO2_normalised))


ggplot(highst, aes(x = interaction(dt_year, week_group), y = NO2_normalised, fill = dt_year)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier points
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="black", fill="black") +  # Add mean points
  labs(
    title = "Normalized NO2 Levels on High Street",
    subtitle = "Comparison between Aug-Sep 2022/2023",
    x = "Year and Week Group",
    y = expression("NO"[2] * " (Normalized)"),
    fill = "Year"
  ) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    #plot.title = element_text(hjust = 0.5),  # Center the plot title
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),  # Rotate x-axis labels and adjust justification
    legend.position = "top"  # Position the legend at the top
  ) +
  scale_x_discrete(labels = function(x) gsub("\\.", "\n", x)) +
  scale_y_continuous(limit = combined_range)

ggsave("normalized_no2_highst.jpg", width = 6.5, height = 5.5)


ggplot(hopest, aes(x = interaction(dt_year, week_group), y = NO2_normalised, fill = dt_year)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier points
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="black", fill="black") +  # Add mean points
  labs(
    title = "Normalized NO2 Levels on Hope Street",
    subtitle = "Comparison between Aug-Sep 2022/2023",
    x = "Year and Week Group",
    y = expression("NO"[2] * " (Normalized)"),
    fill = "Year"
  ) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    #plot.title = element_text(hjust = 0.5),  # Center the plot title
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),  # Rotate x-axis labels and adjust justification
    legend.position = "top"  # Position the legend at the top
  ) +
  scale_x_discrete(labels = function(x) gsub("\\.", "\n", x)) +
  scale_y_continuous(limits = combined_range)

ggsave("normalized_no2_hopest.jpg", width = 6.5, height = 5.5)
