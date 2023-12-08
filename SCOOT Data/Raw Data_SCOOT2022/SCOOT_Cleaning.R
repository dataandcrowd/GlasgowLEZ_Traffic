library(arrow)
library(data.table)
library(tidyverse)


files <- list.files(pattern = ".csv")

# Load those files into a list of data.tables:
dt_list <- lapply(files, fread)


# Name each list element after its file of origin:
names(dt_list) <- files

# Concatenate all files into a single data.table, with
# an additional column containing the filename each row 
# came from (taken from the names(dt_list))
dt <- rbindlist(dt_list, idcol = "file")

dt %>% 
  as_tibble %>% 
  mutate(ID = substr(file, 1, 7)) -> dt_raw


#Clean DF
dt_raw %>% 
  mutate(dt = paste(date, time, sep = " "),
         dt = paste(dt, ":00:00"),
         dt = ymd_hms(dt)) %>% 
  select(ID, dt, flow) -> dt_df

dt_df %>% 
  arrow_table %>% 
  write_parquet("glasgow2022.parquet")
