library(arrow)
library(data.table)
library(dplyr)


df <- read_parquet("glasgow.parquet")

df %>% 
  as_tibble
