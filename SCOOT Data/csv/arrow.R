library(arrow)
library(data.table)
library(dplyr)


df <- 
  list.files(pattern = "*.csv") %>% 
  purrr::map_df(~fread(.))


df %>% 
  as_arrow_table() %>% 
  write_parquet("glasgow.parquet")
