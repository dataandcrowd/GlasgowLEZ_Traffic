library(data.table)
library(dplyr)
library(lubridate)

ga1321 <- fread("GA1321_B&2022-09-01&2023-09-30.csv") |> 
  as_tibble() |> mutate(ID = "GA1321_B")
# ga1571 <- fread("GA1571_Q&2022-09-01&2023-09-30.csv") |> 
#   as_tibble() |> mutate(ID = "GA1571_Q")
ga2401 <- fread("GA2401_D&2022-09-01&2023-09-30.csv") |> 
  as_tibble() |> mutate(ID = "GA2401_D")
ga5371 <- fread("GA5371_C&2022-09-01&2023-09-30.csv") |> 
  as_tibble() |> mutate(ID = "GA5371_C")
gg2001 <- fread("GG2001_S&2022-09-01&2023-09-30.csv") |> 
  as_tibble() |> mutate(ID = "GG2001_S")

ga1321 |> 
  mutate(dttm = as_datetime(paste0(date, " ", time, ":00:00")),
         year = year(dttm)) |> 
  select(ID, dttm, year, flow) -> ga1321

ga2401 |> 
  mutate(dttm = as_datetime(paste0(date, " ", time, ":00:00")),
         year = year(dttm)) |> 
  select(ID, dttm, year, flow) -> ga2401

ga5371 |> 
  mutate(dttm = as_datetime(paste0(date, " ", time, ":00:00")),
         year = year(dttm)) |> 
  select(ID, dttm, year, flow) -> ga5371

gg2001 |> 
  mutate(dttm = as_datetime(paste0(date, " ", time, ":00:00")),
         year = year(dttm)) |> 
  select(ID, dttm, year, flow) -> gg2001


dt22_sep <- bind_rows(ga1321 |> filter(year == 2022), 
                      ga2401 |> filter(year == 2022),
                      ga5371 |> filter(year == 2022), 
                      gg2001 |> filter(year == 2022))


dt23_sep <- bind_rows(ga1321 |> filter(year == 2023), 
                      ga2401 |> filter(year == 2023),
                      ga5371 |> filter(year == 2023), 
                      gg2001 |> filter(year == 2023))


rm(list=setdiff(ls(), c("dt22_sep", "dt23_sep")))

save.image("SCOOT_SEP22_SEP23.RData")
