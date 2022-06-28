

################################################################################

# 0) library tools & Set worksapce

suppressPackageStartupMessages({
  library(ipumsr)
  library(dplyr, warn.conflicts = FALSE)  
  library(sf)
  library(tidyr)
  library(devtools)
  library(ggplot2)
  library(mapview)
  library(stringi)
  library(tidyverse)
  library(tidycensus)
  library(leaflet)
  library(purrr)
  library(geosphere)
  library(rmapshaper)
  library(htmltools)
  library(scales)
  library(tmap)
  library(RColorBrewer)
  library(psych)
  library(reshape)
  library(rlang)
  library(timetk)
  library(kableExtra)
  library(highcharter)
  library(PerformanceAnalytics)
  library(httr)
  library(jsonlite)
  library(geojsonio)
  library(sp)
  library(tigris)
  library(rgdal)
  library(downloader)
  library(geojsonio)
  require(quantmod)
  options(tigris_class = "sf")
  options(tigris_use_cache = TRUE)
})
theme_set(theme_bw())

# Set file path
setwd("C:/ArcGIS/Projects/Dev_SAS/COPY")
# rm(list=ls())

################################################################################

# 1. Employee csv file
# 1) Load csv files

route_1 <- read.csv(file = 'Direction2.csv')
view(route_1)
colnames(route_1)

# 1. Right  
#   1) Turn right
#   2) Bear right
#   3) Make a sharp right

# 2. Left
#   1) Turn left
#   2) Bear left
#   3) Make a sharp left

route_turn_1 <-
route_1 %>%
  unite("com_route", Route:State, remove = FALSE, sep="") %>%
  select(com_route) %>%
  mutate(
    start_div = case_when(
      grepl("Begin route", com_route) ~ "Start"
    ),
    route_div = case_when(
      start_div == "Start" ~ 1,
      start_div == NA ~ 0
    )
  ) %>%
  replace(is.na(.), 0) %>%
  mutate(
    route_num = cumsum(route_div)
  ) %>%
  mutate(
    turn_right = case_when (
      grepl("Turn right", com_route) ~ "Right",
      grepl("turn right", com_route) ~ "Right",
      grepl("Bear right", com_route) ~ "Right",
      grepl("bear right", com_route) ~ "Right",
      grepl("Make a sharp right", com_route) ~ "Right",
      grepl("make a sharp right", com_route) ~ "Right"
    ),
    turn_left = case_when (
      grepl("Turn left", com_route) ~ "Left",
      grepl("turn left", com_route) ~ "Left",
      grepl("Bear left", com_route) ~ "Left",
      grepl("bear left", com_route) ~ "Left",
      grepl("Make a sharp left", com_route) ~ "Left",
      grepl("make a sharp left", com_route) ~ "Left"
    )
  ) %>%
  replace(is.na(.), 0)
view(route_turn_1)
colnames(route_turn_1)

################################################################################

route_turn_2 <-
route_1 %>%
  unite("com_route", Route:State, remove = FALSE, sep="") %>%
  select(com_route) %>%
  mutate(
    com_route_1 = com_route,
    start_div = case_when(
      grepl("Begin route", com_route) ~ "Start"
    ),
    route_div = case_when(
      start_div == "Start" ~ 1,
      start_div == NA ~ 0
    )
  ) %>%
  separate(com_route_1, c("other","market"), sep = "- ") %>%
  fill(market, .direction = "up") %>%
  select(com_route, market, start_div, route_div) %>%
  replace(is.na(.), 0) %>%
  mutate(
    route_num = cumsum(route_div)
  ) %>%
  mutate(
    turn_right = case_when (
      grepl("Turn right", com_route) ~ "Right",
      grepl("turn right", com_route) ~ "Right",
      grepl("Bear right", com_route) ~ "Right",
      grepl("bear right", com_route) ~ "Right",
      grepl("Make a sharp right", com_route) ~ "Right",
      grepl("make a sharp right", com_route) ~ "Right"
    ),
    turn_left = case_when (
      grepl("Turn left", com_route) ~ "Left",
      grepl("turn left", com_route) ~ "Left",
      grepl("Bear left", com_route) ~ "Left",
      grepl("bear left", com_route) ~ "Left",
      grepl("Make a sharp left", com_route) ~ "Left",
      grepl("make a sharp left", com_route) ~ "Left"
    )
  ) %>%
  select(
    com_route, market, route_num, turn_right, turn_left
  ) %>%
  replace(is.na(.), 0)
view(route_turn_2)
colnames(route_turn_2)  

################################################################################

# Summarize the number of turn right by market and route 
route_summ_right <-
  route_1 %>%
  unite("com_route", Route:State, remove = FALSE, sep="") %>%
  select(com_route) %>%
  mutate(
    com_route_1 = com_route,
    start_div = case_when(
      grepl("Begin route", com_route) ~ "Start"
    ),
    route_div = case_when(
      start_div == "Start" ~ 1,
      start_div == NA ~ 0
    )
  ) %>%
  separate(com_route_1, c("other","market"), sep = "- ") %>%
  fill(market, .direction = "up") %>%
  select(com_route, market, start_div, route_div) %>%
  replace(is.na(.), 0) %>%
  mutate(
    route_num = cumsum(route_div)
  ) %>%
  mutate(
    turn_right = case_when (
      grepl("Turn right", com_route) ~ 1,
      grepl("turn right", com_route) ~ 1,
      grepl("Bear right", com_route) ~ 1,
      grepl("bear right", com_route) ~ 1,
      grepl("Make a sharp right", com_route) ~ 1,
      grepl("make a sharp right", com_route) ~ 1
    ),
    turn_left = case_when (
      grepl("Turn left", com_route) ~ 1,
      grepl("turn left", com_route) ~ 1,
      grepl("Bear left", com_route) ~ 1,
      grepl("bear left", com_route) ~ 1,
      grepl("Make a sharp left", com_route) ~ 1,
      grepl("make a sharp left", com_route) ~ 1
    )
  ) %>%
  select(
    com_route, market, route_num, turn_right
  ) %>%
  replace(is.na(.), 0) %>%
  group_by(market, route_num, turn_right) %>%
  summarise(sum_right = sum(turn_right)) %>%
  group_by(market, route_num) %>%
  summarise(sum_right2 = sum(sum_right)) %>%
  arrange(route_num)
view(route_summ_right)

# Summarize the number of turn left by market and route 
route_summ_left <-
  route_1 %>%
  unite("com_route", Route:State, remove = FALSE, sep="") %>%
  select(com_route) %>%
  mutate(
    com_route_1 = com_route,
    start_div = case_when(
      grepl("Begin route", com_route) ~ "Start"
    ),
    route_div = case_when(
      start_div == "Start" ~ 1,
      start_div == NA ~ 0
    )
  ) %>%
  separate(com_route_1, c("other","market"), sep = "- ") %>%
  fill(market, .direction = "up") %>%
  select(com_route, market, start_div, route_div) %>%
  replace(is.na(.), 0) %>%
  mutate(
    route_num = cumsum(route_div)
  ) %>%
  mutate(
    turn_right = case_when (
      grepl("Turn right", com_route) ~ 1,
      grepl("turn right", com_route) ~ 1,
      grepl("Bear right", com_route) ~ 1,
      grepl("bear right", com_route) ~ 1,
      grepl("Make a sharp right", com_route) ~ 1,
      grepl("make a sharp right", com_route) ~ 1
    ),
    turn_left = case_when (
      grepl("Turn left", com_route) ~ 1,
      grepl("turn left", com_route) ~ 1,
      grepl("Bear left", com_route) ~ 1,
      grepl("bear left", com_route) ~ 1,
      grepl("Make a sharp left", com_route) ~ 1,
      grepl("make a sharp left", com_route) ~ 1
    )
  ) %>%
  select(
    com_route, market, route_num, turn_left
  ) %>%
  replace(is.na(.), 0) %>%
  group_by(market, route_num, turn_left) %>%
  summarise(sum_left = sum(turn_left)) %>%
  group_by(market, route_num) %>%
  summarise(sum_left2 = sum(sum_left)) %>%
  arrange(route_num)
view(route_summ_left)
colnames(route_summ_left)

# Join the Two table
route_summ_join <-
full_join(route_summ_right, route_summ_left, by = 'route_num') %>%
  select(route_num, market.x, sum_right2, sum_left2) %>%
  dplyr::rename(
    market = "market.x",
    num_right = "sum_right2",
    num_left = "sum_left2"
  )
view(route_summ_join)
head(route_summ_join)

# calculate values by the markets

route_summ_final <-
route_summ_join %>%
  group_by(market) %>%
  summarise(
    MEAN_Right_Turn = mean(num_right),
    MAX_Right_Turn = max(num_right),
    MIN_Right_Turn = min(num_right),
    MEDIAN_Right_Turn = median(num_right),
    RANGE_Right_Turn = MAX_Right_Turn - MIN_Right_Turn,
    MEAN_Left_Turn = mean(num_left),
    MAX_Left_Turn = max(num_left),
    MIN_Left_Turn = min(num_left),
    MEDIAN_Left_Turn = median(num_left),
    RANGE_Left_Turn = MAX_Left_Turn - MIN_Left_Turn
  )
view(route_summ_final)

################################################################################

# Export excel
write.csv(route_summ_final, file = 'route_summ_final.csv')

################################################################################





