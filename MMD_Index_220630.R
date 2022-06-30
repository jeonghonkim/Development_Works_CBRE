
#install.packages("lubridate")
#install.packages("linelist")
#install.packages("aweek")
#install.packages("zoo")
#install.packages("rio")
install.packages(dbplyr)


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
  library(readxl)
  library(writexl)
  library(lubridate)  
  library(linelist)   
  library(aweek)
  library(zoo)
  library(rio)
  require(quantmod)
  options(tigris_class = "sf")
  options(tigris_use_cache = TRUE)
})
theme_set(theme_bw())

# Set file path
setwd("C:/ArcGIS/Projects/Dev_MMD/csv")
# rm(list=ls())

################################################################################

# 0. Import Excel file
mmd_ft_1 <- read_excel("AEO_UWS_ColumbusAve-MMDResults-20220627170015.xlsx", sheet = "Ft Per Hour")
mmd_ft_2 <- read_excel("AEO_UWS_ColumbusCircle-MMDResults-20220627160037.xlsx", sheet = "Ft Per Hour")
#view(mmd_ft_1)
#view(mmd_ft_2)

################################################################################

# 1. Foot Traffic per Hour

# 1-1. Create a summary table
mmd_ft_1_Hour <-
mmd_ft_1 %>%
  rename_with(~ gsub(" ", ".", .x)) %>%
  select(
    Day.of.Week, Hour, Count
  ) %>%
  group_by(
    Day.of.Week, Hour
  ) %>%
  summarise(Count = sum(Count)) %>%
  cast(Hour ~ Day.of.Week) %>%
  select(
    Hour, Mon, Tue, Wed, Thu, Fri, Sat, Sun
  ) %>%
  mutate(
    MeanHour = rowMeans(.[,2:8]),
    Project.Name = "MMD_1"
  )

mmd_ft_2_Hour <-
  mmd_ft_2 %>%
  rename_with(~ gsub(" ", ".", .x)) %>%
  select(
    Day.of.Week, Hour, Count
  ) %>%
  group_by(
    Day.of.Week, Hour
  ) %>%
  summarise(Count = sum(Count)) %>%
  cast(Hour ~ Day.of.Week) %>%
  select(
    Hour, Mon, Tue, Wed, Thu, Fri, Sat, Sun
  ) %>%
  mutate(
    MeanHour = rowMeans(.[,2:8]),
    Project.Name = "MMD_2"
  )

mmd_ft_12_Hour <-
mmd_ft_1_Hour %>%
  rbind(mmd_ft_2_Hour)
view(mmd_ft_12_Hour)

mmd_ft_hour_index <-
mmd_ft_12_Hour %>%
  mutate(
    mean_MeanHour = mean(MeanHour),
    stdv_MeanHour = sd(MeanHour),
    var1_weight = 1,
    Var1_Z = (MeanHour-mean_MeanHour)/stdv_MeanHour,
    Var1_cappped = if_else(Var1_Z > 3, 3, 
                           if_else(Var1_Z < -3, -3, Var1_Z)),
    com_z = var1_weight*Var1_cappped,
    max_z = max(com_z),
    demand_index = (1+(com_z/max_z))*100,
    index_adj = mean(demand_index),
    final_score_pre = demand_index/index_adj*100,
    final_score = if_else(final_score_pre < 0, 0, final_score_pre)
  ) %>%
  select(
    Hour, final_score, Project.Name
  ) %>% 
  mutate_if(is.numeric,
            round,
            digits = 1)
# view(mmd_ft_hour_index)

# 1-2. Create a Chart
hc_theme_example = 
  hc_theme_merge(
    hc_theme_google(),
    hc_theme(
      colors = c(
        "#0C6A4D", # Dark Green
        "#80BA42", # Light Green
        "#0C2340", # Navy
        "#C8102E", # Red
        "#85714D" # Gold
      ),
      chart = list(
        backgroundColor = "#FFFFFF",
        style = list(
          fontFamily = "Calibre"
        )
      ),
      title = list(
        color = "#333333",
        fontFamily = "Calibre"
      ),
      subtitle = list(
        color = "#666666",
        fontFamily = "Calibre"
      ),
      plotOptions = list(
        line = list(marker = list(symbol = "circle", lineWidth = 2, radius = 5))
      )
    )
  )

mmd_ft_hour_index %>%
  hchart(.,
         type = "line",
         hcaes(x = Hour,
               y = final_score,
               group = Project.Name
         )) %>%
  hc_yAxis(
    opposite = FALSE,
    labels = list(format = "{value}")
  ) %>%
  hc_xAxis(
    opposite = FALSE,
    labels = list(format = "{value}")
  ) %>%
  hc_title(
    text = "Foot Traffic per Hour"
  ) %>%
  hc_add_theme(hc_theme_example) %>%
  hc_size(width=800,height=500)

################################################################################

# 2. Foot Traffic per Hour

# 2-1. Create a summary table
mmd_ft_1_Day <-
mmd_ft_1 %>%
  rename_with(~ gsub(" ", ".", .x)) %>%
  select(
    Day.of.Week, Count
  ) %>%
  group_by(
    Day.of.Week
  ) %>%
  summarise(Count = sum(Count)) %>%
  mutate(
    Day.of.week.num = case_when(
    Day.of.Week == "Mon" ~ 1,
    Day.of.Week == "Tue" ~ 2,
    Day.of.Week == "Wed" ~ 3,
    Day.of.Week == "Thu" ~ 4,
    Day.of.Week == "Fri" ~ 5,
    Day.of.Week == "Sat" ~ 6,
    Day.of.Week == "Sun" ~ 7,
  )) %>%
  arrange(Day.of.week.num) %>%
  select(Day.of.Week, Count) %>%
  mutate(
    Project.Name = "MMD_1"
  )

mmd_ft_2_Day <-
  mmd_ft_2 %>%
  rename_with(~ gsub(" ", ".", .x)) %>%
  select(
    Day.of.Week, Count
  ) %>%
  group_by(
    Day.of.Week
  ) %>%
  summarise(Count = sum(Count)) %>%
  mutate(
    Day.of.week.num = case_when(
      Day.of.Week == "Mon" ~ 1,
      Day.of.Week == "Tue" ~ 2,
      Day.of.Week == "Wed" ~ 3,
      Day.of.Week == "Thu" ~ 4,
      Day.of.Week == "Fri" ~ 5,
      Day.of.Week == "Sat" ~ 6,
      Day.of.Week == "Sun" ~ 7,
    )) %>%
  arrange(Day.of.week.num) %>%
  select(Day.of.Week, Count) %>%
  mutate(
    Project.Name = "MMD_2"
  )   

mmd_ft_12_Day <-
mmd_ft_1_Day %>%
  rbind(mmd_ft_2_Day)
# view(mmd_ft_12_Day)

mmd_ft_day_index <-
mmd_ft_12_Day %>%
  mutate(
    mean_Hour = mean(Count),
    stdv_Hour = sd(Count),
    var1_weight = 1,
    Var1_Z = (Count-mean_Hour)/stdv_Hour,
    Var1_cappped = if_else(Var1_Z > 3, 3, 
                           if_else(Var1_Z < -3, -3, Var1_Z)),
    com_z = var1_weight*Var1_cappped,
    max_z = max(com_z),
    demand_index = (1+(com_z/max_z))*100,
    index_adj = mean(demand_index),
    final_score_pre = demand_index/index_adj*100,
    final_score = if_else(final_score_pre < 0, 0, final_score_pre)
  ) %>%
  select(
    Day.of.Week, final_score, Project.Name
  ) %>%
  mutate_if(is.numeric,
            round,
            digits = 1)
# view(mmd_ft_day_index)

# 2-2. Create a Chart
mmd_ft_day_index %>%
  hchart(.,
         type = "bar",
         hcaes(x = Day.of.Week,
               y = final_score,
               group = Project.Name
         )) %>%
  hc_yAxis(
    opposite = FALSE,
    labels = list(format = "{value}")
  ) %>%
  hc_xAxis(
    opposite = FALSE,
    labels = list(format = "{value}")
  ) %>%
  hc_title(
    text = "Traffic by Day"
  ) %>%
  hc_add_theme(hc_theme_example) %>%
  hc_size(width=800,height=500)



################################################################################
################################################################################
################################################################################

mmd_ft_history1 <- read_excel("AEO_UWS_ColumbusAve-MMDResults-20220627170015.xlsx", sheet = "FT Historical Trends")
mmd_ft_history2 <- read_excel("AEO_UWS_ColumbusCircle-MMDResults-20220627160037.xlsx", sheet = "FT Historical Trends")

view(mmd_ft_history)
colnames(mmd_ft_history)

mmd_ft_history1_month <-
mmd_ft_history1 %>%
  select(Date, Hour, Count) %>%
  mutate(
    date_onset = as.Date(Date),
    year = year(date_onset),
    month = month(date_onset),
    day = mday(date_onset),
    day.of.week = wday(date_onset, label = TRUE)
  ) %>%
  filter(
    Date >= "2019-01-01" & Date <= "2019-12-31"
  ) %>%
  select(date_onset, year, month, day, day.of.week, Hour, Count) %>%
  dplyr::rename(
    hour = "Hour",
    count = "Count"
  ) %>%
  group_by(month) %>%
  summarise(m.count = sum(count)) %>%
  mutate(
    MMD = "MMD_1"
  )

mmd_ft_history1_day <-
mmd_ft_history1 %>%
  select(Date, Hour, Count) %>%
  mutate(
    date_onset = as.Date(Date),
    year = year(date_onset),
    month = month(date_onset),
    day = mday(date_onset),
    day.of.week = wday(date_onset, label = TRUE)
  ) %>%
  filter(
    Date >= "2019-01-01" & Date <= "2019-12-31"
  ) %>%
  select(date_onset, year, month, day, day.of.week, Hour, Count) %>%
  dplyr::rename(
    hour = "Hour",
    count = "Count"
  ) %>%
  group_by(day.of.week) %>%
  summarise(d.count = sum(count)) %>%
  mutate(
    MMD = "MMD_1"
  )

mmd_ft_history1_hour <-
mmd_ft_history1 %>%
  select(Date, Hour, Count) %>%
  mutate(
    date_onset = as.Date(Date),
    year = year(date_onset),
    month = month(date_onset),
    day = mday(date_onset),
    day.of.week = wday(date_onset, label = TRUE)
  ) %>%
  filter(
    Date >= "2019-01-01" & Date <= "2019-12-31"
  ) %>%
  select(date_onset, year, month, day, day.of.week, Hour, Count) %>%
  dplyr::rename(
    hour = "Hour",
    count = "Count"
  ) %>%
  group_by(hour) %>%
  summarise(h.count = sum(count)) %>%
  mutate(
    MMD = "MMD_1"
  )
  

mmd_ft_history2_month <-
  mmd_ft_history2 %>%
  select(Date, Hour, Count) %>%
  mutate(
    date_onset = as.Date(Date),
    year = year(date_onset),
    month = month(date_onset),
    day = mday(date_onset),
    day.of.week = wday(date_onset, label = TRUE)
  ) %>%
  filter(
    Date >= "2019-01-01" & Date <= "2019-12-31"
  ) %>%
  select(date_onset, year, month, day, day.of.week, Hour, Count) %>%
  dplyr::rename(
    hour = "Hour",
    count = "Count"
  ) %>%
  group_by(month) %>%
  summarise(m.count = sum(count)) %>%
  mutate(
    MMD = "MMD_2"
  )

mmd_ft_history2_day <-
  mmd_ft_history2 %>%
  select(Date, Hour, Count) %>%
  mutate(
    date_onset = as.Date(Date),
    year = year(date_onset),
    month = month(date_onset),
    day = mday(date_onset),
    day.of.week = wday(date_onset, label = TRUE)
  ) %>%
  filter(
    Date >= "2019-01-01" & Date <= "2019-12-31"
  ) %>%
  select(date_onset, year, month, day, day.of.week, Hour, Count) %>%
  dplyr::rename(
    hour = "Hour",
    count = "Count"
  ) %>%
  group_by(day.of.week) %>%
  summarise(d.count = sum(count)) %>%
  mutate(
    MMD = "MMD_2"
  )

mmd_ft_history2_hour <-
mmd_ft_history2 %>%
  select(Date, Hour, Count) %>%
  mutate(
    date_onset = as.Date(Date),
    year = year(date_onset),
    month = month(date_onset),
    day = mday(date_onset),
    day.of.week = wday(date_onset, label = TRUE)
  ) %>%
  filter(
    Date >= "2019-01-01" & Date <= "2019-12-31"
  ) %>%
  select(date_onset, year, month, day, day.of.week, Hour, Count) %>%
  dplyr::rename(
    hour = "Hour",
    count = "Count"
  ) %>%
  group_by(hour) %>%
  summarise(h.count = sum(count)) %>%
  mutate(
    MMD = "MMD_2"
  )



mmd_ft_history12_month <-
mmd_ft_history1_month %>%
  rbind(mmd_ft_history2_month)

mmd_ft_history12_day <-
mmd_ft_history1_day %>%
  rbind(mmd_ft_history2_day)

mmd_ft_history12_hour <-
mmd_ft_history1_hour %>%
  rbind(mmd_ft_history2_hour)

view(mmd_ft_history12_month)
view(mmd_ft_history12_day)
view(mmd_ft_history12_hour)





head(mmd_ft_history12_month)

mmd_ft_history12_month %>%
mutate(
  mean_month = mean(m.count),
  stdv_month = sd(m.count),
  var1_weight = 1,
  Var1_Z = (m.count-mean_month)/stdv_month,
  Var1_cappped = if_else(Var1_Z > 3, 3, 
                         if_else(Var1_Z < -3, -3, Var1_Z)),
  com_z = var1_weight*Var1_cappped,
  max_z = max(com_z),
  demand_index = (1+(com_z/max_z))*100,
  index_adj = mean(demand_index),
  final_score_pre = demand_index/index_adj*100,
  final_score = if_else(final_score_pre < 0, 0, final_score_pre)
) %>%
  select(
    month, final_score, MMD
  ) %>% 
  mutate_if(is.numeric,
            round,
            digits = 1)



hc_theme_example = 
  hc_theme_merge(
    hc_theme_google(),
    hc_theme(
      colors = c(
        "#0C6A4D", # Dark Green
        "#80BA42", # Light Green
        "#0C2340", # Navy
        "#C8102E", # Red
        "#85714D" # Gold
      ),
      chart = list(
        backgroundColor = "#FFFFFF",
        style = list(
          fontFamily = "Calibre"
        )
      ),
      title = list(
        color = "#333333",
        fontFamily = "Calibre"
      ),
      subtitle = list(
        color = "#666666",
        fontFamily = "Calibre"
      ),
      plotOptions = list(
        line = list(marker = list(symbol = "circle", lineWidth = 2, radius = 5))
      )
    )
  )

mmd_ft_hour_index %>%
  hchart(.,
         type = "line",
         hcaes(x = Hour,
               y = final_score,
               group = Project.Name
         )) %>%
  hc_yAxis(
    opposite = FALSE,
    labels = list(format = "{value}")
  ) %>%
  hc_xAxis(
    opposite = FALSE,
    labels = list(format = "{value}")
  ) %>%
  hc_title(
    text = "Foot Traffic per Hour"
  ) %>%
  hc_add_theme(hc_theme_example) %>%
  hc_size(width=800,height=500)












