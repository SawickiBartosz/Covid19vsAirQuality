library(ggplot2)
library(dplyr)
library(tidyr)

apple_mobility_raw <- read.csv("applemobilitytrends-2020-11-22.csv")

apple_mobility <- apple_mobility_raw %>% 
  pivot_longer(-c(1:6), names_to = "Date_raw") %>% 
  mutate(Date = as.Date(substr(Date_raw,2,100000000L), format = "%Y.%m.%d")) %>%
  select(region, transportation_type,sub.region,Date,value)

apple_mobility %>% filter(region == "Poland") %>%
  ggplot(aes(x = Date, y = value)) + geom_line(aes(group = transportation_type, color = transportation_type))
