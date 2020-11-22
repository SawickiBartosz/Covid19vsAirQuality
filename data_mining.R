library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)


df <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

countries = c("Poland", "Sweden", "South Korea")

df %>% filter(location == countries) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location)) + 
  scale_y_log10() + 
  ggtitle("Total cases per million")


yesterday <- Sys.Date() -1

df %>% filter(date == yesterday) %>%
  ggplot() + 
  geom_point(aes(x = population, y = total_cases_per_million, color = continent)) + 
  scale_x_log10() + 
  scale_y_log10() +
  scale_color_brewer(palette = "Set1")


df %>% filter(date == yesterday) %>%
  ggplot() + 
  geom_point(aes(x = human_development_index, y = total_cases_per_million, color = continent)) + 
  scale_y_log10() +
  scale_color_brewer(palette = "Set1")

df %>% filter(date == yesterday) %>%
  ggplot() + 
  geom_density2d(aes(x = human_development_index, y = total_cases_per_million))

df %>% filter(date == yesterday) %>%
  ggplot() + 
  geom_point(aes(x = human_development_index, y = total_tests_per_thousand * 1000, color = continent)) + 
  scale_y_log10() +
  scale_color_brewer(palette = "Set1")

df %>% filter(date == yesterday) %>%
  ggplot() + 
  geom_point(aes(x = human_development_index, y = total_deaths_per_million, color = continent)) + 
  scale_y_log10() +
  scale_color_brewer(palette = "Set1")


df %>% filter(date == yesterday) %>%
  ggplot() + 
  geom_point(aes(x = median_age, y = total_deaths_per_million, color = continent)) + 
  scale_y_log10() +
  scale_color_brewer(palette = "Set1")


df %>% filter(date == yesterday) %>%
  ggplot() + 
  geom_point(aes(x = human_development_index, y = total_deaths_per_million,color = cardiovasc_death_rate)) + 
  scale_color_gradient(low = "blue", high = "red")

df %>% filter(date == yesterday) %>%
  ggplot() + 
  geom_point(aes(x = life_expectancy, y = total_deaths_per_million))

df %>% filter(date == yesterday-5, total_cases >= 100000) %>%
  ggplot() + 
  geom_point(aes(y = total_deaths_per_million, x = hospital_beds_per_thousand, size = total_cases_per_million))


