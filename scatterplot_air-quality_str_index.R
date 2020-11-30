


library(XML)
library(methods)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rjson)
library(data.table)
library(maps)
library(readr)


iso_codes<-read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")

raw_data_2019 <- rbind(read_csv("waqi-covid-2019Q1.csv", skip = 4),
                  read_csv("waqi-covid-2019Q2.csv", skip = 4),
                  read_csv("waqi-covid-2019Q3.csv", skip = 4),
                  read_csv("waqi-covid-2019Q4.csv", skip = 4))

means_2019 <- raw_data_2019 %>%
  filter(Specie == "aqi") %>% 
  group_by(City) %>%
  summarize(mean_2019 = mean(median))

raw_data_2020 <- read_csv("waqi-covid-2020.csv", skip=4) %>% filter(Specie == "aqi")

covid_df <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% mutate(Date = as.Date(date))

raw_data_2020 %>%
  inner_join(means_2019, by = "City")%>%
  inner_join(iso_codes, by = c("Country"="Alpha-2 code"))%>%
  inner_join(covid_df, by = c("Date"="Date","Alpha-3 code"="iso_code")) %>%
  ggplot(aes(y = median/mean_2019, x = stringency_index)) + 
  geom_density2d(color = "steelblue")+
  ggtitle("Air Quality Index in comparison to 2019 mean by stringency index")+
  theme_bw()


