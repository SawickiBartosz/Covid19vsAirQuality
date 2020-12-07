library(XML)
library(methods)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rjson)
library(data.table)
library(maps)
library(readr)
library(RColorBrewer)



raw_data <- rbind(read_csv("waqi-covid-2019Q1.csv", skip = 4),
                       read_csv("waqi-covid-2019Q2.csv", skip = 4),
                       read_csv("waqi-covid-2019Q3.csv", skip = 4),
                       read_csv("waqi-covid-2019Q4.csv", skip = 4),
                       read_csv("waqi-covid-2020.csv", skip=4))

covid_df <- read.csv("stay-at-home-covid.csv") %>% mutate(Date = as.Date(Date))
iso_codes <-read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")
                     
raw_data %>%
  inner_join(iso_codes, by = c("Country"="Alpha-2 code"))%>%
  inner_join(covid_df, by = c("Date"="Date","Alpha-3 code"="Code")) %>%
  mutate(year = as.factor(year(Date))) %>%
  filter(Specie %in% c("o3","so2","pm25","no2","co","pm10")) %>%
  filter(Country == "CN") %>%
  ggplot() +
  geom_bar(aes(x = year,y = median)) +
             facet_wrap(~Specie) + 
             scale_y_continuous(limits = c(0,100))
  

brewer.pal(3,"PRGn")

covid_df <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% mutate(Date = as.Date(date))
raw_data %>%
  inner_join(iso_codes, by = c("Country"="Alpha-2 code"))%>%
  inner_join(covid_df, by = c("Date"="Date","Alpha-3 code"="iso_code")) %>%
  mutate(year = as.factor(year(Date))) %>%
  filter(Specie == "no2") %>%
  filter(City %in% c("Berlin","Barcelona","Moscow","Milan","London", "Warsaw")) %>%
  ggplot(aes(y = median,x = stringency_index)) +
  facet_wrap(~City, nrow = 2)+
  geom_jitter(color = "#7FBF7B") + 
  ggtitle("NO2 in chosen european cities by Stringency Index in 2020")+
  ylab("Daily median of NO2 concentration") + 
  xlab("Stringency Index") +
  theme_bw(base_family = "Crimson Pro",
           base_size = 18)#+ 
#  theme(axis.title = element_text(size=14),
        # axis.text = element_text(size = 12),
        # plot.title = element_text(size = 18))

# The OxCGRT project calculate a Government Stringency Index, a composite measure of governments' response metrics.
# The nine metrics used to calculate the Government Stringency Index are:
# school closures; workplace closures; cancellation of public events; restrictions on public gatherings; closures of public transport;
# stay-at-home requirements; public information campaigns; restrictions on internal movements; and international travel controls.
# A higher score indicates a stricter government response (i.e. 100 = strictest response). If policies vary at the subnational level,
# the index is shown as the response level of the strictest sub-region.
# We've checked if Index value corelates with air pollution in Europe.

