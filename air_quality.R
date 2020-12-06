


library(XML)
library(methods)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rjson)
library(data.table)
library(maps)
library(readr)
library(data.table)


# dane z aqcin.org

# jak pobrać csv ?

# curl --compressed -o waqi-covid-2020.csv   https://aqicn.org/data-platform/covid19/report/19780-07745f90/2020
# curl --compressed -o waqi-covid-2019Q1.csv https://aqicn.org/data-platform/covid19/report/19780-07745f90/2019Q1
# curl --compressed -o waqi-covid-2019Q2.csv https://aqicn.org/data-platform/covid19/report/19780-07745f90/2019Q2
# curl --compressed -o waqi-covid-2019Q3.csv https://aqicn.org/data-platform/covid19/report/19780-07745f90/2019Q3
# curl --compressed -o waqi-covid-2019Q4.csv https://aqicn.org/data-platform/covid19/report/19780-07745f90/2019Q4
# curl --compressed -o waqi-covid-2018H1.csv https://aqicn.org/data-platform/covid19/report/19780-07745f90/2018H1
# curl --compressed -o waqi-covid-2017H1.csv https://aqicn.org/data-platform/covid19/report/19780-07745f90/2017H1
# curl --compressed -o waqi-covid-2016H1.csv https://aqicn.org/data-platform/covid19/report/19780-07745f90/2016H1
# curl --compressed -o waqi-covid-2015H1.csv https://aqicn.org/data-platform/covid19/report/19780-07745f90/2015H1

raw_data <- read_csv("waqi-covid-2020.csv", skip = 4) # omijamy pierwsze 4 wiersze, jakieś nagłówki
raw_data <- rbind(raw_data, read_csv("waqi-covid-2019Q1.csv", skip = 4))
raw_data <- rbind(raw_data, read_csv("waqi-covid-2019Q2.csv", skip = 4))
raw_data <- rbind(raw_data, read_csv("waqi-covid-2018H1.csv", skip = 4))
raw_data <- rbind(raw_data, read_csv("waqi-covid-2017H1.csv", skip = 4))
raw_data <- rbind(raw_data, read_csv("waqi-covid-2016H1.csv", skip = 4))

raw_data %>% 
  filter(Country == "PL") %>%
  filter(Specie == "co") %>% 
  filter(City %in% c("Warsaw","Wrocław","Łódź","Gdańsk","Kraków"))%>% #nie mają wszystkich lat
  mutate(year = as.factor(year(Date))) %>%
  arrange(Date) %>%
  distinct() %>%
  select(-c(min,max,count,variance))%>%
  pivot_wider(names_from = City, values_from = median)-> pol_co_raw

pol_co_rolling_raw <- cbind(pol_co_raw,frollmean(pol_co_raw[,5:9],n=7)) # 7 day rolling avarage
colnames(pol_co_rolling_raw)[10:14] <- paste(colnames(pol_co_rolling_raw)[5:9],"_rolling")
pol_co_rolling_raw %>% select(-(5:9)) -> pol_co_rolling
colnames(pol_co_rolling)[5:9] <- colnames(pol_co_rolling_raw)[5:9]

pol_co_rolling %>%
  pivot_longer(cols = 5:9,names_to= "City", values_to = "median") %>%
  mutate(day = as.Date(format(Date, format = "%d-%m"),format = "%d-%m"))%>%
  filter(day<as.Date("01.07.2020", format = "%d.%m.%Y"))%>%
  ggplot(aes(x = day)) + 
  geom_line(aes(y = median, group = year, color = year)) +
  facet_wrap(~City) + 
  ggtitle("CO concentration in polish cities") +
  scale_color_manual(values = c("gray","gray","gray","gray","steelblue")) +
  theme_bw()



raw_data %>% 
  filter(Country == "PL") %>%
  filter(City == "Warsaw")%>% #nie mają wszystkich lat
  mutate(year = as.factor(year(Date)))%>%
  mutate(day = as.Date(format(Date, format = "%d-%m"),format = "%d-%m"))%>%
  filter(day<as.Date("01.07.2020", format = "%d.%m.%Y"))%>%
  ggplot(aes(x = day)) + 
  geom_line(aes(y = median, group = year, color = year)) +
  facet_wrap(~Specie, scales = 'free_y') + 
  ggtitle("Warsaw") +
  scale_color_manual(values = c("steelblue","orange")) +
  theme_bw()

cities_raw <- fromJSON(file = "airquality-covid19-cities.json")$data

cities<- lapply(cities_raw,'[[', "Place")
cbind(sapply(cities,'[[',"country"),sapply(cities,'[[',"geo"))

geos <- transpose(as.data.frame(sapply(lapply(cities,'[[', "Place"), '[[',c("geo","country"))))

world <- map_data("world")
ggplot() + geom_polygon(data = world,aes(x=long, y = lat, group = group), color = "white" )+ 
  geom_point(data = geos, aes(x = V2, y = V1, color = "red")) +
  coord_fixed(1.3)+
  guides(fill=FALSE) 

