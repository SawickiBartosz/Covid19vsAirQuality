


library(XML)
library(methods)
library(ggplot2)
library(dplyr)
library(tidyr)

# Te dane koncza sie na styczniu 2020 chyba nieuzywalne
source_links <- c("https://ereporting.blob.core.windows.net/downloadservice/PL_8_29920_2019_timeseries.csv", 
"https://ereporting.blob.core.windows.net/downloadservice/PL_8_29748_2019_timeseries.csv",
"https://ereporting.blob.core.windows.net/downloadservice/PL_8_64601_2019_timeseries.csv",
"https://ereporting.blob.core.windows.net/downloadservice/PL/PL_8_29500_2020_timeseries.csv ",
"https://ereporting.blob.core.windows.net/downloadservice/PL/PL_8_29920_2020_timeseries.csv" ,
"https://ereporting.blob.core.windows.net/downloadservice/PL/PL_8_29748_2020_timeseries.csv" ,
"https://ereporting.blob.core.windows.net/downloadservice/PL/PL_8_64601_2020_timeseries.csv")

raw_data <- read.csv("https://ereporting.blob.core.windows.net/downloadservice/PL_8_29500_2019_timeseries.csv")

for(link in source_links){
  df <- read.csv(link)
  bind_rows(raw_data,df)
}

data <- raw_data %>% select(DatetimeBegin,Concentration) %>% mutate(date = as.Date(DatetimeBegin)) %>% select(date,Concentration) %>% group_by(date) %>% summarise(mean = mean(Concentration))

ggplot(data = data, aes(x = date)) + geom_line(aes(y = mean, group = mean))



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

