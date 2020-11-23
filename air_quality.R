# Te dane koncza sie na styczniu 2020 chyba nieuzywalne


library(XML)
library(methods)
library(ggplot2)
library(dplyr)
library(tidyr)

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
