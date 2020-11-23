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

data <- read.csv("https://ereporting.blob.core.windows.net/downloadservice/PL_8_29500_2019_timeseries.csv")

for(link in source_links){
  df <- read.csv(link)
  bind_rows(data,df)
}

ggplot(data = data, aes(x = DatetimeBegin)) + geom_point(aes(y = Concentration))
