library(dplyr)
library(ggplot2)
library(tidyr)

stringency_index <- read.csv("covid-stringency-index.csv")

excess_mortality <- read.csv("excess-mortality-p-scores.csv")

stringency_index %>%
  left_join(excess_mortality,by = c("Entity","Date", "Code")) %>%
  mutate(excess_mortality = Excess.mortality.P.scores..all.ages) %>%
  filter(Entity == "United States") %>%  
  ggplot(aes(x = Date)) + geom_line(aes(y = stringency_index, group = Entity)) + geom_point(aes(y = excess_mortality, group = Entity))

