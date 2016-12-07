library(dplyr)

setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADS\\project_5')
load('df(final).RData')

baseline <- df[ , c(2, 25:48)] %>%
  group_by(ncodpers) %>%
  summarise_each(funs(mean))

baseline <- ifelse(baseline >= 0.5, 1, 0)