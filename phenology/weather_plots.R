#load libraries
library("ggplot2")
library("dplyr")
library("tidyr")

# load weather data
source("phenology/load_weather.R")

##weather
#daily
ggplot(weather, aes(x = date, y = TMean)) + geom_line()
ggplot(weather, aes(x = date, y = ppt)) + geom_line()
ggplot(weather, aes(x = date, y = snowCover)) + geom_line()
ggplot(weather, aes(x = date, y = sunnyHours)) + geom_line()
ggplot(weather, aes(x = date, y = sunlight)) + geom_line()#unit problem?

# by day of year
ggplot(weather, aes(x = yday(date), y = TMean, colour = year(date), group = year(date))) + 
  geom_line()

ggplot(weather, aes(x = yday(date), y = snowCover, colour = year(date), group = year(date))) + 
  geom_line()

ggplot(lastSnow, aes(x = year, y = lastSnow)) + geom_bar(stat = "identity")  


#annual ppt
weather %>% mutate(year = year(date)) %>% group_by(year) %>% summarise(ppt  = sum(ppt)) %>% ggplot(aes(x = year, y = ppt)) + geom_line()

#monthly
ggplot(monthly %>% filter(variable == "temperature"), aes(x = month, y = value, colour = year, group = year)) + geom_line()
ggplot(monthly %>% filter(variable == "precipitation"), aes(x = month, y = value, colour = year, group = year)) + geom_line()

#correlation  & PCA
monthlyClimFat <- monthlyClim %>% 
  ungroup() %>%
  mutate(variable = paste(month, variable, sep = "_")) %>%
  select(-month) %>%
  spread(key = variable, value = value)

round(cor(monthlyClimFat), 2)
PCA <- prcomp(monthlyClimFat, scale = TRUE)
biplot(PCA)