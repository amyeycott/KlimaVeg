#load libraries
library("readxl")
library("tidyr")
library("plyr")
library("dplyr")
library("lubridate")

#weather
weather <- read_excel("phenology/data/Meteo Bialowieza 1948 2008.xls")
#ignore warnings - refer to rows deleted in next step
weather <- weather[!is.na(weather$Date), ] 
names(weather) <- c("date", "month", "TMean", "ppt", "snowCover", "sunnyHours", "sunlight")

#remove single temperature gross outlier
max(weather$TMean, na.rm = TRUE)
weather$TMean[weather$TMean > 40] <- NA

#calculate monthly means
monthly <- weather %>% 
  mutate(month = month.name[month(date)], year = year(date)) %>% 
  mutate(month = factor(month, levels = month.name)) %>%
  group_by(year, month) %>% 
  summarise(temperature = mean(TMean, na.rm = TRUE), precipitation = sum(ppt)) %>%
  gather(key = "variable", value = "value", -year, -month)

#last day of snow
lastSnow <- weather %>%
  filter(yday(date) < 200, snowCover > 0) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(lastSnow = last(yday(date)))

#combine temperature/ppt with snow
monthlyClim <- monthly %>% 
  rbind(lastSnow %>% 
          mutate(variable = "Snow", month = "lastSnow", value = lastSnow) %>% 
          select(-lastSnow)
  )
