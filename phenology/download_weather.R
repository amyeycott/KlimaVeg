#Down load Bialowieza and regional temperature data 
library("tidyverse")
library("rnoaa")
library("lubridate")
library("ggrepel")


##regional climate
#find nearby sites
dat <- read.table(header = TRUE, sep = ",", text = "
  longitude, latitude, id
  23.894614, 52.744313,Białowieża"
)

stations <- meteo_nearby_stations(dat, lat_colname = "latitude", lon_colname = "longitude", station_data = ghcnd_stations(), var = "all",  year_min = 1960, year_max = 2000, radius = NULL, limit = 20)

#map
mp <- map_data("world", xlim = c(16, 30), ylim = c(48, 58))

ggplot(stations$Białowieża, aes(x = longitude, y = latitude, label = name)) + 
  geom_map(data = mp, mapping = aes(map_id = region), map = mp, fill = "grey80", colour = "black", inherit.aes = FALSE) + 
  geom_point() +
  geom_label_repel() +
  geom_point(data = dat[1, ], aes(x = longitude, y = latitude), colour = "red", size = 3, inherit.aes = FALSE)

#download data
regionalData <- stations$Białowieża %>% 
  filter(distance < 100) %>% # 100 km radius
  group_by(id) %>%
  do(cbind(select(., name), ghcnd_search(.$id, var = "TAVG")$tavg)) %>% 
  mutate(tavg = tavg/10, 
         variable = "tavg") %>%
  rename(value = tavg)

#plot regional data
g <- regionalData %>% filter(year(date) == 2000) %>% 
  ggplot(aes(x = date, y = value, colour = name, group = name)) + 
    geom_line()
print(g)

g %+% regionalData

#monthly
monthlyRegionalData <- regionalData %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(name, id, month, year, variable) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(date = as.Date(paste(year, month, 15, sep = "-")))

monthlyRegionalData %>% 
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line() + 
  facet_wrap( ~ month, scale = "free_y")


## Bialowieza data from 
source("phenology/R/get_polish_weather.R")

#authentic <- "richard.telford@uib.no:Pa55w0rd"#not real password
#save(authentic, file = "phenology/data/authentic.Rdata")
load("phenology/data/authentic.Rdata")#
startDate <- as.Date("1964-1-1")
endDate <- as.Date("2015-12-31")
siteCode <- "252230120"

meanTemperature <- get_Polish_weather_data(
  siteCode = siteCode, 
  variableCode = "B100B007CD", 
  startDate = startDate, 
  endDate = endDate, 
  authentic = authentic
  )

minTemperature <- get_Polish_weather_data(
  siteCode = siteCode, 
  variableCode = "B100B007AD",# Minimalna temperatura powietrza-doba-klimat 
  startDate = startDate, 
  endDate = endDate, 
  authentic = authentic
  )
BialowiezaMinima <- minTemperature %>%
  mutate(name = "Białowieża", variable = "tmin")
  

save(meanTemperature, file = "phenology/data/BialowiezaMeanTemp.Rdata")
save(BialowiezaMinima, file = "phenology/data/BialowiezaMinTemp.Rdata")

BialowiezaDaily <- meanTemperature %>%
  mutate(name = "Białowieża", variable = "tavg")


BialowiezaDaily %>% ggplot(aes(x = date, y = value, colour = variable))  + geom_line()
BialowiezaDaily %>% ggplot(aes(x = value)) + geom_histogram()

## compare with regional data
all_temperatures <- bind_rows(BialowiezaDaily, regionalData)

g %+% filter(all_temperatures, year(date) == 1970) 

#ALL MONTHLY
monthly_all_temperatures <- all_temperatures %>% 
  filter(date >= "1964-01-01", date <= "2015-12-31") %>%
  mutate(month = month.name[month(date)], 
         month = factor(month, levels = month.name), 
         year = year(date)) %>%
  group_by(name, id, month, year, variable) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(date = as.Date(paste(year, month, 15, sep = "-"), format = "%Y-%B-%d"))

monthly_all_temperatures %>% 
  ggplot(aes(x = date, y = value, colour = name)) +
  geom_line() + 
  facet_wrap(. ~ month, scale = "free_y")


Bialowieza_monthly_lag <- monthly_all_temperatures %>% 
  filter(name == "Białowieża") %>%
  ungroup() %>%
  mutate(year = ifelse(month %in% month.name[8:12], year - 1, year),
         month = factor(month, level = month.name[c(8:12, 1:7)]))


##save data
save(regionalData, BialowiezaDaily, monthly_all_temperatures, Bialowieza_monthly_lag, file = "phenology/data/downloaded_weather.Rdata")
