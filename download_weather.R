#Down load Bialowieza and regional temperature data 
library("dplyr")
library("rnoaa")
library("ggplot2")
library("tidyr")
library("lubridate")



##regional climate
#find nearby sites
dat <- read.table(header = TRUE, sep = ",", text = "
  longitude, latitude, id
  23.894614, 52.744313,Białowieża"
)

stations <- meteo_nearby_stations(dat, lat_colname = "latitude", lon_colname = "longitude", station_data = ghcnd_stations(), var = "all",  year_min = 1960, year_max = 2000, radius = NULL, limit = 20)

#map
mp <- map_data("world", xlim = c(18, 30), ylim = c(48, 56))

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
  facet_wrap(~month, scale = "free_y")


## Bialowieza data from 
library("RCurl")
library("readr")
#authentic <- "richard.telford@uib.no:Pa55w0rd"#not real password
#save(authentic, file = "phenology/data/authentic.Rdata")
load("phenology/data/authentic.Rdata")#


meanTemperature <- plyr::ldply(seq(from = as.Date("1964-1-1"), to = as.Date("2015-12-31"), by = 7), function(D){
  url <- paste0("https://dane.imgw.pl/1.0/pomiary/cbdh/252230120-B100B007CD/tydzien/", D,"?format=csv")
  Sys.sleep(0.9)#don't hit server too hard
  read_delim(getURL(url, userpwd = authentic), delim = ";")
}, .progress = "text")

save(meanTemperature, file = "phenology/data/BialowiezaMeanTemp.Rdata")

BialowiezaDaily <- meanTemperature %>% 
  rename(value = wartosc, date = data) %>% 
  mutate(name = "Białowieża",
         date = as.Date(format(date, "%Y-%m-%d")),
         variable = "tavg")


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
  facet_wrap(~month, scale = "free_y")


##save data
save(regionalData, BialowiezaDaily, monthly_all_temperatures, file = "phenology/data/downloaded_weather.Rdata")
