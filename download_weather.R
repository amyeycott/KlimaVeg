#Bialystok
library("dplyr")
library("rnoaa")
library("ggplot2")
library("tidyr")
library("lubridate")

#functions
tidy_climate <- function(station, variable){
  station %>% 
    filter(element == variable) %>% 
    select(id, year, month, element, matches("VALUE")) %>% 
    gather(key = "day", value = "TAVG", -id, -year, -month, -element) %>%
    mutate(TAVG = as.numeric(TAVG)) %>%
    group_by(id, year, month, element) %>%
    summarise(TAVG = mean(TAVG, na.rm = TRUE)/10, n = n()) %>%
    mutate(date = as.Date(paste(year, month, 15, sep = "-")))
}



dat <- read.table(header = TRUE, sep = ",", text = "
  longitude, latitude, id
  23.894614, 52.744313,Białowieża"
)

stations <- meteo_nearby_stations(dat, lat_colname = "latitude",
                      lon_colname = "longitude", station_data = ghcnd_stations(), var = "all",
                      year_min = 1960, year_max = 2000, radius = NULL, limit = 20)

mp <- map_data("world", xlim = c(18, 30), ylim = c(48, 56))

ggplot(stations$Białowieża, aes(x = longitude, y = latitude, label = name)) + 
  geom_map(data = mp, mapping = aes(map_id = region), map = mp, fill = "grey80", colour = "black", inherit.aes = FALSE) + 
  geom_point() +
  geom_label_repel() +
  geom_point(data = dat[1, ], aes(x = longitude, y = latitude), colour = "red", size = 3, inherit.aes = FALSE)


Bialystok <- ghcnd(stationid = "PLM00012295")
levels(Bialystok$element)


regionalData <- stations$Białowieża %>% 
  filter(distance < 100) %>%
  group_by(id) %>%
  do(cbind(select(., name), ghcnd_search(.$id, var = "TAVG")$tavg)) %>% 
  mutate(tavg = tavg/10)

regionalData %>% filter(year(date) == 2000) %>% 
  ggplot(aes(x = date, y = tavg, colour = name, group = name)) + geom_line()

regionalData %>%  
  ggplot(aes(x = date, y = tavg, colour = name, group = name)) + geom_line()

#monthly

monthlyRegionalData <- regionalData %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(name, id, month, year) %>%
  summarise(tavg = mean(tavg, na.rm = TRUE)) %>%
  mutate(date = as.Date(paste(year, month, 15, sep = "-")))

monthlyRegionalData %>% 
  ggplot(aes(x = date, y = tavg, colour = name)) +
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



meanTemperature %>% ggplot(aes(x = data, y = wartosc))  + geom_line()
meanTemperature %>% ggplot(aes(x = wartosc))  + geom_histogram()

## compare with regional data
all_temperatures <- meanTemperature %>% 
  rename(tavg = wartosc, date = data) %>% 
  mutate(name = "Białowieża") %>%
  mutate(date = as.Date(format(date, "%Y-%m-%d"))) %>%
  bind_rows(regionalData)

filter(all_temperatures, year(date) == 1970) %>% 
  ggplot(aes(x = date, y = tavg, colour = name)) +geom_line()

#ALL MONTHLY
monthly_all_temperatures <- all_temperatures %>% 
  filter(date >= "1964-01-01", date <= "2015-12-31") %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(name, id, month, year) %>%
  summarise(tavg = mean(tavg, na.rm = TRUE)) %>%
  mutate(date = as.Date(paste(year, month, 15, sep = "-")))

monthly_all_temperatures  %>% 
  ggplot(aes(x = date, y = tavg, colour = name)) +
  geom_line() + 
  facet_wrap(~month, scale = "free_y")
