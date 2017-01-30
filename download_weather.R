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
  do(cbind(select(., name), ghcnd_search(.$id, var = "TAVG")$tavg)) 

regionalData %>% filter(year(date) == 2000) %>% 
  ggplot(aes(x = date, y = tavg/100, colour = name, group = name)) + geom_line()

regionalData %>%  
  ggplot(aes(x = date, y = tavg/100, colour = name, group = name)) + geom_line()

#monthly

monthlyRegionalData <- regionalData %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(name, id, month, year) %>%
  summarise(tavg = mean(tavg, na.rm = TRUE)/100) %>%
  mutate(date = as.Date(paste(year, month, 15, sep = "-")))

monthlyRegionalData %>% 
  ggplot(aes(x = date, y = tavg, colour = name)) +
  geom_line() + 
  facet_wrap(~month, scale = "free_y")


## Bialowieza data from 
library(rvest)


