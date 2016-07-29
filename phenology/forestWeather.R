#load libraries
library("readxl")
library("tidyr")
library("dplyr")


#load data
forest1 <- read_excel("phenology/data/podpisane dane meteo.xlsx", sheet = "Żubr 1", skip = 1)
colnames(forest1)


forest2 <- read_excel("phenology/data/podpisane dane meteo.xlsx", sheet = "Żubr 2", skip = 1)

#process first sheet
names(forest1[, c(1, 2:6, 17:18)])
Area38W <- forest1[, c(1, 2:6, 17:18)]
colnames(Area38W) <- c("datetime", "t_200", "t_50", "t_5", "t_-20", "t_-50", "rh_200", "t_200rh")
Area38W <- Area38W %>%  mutate(area = "38", end = "W")

names(forest1[, c(1, 7:11, 19:20)])
Area39E <- forest1[, c(1, 7:11, 19:20)]
colnames(Area39E) <- c("datetime", "t_200", "t_50", "t_5", "t_-20", "t_-50", "rh_200", "t_200rh")
Area39E <- Area39E %>%  mutate(area = "39", end = "E")

names(forest1[, c(1, 12:16)])
Area39W <- forest1[, c(1, 12:16)]
colnames(Area39W) <- c("datetime", "t_200", "t_50", "t_5", "t_-20", "t_-50")
Area39W <- Area39W %>%  mutate(area = "39", end = "W")

#process second sheet
names(forest2[, c(1, 2:6, 17:18)] )
Area37E <- forest2[, c(1, 2:6, 17:18)]
colnames(Area37E) <- c("datetime", "t_200", "t_50", "t_5", "t_-20", "t_-50", "rh_200", "t_200rh")
Area37E <- Area37E %>%  mutate(area = "37", end = "E")


names(forest2[, c(1, 7:11)] )
Area38E <- forest2[, c(1, 7:11)]
colnames(Area38E) <- c("datetime", "t_200", "t_50", "t_5", "t_-20", "t_-50")
Area38E <- Area38E %>%  mutate(area = "38", end = "E")

names(forest2[, c(1, 12:16)] )
Area37W <- forest2[, c(1, 12:16)]
colnames(Area37W) <- c("datetime", "t_200", "t_50", "t_5", "t_-20", "t_-50")
Area37W <- Area37W %>%  mutate(area = "37", end = "W")

forest <- rbind.fill(Area37W, Area37E, Area38E, Area38W, Area39E, Area39W)

#cleanup
rm(forest1, forest2, Area37W, Area37E, Area38E, Area38W, Area39E, Area39W)

forest <- forest %>%
  gather(key = "variable", value = "value", -datetime, -area, -end) %>%
  filter(!is.na(value)) #remove NA values (mostly absent RH loggers)

#clean times
table(minute(forest$datetime))
forest$datetime <- as.POSIXct(round(forest$datetime, "mins")) 
forest <- forest[minute(forest$datetime) == 0, ]

#seperate out RH?
RH <- forest %>% filter(variable == "rh_200")
forest <- forest %>% filter(variable != "rh_200")

#remove gross temperature outliers
forest <- forest %>% filter(value > -50 & value < 50)

##
library("ggplot2")
ggplot(forest %>% filter(variable == "t_200"), aes(x = datetime, y = value, colour = area, linetype = end)) + geom_line()
ggplot(forest %>% filter(variable == "t_50"), aes(x = datetime, y = value, colour = area, linetype = end)) + geom_line()
ggplot(forest %>% filter(variable == "t_5"), aes(x = datetime, y = value, colour = area, linetype = end)) + geom_line()
ggplot(forest %>% filter(variable == "t_-20"), aes(x = datetime, y = value, colour = area, linetype = end)) + geom_line()
ggplot(forest %>% filter(variable == "t_-50"), aes(x = datetime, y = value, colour = area, linetype = end)) + geom_line()
ggplot(forest %>% filter(variable == "t_200rh"), aes(x = datetime, y = value, colour = area, linetype = end)) + geom_line()
ggplot(RH, aes(x = datetime, y = value, colour = area, linetype = end)) + geom_line()

#July 1 
ggplot(forest %>% filter(format(datetime, "%Y-%m-%d") == "2016-05-01", variable != "t_200rh"), aes(x = datetime, y = value, linetype = area, colour = variable, group = paste(area, end, variable))) + geom_line()
#by height/depth
ggplot(
  forest %>% filter(year(datetime) == 2015, day(datetime) == 31, variable != "t_200rh") %>% mutate(mon = month(datetime)),
  aes(x = datetime, y = value, colour = area, linetype = end)) + 
  geom_line() +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%H") + 
  facet_grid(variable ~ mon, scales =  "free", space = "free_x")

#by location
ggplot(
  forest %>% filter(year(datetime) == 2015, day(datetime) == 31, variable != "t_200rh") %>% mutate(mon = month(datetime)),
  aes(x = datetime, y = value, colour = variable, linetype = end)) + 
  geom_line() +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%H") + 
  facet_grid(area ~ mon, scales =  "free", space = "free_x")

##daily means
mforest <- forest %>%
  mutate(date = as.POSIXct(trunc(datetime, "days"))) %>%
  group_by(date, variable) %>% 
  summarise(mtemp = mean(value))

mforest_met <- merge(mforest %>% filter(variable == "t_200"), weather)

ggplot(mforest_met %>% select(date, mtemp, TMean) %>% gather(key = "variable", value = "temperature", -date), aes(x = date, y = temperature, colour = variable)) + scale_colour_discrete(limits = c("mtemp", "TMean"), labels = c("Forest", "Clearing")) + 
  geom_line()

#differences
ggplot(mforest_met, aes(x = date, y = mtemp-TMean)) + 
  geom_line() 

#2m vs 50 cm 
forest %>% filter(variable %in% c("t_200", "t_50")) %>% group_by(datetime, variable) %>% summarise(mvalue = mean(value)) %>% spread(key = variable, value  = mvalue) %>% mutate(delta = t_200 - t_50, date = as.POSIXct(trunc(datetime, "days"))) %>% group_by(date) %>% summarise(meandelta = mean(delta)) %>% ggplot(aes(x = date, y = meandelta)) + geom_line()

#2m vs 50 cm 
forest %>% 
  filter(variable %in% c("t_200", "t_50"), year(datetime) == 2014) %>% 
           group_by(datetime, variable) %>% 
           summarise(mvalue = mean(value)) %>% 
           spread(key = variable, value  = mvalue) %>% 
           mutate(delta = t_200 - t_50, mon = month(datetime)) %>% 
           ggplot(aes(x = datetime, y = delta, group = yday(datetime))) + 
           geom_line() + 
           facet_wrap(~mon, scales = "free_x") 
