library("tidyr")
library("dplyr")
library("lubridate")

#central England
cet <- read.table("phenology/data/cetdl1772on.dat", header = FALSE)
names(cet) <- c("year", "dom", month.name)

cet <- cet %>% 
  filter(year >= 1948) %>%
  gather(key = month, value = temperature, -year, -dom) %>%
  filter(temperature != -999) %>%
  mutate(date = ymd(paste(year, month, dom))) %>%
  select(-year, -month, -dom) %>%
  mutate(temperature = temperature / 10)

cet_B <- rbind(cet %>% mutate(station = "CET"), weather %>% select(date, TMean) %>% rename(temperature = TMean) %>% mutate(station = "Bialowieza"))

# by day of year
meanDay_cetB <- cet_B %>% mutate(doy = yday(date)) %>% group_by(doy, station) %>% summarise(meanDay = mean(temperature, na.rm = TRUE))

ggplot(cet_B, aes(x = yday(date), y = temperature, colour = year(date), group = year(date))) + 
  geom_line() +
  geom_line(aes(x = doy, y = meanDay), meanDay_cetB, colour = "red", inherit.aes = FALSE) + 
  facet_wrap(~station)

cet_B %>% mutate(month = month(date)) %>% group_by(month, station) %>% summarise(mMean = mean(temperature, na.rm = TRUE)) %>% spread(key = station, value = mMean) %>% ggplot(aes(x = Bialowieza, y  = CET)) + geom_path()


