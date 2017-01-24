## ---- loadWeather
#load libraries
library("ggplot2")
library("dplyr")
library("tidyr")
library("broom")

#default theme
th <- theme()

# load weather data
if(interactive()){
  source("phenology/load_weather.R")
}else{
  source("load_weather.R")
}
## ----makeWeatherPlots
##weather
#daily
ggplot(weather, aes(x = date, y = TMean)) + geom_line()
ggplot(weather, aes(x = date, y = ppt)) + geom_line()
ggplot(weather, aes(x = date, y = snowCover)) + geom_line()
ggplot(weather, aes(x = date, y = sunnyHours)) + geom_line()
ggplot(weather, aes(x = date, y = sunlight)) + geom_line()#unit problem?

# by day of year
meanDay <- weather %>% mutate(doy = yday(date)) %>% group_by(doy) %>% summarise(meanDay = mean(TMean, na.rm = TRUE))
ggplot(weather, aes(x = yday(date), y = TMean, colour = year(date), group = year(date))) + 
  geom_line() +
  geom_line(aes(x = doy, y = meanDay), meanDay, colour = "red", inherit.aes = FALSE) +
  labs(x = "Day of year", y = "Temperature, °C", colour = "Year")

range(meanDay$meanDay)

#snow cover plot
ggplot(mutate(weather, year = year(date)), aes(x = yday(date), y = snowCover, colour = year, group = year)) + 
  geom_line()+ 
  facet_wrap(~year)

weather %>% 
  filter(month(date) < 7) %>% #spring snow
  group_by(year = year(date)) %>% 
  summarise(maxSnow = max(snowCover, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = maxSnow)) + 
    geom_bar(stat = "identity")

#Last dat of snow
ggplot(lastSnow, aes(x = year, y = lastSnow)) + geom_bar(stat = "identity")  


summary(lm(lastSnow~year, data = lastSnow))# negative trend not statistically significant

#annual ppt
weather %>% mutate(year = year(date)) %>% group_by(year) %>% summarise(ppt  = sum(ppt)) %>% ggplot(aes(x = year, y = ppt)) + 
  geom_line() + 
  labs(x =  "Year", y = "Precipitation, mm")

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


#regression by month
## ---- monthPlot
monthlyRegressionPlot <- monthly %>% filter(variable == "temperature") %>%
  ggplot(aes(x = year, y = value, colour = month)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x") +
  labs(x = "Year", y = "Temperature, °C", colour = "Month") +
  scale_color_hue(h.start = 180) +
  th
print(monthlyRegressionPlot)

## ---- tempChange
tempEffectSize <- monthly %>% 
  filter(variable == "temperature") %>% 
  group_by(month) %>%
  do(mod = lm((value * 10) ~ year, data = .)) %>%
  tidy(mod) %>%
  filter(term == "year") %>%
  ggplot(aes(x = month, y = estimate, ymax = estimate + 1.96 * std.error, ymin = estimate - 1.96 * std.error)) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_errorbar() + 
  geom_point() + 
  labs(x = "", y = "Temperature change, °C / decade") +
  th +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
print(tempEffectSize)


## ---- seasonalVelocity
#mean climate by day
library(zoo)
seasonalwarming <- weather %>% 
  mutate(doy = yday(date)) %>% 
  group_by(doy) %>%
  summarise(Tmean = mean(TMean, na.rm = TRUE)) %>%
  do(cbind(., 
           rollapply(data = ., width = 30, FUN = function(x){
    coef(summary(lm(Tmean ~ doy, data = as.data.frame(x))))["doy", 1:2] 
  },
  by.column = FALSE, fill = NA)
  )) %>% 
  mutate(doy2 = as.Date(doy, origin = ymd("2017-01-01")))

ggplot(seasonalwarming, aes(x = doy2, y = Estimate, ymax = Estimate + 1.96 * `Std. Error`, ymin = Estimate - 1.96 * `Std. Error`)) + 
  geom_ribbon(alpha = .4)+
  geom_line() +
  scale_x_date(name = "Month", date_breaks = "1 month", date_labels = "%b") +
  ylab("Rate of temperature change °C/day")

