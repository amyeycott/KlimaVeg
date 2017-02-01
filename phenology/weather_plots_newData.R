## ---- loadWeather
#load packages
library("ggplot2")
library("dplyr")
library("tidyr")
library("broom")
library("zoo")

#default theme
th <- theme()

# load weather data
if(interactive()){
  load("phenology/data/downloaded_weather.Rdata")
}else{
  load("data/downloaded_weather.Rdata")
}
BialowiezaMonthly <- monthly_all_temperatures %>% filter(name == "Białowieża") 

## ----makeWeatherPlots
##weather

# by day of year
ggplot(BialowiezaDaily, aes(x = as.Date(yday(date) - 1, origin = "2017-01-01"), y = value, colour = year(date), group = year(date))) + 
  geom_line() +
  stat_summary(aes(group = 1), fun.y = "mean", colour = "red", geom = "line") +
  labs(x = "Month", y = "Temperature, °C", colour = "Year") +
  scale_x_date(date_labels = "%b", expand = c(0.02, 0))


#monthly
ggplot(BialowiezaMonthly %>% filter(variable == "tavg"), aes(x = month, y = value, colour = year, group = year)) + 
  geom_line()

#regression by month
## ---- monthPlot
monthlyRegressionPlot <- BialowiezaMonthly %>% filter(variable == "tavg") %>%
  ggplot(aes(x = year, y = value, colour = month)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x") +
  labs(x = "Year", y = "Temperature, °C", colour = "Month") +
  scale_color_hue(h.start = 180) +
  th
print(monthlyRegressionPlot)

## ---- tempChange
tempEffectSize <- BialowiezaMonthly %>% 
  filter(variable == "tavg") %>% 
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
seasonalwarming <- BialowiezaDaily %>% 
  mutate(doy = yday(date)) %>% 
  group_by(doy) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  do(bind_cols(., 
      rollapply(data = ., width = 30, 
                FUN = function(x){
                    tidy(lm(value ~ doy, data = as.data.frame(x)))[2, -1]
                },
                by.column = FALSE, fill = NA) %>% as.data.frame()
  )) %>% 
  mutate(doy2 = as.Date(doy - 1, origin = ymd("2017-01-01")))

ggplot(seasonalwarming, aes(x = doy2, y = estimate, ymax = estimate + 1.96 * std.error, ymin = estimate - 1.96 * std.error)) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_ribbon(alpha = .4)+
  geom_line() +
  scale_x_date(name = "Month", date_breaks = "1 month", date_labels = "%b") +
  ylab("Rate of temperature change °C/day") +
  th

