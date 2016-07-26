#load libraries
library("readxl")
library("tidyr")
library("dplyr")
library("ggplot2")
library("vegan")
library("lubridate")

#load data
#weather
weather <- read_excel("phenology/data/Meteo Bialowieza 1948 2008.xls")
weather <- weather[!is.na(weather$Date), ] 
names(weather) <- c("date", "month", "TMean", "ppt", "snowCover", "sunnyHours", "sunlight")

monthly <- weather %>% 
  mutate(month = month.name[month(date)], year = year(date)) %>% 
  mutate(month = factor(month, levels = month.name)) %>%
  group_by(year, month) %>% 
  summarise(temperature = mean(TMean), precipitation = sum(ppt)) %>%
  gather(key = "variable", value = "value", -year, -month)

#last day of snow
lastSnow <- weather %>%
  filter(yday(date) < 200, snowCover > 0) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(lastSnow = last(yday(date)))

ggplot(lastSnow, aes(x = year, y = lastSnow)) + geom_bar(stat = "identity")  

monthlyClim <- monthly %>% 
  mutate(variable = paste(variable, month, sep = "_")) %>%
  select(-month) %>%
  rbind(lastSnow %>% 
          mutate(variable = "lastSnow", value = lastSnow) %>% 
          select(-lastSnow)
  )
monthlyClim <- monthly %>% 
  rbind(lastSnow %>% 
          mutate(variable = "Snow", month = "lastSnow", value = lastSnow) %>% 
          select(-lastSnow)
  )


#phenology
phenology <- read_excel("phenology/data/Fenology36.xlsx")
head(phenology)

#year 1964 - 2016 
#species number
#delete
#phenological stages 1-6 vegetative, budding, flowering, fruiting, dispersal, resting
#1:73 dates of observations Originally every 5th day. Then every 10th (after 1989) Missing values filled with @
#. plant not observed
# 1-10 decile of plant pop. in given phenological state
# + single plants

phenology$year <- ifelse(phenology$ROK < 2000, phenology$ROK + 1900, phenology$ROK) 
phenology$ROK <- NULL
phenology$Gatunki36I <- NULL
phenology$speciesN <- phenology$'Nr gatunku'
phenology$'Nr gatunku' <- NULL
phenology$stage <- phenology$`Nr jednostki`
phenology$`Nr jednostki` <- NULL

#spp names 
sppNames <- read_excel("phenology/data/Species names 36.xlsx")
phenology$species <- sppNames$Gatunek[phenology$speciesN]
phenology$species <- factor(phenology$species, levels = sppNames$Gatunek)
phenology$speciesN <- NULL

#phenological change
#compositional chage

phenology2 <- gather(phenology, key = "pentad", value = "decile", -species, -year, -stage ) %>%
  filter(decile != "@") %>%
  mutate(decile = trimws(decile)) %>%
  mutate(pentad = as.numeric(pentad))

phenology2

#clean
as.matrix(table(phenology2$decile))

oddities <- phenology2[!phenology2$decile %in% c(".", "+", 1:10, "x"), ]
write.csv2(oddities, "phenology/oddities.csv")


phenology2$decile[phenology2$decile == ","] <- "."
phenology2$decile[phenology2$decile == ".."] <- "."
phenology2$decile[phenology2$decile == ".+"] <- "+"
phenology2$decile[phenology2$decile == "+."] <- "+"
phenology2$decile[phenology2$decile == ".1"] <- "1"
phenology2$decile[phenology2$decile == "+1"] <- "1"
phenology2$decile[phenology2$decile == ".10"] <- "10"
phenology2$decile[phenology2$decile == "100"] <- "10"
phenology2$decile[phenology2$decile == "101"] <- "10"
phenology2$decile[phenology2$decile == "1010"] <- "10"
phenology2$decile[phenology2$decile == ".2"] <- "2"
phenology2$decile[phenology2$decile == ".3"] <- "3"
phenology2$decile[phenology2$decile == ".4"] <- "4"
phenology2$decile[phenology2$decile == ".5"] <- "5"
phenology2$decile[phenology2$decile == ".6"] <- "6"


phenology2[!phenology2$decile %in% c(".", "+", 1:10, "x")]
phenology2

#. to 0
phenology2$decile[phenology2$decile == "."] <- "0"
#x to NA
phenology2$decile <- as.numeric(phenology2$decile)

table(phenology2$species)

ggplot(phenology2 %>% filter(species == "DAPHNE ME"), aes(x = pentad, y = decile, colour = year)) + geom_line() + facet_wrap(~stage)
ggplot(phenology2 %>% filter(species == "ALLIUM UR"), aes(x = pentad, y = decile, colour = year)) + geom_line() + facet_wrap(~stage)

#community
commF <- phenology2 %>%
  filter(stage < 6, stage > 0) %>%
  group_by(year, species) %>%
  summarise(present  = sum(decile, na.rm = TRUE) > 0) %>%
  spread(key = species, value = present, fill = 0)
  
decorana(commF)

comm <- phenology2 %>%
  filter(stage < 6, stage > 0, !is.na(decile)) %>%
  group_by(year, species, stage) %>%
  summarise(present  = sum(decile, na.rm = TRUE) > 0) %>%
    filter(present)
  
ggplot(comm, aes(x = year, y = species)) + 
  geom_point(data = comm %>% filter(stage == 1) %>% mutate(stage2 = stage) %>% select(-stage), colour = "grey70") + 
  scale_y_discrete(limits = levels(comm$species)) +
  geom_point() + 
  facet_wrap(~stage)

ggplot(comm %>% filter(stage == 3), aes(x = year, y = species)) + 
  geom_point(data = comm %>% filter(stage == 1) %>% mutate(stage2 = stage) %>% select(-stage), colour = "grey70") + 
  geom_point() 

#richness
comm %>% 
  group_by(year, stage) %>%
  summarise(n = n()) %>% ggplot(aes(x = year, y = n, colour = as.factor(stage))) + geom_line() + ylim(0, NA)

##first flowering
first_flowering <- phenology2 %>% 
  filter(decile > 0, stage < 6) %>% 
  group_by(year, species, stage) %>% 
  summarise(first = first(pentad)) %>%
  spread(key = year, value = first) %>%
  gather(key = "year", value = "first", -species, -stage) %>%
  mutate(year = as.integer(year)) 

ggplot(first_flowering, aes(x = year, y = first, colour = species)) + 
    geom_line(show.legend = FALSE) + 
    geom_point(size = 0.5, show.legend = FALSE) +
    facet_wrap(~stage)


ggplot(first_flowering %>% filter(stage == 3), aes(x = first)) + geom_histogram()


first_flowering <- first_flowering %>% filter(stage == 3) %>% 
  mutate(median = median(first, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(timing = cut(median, breaks = quantile(first, probs = seq(0, 1, 1/3), na.rm = TRUE), labels = c("Early", "Mid", "Late"), include.lowest = TRUE))

first_flowering %>% group_by(species, timing) %>% summarise(mn = min(median), mx = max(median)) %>% as.data.frame()

##weather
ggplot(weather, aes(x = date, y = TMean)) + geom_line()
ggplot(weather, aes(x = date, y = ppt)) + geom_line()
ggplot(weather, aes(x = date, y = snowCover)) + geom_line()
ggplot(weather, aes(x = date, y = sunnyHours)) + geom_line()
ggplot(weather, aes(x = date, y = sunlight)) + geom_line()

ggplot(weather, aes(x = yday(date), y = snowCover, colour = year(date), group = year(date))) + 
  geom_line()

weather %>% mutate(year = year(date)) %>% group_by(year) %>% summarise(ppt  = sum(ppt)) %>% ggplot(aes(x = year, y = ppt)) + geom_line()


ggplot(monthly %>% filter(variable == "temperature"), aes(x = month, y = value, colour = year, group = year)) + geom_line()
ggplot(monthly %>% filter(variable == "precipitation"), aes(x = month, y = value, colour = year, group = year)) + geom_line()

monthlyClimFat <- monthlyClim %>% 
  ungroup() %>%
  mutate(variable = paste(month, variable, sep = "_")) %>%
  select(-month) %>%
  spread(key = variable, value = value)

round(cor(monthlyClimFat), 2)
PCA <- prcomp(monthlyClimFat, scale = TRUE)
biplot(PCA)

#
first_floweringClim <- first_flowering %>% 
  filter(stage == 3) %>% 
  select(-stage) %>%
  merge(monthlyClim)

firstflowerSnowCor <- ddply(first_floweringClim, .(species, variable, month, timing), function(x) {
    if (sum(!is.na(x$first)) > 10) {
      c(correlation = cor(x$first, x$value, use = "pair"))
    }
  })
firstflowerSnowCor %>%
  mutate(correlation = round(correlation, 2)) %>% 
  spread(key = variable, value = correlation)

ggplot(firstflowerSnowCor, aes(x = month, y = correlation)) + 
  geom_boxplot() +
  facet_grid(~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(firstflowerSnowCor, aes(x = month, y = correlation, fill = timing)) + 
  geom_boxplot() +
  facet_grid(~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))
