#load libraries
library("ggplot2")
library("vegan")

source("phenology/load_phenology.R")
source("phenology/load_weather.R")


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
