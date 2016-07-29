#load libraries
library("ggplot2")
library("vegan")

source("phenology/load_phenology.R")
source("phenology/load_weather.R")

#pheology plots
ggplot(phenology2 %>% filter(species == "DAPHNE ME"), aes(x = pentad, y = decile, colour = year)) + geom_line() + facet_grid(transect~stage)
ggplot(phenology2 %>% filter(species == "ALLIUM UR"), aes(x = pentad, y = decile, colour = year)) + geom_line() + facet_grid(transect~stage)

#community
commF <- phenology2 %>%
  filter(stage < 6, stage > 0) %>%
  group_by(year, species) %>%
  summarise(present  = sum(decile, na.rm = TRUE) > 0) %>%
  spread(key = species, value = present, fill = 0)
  
decorana(commF)


#what's growing/flowering
comm <- phenology2 %>%
  filter(stage < 6, stage > 0, !is.na(decile)) %>%
  group_by(transect, year, species, stage) %>%
  summarise(present  = sum(decile, na.rm = TRUE) > 0) %>%
    filter(present)
  
ggplot(comm, aes(x = year, y = species)) + 
  geom_point(data = comm %>% filter(stage == 1) %>% rename(stage2 = stage), colour = "grey70") + 
  scale_y_discrete(limits = levels(comm$species)) +
  geom_point() + 
  facet_grid(transect~stage, scales = "free_y")

ggplot(comm %>% filter(stage == 3), aes(x = year, y = species)) + 
  geom_point(data = comm %>% filter(stage == 1) %>% rename(stage2 = stage), colour = "grey70") + 
  geom_point() +
  facet_grid(transect~., scales = "free_y")

#richness
comm %>% 
  group_by(year, stage, transect) %>%
  summarise(n = n()) %>% ggplot(aes(x = year, y = n, colour = as.factor(stage), linetype = transect)) + geom_line() + ylim(0, NA)

##first flowering
first_flowering <- phenology2 %>% 
  filter(decile > 0, stage < 6) %>% 
  group_by(year, species, stage, transect) %>% 
  summarise(first = first(pentad), last = last(pentad), duration = last - first, max = max(decile)) %>%
  merge(
    with(phenology2,
      expand.grid(
        year  = unique(year),
        species = unique(species),
        stage = 1:5,
        transect = paste0("t", 36:39)
      )
    ), all = TRUE)
    
    

ggplot(first_flowering, aes(x = year, y = first, colour = species)) + 
    geom_line(show.legend = FALSE) + 
    geom_point(size = 0.5, show.legend = FALSE) +
    facet_grid(transect~stage)


ggplot(first_flowering %>% filter(stage == 3), aes(x = first, fill = transect)) + geom_histogram()

#median by transect
first_flowering %>% 
  group_by(transect) %>% 
  summarise(median = median(first, na.rm = TRUE))


first_flowering <- first_flowering %>% 
  filter(stage == 3) %>%
  group_by(species) %>% 
  mutate(median = median(first, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(timing = cut(median, breaks = quantile(first, probs = seq(0, 1, 1/3), na.rm = TRUE), labels = c("Early", "Mid", "Late"), include.lowest = TRUE))

#
first_floweringClim <- first_flowering %>% 
  filter(stage == 3) %>% 
  select(-stage) %>%
  merge(monthlyClim)

firstflowerSnowCor <- ddply(first_floweringClim, .(species, variable, month, timing, transect), function(x) {
    if (sum(!is.na(x$first)) > 10) {
      c(correlation = cor(x$first, x$value, use = "pair"))
    }
  })

firstflowerSnowCor %>%
  mutate(correlation = round(correlation, 2)) %>% 
  spread(key = variable, value = correlation)

ggplot(firstflowerSnowCor, aes(x = month, y = correlation)) + 
  geom_boxplot() +
  facet_grid(transect~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(firstflowerSnowCor, aes(x = month, y = correlation, fill = timing)) + 
  geom_boxplot() +
  facet_grid(transect~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))


ggplot(firstflowerSnowCor, aes(x = month, y = correlation, fill = transect)) + 
  geom_boxplot() +
  facet_grid(timing~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

###duration
durationflowerSnowCor <- ddply(first_floweringClim, .(species, variable, month, timing, transect), function(x) {
  if (sum(!is.na(x$duration)) > 10) {
    c(correlation = cor(x$duration, x$value, use = "pair"))
  }
})

ggplot(durationflowerSnowCor, aes(x = month, y = correlation, fill = timing)) + 
  geom_boxplot() +
  facet_grid(transect~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))


ggplot(durationflowerSnowCor, aes(x = month, y = correlation, fill = transect)) + 
  geom_boxplot() +
  facet_grid(timing~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))


###maxima
maxflowerSnowCor <- ddply(first_floweringClim, .(species, variable, month, timing, transect), function(x) {
  if (sum(!is.na(x$max)) > 10) {
    c(correlation = cor(x$max, x$value, use = "pair"))
  }
})

ggplot(maxflowerSnowCor, aes(x = month, y = correlation, fill = timing)) + 
  geom_boxplot() +
  facet_grid(transect~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))


ggplot(maxflowerSnowCor, aes(x = month, y = correlation, fill = transect)) + 
  geom_boxplot() +
  facet_grid(timing~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))
