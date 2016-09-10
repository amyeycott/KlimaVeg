#load libraries
library("ggplot2")
library("vegan")

source("phenology/load_phenology.R")
source("phenology/load_weather.R")

#pheology plots
ggplot(phenology2 %>% filter(species == "Daphne mezereum"), aes(x = pentad, y = decile, colour = year)) + geom_line() + facet_grid(transect~stage)
ggplot(phenology2 %>% filter(species ==  "Allium ursinum"), aes(x = pentad, y = decile, colour = year)) + geom_line() + facet_grid(transect~stage)

#autumn flowers
ggplot(phenology2 %>% filter(species == "Daphne mezereum", stage == 3, decile > 0), aes(x = pentad, y = year, colour = decile)) + geom_point() + facet_grid(transect~.)
ggplot(phenology2 %>% filter(species == "Anemone nemorosa", stage == 3, decile > 0), aes(x = pentad, y = year, colour = decile)) + geom_point() + facet_grid(transect~.)
ggplot(phenology2 %>% filter(species == "Oxalis acetosella", stage == 3, decile > 0), aes(x = pentad, y = year, colour = decile)) + geom_point() + facet_grid(transect~.)


ggplot(phenology2 %>% filter(species == "Urtica dioica", stage == 3, decile > 0), aes(x = pentad, y = year, colour = decile)) + geom_point() + facet_grid(transect~.)
ggplot(phenology2 %>% filter(species == "Urtica dioica", stage == 1, decile > 0), aes(x = pentad, y = year, colour = decile)) + geom_point() + facet_grid(transect~.)

phenology2 %>% 
  filter(stage == 3) %>%
  group_by(species) %>% 
  summarise(nyear = n_distinct(year)) %>% ggplot(aes(x = nyear)) + geom_histogram()
  
phenology2 %>% 
  filter(stage == 4) %>%
  group_by(species) %>% 
  mutate(nyear = n_distinct(year)) %>%
  filter(nyear >10) %>%
  group_by(year, species, transect, pentad) %>% 
  summarise(mdecile = mean(decile)) %>% 
  filter(mdecile > 0) %>%
  ggplot(aes(x = pentad, y = species, colour = mdecile)) + 
    geom_point() + 
    facet_wrap(~transect, nrow = 1)


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
first_phenology <- phenology2 %>% 
  filter(decile > 0, stage < 6) %>% 
  group_by(year, species, stage, transect) %>% 
  summarise(first = first(pentad), last = last(pentad), duration = last - first, max = max(decile), maxDate = pentad[which.max(decile)]) %>%
  merge(
    with(phenology2,
      expand.grid(
        year  = unique(year),
        species = unique(species),
        stage = 1:5,
        transect = paste0("t", 36:39)
      )
    ), all = TRUE)

first_flowering <- first_phenology %>% 
  filter(stage == 3, !is.na(first)) %>%
  group_by(species, transect) %>% 
  mutate(nyear = n_distinct(year)) %>%
  filter(nyear > 10) %>%
  mutate(median = median(first, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(timing = cut(median, breaks = quantile(first, probs = seq(0, 1, 1/3), na.rm = TRUE), labels = c("Early", "Mid", "Late"), include.lowest = TRUE)) %>%
  select(-stage)

    
first_flowering %>%
  group_by(species, transect) %>%
  summarise(min = min(first), max = max(first), delta = max - min)

first_flowering %>% 
  group_by(species, transect, first) %>% 
  mutate(n = n()) %>%
  group_by(species)%>%
  mutate(median = median(first)) %>%
  ungroup() %>%
  mutate(species = factor(species, levels = unique(species[order(median)]))) %>%
  ggplot(aes(x = first, y = species, size = n, colour = transect)) + 
    geom_point(pch = 1) 

ggplot(first_flowering, aes(x = year, y = first, colour = species)) + 
    geom_line(show.legend = FALSE) + 
    geom_point(size = 0.5, show.legend = FALSE) +
    facet_grid(transect~stage)


ggplot(first_flowering %>% filter(stage == 3), aes(x = first, fill = transect)) + geom_histogram()

#median by transect
first_flowering %>% 
  group_by(transect) %>% 
  summarise(median = median(first, na.rm = TRUE))


#merge with climate
first_floweringClim <- first_flowering %>% 
  merge(monthlylag)

#correlate climate with phenology
firstflowerSnowCor <- first_floweringClim %>%
  group_by(species, variable, month, timing, transect) %>%
  summarise(correlation = cor(first, value, use = "pair"))

firstflowerSnowCor <- first_floweringClim %>%
  select(-nyear, - median, -year) %>%
  group_by(species, variable, month, timing, transect) %>%
  do(as.data.frame(t(cor(.[, !names(.) %in% c("species", "transect", "timing", "month", "variable", "value")], .$value, use = "pair"))))


firstflowerSnowCor %>%
  mutate(correlation = round(correlation, 2)) %>% 
  spread(key = variable, value = correlation)

ggplot(firstflowerSnowCor, aes(x = month, y = first)) + 
  geom_boxplot() +
  facet_grid(transect~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(firstflowerSnowCor, aes(x = month, y = first, fill = timing)) + 
  geom_boxplot() +
  facet_grid(transect~variable, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))

first_floweringClim %>% filter(variable == "temperature", month  == "March") %>% 
  ggplot(aes(x = value, y = first, colour = transect)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + facet_wrap(~species, scale = "free_y")

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

first_floweringClim
ggplot()
