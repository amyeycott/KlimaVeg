## ---- loadPhenology
#load libraries
library("ggplot2")
library("vegan")
library("broom")

#set default theme
th <- theme()

if(interactive()){
  source("phenology/load_phenology.R")
  load("phenology/data/downloaded_weather.Rdata")
} else {
  source("load_phenology.R")
}

## ---- phenologyPlots

## ---- Allium_ursinum
#pheology plots
pheno_plot <- filter(phenology2, species ==  "Allium ursinum", stage < 6) %>%
  ggplot(aes(x = doy, y = decile, colour = year, group = year)) + 
    geom_line() + 
    facet_grid(transect~stage, labeller = labeller(stage = as_labeller(stage_names))) +   
    scale_x_date(name = "Month", date_breaks = "3 month", date_labels = "%b") +
    th +
    theme(axis.text.x  = element_text(angle = 45, hjust = 1)) 
print(pheno_plot)

## ---- other_species_pheno
pheno_plot %+% filter(phenology2, species == "Daphne mezereum", stage < 6) 

## ---- autumn_flowers
autumn <- filter(phenology2, species == "Daphne mezereum", stage == 3, decile > 0) %>%
  ggplot(aes(x = doy, y = year, colour = decile)) + 
  geom_point() + 
  facet_grid(transect ~ .) + 
  scale_x_date(name = "Month", date_breaks = "1 month", date_labels = "%b") 

autumn
autumn %+% filter(phenology2, species == "Anemone nemorosa", stage == 3, decile > 0)
autumn %+% filter(phenology2, species == "Oxalis acetosella", stage == 3, decile > 0)
autumn %+% filter(phenology2, species == "Carpinus betulus", stage == 3, decile > 0)

#urtica dioica
autumn %+% filter(phenology2, species == "Urtica dioica", stage == 3, decile > 0)
autumn %+% filter(phenology2, species == "Urtica dioica", stage == 1, decile > 0)

#number of years
phenology2 %>% 
  filter(stage == 3) %>%
  group_by(species, transect) %>% 
  summarise(nyear = n_distinct(year)) %>% 
  ggplot(aes(x = nyear, fill = transect)) + 
  geom_histogram()
  
#species phenology - infrequent taxa removed
phenology2 %>% 
  filter(stage == 4) %>%
  group_by(species) %>% 
  mutate(nyear = n_distinct(year)) %>%
  filter(nyear > 10) %>%
  group_by(year, species, transect, pentad) %>% 
  summarise(mdecile = mean(decile, na.rm = TRUE)) %>% 
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
  geom_point(data = filter(comm, stage == 1) %>% rename(stage2 = stage), colour = "grey70") + 
  scale_y_discrete(limits = levels(comm$species)) +
  geom_point() + 
  facet_grid(transect~stage, scales = "free_y")

ggplot(comm %>% filter(stage == 3), aes(x = year, y = species)) + 
  geom_point(data = filter(comm, stage == 1) %>% rename(stage2 = stage), colour = "grey70") + 
  geom_point() +
  facet_grid(transect~., scales = "free_y")

#richness
comm %>% 
  group_by(year, stage, transect) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = year, y = n, colour = as.factor(stage), linetype = transect)) +
    geom_line() +
    ylim(0, NA)

## ---- first_flowering
first_phenology <- phenology2 %>% 
  filter(decile > 0, stage < 6) %>% 
  group_by(year, species, stage, transect) %>% 
  summarise(first = first(doy), last = last(doy), duration = last - first, max = max(decile), maxDate = doy[which.max(decile)]) %>%
  full_join(y = with(phenology2,
                  expand.grid(
                    year  = unique(year),
                    species = unique(species),
                    stage = 1:5,
                    transect = paste0("t", 36:39)
                  ) 
    ))

first_flowering <- first_phenology %>% 
  filter(stage %in% c(2,3), !is.na(first)) %>% # budding or flowering
  group_by(species, transect, stage) %>% 
  mutate(nyear = n_distinct(year)) %>%
  filter(nyear > 10) %>%
  mutate(median = median(first, na.rm = TRUE)) %>%
  group_by(stage) %>%
  mutate(timing = cut(as.vector(median), 
                      breaks = quantile(as.vector(median), probs = seq(0, 1, 1/3), na.rm = TRUE), 
                      labels = c("Early", "Mid", "Late"), 
                      include.lowest = TRUE))

    
first_flowering %>%
  group_by(species, transect, stage) %>%
  summarise(min = min(first), max = max(first), delta = max - min)

## ---- firstFloweringPlot
first_flowering %>% 
  filter(stage == 3) %>%
  group_by(species, transect, first) %>% 
  mutate(n = n()) %>%
  group_by(species)%>%
  mutate(median = median(first)) %>%
  ungroup() %>%
  mutate(species = factor(species, levels = unique(species[order(median)]))) %>%
  ggplot(aes(x = first, y = species, size = n, colour = transect)) + 
    geom_point(pch = 1) + 
    labs(x = "Month of first flowering", y = "") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")

## ----x
ggplot(first_flowering, aes(x = first, fill = transect)) + 
  geom_histogram() +
  facet_wrap(~stage)

#median by transect
first_flowering %>% 
  group_by(transect, stage) %>% 
  summarise(median = median(first, na.rm = TRUE)) %>%
  arrange(stage, transect)

## ----mergeClimateCorrelate
#merge with climate
first_floweringClim <- left_join(first_flowering , Bialowieza_monthly_lag)%>%
  select(-name, -id)

##correlate climate with phenology
firstflowerSnowCor <- first_floweringClim %>%
  group_by(species, variable, month, timing, transect, stage) %>%
  summarise(correlation = cor(as.vector(first), value, use = "pair"))

#all phenological 
firstflowerSnowCor <- first_floweringClim %>%
  select(-nyear, - median, -year, -date) %>%
  mutate(first = as.vector(first), last = as.vector(last), maxDate = as.vector(maxDate)) %>%
  group_by(species, variable, month, timing, transect, stage) %>%
  do(as.data.frame(t(cor(.[, !names(.) %in% c("species", "transect", "timing", "month", "variable", "value", "stage")], .$value, use = "pair"))))


## ----x
g <- filter(firstflowerSnowCor, variable == "tavg") %>%
  ggplot(aes(x = month, y = first)) + 
  geom_boxplot() +
  facet_grid(transect~stage, space = "free_x", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90))
g
g + aes(fill = timing)
g + aes(y = duration, fill = timing)
g + aes(y = max, fill = timing)

## ----firstfloweringCor
ffc <- filter(firstflowerSnowCor, stage == 2, variable == "tavg") %>% 
  ggplot(aes(x = month, y = first, fill = timing)) + 
  geom_boxplot() +
  facet_grid(transect~.) + 
  labs(x = "", y = "Correlation") + 
  th +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ffc
ffc %+% filter(firstflowerSnowCor, stage == 2, variable == "tavg")


## ---- climateRegression
mo <- "April"
g <- filter(first_floweringClim, variable == "tavg", month  == mo) %>% 
  filter(species < "Carex") %>%
  ggplot(aes(x = value, y = first, colour = transect, linetype = factor(stage), shape = factor(stage))) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) + 
    labs(x = "Temperature °C", y = "Date of first flowering") +
    facet_wrap(~species, scale = "free_y") +
    th

## ---- keepingUp1
#regression of phenology temperature in each month

firstflowerReg <- first_floweringClim %>%
  filter(variable == "tavg") %>%
  select(-nyear, -year) %>%
  mutate(first = as.vector(first), last = as.vector(last), maxDate = as.vector(maxDate)) %>%
  group_by(species, month, median, timing, transect, stage) %>%
  do(tidy(lm(first ~ value, data = .))[2, , drop = FALSE]) %>%
  mutate(month2 = ymd(paste0("2017-", month, "-1"))) %>%
  ungroup() %>%
  mutate(transect = factor(transect))

filter(firstflowerReg, month %in% month.name[2:7]) %>% ggplot(aes(x = estimate, fill = factor(stage))) + geom_vline(xintercept = 0, colour = "grey60", linetype = "dashed") + geom_density(alpha = 0.4) + facet_wrap(~month) 

firstflowerReg %>% group_by(stage, month) %>%
  summarise(median = median(estimate)) %>%
  spread(key = stage, value = median)

## ---- keepingUp2    
transect_colours <- scale_colour_discrete(drop = FALSE)
transect_fill <- scale_fill_discrete(drop = FALSE)


g <- filter(firstflowerReg, species == "Allium ursinum", stage == 3) %>% 
     mutate(month2 = case_when(transect == "t36" ~ month2 - 3,
                               transect == "t37" ~ month2 - 1,
                               transect == "t38" ~ month2 + 1,
                               transect == "t39" ~ month2 + 3))%>% 
     ggplot(aes(x = month2, y = estimate, ymax = estimate + 1.96 * std.error, min = estimate - 1.96 * std.error, colour = transect)) +
  geom_pointrange() +
  scale_x_date(name = "Month", date_breaks = "1 month", date_labels = "%b") +
  labs(y = "Effect, days/°C", xlab = "Month") +
  geom_segment(aes(x = median, y = -Inf, yend = Inf, xend = median, colour = transect), linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
  transect_colours 
  
print(g + th)

g2 <- g +  geom_ribbon(data = filter(seasonalwarming, doy < 180 & doy > 40), mapping = aes(x = doy2,  ymax = -1/(estimate + 1.96 * std.error), ymin = -1/(estimate - 1.96 * std.error)), alpha = 0.4, fill = "grey40", inherit.aes = FALSE) +
  geom_line(data = filter(seasonalwarming, doy < 180 & doy > 30), mapping = aes(x = doy2, y = -1/estimate), colour = "grey40", inherit.aes = FALSE) +
  coord_cartesian(ylim = c(-20, 5)) +
  th +
  theme(plot.margin=unit(c(-0.2,1,1,1), "cm"))

print(g2 + th)

h <- first_flowering %>%
  filter(species == "Allium ursinum", stage == 3) %>%
  ggplot(aes(x = first, fill = transect)) + 
  geom_histogram() +
  labs(y = "Number of years") +
  scale_x_date(limits = range(firstflowerReg$month2), name = "", date_breaks = "1 month", date_labels = "%b") + 
  transect_fill +
  th +
  theme(axis.text.x = element_blank(), plot.margin = unit(c(1,1,-0.2,1), "cm"))

cowplot::plot_grid(h, g2,  nrow = 2, align = "v", rel_heights = c(1/3, 2/3))


## ---- keepingUp3

print(g2 %+%  (filter(firstflowerReg, species == "Urtica dioica", stage == 3) %>% 
        mutate(month2 = case_when(transect == "t36" ~ month2 - 3,
                                  transect == "t37" ~ month2 - 1,
                                  transect == "t38" ~ month2 + 1,
                                  transect == "t39" ~ month2 + 3))) +
        th
)






