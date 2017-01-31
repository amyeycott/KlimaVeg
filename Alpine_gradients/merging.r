#####################################################################################
#tidy up suumitdiv

summitdiv.df<-read.csv("data\\SummitDiv.csv", header=TRUE, sep=';', stringsAsFactors = FALSE)

summitdiv.df<-summitdiv.df[!(summitdiv.df$Mountain_name==""),]
summitdiv.df<-summitdiv.df[!is.na(summitdiv.df$find_altitude),]#why are there some NAs in find altitude?

alps<-summitdiv.df[,c("Mountain_name", "find_altitude", "year_of_record", "full_species_name")]
alps<-unique(alps)
library(dplyr)
library(tidyr)


alps <- alps %>%
  mutate(period_record = ifelse(year_of_record < 2000, "old", "new")) %>%
  select(-year_of_record) %>%
  group_by(Mountain_name, full_species_name, period_record) %>%
  slice(which.max(find_altitude)) %>%
  spread(key = period_record, value = find_altitude)

colnames(alps)<-c("mountain", "species", "new", "old")

alps<-alps[,1:4]  #UGLY 

alps$diff <- alps$new - alps$old

################################################################################################
#getting rid of diff.col in KlimaVeg

klimaveg<-read.csv("data\\KlimaVeg.csv", header=TRUE, sep=';', stringsAsFactors = FALSE)

klimaveg.nod<-select(klimaveg, -ends_with("d")) %>%
  rename(species = Species)

klimaveg.m <- select(klimaveg.nod, ends_with ("m"), ends_with("s"))
klimaveg.f <- select(klimaveg.nod, ends_with ("f"), ends_with("s"))

names(klimaveg.m)<-substr(names(klimaveg.m),1,4)
names(klimaveg.f)<-substr(names(klimaveg.f),1,4)                       

new<-gather(klimaveg.m, key=mountain, value=score, -spec)
old<-gather(klimaveg.f, key=mountain, value=score, -spec)

colnames(new)[3]<-"new"
colnames(old)[3]<-"old"

scandes <- bind_cols(new, select(old, old)) %>%
  rename(species = spec) %>%
  mutate(old = gsub("\\+", "", old), old = as.integer(old))

scandes$diff <- scandes$new - scandes$old

###############################################################################################
##Legge til metadata

meta_scandes <- read.csv("data\\Meta_scandes.csv", header=TRUE, sep=';', stringsAsFactors = FALSE)
elevation_scandes <- read.csv("data\\elevation.csv", header=TRUE, sep=';', stringsAsFactors = FALSE)
intensity_scandes <- read.csv("data\\intensity.csv", header=TRUE, sep=';', stringsAsFactors = FALSE)

meta_scandes <- unique(meta_scandes[,c("Fjell_name", "Turisme_mengde", "Område_name")])
meta_scandes <- plyr::rename(meta_scandes, c("Fjell_name"="mountain", "Turisme_mengde"="tourism", "Område_name"="area"))
elevation_scandes <- elevation_scandes[,c("mountain", "elevaton_summit")]
meta_scandes <- merge(meta_scandes, elevation_scandes, by=c("mountain")) 
meta_scandes <- merge(meta_scandes, intensity_scandes, by=c("mountain")) 
meta_scandes <- plyr::rename(meta_scandes, c("elevaton_summit"="altitude", "grazing_intensity"="grazing"))

meta_alps <- read.csv2("data\\Meta_alps.csv", header=TRUE, sep=';', stringsAsFactors = FALSE)
meta_alps <- unique(meta_alps[,c("Mountain_name", "mtn_altitude", "visitor_freq", "Herbivore_freq")])

meta_alps <- meta_alps[-c(66, 276, 346, 359), ] #UGLY magic numbers

meta_alps <- plyr::rename(meta_alps, c("Mountain_name"="mountain", "mtn_altitude"="altitude", "visitor_freq"="tourism", "Herbivore_freq"="grazing"))
meta_alps$area <- "ALPS"



#############################################################################################
## merging

euro_scandes <- merge(scandes, meta_scandes, by=c("mountain"))
euro_alps <- merge(alps, meta_alps, by=c("mountain"))

###WORKING FROM HERE AND DOWN####

euro <- bind_rows(euro_alps, euro_scandes)

