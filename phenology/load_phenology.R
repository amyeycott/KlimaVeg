#load libraries
library("readxl")
library("tidyr")
library("dplyr")
library("lubridate")
library("assertthat")

#load phenology

#format
#year 1964 - 2016 
#species number
#delete
#phenological stages 1-6 vegetative, budding, flowering, fruiting, dispersal, resting
#1:73 dates of observations Originally every 5th day. Then every 10th (after 1989) Missing values filled with @
#. plant not observed
# 1-10 decile of plant pop. in given phenological state
# + single plants

phenfiles <- list(
  t36 = list(file = "Fenology36.xlsx", names = "species list36.xlsx"),
  t37 = list(file = "Phenology37.xlsx", names = "species list37.xlsx"),
  t38 = list(file = "Phenology38.xlsx", names = "species list38.xlsx"),
  t39 = list(file = "Phenology39.xlsx", names = "species list39.xlsx")
)

if(interactive()){
  path <- "phenology/data/"
} else {
  path <- "data/"
}

phenology <- plyr::ldply(phenfiles, function(transect){
  message(transect$file)
  phen <- read_excel(paste0(path, transect$file))
  
  #rename ROK to year and process                                                  
  names(phen)[names(phen) %in% c("ROK", "Year")] <- "year"
  phen$year <- ifelse(phen$year < 2000, phen$year + 1900, phen$year) 
  
  #remove redundant column if present
  phen$Gatunki36I <- NULL 
  
  #rename phenology stage
  names(phen)[names(phen) %in% c("Nr jednostki", "Phenolog state")] <- "stage"
  
  #rename species code
  names(phen)[names(phen) %in% c("Nr gatunku", "N species")] <- "sppCode"
  #browser()
  #spp names 
  sppNames <- read_excel(paste0(path, transect$names))
  sppNames <- as.data.frame(sppNames)
  
  message(paste(setdiff(phen$sppCode, sppNames[, 1]), collapse = " "))
  assert_that(all(phen$sppCode %in% sppNames[, 1])) # check all IDs have names
  assert_that(!any(duplicated(sppNames[, 1]))) # check no duplicated codes in dictionary
  phen$species <- sppNames[phen$sppCode, 2]# second col has spp data but multiple names
  phen$sppCode <- NULL
  
  phen
}, .id = "transect")

##dictionary
dictionary <- read.table(paste0(path, "dictionary.tab"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
 #fill missing new names with old names
dictionary$new[dictionary$new == ""] <- dictionary$old[dictionary$new == ""] 

#update species names
phenology$species <- plyr::mapvalues(phenology$species, dictionary$old, dictionary$new)
phenology$species <- plyr::mapvalues(phenology$species, "GLYCERIA\r\r\nMANNA", "Glyceria maxima")#problems with escape characters

phenology2 <- gather(phenology, key = "pentad", value = "decile", -species, -year, -stage, -transect) %>%
  filter(!decile %in% c("@", "@@"), !is.na(decile)) %>%
  mutate(decile = trimws(decile)) %>%
  mutate(decile = gsub('"', "", decile)) %>% # remove quote marks
  mutate(pentad = as.numeric(pentad)) %>%
  mutate(doy = as.Date(pentad * 5, origin = ymd("2017-01-01")))

#clean
as.matrix(table(phenology2$decile))

oddities <- phenology2[!phenology2$decile %in% c(".", "+", 1:10, "x"), ]
dim(oddities)
if(interactive()){
  write.csv2(oddities, "phenology/oddities.csv")
}
#replace oddities
phenology2$decile <- plyr::revalue(phenology2$decile,
  c(
    "," = ".",
    ".." = ".",
    ".+" = "+", 
    "+." = "+",
#    0  = ?
    ".1" = 1,
    "+1" = 1,
    ".10" = 10,
    "@10" = 10,
    "10," = 10,
    "10." = 10,
    "100" = 10,
    "1000" = 10,
    "101" = 10,
    "1010" = 10,
    "10100" = 10,
    #108         1
    #109         6
    "110" = 10,
    ".2" = 2,
    ".3" = 3,
    ".5" = 5,
    ".6" = 6,
    ".7" = 7,
    ".8" = 8,
    ".9" = 9
    #x       34656
  )
)

phenology2$decile[phenology2$decile == ""] <- "."

# remove remaining oddities
phenology2 <- phenology2[phenology2$decile %in% c(".", "+", 1:10, "x"), ]

#. to 0
phenology2$decile[phenology2$decile == "."] <- "0"
#+ to 0.5
phenology2$decile[phenology2$decile == "+"] <- "0.5"
#x to NA
phenology2$decile <- as.numeric(phenology2$decile)

##phenology stages labels
stage_names <- c(
  '1'="Vegetative",
  '2'="Budding",
  '3'="Flowering",
  '4'="Fruiting",
  '5'="Dispersing",
  '6'="Dormant"
)

##species traits

traits <- read_excel(paste0(path, "Indicator_values_phenology_2017.xlsx"), skip = 1)
traits <- traits[, nchar(names(traits)) > 0]

traits <- traits %>%
  rename(light = L, temperature = T, humidity = W, trophism = Tr, reaction = R)

sort(setdiff(phenology2$species, traits$Species.name))
