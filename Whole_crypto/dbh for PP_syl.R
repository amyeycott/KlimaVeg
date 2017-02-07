#load libraries
library("readxl")
library("ggplot2")
library("tidyr")# tidyr s?uzy do obr?bki danych z wielkich arkuszy do ma?ych
library("dplyr")

#load data
d1990 <- read_excel("Andrzej Keczynski dane.xlsx", sheet = "48_90")
d1998 <- read_excel("Andrzej Keczynski dane.xlsx", sheet =" 48_98")
d2005 <- read_excel("Andrzej Keczynski dane.xlsx", sheet="48_2005")
d2016 <- read_excel("Andrzej Keczynski dane.xlsx", sheet="48_2016")

names(d1998)[1] <- "DBH"
names(d2005)[1] <- "DBH"
names(d2016)[1] <- "DBH"

d1990$year <- 1990
d1998$year <- 1998
d2005$year <- 2005
d2016$year <- 2016

#combine datasets
dall <- plyr::rbind.fill(d1990, d1998, d2005, d2016)
head(dall)

#fat to thin
dall2 <- gather(dall, key = species, value  = number, -year, -DBH) %>% 
  filter(!is.na(number)) %>%
  mutate(DBH = as.numeric(gsub("DBH ", "", DBH))) %>%
  mutate(species = gsub(" 2005", "", species)) %>%
  mutate(species = gsub(" 2016", "", species)) %>%
  mutate(stage = ifelse(grepl("SNAG", species), "snag", "ls")) %>%
  mutate(species = sub(" SNAG", "", species)) %>%
  mutate(species = sub(" LS", "", species)) %>%
  filter(!species %in% c("Frangula alnus", "Populus tremula", "Sorbus aucuparia")) %>%
  mutate(DBHclass = round((DBH+1)/3)*3-1) %>%  #turns into classes of 3 cm
  group_by(DBHclass, species, year, stage) %>%
  summarise(number = sum(number))


dall2 %>% as.data.frame()
summary(dall2)


##plot with four species, ls and snag.

x11(7,7)
#panel per year, colour by species, ltype by snag/ls
ggplot(dall2, aes(x = DBHclass, y = number, colour  = species, linetype = stage)) +
  geom_line()+scale_y_log10()+facet_wrap(~ year)

#one species per panel, colour for year and ltype for ls/snag
ggplot(dall2, aes(x = DBHclass, y = number, colour  = as.factor(year), linetype = stage)) +
  geom_line() + 
  scale_y_log10() +
  facet_wrap(~ species)

#plot with four species in columns, ls and snag in rows.
ggplot(dall2, aes(x = DBHclass, y = number, colour  = as.factor(year), linetype = stage)) +
  geom_line() + 
  scale_y_log10() +
  facet_grid(stage~species) +
  theme_bw()

#plot with just the first and last year, ltype is ls/snag, colour is year, one plot per species.
ggplot(dall2[!dall2$year=="2005"&!dall2$year=="1998",], aes(x = DBHclass, y = number, colour  = as.factor(year), linetype = stage)) +
  geom_line()+scale_y_log10()+facet_wrap(~ species)
savePlot("PP DBH 2 years all spp.png", type="png")

#the same as the previous one but messing with how it looks.
x11(7,4)
ggplot(dall2[!dall2$year=="2005"&!dall2$year=="1998",], aes(x = DBHclass, y = number, colour  = as.factor(year), linetype = stage))+
  geom_line(size = 1)+
  scale_y_log10(limits=c(1,max(dall2$number[!dall2$year=="2005"&!dall2$year=="1998"])))+
  scale_x_continuous(limits=c(1, 80))+
  facet_wrap(~ species, scales = "free")+
  theme_classic()+
  theme(strip.text = element_text(face = "italic"))+
  labs(colour="Year of survey", linetype="", x="D.b.h. (cm)", y="Number of stems")+
  scale_linetype(labels=c("Living standing","Snag"))+
  scale_color_manual(values=c("#3873AE","#EF9335"))
savePlot("PP DBH 2 years all spp_newformat.png", type="png")
savePlot("PP DBH 2 years all spp_newformat.pdf", type="pdf")
savePlot("PP DBH 2 years all spp_newformat.eps", type="eps")
