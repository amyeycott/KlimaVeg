#####################################################################################
#tidy up suumitdiv

summitdiv.df<-read.csv("C:\\Users\\SiriVH\\Dropbox\\KlimaVegFolder\\summits_gradient_siri.xlsx_only two recordings.csv", header=T, sep=';')

summitdiv.df<-summitdiv.df[!(summitdiv.df$Mountain_name==""),]
summitdiv.df<-summitdiv.df[!is.na(summitdiv.df$find_altitude),]#why are there some NAs in find altitude?

alps<-summitdiv.df[,c("Mountain_name", "find_altitude", "year_of_record", "full_species_name")]
alps<-unique(alps)
library(dplyr)
library(tidyr)


step1 <- alps %>%
  mutate(period_record = ifelse(year_of_record < 2000, "old", "new")) %>%
  select(-year_of_record) %>%
  group_by(Mountain_name, full_species_name, period_record) %>%
  slice(which.max(find_altitude)) %>%
  spread(key = period_record, value = find_altitude)





################################################################################################
#getting rid of diff.col

klimaveg<-read.csv("C:\\Users\\SiriVH\\Dropbox\\KlimaVegFolder\\KlimaVeg.csv", header=T, sep=';')

klimaveg.nod<-klimaveg[,!ends_with()]

select()

klimaveg.nod<-select(klimaveg, -ends_with("d"))

names(klimaveg)

#################################################################################################


#first the endswith to pull out 'new' and 'old'

klimaveg.m <- select(klimaveg.nod, ends_with ("m"), ends_with("s"))
klimaveg.f<-select(klimaveg.nod, ends_with ("f"), ends_with("s"))




names(klimaveg.m)<-substr(names(klimaveg.m),1,4)
names(klimaveg.f)<-substr(names(klimaveg.f),1,4)                       


new<-gather(klimaveg.m, key=mountain, value=score, -Spec)
old<-gather(klimaveg.f, key=mountain, value=score, -Spec)


################################################################################################










step2a<-step1 %>% 
  group_by(Mountain_name,full_species_name,find_altitude) %>%
  slice(which.min(year_of_record))#is not necessarily finding the oldest - see step2a[step2a$full_species_name=="Trisetum_distichophyllum",] which returns three years on one mountain.

step2b<-step1 %>% 
  group_by(Mountain_name,full_species_name,find_altitude) %>%
  slice(which.max(year_of_record))


step2a$fuckyou_r<-paste(step2a$Mountain_name, step2a$full_species_name)
step2b$fuckyou_r<-paste(step2b$Mountain_name, step2b$full_species_name)

step3<-merge(step2a,step2b, by="fuckyou_r")





#####################################################################################












eurodata<-read.csv("C:\\Users\\naz001\\Dropbox\\KlimaVegFolder\\EURODATA_RAW.csv", header=T, sep=';')
colnames(eurodata)

summitdiv.df<-read.csv("C:\\Users\\naz001\\Dropbox\\KlimaVegFolder\\SummitDiv.csv", header=T, sep=';')

install.packages('tidyr')
library(tidyr)
install.packages('dplyr')
library(dplyr)

summitdiv<-data.frame(summitdiv.df$Mountain_name, summitdiv.df$find_altitude, summitdiv.df$year_of_record, summitdiv.df$full_species_name)

year<-summitdiv$summitdiv.df.year_of_record

summitdiv<-data.frame(mountain, year, species, recording)

summitsummary<-aggregate(summitdiv, by=list(mountain, year, species), function (x){max(x)},x=summitdiv$recording)

summitdiv<-unique(summitdiv)

step1<-ddply(summitdiv[,c(1,2,3,4)], c("mountain","year","species"),function(df)max(df$recording, na.rm=TRUE))



year<-paste(summitdiv$summitdiv.Mountain_name, summitdiv$summitdiv.year_of_record)

summitdiv<-data.frame(summitdiv$summitdiv.find_altitude, summitdiv$summitdiv.full_species_name, year)

spread(summitdiv, year, summitdiv.summitdiv.find_altitude)
newdata<-spread(summitdiv, year, summitdiv.summitdiv.find_altitude)

species<-summitdiv$summitdiv.summitdiv.full_species_name
recording<-summitdiv$summitdiv.summitdiv.find_altitude

summitdiv<-data.frame(recording, species, year)

spread(summitdiv, year, recording)



summitdiv<-unique(summitdiv)

aggdata <-aggregate(mtcars, by=list(cyl,vs), FUN=mean, na.rm=TRUE)


library(plyr)
#example code: ddply(dd, c("dim1","dim2"), function(df)mean(df$v1))







summitdiv<-unique(summitdiv[,c('species','year')])




spread(summitdiv, year, recording)
