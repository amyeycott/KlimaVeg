#This project is to analyse all taxonomic groups in the crypto-klimaveg dataset. It was written by Amy Eycott starting 7th April 2016.

source("../Ania_and_Martin/AniaMartinLichens_dataloading.R")#the ../ is 'go up one level', doing it this way means that it finds the right directory whether it is on C, O, or another person's computer. This has to be done for all the files loading in the project if other places use this code as source
source("../sylwia/Bryophyte data loading.R")#ignore warning about NAs introduced by coercion, they are fine...
library(readxl)
library(tidyverse)

#BEFORE LOADING. If there is a new version of the following data files, they have to be saved as xlsx and the blank trailing columns removed by hand. Otherwise you get an error if you set column types, and print a bunch of weird extra stuff if you don't (plus setting the column types is good). 
vascOld.thin<-as.data.frame(read_excel("../Whole_crypto/KLIMAVEG_BIALOWIEZA_VASCULAR_OLD_Corr.xlsx"),col_types=c(rep("text",3), rep("numeric", 48), "text"))#saving as xlsx seems to have fixed the problem of printing a load of structure, and allows column type setting, but now complains because column D is stored as text. Seem to work though.
vascOld.thin$`Frequency 1`<-as.numeric(vascOld.thin$`Frequency 1`)
vascNew.thin<-as.data.frame(read_excel("../Whole_crypto/KLIMAVEG_BIALOWIEZA_VASCULAR_2015_FINAL.xlsx", col_types=c(rep("text",3), rep("numeric", 48), "text")))#saving as xlsx seems to have fixed the problem of printing a load of structure, and allows column type setting, and corces column D to numeric neatly.
phytosoc<-as.data.frame(read_excel("../Whole_crypto/habitat share.xlsx"))#Falinski's phytosociological classifications of each plot

##Sanity checks and tidying in vascular data
names(vascOld.thin)<-gsub(" ", "_",names(vascOld.thin))
vascOld.thin$Plot_number<-paste(substr(vascOld.thin$Plot_number,1,1),substr(vascOld.thin$Plot_number,3,4), "1990", sep="") #to match row name formatting in other datasets. Format is letter, 2 numeric, year eg A011990.
vascOld.thin$Frequency_1<-as.numeric(vascOld.thin$Frequency_1)
vascOld.thin$Species_name<-gsub(" ", "_",vascOld.thin$Species_name)
names(vascNew.thin)<-gsub(" ", "_",names(vascNew.thin))
names(vascNew.thin)<-gsub("2016","2015",names(vascNew.thin))
vascNew.thin$Plot_number_2015<-paste(substr(vascNew.thin$Plot_number,1,1),substr(vascNew.thin$Plot_number,3,4), "2015", sep="")
vascNew.thin$Frequency_1_2015<-as.numeric(vascNew.thin$Frequency_1_2015)
vascNew.thin$Species_name_2015<-gsub(" ", "_",vascNew.thin$Species_name_2015)


#check that each species has one, and only one, frequency score
unique(vascOld.thin[,4:6]) #checks that there are no values that are not 1, and no record with a 1 in more than one frequency column. Right now 10552 has no frequency score. Actually 10551 should be the last line and there are 11883
vascOld.thin<-vascOld.thin[!is.na(vascOld.thin$Species_name),]#trims out empty rows
unique(vascNew.thin[,4:6])# Actually 11187 should be the last line and there are 11265
vascNew.thin<-vascNew.thin[!is.na(vascNew.thin$Species_name),]

# to make one column with the frequency in
vascOld.thin$frequency_score <- rowSums(
  mapply("*", vascOld.thin[, c("Frequency_1", "Frequency_2", "Frequency_3")], 1:3),  
  na.rm = TRUE)
vascNew.thin$frequency_score <- rowSums(
  mapply("*", vascNew.thin[, c("Frequency_1_2015", "Frequency_2_2015", "Frequency_3_2015")], 1:3),  
  na.rm = TRUE)

##making fat tables for distance matrices##
vascOld.fat<-spread(vascOld.thin[,c(2,3,53)], Species_name,frequency_score, fill=0)
rownames(vascOld.fat)<-vascOld.fat$Plot_number
vascOld.fat$Plot_number<-NULL
vascNew.fat<-spread(vascNew.thin[,c(2,3,53)], Species_name_2015,frequency_score, fill=0)
rownames(vascNew.fat)<-vascNew.fat$Plot_number_2015
vascNew.fat$Plot_number_2015<-NULL
colnames(vascNew.fat)<-gsub(" ", "_",colnames(vascNew.fat))
colnames(vascOld.fat)<-gsub(" ", "_",colnames(vascOld.fat))

##making a combined file for vegdist
library(analogue)
vascall.df<-join(vascOld.fat, vascNew.fat, na.replace=TRUE, split=FALSE, type="outer")

#fixing comp(lichens) rownames for merging with other datasets
rownames(comp)<-gsub(".x",1990, rownames(comp))
rownames(comp)<-gsub(".y",2015, rownames(comp))
#fixing the seperate-by-year vasc data rownames for merging purposes. This step has to happen *AFTER* vascall
rownames(vascOld.fat)<-substr(rownames(vascOld.fat),1,3)
rownames(vascNew.fat)<-substr(rownames(vascNew.fat),1,3)

#phytosociological file from falinski: data tidying and matching plot name format
colnames(phytosoc)<-gsub(" ", "_",colnames(phytosoc))#takes spaces out of column names
phytosoc$Plot_No.[nchar(phytosoc$Plot_No.)==2]<-paste0(substr(phytosoc$Plot_No.[nchar(phytosoc$Plot_No.)==2], 1,1),"0", substr(phytosoc$Plot_No.[nchar(phytosoc$Plot_No.)==2], 2,2))#makes format for Site match that elsewhere (i.e. F02 not F2). Note: ONLY RUN ONCE each time the read_excel is run. 
phytosoc[is.na(phytosoc)]<-0
phytosoc$ncomms<-rowSums(phytosoc[,c("TC","CA","PQ","QP","PP","CelA")]>0)
phytosoc$dominant<-as.factor(colnames(phytosoc[,c("TC","CA","PQ","QP","PP","CelA")])[max.col(phytosoc[,c("TC","CA","PQ","QP","PP","CelA")], ties.method = 'first')])  #return the column name of the column with the highest value. Should probably check how many ties there are though.

####Species characteristics - Indicator values and conservation status####
#for bryophytes and lichens, the code is in their respective projects
vasc.ellen<-read_excel("Sierhanowo_species_indicator_values_final_final.xlsx", skip=1)
vasc.ellen<-vasc.ellen[, colSums(!is.na(vasc.ellen)) != 0]#removes columns filled entirely with NA
names(vasc.ellen)<-make.names(names(vasc.ellen))
vasc.ellen$Species.name<-gsub(" ", "_",vasc.ellen$Species.name)

vasc.protected<-read_excel("Sierhanowo_protected_red_listed_vascular.xlsx", skip=1)#skip=1 because the column headings are on line 2, line 1 describes the contents of the worksheet. 
vasc.protected<-vasc.protected[, colSums(!is.na(vasc.protected)) != 0]
names(vasc.protected)<-gsub(" ", "_",names(vasc.protected))
vasc.protected$Red_coded<-as.factor(vasc.protected$Red_coded)
vasc.protected$Protected<-as.factor(vasc.protected$Protected)
vasc.protected$Species.name<-gsub(" ", "_",vasc.protected$Species.name)

#subsetting composition objects
dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")
lichall.df.ss<-comp[!substr(rownames(comp),1,3)%in%dodgysquares,!names(comp)=="Year"]
lichall.df.ss<-lichall.df.ss[,colSums(lichall.df.ss)>0]
bryoall.df.ss<-bryo.fat[!substr(rownames(bryo.fat),1,3)%in%dodgysquares,]
bryoall.df.ss<-bryoall.df.ss[,colSums(bryoall.df.ss)>0]
vascall.df.ss<-vascall.df[!substr(rownames(vascall.df),1,3)%in%dodgysquares,]
vascall.df.ss<-vascall.df.ss[,!is.na(colSums(vascall.df.ss))]

#colour scheme to match falinski maps
coloury <- data.frame(
  Phytosociology_Latin = c("CA","CelA","PP","PQ","QP","TC"),
  Colour_softer = c("#6CC8F0", "#AB66AD","#FCF19C","#B5634B","#E0A575","#76B588"),
  Community_in_1990 = c("Streamside alder-ash forest", "Black alder bog forest","Mesotrophic pine forest","Meso-oligotrophic mixed for.", "Spruce forest", "Mixed deciduous forest"), 
  Colour_bolder = c("#3AAEE3", "#924884","#FFF383","#A75F4A","#D19563","#61A375"), 
  stringsAsFactors = FALSE
)

