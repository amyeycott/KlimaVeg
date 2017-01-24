#This project is to analyse all taxonomic groups in the crypto-klimaveg dataset. It was written by Amy Eycott starting 7th April 2016.

source("../Ania_and_Martin/AniaMartinLichens_dataloading.R")#the ../ is 'go up one level', doing it this way means that it finds the right directory whether it is on C, O, or another person's computer. This has to be done for all the files loading in the project if other places use this code as source
source("../sylwia/Bryophyte data loading.R")#in an ideal world, work out which step is printing a bunch of structure...
library(readxl)
VascOld.thin<-as.data.frame(read_excel("../Whole_crypto/KLIMAVEG_BIALOWIEZA_VASCULAR_OLD_Corr.xls"), col_types=c(rep("text",3), rep("numeric", 48), "text"))
VascNew.thin<-as.data.frame(read_excel("../Whole_crypto/KLIMAVEG_BIALOWIEZA_VASCULAR_2015.xls"))
phytosoc<-as.data.frame(read_excel("../Whole_crypto/habitat share.xlsx"))#Falinski's phytosociological classifications of each plot

##Sanity checks and tidying in vascular data
colnames(VascOld.thin)<-gsub(" ", "_",colnames(VascOld.thin))
VascOld.thin$Plot_number<-paste(substr(VascOld.thin$Plot_number,1,1),substr(VascOld.thin$Plot_number,3,4), "1992", sep="") #to match row name formatting in other datasets. Format is letter, 2 numeric, year eg A011992 
VascOld.thin$Frequency_1<-as.numeric(VascOld.thin$Frequency_1)
VascOld.thin$Species_name<-gsub(" ", "_",VascOld.thin$Species_name)

colnames(VascNew.thin)<-gsub(" ", "_",colnames(VascNew.thin))
VascNew.thin$Plot_number_2015<-paste(substr(VascNew.thin$Plot_number,1,1),substr(VascNew.thin$Plot_number,3,4), "2015", sep="")
VascNew.thin$Frequency_1_2015<-as.numeric(VascNew.thin$Frequency_1_2015)
VascNew.thin$Species_name_2015<-gsub(" ", "_",VascNew.thin$Species_name_2015)

#check that each species has one, and only one, frequency score
unique(VascOld.thin[,4:6]) #checks that there are no values that are not 1, and no record with a 1 in more than one frequency column. Right now 10552 has no frequency score. Actually 10551 should be the last line and there are 11883
VascOld.thin<-VascOld.thin[!is.na(VascOld.thin$Species_name),]#trims out empty rows
unique(VascNew.thin[,4:6])# Actually 11187 should be the last line and there are 11265
VascNew.thin<-VascNew.thin[!is.na(VascNew.thin$Species_name),]

# to make one column with the frequency in
VascOld.thin$frequency_score <- rowSums(
  mapply("*", VascOld.thin[, c("Frequency_1", "Frequency_2", "Frequency_3")], 1:3),  
  na.rm = TRUE)
VascNew.thin$frequency_score <- rowSums(
  mapply("*", VascNew.thin[, c("Frequency_1_2015", "Frequency_2_2015", "Frequency_3_2015")], 1:3),  
  na.rm = TRUE)

##making fat tables for distance matrices##
library(tidyr)
VascOld.fat<-spread(VascOld.thin[,c(2,3,53)], Species_name,frequency_score, fill=0)
rownames(VascOld.fat)<-VascOld.fat$Plot_number
VascOld.fat$Plot_number<-NULL
VascNew.fat<-spread(VascNew.thin[,c(2,3,53)], Species_name_2015,frequency_score, fill=0)
rownames(VascNew.fat)<-VascNew.fat$Plot_number_2015
VascNew.fat$Plot_number_2015<-NULL
colnames(VascNew.fat)<-gsub(" ", "_",colnames(VascNew.fat))
colnames(VascOld.fat)<-gsub(" ", "_",colnames(VascOld.fat))

##making a combined file for vegdist
library(analogue)
Vascall.df<-join(VascOld.fat, VascNew.fat, na.replace=TRUE, split=FALSE, type="outer")

#fixing comp(lichens) rownames for merging with other datasets
rownames(comp)<-gsub(".x",1992, rownames(comp))
rownames(comp)<-gsub(".y",2015, rownames(comp))
#fixing the seperate-by-year vasc data rownames for merging purposes. This step has to happen *AFTER* vascall
rownames(VascOld.fat)<-substr(rownames(VascOld.fat),1,3)#broken
rownames(VascNew.fat)<-substr(rownames(VascNew.fat),1,3)

#phytosociological file from falinski: data tidying and matching plot name format
colnames(phytosoc)<-gsub(" ", "_",colnames(phytosoc))#takes spaces out of column names
phytosoc$Plot_No.[nchar(phytosoc$Plot_No.)==2]<-paste0(substr(phytosoc$Plot_No.[nchar(phytosoc$Plot_No.)==2], 1,1),"0", substr(phytosoc$Plot_No.[nchar(phytosoc$Plot_No.)==2], 2,2))#makes format for Site match that elsewhere (i.e. F02 not F2). Note: ONLY RUN ONCE each time the read_excel is run. 
phytosoc[is.na(phytosoc)]<-0
phytosoc$ncomms<-rowSums(phytosoc[,2:7]>0)#magic numbers are very bad, but subset wasn't working
phytosoc$dominant<-colnames(phytosoc[,2:7])[max.col(phytosoc[,2:7], ties.method = 'first')]  #return the column name of the column with the highest value. Should probably check how many ties there are though.

####Species characteristics - Indicator values and conservation status####
#for bryophytes and lichens, the code is in their respective projects
vasc.ellen<-read_excel("Sierhanowo_species_indicator_values_final.xlsx", skip=1)
#sapply(vasc.ellen, FUN=unique)checks for sensible values
vasc.ellen<-vasc.ellen[,1:8]#needs magic numbers because some of the columns are not named
vasc.ellen$Species.name<-gsub(" ", "_",vasc.ellen$Species.name)

vasc.protected<-read_excel("Sierhanowo_protected_red_listed_vascular.xlsx", skip=1)[,1:3]#skip=1 because the column headings are on line 2, line 1 describes the contents of the worksheet. [1:3] because due to the title line, several blank column are registering.
vasc.protected$Red_coded<-as.factor(vasc.protected$`Red coded`)
vasc.protected<-vasc.protected[,-3]
vasc.protected$Protected<-as.factor(vasc.protected$Protected)
vasc.protected$Species.name<-gsub(" ", "_",vasc.protected$Species.name)
#