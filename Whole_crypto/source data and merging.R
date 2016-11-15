#This project is to analyse all taxonomic groups in the crypto-klimaveg dataset. It was written by Amy Eycott starting 7th April 2016.

source("../Ania_and_Martin/AniaMartinLichens_dataloading.R")#the ../ is 'go up one level', doing it this way means that it finds the right directory whether it is on C, O, or another person's computer.
source("../sylwia/Bryophyte data loading.R")#in an ideal world, work out which step is printing a bunch of structure...
library(readxl)
VascOld.thin<-as.data.frame(read_excel("KLIMAVEG_BIALOWIEZA_VASCULAR_OLD_Corr.xls"), col_types=c(rep("text",3), rep("numeric", 48), "text"))
VascNew.thin<-as.data.frame(read_excel("KLIMAVEG_BIALOWIEZA_VASCULAR_2015.xls"))

##Sanity checks and tidying in vascular data
colnames(VascOld.thin)<-gsub(" ", "_",colnames(VascOld.thin))
VascOld.thin$Plot_number<-paste(VascOld.thin$Plot_number, "1992", sep="_") #spaces need to be replaced in column names otherwise it gets hard to refer to them by name
VascOld.thin$Frequency_1<-as.numeric(VascOld.thin$Frequency_1)

colnames(VascNew.thin)<-gsub(" ", "_",colnames(VascNew.thin))
VascNew.thin$Frequency_1_2015<-as.numeric(VascNew.thin$Frequency_1_2015)

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

##making fat tables for ordinations##
library(tidyr)
VascOld.fat<-spread(VascOld.thin[,c(2,3,53)], Species_name,frequency_score, fill=0)
rownames(VascOld.fat)<-VascOld.fat$Plot_number
VascOld.fat$Plot_number<-NULL
VascNew.fat<-spread(VascNew.thin[,c(2,3,53)], Species_name_2015,frequency_score, fill=0)
rownames(VascNew.fat)<-VascNew.fat$Plot_number_2015
VascNew.fat$Plot_number_2015<-NULL

##making a combined file for vegdist

