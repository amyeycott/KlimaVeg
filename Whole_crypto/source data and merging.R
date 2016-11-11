#This project is to analyse all taxonomic groups in the crypto-klimaveg dataset. It was written by Amy Eycott starting 7th April 2016.

source("../Ania and Martin/AniaMartinLichens_dataloading.R")#the ../ is 'go up one level', doing it this way means that it finds the right directory whether it is on C, O, or another person's computer.
source("../sylwia/Bryophyte data loading.R")#in an ideal world, work out which step is printing a bunch of structure...
library(readxl)
VascOld.thin<-as.data.frame(read_excel("KLIMAVEG_BIALOWIEZA_VASCULAR_OLD_Corr.xls"), col_types=c(rep("text",3), rep("numeric", 48), "text"))
VascNew.thin<-as.data.frame(read_excel("KLIMAVEG_BIALOWIEZA_VASCULAR_2015.xls"))

##Sanity checks and tidying in vascular data
colnames(VascOld.thin)<-gsub(" ", "_",colnames(VascOld.thin))
VascOld.thin$Plot_number<-paste(VascOld.thin$Plot_number, "1992", sep="_") #spaces need to be replaced in column names otherwise it gets hard to refer to them by name
VascOld.thin$Frequency_1<-as.numeric(VascOld.thin$Frequency_1)

colnames(VascNew.thin)<-gsub(" ", "_",colnames(VascNew.thin))
VascNew.thin$Frequency_1<-as.numeric(VascNew.thin$Frequency_1)

#check that each species has one, and only one, frequency score
unique(VascOld.thin[,4:6]) #checks that there are no values that are not 1, and no record with a 1 in more than one frequency column. Right now 10552 has no frequency score. Actually 10551 should be the last line and there are 11883
VascOld.thin<-VascOld.thin[!is.na(VascOld.thin$Species_name),]#trims out empty rows
unique(VascNew.thin[,4:6])# Actually 11187 should be the last line and there are 11265
VascNew.thin<-VascNew.thin[!is.na(VascNew.thin$Species_name),]
unique(VascNew.thin[,4:6])

#At this stage I have three columns, Frequency_1 Frequency_2 and Frequency_3, all containing binary information. I want the new column frequency_score to have a 1 if there's a 1 in Frequency_1, a 2 if there's a 1 in Frequency_2, and a 3 if there's a 1 in Frequency_3. I could maybe make it work with three lines and subsetting (first line as below), but that seems clunky. More elegant solution?
#VascOld.thin$frequency_score[!is.na(VascOld.thin$Frequency_3),]<-(VascOld.thin$Frequency_3[!is.na(VascOld.thin$Frequency_3),])*3

#Can be simplified to 
VascOld.thin$frequency_score[VascOld.thin$Frequency_1] <- 1
VascOld.thin$frequency_score[VascOld.thin$Frequency_2] <- 2
VascOld.thin$frequency_score[VascOld.thin$Frequency_3] <- 3
#how does that even work? Taking the left hand side of the arrow only yields 'null'

# Alternatively - assuming max one TRUE per row
VascOld.thin$frequency_score <- rowSums(
  mapply("*", VascOld.thin[, c("Frequency_1", "Frequency_2", "Frequency_3")], 1:3),  na.rm = TRUE)
VascNew.thin$frequency_score <- rowSums(
  mapply("*", VascNew.thin[, c("Frequency_1", "Frequency_2", "Frequency_3")], 1:3),  na.rm = TRUE)

##making fat tables for ordinations##
library(tidyr)
VascOld.fat<-spread(VascOld.thin[,c(2,3,53)], Species_name,frequency_score, fill=0)
rownames(VascOld.fat)<-VascOld.fat$Plot_number
VascOld.fat$Plot_number<-NULL
VascNew.fat<-spread(VascNew.thin[,c(2,3,53)], Species_name,frequency_score, fill=0)
rownames(VascNew.fat)<-VascNew.fat$Plot_number
VascNew.fat$Plot_number<-NULL