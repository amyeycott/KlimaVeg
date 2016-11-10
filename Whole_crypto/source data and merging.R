#This project is to analyse all taxonomic groups in the crypto-klimaveg dataset. It was written by Amy Eycott starting 7th April 2016.

source("../Ania and Martin/AniaMartinLichens_dataloading.R")#the ../ is 'go up one level', doing it this way means that it finds the right directory whether it is on C, O, or another person's computer.
source("../sylwia/Bryophyte data loading.R")#in an ideal world, work out which step is printing a bunch of structure...
library(readxl)
VascOld.thin<-as.data.frame(read_excel("KLIMAVEG_BIALOWIEZA_VASCULAR_OLD_Corr.xls"), col_types=c(rep("text",3), rep("numeric", 48), "text"))

#Richard:  the col_types is not having any effect. I used it because the fourth column was returning 'text' when it is numeric, and the final column is returning numeric when it is made of text and so prints a whole bunch of warnings.

VascNew.thin<-as.data.frame(read_excel("KLIMAVEG_BIALOWIEZA_VASCULAR_2015.xls"))
VascOld.thin$Plot_number<-paste(VascOld.thin$Plot_number, "1992", sep="_")
#replace spaces with underscores in all column names
colnames(VascOld.thin)<-gsub(" ", "_",colnames(VascOld.thin))
#check that each species has one, and only one, frequency score
unique(VascOld.thin[,4:6])#checks that there are no values that are not 1, and no record with a 1 in more than one frequency column.


#At this stage I have three columns, Frequency_1 Frequency_2 and Frequency_3, all containing binary information. I want the new column frequency_score to have a 1 if there's a 1 in Frequency_1, a 2 if there's a 1 in Frequency_2, and a 3 if there's a 1 in Frequency_3. I could maybe make it work with three lines and subsetting (first line as below), but that seems clunky. More elegant solution?
#VascOld.thin$frequency_score[!is.na(VascOld.thin$Frequency_3),]<-(VascOld.thin$Frequency_3[!is.na(VascOld.thin$Frequency_3),])*3


library(tidyr)
#VascOld.fat<-spread(VascOld.thin[,c()], Species_name,frequency_score, fill=0) #see what worked in Patrick ferns

  
  