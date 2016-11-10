#This project is to analyse all taxonomic groups in the crypto-klimaveg dataset. It was written by Amy Eycott starting 7th April 2016.
#Before running any scripts in this project, check that the latest version of each database, and all supporting data sheets (e.g.ellenberg values) has been copied into the same project folder as this project, and that the folder is called "Whole crypto" with a capital W.

source("../Ania and Martin/AniaMartinLichens_dataloading.R")#the ../ is 'go up one level', doing it this way means that it finds the right directory whether it is on C, O, or another person's computer.
source("../sylwia/Bryophyte data loading.R")#in an ideal world, work out which step is printing a bunch of structure...
library(readxl)
VascOld.thin<-as.data.frame(read_excel("KLIMAVEG_BIALOWIEZA_VASCULAR_OLD_Corr.xls"), col_types=c(rep("text",3), rep("numeric", 48), "text"))
VascNew.thin<-as.data.frame(read_excel("KLIMAVEG_BIALOWIEZA_VASCULAR_2015.xls"))
VascOld.thin$Plot_number<-paste(VascOld.thin$Plot_number, "1992", sep="_")
#replace spaces with underscores in all column names
colnames(VascOld.thin)<-gsub(" ", "_",colnames(VascOld.thin))
#check that each species has one, and only one, frequency score
unique(VascOld.thin[4:6])
rowSums(VascOld.thin[4:6])
VascOld.thin$frequency_score<-


library(tidyr)


VascOld.fat<-spread(VascOld.thin, Species_name, )

  
  spread(ferns.thindf[,c(1,2,4,5)], SPECIES, FREQUENCE, fill=0)