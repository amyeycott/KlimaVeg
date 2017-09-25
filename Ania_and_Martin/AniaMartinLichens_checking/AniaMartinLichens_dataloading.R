library(readxl)
library(tidyverse)

#load 2015 full data set
newdb<-read_excel("LICHENES CRYPTO - new data-2014-2015-final ver..xlsx")
newdb<-as.data.frame(newdb)
newdb[is.na(newdb)] <-0
#take spaces out of variable names
colnames(newdb)<-gsub(" ", "_", colnames(newdb))
newdb$Species<-as.factor(newdb$Species)
newdb$Site<-as.factor(newdb$Site)

#Trimmed is the data with only the species comparable between 1992 and 2015 in it. Same data prep as above.
trimmednewdb<-read_excel("LICHENES CRYPTO - new data-2014-2015-final ver..xlsx", sheet=3)
trimmednewdb<-as.data.frame(trimmednewdb)
trimmednewdb[is.na(trimmednewdb)] <-0
colnames(trimmednewdb)<-gsub(" ", "_", colnames(trimmednewdb))
trimmednewdb$Species<-as.factor(trimmednewdb$Species)
trimmednewdb$Site<-as.factor(trimmednewdb$Site)

#old is the 1992 sampling period
olddb<-read_excel("LICHENES CRYPTO - hist data-1987-1989-final ver..xlsx")
olddb<-as.data.frame(olddb)
olddb[is.na(olddb)] <-0
colnames(olddb)<-gsub(" ", "_", colnames(olddb))
olddb$Species<-as.factor(olddb$Species)
olddb$Site<-as.factor(olddb$Site)

#this is needed for checking LS vs hosts
S.in.LS<-c("14_foot_of_living_standing_trees", "16_bark_on_the_living_standing_trees","18_branches_of_living_trees","19_stems_and_branches_of_shrubs")
S.in.CWD<-c("6_mobile_wood_rests", "9_bark_on_tree_trunks_or_branches_of_fallen_trees)", "10_wood_of_tree_trunks_or_branches_of_fallen_trees)", "11_tree_small_stumps", "13_foot_of_dead_standing_trees", "15_bark_on_the_dead_standing_trees", "17_wood_of_dead_standing_trees")
S.in.inorganic<-c("1_stones", "2_concrete_","3_peat", "4_mineral_soil_-_humus_horizon","5_mineral_soil_-_lower_horizon","12_root_system_of_fallen_spruces", "25_permanent_or_periodical_water_basins")
S.in.fineorganic<-c("7_broadleaved_or_mixed_litter","8_conifer_litter","20_shoots,_leaves_of_living_plants", "21_a_last_year_herb_shoots","22_excrements", "23_dead_animal_rests","24_other_substrata")

#all the frequencies in one column
olddb$Frequency<-olddb$`1_rare` +  olddb$`2_frequent` * 2 + olddb$`3_common` *3

#olddb$Frequency<-olddb$`1_rare`
#olddb$Frequency[(olddb$`2_frequent`)==1]<-2
#olddb$Frequency[(olddb$`3_common`)==1]<-3
newdb$Frequency<-newdb$`1_rare`
newdb$Frequency[(newdb$`2_frequent`)==1]<-2
newdb$Frequency[(newdb$`3_common`)==1]<-3
trimmednewdb$Frequency<-trimmednewdb$`1_rare`
trimmednewdb$Frequency[(trimmednewdb$`2_frequent`)==1]<-2
trimmednewdb$Frequency[(trimmednewdb$`3_common`)==1]<-3

#a combined datafile so we can do a giant ordination

comp_old<-as.data.frame(unclass(xtabs(Frequency~Site+Species, data=olddb)))
comp_old$Year<-1992
comp_trimnew<-as.data.frame(unclass(xtabs(Frequency~Site+Species, data=trimmednewdb)))
comp_trimnew$Year<-2015
comp<-as.data.frame(merge(t(comp_old), t(comp_trimnew), by=0, all=TRUE)) # Merge in rioja package is neater
rownames(comp)<-comp$Row.names
comp[is.na(comp)]<-0
compz<-as.data.frame(comp[,-1])#compz is for summarising to genus
comp<-as.data.frame(t(comp[,-1]))

#summarise the whole dataset by genus
temp<-strsplit(rownames(compz), " ")
compz$genus <- sapply(temp, "[[", 1) #extract first element
#genus<-vector(mode = "integer", length = 0)
#for (i in 1:length(temp)){genus<-append(genus, temp[[i]][1], after=length(genus))}
#compz$genus<-genus
head(compz[,c(1:4, length(compz))])
genus_comp<-aggregate(compz[,1:length(compz)-1], by=list(compz$genus), FUN=max)
rownames(genus_comp)<-genus_comp$Group.1
genus_comp<-as.data.frame(t(genus_comp[-1]))
