library(readxl)
library(plyr)

#load 2015 full data set
newdb<-read_excel("LICHENES CRYPTO - new data-2014-2015-final ver..xlsx")
newdb<-as.data.frame(newdb)
newdb[is.na(newdb)] <-0
#take spaces out of variable names
colnames(newdb)<-gsub(" ", "_", colnames(newdb))
newdb$Species<-as.factor(newdb$Species)
newdb$Site<-as.factor(newdb$Site)

#needs Site harmonising with the other groups (P01 not P1). How do I make it pick up P1 and make P01, but not make P10 into P010?
#gsub([:alpha:][:digit] AND NOTHING ELSE, [:alpha:]0[:digit], newdb$Site)
#Note to myself: but this affects other lines. So far line 45 in this file, I checked the other files and I don't think there is anything but after the change they should be re-run to look for error messages.


#new.harm. is the data with only the species comparable between 1992 and 2015 in it. Same data prep as above.
new.harm.db<-read_excel("LICHENES CRYPTO - new data-2014-2015-final ver..xlsx", sheet=3)
new.harm.db<-as.data.frame(new.harm.db)
new.harm.db[is.na(new.harm.db)] <-0
colnames(new.harm.db)<-gsub(" ", "_", colnames(new.harm.db))
new.harm.db$Species<-as.factor(new.harm.db$Species)
new.harm.db$Site<-as.factor(new.harm.db$Site)

#old is the 1992 sampling period
olddb<-read_excel("LICHENES CRYPTO - hist data-1987-1989-final ver..xlsx", sheet=1)
olddb<-as.data.frame(olddb)
olddb[is.na(olddb)] <-0
colnames(olddb)<-gsub(" ", "_", colnames(olddb))
olddb$Species<-as.factor(olddb$Species)
olddb$Site<-as.factor(olddb$Site)

#old.harm. is the 1992 sampling period with only the species comparable between 1992 and 2015 in it. 
old.harm.db<-read_excel("../Ania_and_Martin/LICHENES CRYPTO - hist data-1987-1989-final ver..xlsx", sheet=2)# need some filepath so that this still runs when used as 'source' in other folders
old.harm.db<-as.data.frame(old.harm.db)
old.harm.db[is.na(old.harm.db)] <-0
colnames(old.harm.db)<-gsub(" ", "_", colnames(old.harm.db))
old.harm.db$Species<-as.factor(old.harm.db$Species)
old.harm.db$Site<-as.factor(old.harm.db$Site)


#this is needed for checking LS vs hosts
S.in.LS<-c("14_foot_of_living_standing_trees", "16_bark_on_the_living_standing_trees","18_branches_of_living_trees","19_stems_and_branches_of_shrubs")
S.in.CWD<-c("6_mobile_wood_rests", "9_bark_on_tree_trunks_or_branches_of_fallen_trees)", "10_wood_of_tree_trunks_or_branches_of_fallen_trees)", "11_tree_small_stumps", "13_foot_of_dead_standing_trees", "15_bark_on_the_dead_standing_trees", "17_wood_of_dead_standing_trees")
S.in.inorganic<-c("1_stones", "2_concrete_","3_peat", "4_mineral_soil_-_humus_horizon","5_mineral_soil_-_lower_horizon","12_root_system_of_fallen_spruces", "25_permanent_or_periodical_water_basins")
S.in.fineorganic<-c("7_broadleaved_or_mixed_litter","8_conifer_litter","20_shoots,_leaves_of_living_plants", "21_a_last_year_herb_shoots","22_excrements", "23_dead_animal_rests","24_other_substrata")

#Peucedanum list
PP<- c("B1","C1","C2","D1","D2","E1","E2","F1","F2","G1", "G2","G3","G4","H1","H2","H3","H4","H5","I1","I2","I3","I4","I5","K1","K2","K3","K4","K5","L1","L2","L3","M1","M2","M3")

#environmental indicator value data, plus some other traity stuff
envir<-read_excel("LICHENES CRYPTO - new data-2014-2015-final ver..xlsx", sheet=4)

#trim trailing spaces in species names. Don't do it for the whole file, that is very very crashy
envir$Species<-trimws(envir$Species, which = "both")
olddb$Species<-trimws(olddb$Species, which = "both")
old.harm.db$Species<-trimws(old.harm.db$Species, which = "both")
newdb$Species<-trimws(newdb$Species, which = "both")
new.harm.db$Species<-trimws(new.harm.db$Species, which = "both")

#Wirth's values (like Ellenberg) for each year's ocurrences
old.wirths<-merge(old.harm.db[1:2], envir, by="Species", all=TRUE)
new.wirths<-merge(new.harm.db[1:2], envir, by="Species", all=TRUE)

#all the frequencies in one column
olddb$Frequency<-olddb$`1_rare` +  olddb$`2_frequent` * 2 + olddb$`3_common` *3
old.harm.db$Frequency<-old.harm.db$`1_rare` +  old.harm.db$`2_frequent` * 2 + old.harm.db$`3_common` *3
newdb$Frequency<-newdb$`1_rare` +  newdb$`2_frequent` * 2 + newdb$`3_common` *3
new.harm.db$Frequency<-new.harm.db$`1_rare` +  new.harm.db$`2_frequent` * 2 + new.harm.db$`3_common` *3

#a combined datafile so we can do a giant ordination. From here on we use harmonised data all the time unless stated

comp_old<-as.data.frame(unclass(xtabs(Frequency~Site+Species, data=old.harm.db)))
comp_old$Year<-1992
comp_new<-as.data.frame(unclass(xtabs(Frequency~Site+Species, data=new.harm.db)))
comp_new$Year<-2015
comp<-as.data.frame(merge(t(comp_old), t(comp_new), by=0, all=TRUE)) # Merge in rioja package is neater, join in analogue is nice and easy to understand.
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



