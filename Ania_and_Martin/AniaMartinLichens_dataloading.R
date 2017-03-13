library(readxl)
library(plyr)

#load 2015 full data set
newdb<-read_excel("../Ania_and_Martin/LICHENES CRYPTO - new data-2014-2015-final ver..xlsx")
newdb<-as.data.frame(newdb)
newdb[is.na(newdb)] <-0
#take spaces out of variable names
colnames(newdb)<-gsub(" ", "_", colnames(newdb))
newdb$Site[nchar(newdb$Site)==2]<-paste0(substr(newdb$Site[nchar(newdb$Site)==2], 1,1),"0", substr(newdb$Site[nchar(newdb$Site)==2], 2,2))#makes format for Site match that used elsewhere (i.e. F02 not F2). Note: ONLY RUN ONCE each time the read_excel is run. This affects other lines: line 49 in this file is fixed.

#database corrections (do before factor generation):
newdb$Species[newdb$Species=="Lecanora argentata"]<-"Lecanora argentata s.l."
newdb$Species[newdb$Species=="Pertusaria ophtalmiza"]<-"Pertusaria ophthalmiza"
newdb$Species[newdb$Species=="Lecanora sarcopidioides"]<-"Lecanora sarcopidoides"
newdb$Species[newdb$Species=="Lecanatis abietina"]<-"Lecanactis abietina"
newdb$Species[newdb$Species=="Phlyctia agelaea"]<-"Phlyctis agelaea"
newdb$Species<-as.factor(newdb$Species)
newdb$Site<-as.factor(newdb$Site)

#new.harm. is the data with only the species comparable between 1992 and 2015 in it. Same data prep as above.
new.harm.db<-read_excel("../Ania_and_Martin/LICHENES CRYPTO - new data-2014-2015-final ver..xlsx", sheet=3)
new.harm.db<-as.data.frame(new.harm.db)
new.harm.db[is.na(new.harm.db)] <-0
colnames(new.harm.db)<-gsub(" ", "_", colnames(new.harm.db))
new.harm.db$Site[nchar(new.harm.db$Site)==2]<-paste0(substr(new.harm.db$Site[nchar(new.harm.db$Site)==2], 1,1),"0", substr(new.harm.db$Site[nchar(new.harm.db$Site)==2], 2,2))#makes format for Site match that elsewhere (i.e. F02 not F2).

#database corrections (do before factor generation):
new.harm.db$Species[new.harm.db$Species=="Lecanora argentata"]<-"Lecanora argentata s.l."
new.harm.db$Species[new.harm.db$Species=="Pertusaria ophtalmiza"]<-"Pertusaria ophthalmiza"
new.harm.db$Species[new.harm.db$Species=="Lecanora sarcopidioides"]<-"Lecanora sarcopidoides"
new.harm.db$Species[new.harm.db$Species=="Lecanatis abietina"]<-"Lecanactis abietina"
new.harm.db$Species[new.harm.db$Species=="Phlyctia agelaea"]<-"Phlyctis agelaea"

new.harm.db$Species<-as.factor(new.harm.db$Species)
new.harm.db$Site<-as.factor(new.harm.db$Site)

#old is the 1992 sampling period
olddb<-read_excel("../Ania_and_Martin/LICHENES CRYPTO - hist data-1987-1989-final ver..xlsx", sheet=1)
olddb<-as.data.frame(olddb)
olddb[is.na(olddb)] <-0
colnames(olddb)<-gsub(" ", "_", colnames(olddb))
olddb$Site[nchar(olddb$Site)==2]<-paste0(substr(olddb$Site[nchar(olddb$Site)==2], 1,1),"0", substr(olddb$Site[nchar(olddb$Site)==2], 2,2))#makes format for Site match that elsewhere (i.e. F02 not F2).

#database corrections (do before factor generation):
olddb$Species[olddb$Species=="Cladonia arbuscula subsp. Beringiana"]<-"Cladonia arbuscula subsp. beringiana"
olddb$Species[olddb$Species=="Cladonia arbuscula subsp. Mitis"]<-"Cladonia arbuscula subsp. mitis"
olddb$Species[olddb$Species=="Cladonia furcata subsp. Furcata"]<-"Cladonia furcata subsp. furcata"

olddb$Species<-as.factor(olddb$Species)
olddb$Site<-as.factor(olddb$Site)

#old.harm. is the 1992 sampling period with only the species comparable between 1992 and 2015 in it. 
old.harm.db<-read_excel("../Ania_and_Martin/LICHENES CRYPTO - hist data-1987-1989-final ver..xlsx", sheet=2)# need some filepath so that this still runs when used as 'source' in other folders
old.harm.db<-as.data.frame(old.harm.db)
old.harm.db[is.na(old.harm.db)] <-0
colnames(old.harm.db)<-gsub(" ", "_", colnames(old.harm.db))
old.harm.db$Site[nchar(old.harm.db$Site)==2]<-paste0(substr(old.harm.db$Site[nchar(old.harm.db$Site)==2], 1,1),"0", substr(old.harm.db$Site[nchar(old.harm.db$Site)==2], 2,2))#makes format for Site match that elsewhere (i.e. F02 not F2).

#database corrections (do before factor generation):
old.harm.db$Species[old.harm.db$Species=="Cladonia arbuscula subsp. Beringiana"]<-"Cladonia arbuscula subsp. beringiana"
old.harm.db$Species[old.harm.db$Species=="Cladonia arbuscula subsp. Mitis"]<-"Cladonia arbuscula subsp. mitis"
old.harm.db$Species[old.harm.db$Species=="Cladonia furcata subsp. Furcata"]<-"Cladonia furcata subsp. furcata"

old.harm.db$Species<-as.factor(old.harm.db$Species)
old.harm.db$Site<-as.factor(old.harm.db$Site)


#this is needed for checking LS vs hosts
S.in.LS<-c("14_foot_of_living_standing_trees", "16_bark_on_the_living_standing_trees","18_branches_of_living_trees","19_stems_and_branches_of_shrubs")
S.in.CWD<-c("6_mobile_wood_rests", "9_bark_on_tree_trunks_or_branches_of_fallen_trees)", "10_wood_of_tree_trunks_or_branches_of_fallen_trees)", "11_tree_small_stumps", "13_foot_of_dead_standing_trees", "15_bark_on_the_dead_standing_trees", "17_wood_of_dead_standing_trees")
S.in.inorganic<-c("1_stones", "2_concrete_","3_peat", "4_mineral_soil_-_humus_horizon","5_mineral_soil_-_lower_horizon","12_root_system_of_fallen_spruces", "25_permanent_or_periodical_water_basins")
S.in.fineorganic<-c("7_broadleaved_or_mixed_litter","8_conifer_litter","20_shoots,_leaves_of_living_plants", "21_a_last_year_herb_shoots","22_excrements", "23_dead_animal_rests","24_other_substrata")

#Peucedanum list
PP<- c("B01","C01","C02","D01","D02","E01","E02","F01","F02","G01", "G02","G03","G04","H01","H02","H03","H04","H05","I01","I02","I03","I04","I05","K01","K02","K03","K04","K05","L01","L02","L03","M01","M02","M03")

#environmental indicator value data, plus some other traity stuff
envir<-read_excel("../Ania_and_Martin/LICHENES CRYPTO - new data-2014-2015-final ver..xlsx", sheet=4)
envir$Species<-trimws(envir$Species, which = "both")
#needs an NA line rmeoved maybe.envir<-
#corrections to species names
envir$Species[envir$Species=="Lecanora argentata"]<-"Lecanora argentata s.l."


#trim trailing spaces in species names. Don't do it for the whole file, that is very very crashy
envir$Species<-trimws(envir$Species, which = "both")
olddb$Species<-trimws(olddb$Species, which = "both")
old.harm.db$Species<-trimws(old.harm.db$Species, which = "both")
newdb$Species<-trimws(newdb$Species, which = "both")
new.harm.db$Species<-trimws(new.harm.db$Species, which = "both")

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

####CONSERVATION STATUS####
lich.protect<-read_excel("../Ania_and_Martin/Kopia Protected Red coded species.xlsx", sheet=2)
names(lich.protect)<-c("Species_name","Is_protected","Protected_status","Is_Redlisted","Redlist_status")
lich.protect<-lich.protect[!is.na(lich.protect$Species_name),]#list is 100 species long, contains only species which are NT-upwards on the redlist.
lich.protect$Is_Redlisted[is.na(lich.protect$Is_Redlisted)]<-0
lich.protect$Is_protected[is.na(lich.protect$Is_protected)]<-0#these are needed because the NAs crash a later step

#Wirth's values (like Ellenberg) for each year's ocurrences. There are some species in there not in the main dataset for some reason.
old.wirths<-merge(old.harm.db[c("Species","Site","Frequency")], envir, by="Species", all.x=TRUE, all.y=FALSE)
new.wirths<-merge(new.harm.db[c("Species","Site","Frequency")], envir, by="Species",, all.x=TRUE, all.y=FALSE)
