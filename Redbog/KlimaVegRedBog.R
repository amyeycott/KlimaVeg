#this file is for loading the data for Red Bog, Biebrza, written by Amy Eycott in early 2016.
#Be aware that it uses a lot of column indexing ("magic numbers") which will break the code if the data structure changes.
#So far: data fixing is lines 36-95 for duplicate rows, and 95 onwards for rows with NA or silly things in layer.

setwd("\\\\helix.klient.uib.no\\biohome\\aey022\\rdata\\KlimaVeg\\")
#library(readxl)
#sheet1plants<-read_excel("TVEXPORT_1.xlsx", sheet = 1, col_names = FALSE, col_types = NULL,  na = "", skip = 30) #this returns an odd data fromat. It looks like a df but each column has both a factor and an attribute part. Askinf what class it is returns three items: tbl.df, tbl and data.frame. But trying to extract one element returns one row.
sheet1<-read.table("TVEXPORT_1.csv", sep=";",  na = "", header=TRUE)
sheet1plants<-sheet1[30:nrow(sheet1),1:length(sheet1)-1]
sheet1env<-sheet1[1:28,1:length(sheet1)-1]#29 is a blank row
names(sheet1env)<-paste("t1",names(sheet1env), sep="")
names(sheet1plants)<-names(sheet1env)

sheet2<-read.table("TVEXPORT_2.csv", sep=";",  na = "", header=TRUE)
sheet2plants<-sheet2[30:nrow(sheet2),1:length(sheet2)-1]
sheet2env<-sheet2[1:28,1:length(sheet2)-1]
names(sheet2env)<-paste("t2",names(sheet2env), sep="")
names(sheet2plants)<-names(sheet2env)

sheet3<-read.table("TVEXPORT_3.csv", sep=";",  na = "", header=TRUE)
sheet3plants<-sheet3[30:nrow(sheet3),1:length(sheet3)-1]
sheet3env<-sheet3[1:28,1:length(sheet3)-1]
names(sheet3env)<-paste("t3",names(sheet3env), sep="")
names(sheet3plants)<-names(sheet3env)

###env data###
rownames(sheet1env)<-sheet1env$t1Table.number
rownames(sheet2env)<-sheet2env$t2Table.number
rownames(sheet3env)<-sheet3env$t3Table.number
t1env<-as.data.frame(t(sheet1env[3:length(sheet1env)]))
t2env<-as.data.frame(t(sheet2env[3:length(sheet2env)]))
t3env<-as.data.frame(t(sheet3env[3:length(sheet3env)]))
envall<-rbind(t1env, t2env, t3env)
names(envall)<-c("Relevee.number","Field.number","Archive.number","Year","Month","Day","Relevee.area","total.cover.percent","tree.cover.percent","shrub.cover.percent","herb.cover.percent","moss.cover.percent","lichen.cover.percent","algae.cover.percent","litter.cover.percent","water.cover.percent","rock.cover.percent","height.tallest.trees","height.shortest.trees","height.tallest.shrubs","height.shortest.shrubs","avheight.tallest.herbs","avheight.shortest.herbs", "max.height.herb","max.height.crypto","Long","Lat","n.spp")
envall[7:length(envall)] <- lapply(envall[7:length(envall)], function(x) as.numeric(as.character(x)))


###Species data: finding and removing duplicate rows#####
sheet1plants[duplicated(paste(sheet1plants$Table.number, sheet1plants$X))==TRUE,]#there are repeats. ‘Aneura pinguis ml’, ‘Dicranum polysetum ml’, ‘Plagiomnium rostratum ml’, ‘Salix rosmarinifolia hl’,  Bogdan says that they should be merged with their duplicates. Sphagnum warnstorfii ml shows up as a repeat later... why???

sheet1plants[paste(sheet1plants$Table.number, sheet1plants$X)=="Aneura pinguis ml",]#rownames 194 and 321, column name 225 has something in row 321
sheet1plants[paste(sheet1plants$Table.number, sheet1plants$X)=="Aneura pinguis ml",225]
sheet1plants[rownames(sheet1plants)=="194",sheet1plants[rownames(sheet1plants)=="225",]!="."]<-sheet1plants[rownames(sheet1plants)=="225",sheet1plants[rownames(sheet1plants)=="225",]!="."]

sheet1plants[paste(sheet1plants$Table.number, sheet1plants$X)=="Dicranum polysetum ml",]#there are things in rows called 157 and 277. There are never things in both rows in one column.
sheet1plants[rownames(sheet1plants)=="157",sheet1plants[rownames(sheet1plants)=="277",]!="."]<-sheet1plants[rownames(sheet1plants)=="277",sheet1plants[rownames(sheet1plants)=="277",]!="."]

sheet1plants[paste(sheet1plants$Table.number, sheet1plants$X)=="Plagiomnium rostratum ml",]#there are things in rows called 121 and 227. There are never things in both rows in one column.
sheet1plants[rownames(sheet1plants)=="121",sheet1plants[rownames(sheet1plants)=="227",]!="."]<-sheet1plants[rownames(sheet1plants)=="227",sheet1plants[rownames(sheet1plants)=="227",]!="."]


sheet1plants[paste(sheet1plants$Table.number, sheet1plants$X)=="Salix rosmarinifolia hl",]#there are things in rows called 61 and 282. There are never things in both rows in one column.
sheet1plants[rownames(sheet1plants)=="61",sheet1plants[rownames(sheet1plants)=="282",]!="."]<-sheet1plants[rownames(sheet1plants)=="282",sheet1plants[rownames(sheet1plants)=="282",]!="."]

sheet1plants[paste(sheet1plants$Table.number, sheet1plants$X)=="Sphagnum warnstorfii ml",]#there are things in rows called 194 and 225. There are never things in both rows in one column.
sheet1plants[rownames(sheet1plants)=="194",sheet1plants[rownames(sheet1plants)=="225",]!="."]<-sheet1plants[rownames(sheet1plants)=="225",sheet1plants[rownames(sheet1plants)=="225",]!="."]

toDrop.t1<-c("321", "277", "227", "282", "225")
sheet1plants<-sheet1plants[!(rownames(sheet1plants) %in% toDrop.t1), ] 


rownames(sheet1plants)<-paste(sheet1plants$t1Table.number, sheet1plants$t1X)#if this throws a duplicated results error, I've missed a dupliacted row! 
head(sheet1plants[1:12])

sheet2plants[duplicated(paste(sheet2plants$t2Table.number, sheet2plants$t2X))==TRUE,]#252  Cephaloziella divaricata  ml, Calypogeia azurea  ml,  Campyliadelphus stellatus  ml, Cephalozia lunulifolia  ml, Dicranum polysetum  ml
sheet2plants[paste(sheet2plants$t2Table.number, sheet2plants$t2X)=="Cephaloziella divaricata ml",]#there are things in rows called 156 and 252. There are never things in both rows in one column.
sheet2plants[rownames(sheet2plants)=="156",sheet2plants[rownames(sheet2plants)=="252",]!="."]<-sheet2plants[rownames(sheet2plants)=="252",sheet2plants[rownames(sheet2plants)=="252",]!="."]
sheet2plants[paste(sheet2plants$t2Table.number, sheet2plants$t2X)=="Calypogeia azurea ml",]#there are things in rows called 272 and 339. There are never things in both rows in one column.
sheet2plants[rownames(sheet2plants)=="272",sheet2plants[rownames(sheet2plants)=="339",]!="."]<-sheet2plants[rownames(sheet2plants)=="339",sheet2plants[rownames(sheet2plants)=="339",]!="."]
sheet2plants[paste(sheet2plants$t2Table.number, sheet2plants$t2X)=="Campyliadelphus stellatus ml",]#there are things in rows called 47 and 356. There are never things in both rows in one column.
sheet2plants[rownames(sheet2plants)=="47",sheet2plants[rownames(sheet2plants)=="356",]!="."]<-sheet2plants[rownames(sheet2plants)=="356",sheet2plants[rownames(sheet2plants)=="356",]!="."]
sheet2plants[paste(sheet2plants$t2Table.number, sheet2plants$t2X)=="Cephalozia lunulifolia ml",]#there are things in rows called 352 and 376. There are never things in both rows in one column.
sheet2plants[rownames(sheet2plants)=="352",sheet2plants[rownames(sheet2plants)=="376",]!="."]<-sheet2plants[rownames(sheet2plants)=="376",sheet2plants[rownames(sheet2plants)=="376",]!="."]
sheet2plants[paste(sheet2plants$t2Table.number, sheet2plants$t2X)=="Dicranum polysetum ml",]#there are things in rows called 133 and 379. There are never things in both rows in one column.
sheet2plants[rownames(sheet2plants)=="133",sheet2plants[rownames(sheet2plants)=="379",]!="."]<-sheet2plants[rownames(sheet2plants)=="379",sheet2plants[rownames(sheet2plants)=="379",]!="."]

toDrop.t2<-c("252", "339", "356", "376", "379")
sheet2plants<-sheet2plants[!(rownames(sheet2plants) %in% toDrop.t2), ] 
rownames(sheet2plants)<-paste(sheet2plants$t2Table.number, sheet2plants$t2X)

sheet3plants[duplicated(paste(sheet3plants$t3Table.number, sheet3plants$t3X))==TRUE,]#Dicranum polysetum  ml , Bidens cernua  hl, Plagiomnium rostratum  ml, Plagiomnium medium  ml  
sheet3plants[paste(sheet3plants$t3Table.number, sheet3plants$t3X)=="Dicranum polysetum ml",]#there are things in rows called 66 and 252. There are never things in both rows in one column.
sheet3plants[rownames(sheet3plants)=="66",sheet3plants[rownames(sheet3plants)=="252",]!="."]<-sheet3plants[rownames(sheet3plants)=="252",sheet3plants[rownames(sheet3plants)=="252",]!="."]
sheet3plants[paste(sheet3plants$t3Table.number, sheet3plants$t3X)=="Bidens cernua hl",]#there are things in rows called 223 and 264. There are never things in both rows in one column.
sheet3plants[rownames(sheet3plants)=="223",sheet3plants[rownames(sheet3plants)=="264",]!="."]<-sheet3plants[rownames(sheet3plants)=="264",sheet3plants[rownames(sheet3plants)=="264",]!="."]
sheet3plants[paste(sheet3plants$t3Table.number, sheet3plants$t3X)=="Plagiomnium rostratum ml",]#there are things in rows called 67 and 275. There are never things in both rows in one column.
sheet3plants[rownames(sheet3plants)=="67",sheet3plants[rownames(sheet3plants)=="275",]!="."]<-sheet3plants[rownames(sheet3plants)=="275",sheet3plants[rownames(sheet3plants)=="275",]!="."]
sheet3plants[paste(sheet3plants$t3Table.number, sheet3plants$t3X)=="Plagiomnium medium ml",]#there are things in rows called 181 and 307. There are never things in both rows in one column.
sheet3plants[rownames(sheet3plants)=="181",sheet3plants[rownames(sheet3plants)=="307",]!="."]<-sheet3plants[rownames(sheet3plants)=="307",sheet3plants[rownames(sheet3plants)=="307",]!="."]

toDrop.t3<-c("252", "264", "275", "307")
sheet3plants<-sheet3plants[!(rownames(sheet3plants) %in% toDrop.t3), ]
rownames(sheet3plants)<-paste(sheet3plants$t3Table.number, sheet3plants$t3X)

#Here is where we need to rename things in unexpected layers or NA layers. It has to happen after the duplicate removal in order to select on rows not row indexes 
#unique(sheet1plants[is.na(sheet1plants$t1X),])

#sheet1plants[sheet1plants$t1Table.number=="Calliergon giganteum",]
#sheet1plants[rownames(sheet1plants)=="Calliergon giganteum ml",sheet1plants[rownames(sheet1plants)=="Calliergon giganteum NA",]!="."]<-sheet1plants[rownames(sheet1plants)=="Calliergon giganteum NA",sheet1plants[rownames(sheet1plants)=="Calliergon giganteum NA",]!="."]#broken. But also, this replaces the ml entry in t1X with NA...

#sheet1plants[sheet1plants$t1Table.number=="Rorippa  amphibia",]
#sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia hl",sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia NA",]!="."]<-sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia NA",sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia NA",]!="."]
#sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia hl",sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia s2",]!="."]<-sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia s2",sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia s2",]!="."]

#toDrop.t1<-c("Calliergon giganteum NA", "Rorippa  amphibia NA", "227", "282", "225")
#sheet1plants<-sheet1plants[!(rownames(sheet1plants) %in% toDrop.t1), ] 


fullset.plants<-merge(sheet1plants,sheet2plants,by=0, all=TRUE)
rownames(fullset.plants)<-fullset.plants$Row.names
fullset.plants<-merge(fullset.plants, sheet3plants, by=0, all=TRUE)
rownames(fullset.plants)<-fullset.plants$Row.names

names(fullset.plants)
layersnames<-c("t1Table.number", "t1X","t2Table.number", "t2X","t3Table.number", "t3X")
layers<-fullset.plants[layersnames]

#I need to put the species names into layers.
levels(layers$t1X)<-unique(c(levels(layers$t1X), levels(layers$t2X), levels(layers$t3X)))
levels(layers$t1Table.number)<-unique(c(levels(layers$t1Table.number), levels(layers$t2Table.number), levels(layers$t3Table.number)))
layers$t1X[is.na(layers$t1X)]<-layers$t2X[is.na(layers$t1X)]
layers$t1X[is.na(layers$t1X)]<-layers$t3X[is.na(layers$t1X)]
layers$t1Table.number[is.na(layers$t1Table.number)]<-layers$t2Table.number[is.na(layers$t1Table.number)]
layers$t1Table.number[is.na(layers$t1Table.number)]<-layers$t3Table.number[is.na(layers$t1Table.number)]
layers[is.na(layers$t1X),]
layers$t1Table.number<-droplevels(layers$t1Table.number)


#Alnus glutinosa NA          <NA> <NA> <NA>
 # Betula pendula NA           <NA> <NA> <NA>
  #Calliergon giganteum NA     <NA> <NA> <NA>
  #Deschampsia cespitosa NA    <NA> <NA> <NA>
  #Eriophorum angustifolium NA <NA> <NA> <NA>
  #Picea abies NA              <NA> <NA> <NA>
  #Rorippa  amphibia NA        <NA> <NA> <NA>
  #Sphagnum squarrosum NA      <NA> <NA> <NA>
  #Viscum album NA

layers[is.na(layers$t1Table.number),]#is fine, phew

gotNA<-layers$t1Table.number[is.na(layers$t1X)]
fullset.plants[layers$t1Table.number%in%gotNA,layersnames]#This way round is for finding out which table the NAs are from. but if it works it can be used with minus, to take out the not-needed columns
write.table((fullset.plants[layers$t1Table.number%in%gotNA,layersnames]), file="problems.tab", sep="\t")

sheet3[is.na(sheet3$X), 1:3]###got to here

#take out unecessary columns before converting to numeric
fullset.plants<-fullset.plants[!names(fullset.plants)%in%c(layersnames, "Row.names","Row.names.1")]

#convert to full numeric before the merging by species step...
lookup.table<-cbind(sort(unique(fullset.plants[!is.na(fullset.plants)])), c(0,1.2,2.5,10,5,10,20,40,80,160,0.6))#ref Maarel 2007 J Veg Sci


percent.plants<-fullset.plants
for(j in 1:length(percent.plants)){
  levels(percent.plants[,j])<-list("0"="0", "0"=".","1.2"="+","0.6"="r","2.5"="1","10"="2","10"="2a", "20"="2b", "5"="2m","40"="3","80"="4","160"="5")
  percent.plants[,j]<-as.numeric(as.character(percent.plants[,j]))
}
percent.plants[is.na(percent.plants)]<-0
percent.plants[,43]#just to check there are no NAs left

str(percent.plants)

mergedplants<-as.data.frame(t(aggregate(percent.plants, by=list(layers$t1Table.number), FUN=sum)[,2:length(percent.plants)]))
names(mergedplants)<-unique(layers$t1Table.number)
