#this file is for loading the data for Red Bog, Biebrza, written by Amy Eycott in early 2016.
#Be aware that it uses a lot of column indexing ("magic numbers") which will break the code if the data structure changes.
#So far: data fixing is lines 36-95 for duplicate rows, and 95 onwards for rows with NA or silly things in layer.

library(plyr)

read.tvexport <- function(file, time){
  sheet1 <- read.table(file, sep=";",  na = "", header=TRUE, stringsAsFactors = FALSE)
  plants <- sheet1[30:nrow(sheet1),-ncol(sheet1)]
  env <- sheet1[1:28,-ncol(sheet1)]#29 is a blank row
  rownames(env) <- env$Table.number
  names(env) <- paste("t", time,names(env), sep = "")
  names(plants)[-(1:2)] <- names(env)[-(1:2)]  
  env <- as.data.frame(t(env[, 3:length(env)]), stringsAsFactors = FALSE)
  list(plants = plants, env = env)
}

t1 <- read.tvexport("redbog/data/TVEXPORT_1.csv", time = 1)
t2 <- read.tvexport("redbog/data/TVEXPORT_2.csv", time = 2)
t3 <- read.tvexport("redbog/data/TVEXPORT_3.csv", time = 3)

###env data###
env <- rbind(t1$env, t2$env, t3$env)
names(env) <- c("Relevee.number", "Field.number", "Archive.number","Year", "Month", "Day", "Relevee.area", "total.cover.percent", "tree.cover.percent", "shrub.cover.percent","herb.cover.percent", "moss.cover.percent", "lichen.cover.percent", "algae.cover.percent", "litter.cover.percent", "water.cover.percent", "rock.cover.percent","height.tallest.trees", "height.shortest.trees", "height.tallest.shrubs", "height.shortest.shrubs", "avheight.tallest.herbs", "avheight.shortest.herbs", "max.height.herb", "max.height.crypto", "Long", "Lat", "n.spp")
env[, 7:ncol(env)] <- lapply(env[, 7:ncol(env)], function(x) as.numeric(x))
env[, -2] <- lapply(env[, -2], function(x) as.numeric(x))
env$Lat[env$Lat > 55] <- env$Lat[env$Lat > 55] - 2#correct one latitude of 55.65
env$Long[env$Long == 0] <- NA
env$Lat[env$Lat == 0] <- NA
env$decade<-factor(floor(env$Year/10)*10)


###plant data
merge.duplicates <- function(plants){
  #find duplicate pairs
  dup <- table(plants$Table.number, plants$X)
  dups <- which(dup>1, arr.ind = TRUE, useNames = TRUE)
  dup.pairs <- apply(dups, 1, function(r){
    sp <- rownames(dup)[r[1]]
    layer <- colnames(dup)[r[2]]
    list(sp = sp, layer = layer,  rownum = which(plants$Table.number == sp & plants$X == layer))
  })
  dup.pairs <- alply(dups, 1, function(r){
    sp <- rownames(dup)[r[1]]
    layer <- colnames(dup)[r[2]]
    list(sp = sp, layer = layer,  rownum = which(plants$Table.number == sp & plants$X == layer))
  })
  print(dup.pairs)
  #check for overlap & merge
  merged<-ldply(dup.pairs, function(p){
    original <- plants[p$rownum, ]
    test<-original[, -(1:2)]
    if(any(test[1,]!="."&test[2,]!=".")){
      stop(paste("overlap in ", p$sp, p$layer, p$rownum))
    }
    merged <- original[1, ]
    merged[1, original[2, ] != "."] <- original[2, original[2, ] != "."]
    merged
  })
  #delete duplicated and rbind merged  
  todelete <- unlist(lapply(dup.pairs, "[[", "rownum"))
  plants <- rbind(plants[-todelete,], merged[,-1])
  rownames(plants)<-paste(plants$Table.number, plants$X)
  plants
}  
  
plants1 <- merge.duplicates(t1$plants)
plants2 <- merge.duplicates(t2$plants)
plants3 <- merge.duplicates(t3$plants)

#Here is where we need to rename things in unexpected layers or NA layers. It has to happen after the duplicate removal in order to select on rows not row indexes 
#unique(sheet1plants[is.na(sheet1plants$t1X),])

#sheet1plants[sheet1plants$t1Table.number=="Calliergon giganteum",]
#sheet1plants[rownames(sheet1plants)=="Calliergon giganteum ml",sheet1plants[rownames(sheet1plants)=="Calliergon giganteum NA",]!="."]<-sheet1plants[rownames(sheet1plants)=="Calliergon giganteum NA",sheet1plants[rownames(sheet1plants)=="Calliergon giganteum NA",]!="."]#broken. But also, this replaces the ml entry in t1X with NA...

#sheet1plants[sheet1plants$t1Table.number=="Rorippa  amphibia",]
#sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia hl",sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia NA",]!="."]<-sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia NA",sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia NA",]!="."]
#sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia hl",sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia s2",]!="."]<-sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia s2",sheet1plants[rownames(sheet1plants)=="Rorippa  amphibia s2",]!="."]

#toDrop.t1<-c("Calliergon giganteum NA", "Rorippa  amphibia NA", "227", "282", "225")
#sheet1plants<-sheet1plants[!(rownames(sheet1plants) %in% toDrop.t1), ] 


plantsall <- merge(plants1, plants2, by = c("Table.number", "X"), all = TRUE)
plantsall <- merge(plantsall, plants3, by = c("Table.number", "X"), all = TRUE)

layers<-plantsall[, c("Table.number", "X")]
layers[is.na(layers$X),]
colSums(table(layers$Table.number, layers$X))
sort(rowSums(table(layers$Table.number, layers$X)))

lapply(list(plants1, plants2, plants3), function(p)p[is.na(p$X), ])

#take out unecessary columns before converting to numeric
plantsall$Table.number <- NULL
plantsall$X <- NULL

#convert to full numeric before the merging by species step...
sort(unique(unlist(plantsall)))
plantspc<-sapply(plantsall, function(cn){
  cn<-factor(cn, levels=c(".",  "r", "+",  "1",  "2",  "2a", "2b", "2m", "3",  "4",  "5"))
  c(0, 0.6, 1.2, 2.5, 5, 10, 10, 20, 40, 80, 160)[cn]
})
plantspc[is.na(plantspc)]<-0
rbind(plantsall[1,], plantspc[1,])#check

#merge different layers
mergedplants <- as.data.frame(t(aggregate(plantspc, by = list(layers$Table.number), FUN = sum) [,-1]))
names(mergedplants) <- unique(layers$Table.number)

#cleanup
rm(t1, t2, t3, plants1, plants2, plants3, plantsall)
