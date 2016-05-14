source("AniaMartinLichens_dataloading.R")#this runs the same code as in that file

#not updated to include old.harm.db

###NEW###
#checking that one (and only one) frequency column has a 1 in it - should return the word "false"
unique(!rowSums(newdb[3:5], na.rm=TRUE)==1)

#checking that all numeric entries have sensible values. It works ok on rows which are all zeros. So far, not-working rows are 9,12,15. It's something about colwise, because unique(newdb[15]) works fine, but "colwise(unique)(newdb[,13:15])" returns "Error: length(rows) == 1 is not TRUE".
colwise(unique)(newdb[,3:(length(newdb)-1)])
#but this works nicely
for(i in 1:length(newdb)) {print(unique(newdb[i]))}
for(i in 1:length(newdb)) {print(dim(newdb[i]))}#is a neat db

#Has everything got a substrate? Not everything has a community or a host though.
newdb[rowSums(newdb[12:36])==0,1:4]

#this is lines where there is a host but no S in LS. Checked with Ania, these are most likely on deadwood
newdb[(apply(newdb[,which(names(newdb)%in%c(S.in.LS, S.in.CWD))],1,max))-(apply(newdb[,37:52],1,max))==-1,c(1:2,37:52)]
write.table(newdb[(apply(newdb[,which(names(newdb)%in%c(S.in.LS, S.in.CWD))],1,max))-(apply(newdb[,37:52],1,max))==-1,c(1:2, which(names(newdb)%in%S.in.LS),37:52)], "Newdata_epiphyte with no substrate.txt", sep="\t", row.names=TRUE)
#this is lines where there is an S in LS but no host
newdb[(apply(newdb[,which(names(newdb)%in%c(S.in.LS))],1,max))-(apply(newdb[,37:52],1,max))==1,c(1:2,which(names(newdb)%in%S.in.LS), 37:52)]#none there now :-)
length(unique(newdb$Site))#144

### NEW Harmonised
#checking that one (and only one) frequency column has a 1 in it - should return the word "false"
unique(!rowSums(new.harm.db[3:5], na.rm=TRUE)==1)
#checking that all numeric entries have sensible values. It works ok on rows which are all zeros. 
for(i in 1:length(new.harm.db)) {print(unique(new.harm.db[i]))}
for(i in 1:length(new.harm.db)) {print(dim(new.harm.db[i]))}#is a neat db

#Has everything got a substrate? Not everything has a community or a host though.
new.harm.db[rowSums(new.harm.db[12:36])==0,1:4]#yes

#this is lines where there is a host but no S in LS. Checked with Ania, these are most likely on deadwood
new.harm.db[(apply(new.harm.db[,which(names(new.harm.db)%in%c(S.in.LS, S.in.CWD))],1,max))-(apply(new.harm.db[,37:52],1,max))==-1,c(1:2,37:52)]
write.table(new.harm.db[(apply(new.harm.db[,which(names(new.harm.db)%in%c(S.in.LS, S.in.CWD))],1,max))-(apply(new.harm.db[,37:52],1,max))==-1,c(1:2, which(names(new.harm.db)%in%S.in.LS),37:52)], "Newdata_epiphyte with no substrate.txt", sep="\t", row.names=TRUE)
#this is lines where there is an S in LS but no host
new.harm.db[(apply(new.harm.db[,which(names(new.harm.db)%in%c(S.in.LS))],1,max))-(apply(new.harm.db[,37:52],1,max))==1,c(1:2,which(names(new.harm.db)%in%S.in.LS), 37:52)]#none there now :-)
length(unique(new.harm.db$Site))#144

###OLD###
#checking that one (and only one) frequency column has a 1 in it - should return the word "false"
unique(!rowSums(olddb[3:5], na.rm=TRUE)==1)
#checking everything has a substrate
olddb[rowSums(olddb[12:36])==0,1:4]#yes it does
#checking that all numeric entries have sensible values. It works ok on rows which are all zeros. So far, not-working rows are 9,12,15. It's something about colwise, because unique(olddb[15]) works fine, but "colwise(unique)(olddb[,13:15])" returns "Error: length(rows) == 1 is not TRUE".
colwise(unique)(olddb[,3:(length(olddb)-1)])
#but this works nicely
for(i in 1:length(olddb)) {print(unique(olddb[i]))}#no crazy values, no misspellings :-)
for(i in 1:length(olddb)) {print(dim(olddb[i]))}#is a neat db

olddb[(apply(olddb[,which(names(olddb)%in%S.in.LS)],1,max))-(apply(olddb[,37:52],1,max))==-1,c(1:2,which(names(olddb)%in%S.in.LS),37:52)]
write.table(olddb[(apply(olddb[,which(names(olddb)%in%S.in.LS)],1,max))-(apply(olddb[,37:52],1,max))==-1,c(1:2, which(names(olddb)%in%S.in.LS),37:52)], "olddata_epiphyte with no substrate.txt", sep="\t", row.names=TRUE)
#this is lines where there is an S in LS but no host (note, ignore the line of the stray 2)
olddb[(apply(olddb[,which(names(olddb)%in%S.in.LS)],1,max))-(apply(olddb[,37:52],1,max))==1,c(1:2,which(names(olddb)%in%S.in.LS), 37:52)]#none now :-)

#are all the plots in?
length(unique(olddb$Site))#144

#do species match, and are they in the environment table?
alldb <- rbind(cbind(year = 1992, olddb), cbind(year = 2015, new.harm.db))
setdiff(alldb$Species, envir$Species)#missing from envir
setdiff(envir$Species, alldb$Species)#extras in envir
setdiff(hastrent,alldb$Species)

setdiff(olddb$Species, new.harm.db$Species)
setdiff(new.harm.db$Species, olddb$Species)
