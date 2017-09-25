source("AniaMartinLichens_dataloading.R")#this runs the same code as in that file

###NEW###
#checking that one (and only one) frequency column has a 1 in it - should return the word "false"
unique(!rowSums(new.harm.db[3:5], na.rm=TRUE)==1)

#checking that all numeric entries have sensible values. It works ok on rows which are all zeros. So far, not-working rows are 9,12,15. It's something about colwise, because unique(new.harm.db[15]) works fine, but "colwise(unique)(new.harm.db[,13:15])" returns "Error: length(rows) == 1 is not TRUE".
colwise(unique)(new.harm.db[,3:(length(new.harm.db)-1)])
#but this works nicely
for(i in 1:length(new.harm.db)) {print(unique(new.harm.db[i]))}
for(i in 1:length(new.harm.db)) {print(dim(new.harm.db[i]))}#is a neat db

#Has everything got a substrate? Not everything has a community or a host though.
new.harm.db[rowSums(new.harm.db[12:36])==0,1:4]

#this is lines where there is a host but no S in LS. Checked with Ania, these are most likely on deadwood
new.harm.db[(apply(new.harm.db[,which(names(new.harm.db)%in%c(S.in.LS, S.in.CWD))],1,max))-(apply(new.harm.db[,37:52],1,max))==-1,c(1:2,37:52)]
write.table(new.harm.db[(apply(new.harm.db[,which(names(new.harm.db)%in%c(S.in.LS, S.in.CWD))],1,max))-(apply(new.harm.db[,37:52],1,max))==-1,c(1:2, which(names(new.harm.db)%in%S.in.LS),37:52)], "Newdata_epiphyte with no substrate.txt", sep="\t", row.names=TRUE)
#this is lines where there is an S in LS but no host
new.harm.db[(apply(new.harm.db[,which(names(new.harm.db)%in%c(S.in.LS))],1,max))-(apply(new.harm.db[,37:52],1,max))==1,c(1:2,which(names(new.harm.db)%in%S.in.LS), 37:52)]#none there now :-)
length(unique(new.harm.db$Site))#144

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
unique(!rowSums(old.harm.db[3:5], na.rm=TRUE)==1)
#checking everything has a substrate
old.harm.db[rowSums(old.harm.db[12:36])==0,1:4]#yes it does
#checking that all numeric entries have sensible values. 
for(i in 1:length(old.harm.db)) {print(unique(old.harm.db[i]))}#no crazy values, no misspellings :-)
for(i in 1:length(old.harm.db)) {print(dim(old.harm.db[i]))}#is a neat db

old.harm.db[(apply(old.harm.db[,which(names(old.harm.db)%in%S.in.LS)],1,max))-(apply(old.harm.db[,37:52],1,max))==-1,c(1:2,which(names(old.harm.db)%in%S.in.LS),37:52)]
write.table(old.harm.db[(apply(old.harm.db[,which(names(old.harm.db)%in%S.in.LS)],1,max))-(apply(old.harm.db[,37:52],1,max))==-1,c(1:2, which(names(old.harm.db)%in%S.in.LS),37:52)], "olddata_epiphyte with no substrate.txt", sep="\t", row.names=TRUE)
#this is lines where there is an S in LS but no host (note, ignore the line of the stray 2)
old.harm.db[(apply(old.harm.db[,which(names(old.harm.db)%in%S.in.LS)],1,max))-(apply(old.harm.db[,37:52],1,max))==1,c(1:2,which(names(old.harm.db)%in%S.in.LS), 37:52)]#none now :-)

#are all the plots in?
length(unique(old.harm.db$Site))#144

#do species match, and are they in the environment table?
all.harm.db <- rbind(cbind(year = 1990, old.harm.db), cbind(year = 2015, new.harm.db))
setdiff(all.harm.db$Species, envir$Species)#missing from envir
setdiff(envir$Species, all.harm.db$Species)#extras in envir
setdiff(hastrent,all.harm.db$Species)

setdiff(old.harm.db$Species, new.harm.db$Species)
setdiff(new.harm.db$Species, old.harm.db$Species)
