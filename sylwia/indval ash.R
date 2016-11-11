#indval - are there ash specialists?
library(readxl)
bryophytes<-read_excel("Baza Styczen 2016.xls", sheet=1,col_names = TRUE) 
bryophytes<-as.data.frame(bryophytes)
bryophytes[is.na(bryophytes)] <-0 
bryophytes<-bryophytes[!(rowSums(bryophytes[,5:length(bryophytes)]))==0,]
colnames(bryophytes)<-gsub(" ", "_", colnames(bryophytes))
str(bryophytes)
library(indicspecies)
library(reshape)
library(dplyr)#download is crashy
epiphytest<-bryophytes[rowSums(bryophytes[,61:74])>0,c(1:3,61:74)]
epiphytes<-melt(epiphytest, id.vars = c("Year","Plot","Species_name"), measure.vars=c(4:length(epiphytest)))
forindval<-cast(epiphytes,  ... ~ Species_name)
forindval[is.na(forindval)]<-0
urk<-multipatt(forindval[,4:length(forindval)], forindval$variable,  duleg=TRUE)
summary(urk, alpha=(1-(1-0.05)^(1/length(unique(epiphytest$Species_name))))) #of 87 significant specialists, only three are Fe specialists Plagiochila asplenioides 0.129   0.005 **, Cirriphyllum piliferum   0.116   0.005 **,   Mnium hornum             0.108   0.005 **, Bonferroni kills all of them.

#habtest<-transmute(bryophytes, inorganics=rowSums(bryophytes[which(names(bryophytes)%in%S.in.inorganic)]))
habtest<-bryophytes[,1:3]
habtest$inorganic<-rowSums(bryophytes[,which(names(bryophytes)%in%S.in.inorganic)])
habtest$fineorganic<-rowSums(bryophytes[,which(names(bryophytes)%in%S.in.fineorganic)])
habtest$CWD<-rowSums(bryophytes[,which(names(bryophytes)%in%S.in.CWD)])
habtest$LS<-rowSums(bryophytes[,which(names(bryophytes)%in%S.in.LS)])

habitats<-melt(habtest, id.vars = c("Year","Plot","Species_name"), measure.vars=c(4:length(habtest)))
forindvalh<-cast(habitats,  ... ~ Species_name)
forindvalh[is.na(forindvalh)]<-0
urkh<-multipatt(forindvalh[,4:length(forindvalh)], forindvalh$variable,  duleg=TRUE)
summary(urkh, alpha=(1-(1-0.05)^(1/length(unique(habtest$Species_name))))) #Mnium hornum is an LS specialist but would probably fall out for multiple testing, Cirriphyllum  piliferum and plagiochila splenioides are CWD specialist. 123 species are specialists by broad habitat. Bonferroni kills the lot.

CWDonly<-bryophytes[rowSums(bryophytes[,49:60])>0,c(1:3,49:60)]
cwds<-melt(CWDonly, id.vars = c("Year","Plot","Species_name"), measure.vars=c(4:length(CWDonly)))
forindvalCWD<-cast(cwds,  ... ~ Species_name)
forindvalCWD[is.na(forindvalCWD)]<-0
urkCWD<-multipatt(forindvalCWD[,4:length(forindvalCWD)], forindvalCWD$variable,  duleg=TRUE)
summary(urkCWD, alpha=(1-(1-0.05)^(1/length(unique(CWDonly$Species_name))))) #CWD has 23 specialists, but I need to check which of these are actually specialists to other habs (14 are LS specialists, 1 is not specialist, and eight are CWD specialists). Should also drop some for multiple testing. Bonferroni/Sidak kills the lot. Holm-Bonferroni tests the lowest P value against the bonferroni correction then drops the k for each successive higher p value, but if the lowest p value fails, presumably so do all the others.

#betadiv turnover 
