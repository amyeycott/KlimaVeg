#this runs the same plots, then after that the summary analyses, as in turnover but without the squares which are either rectangles or have river boundaries
source("turnover.R")

#I checked, and the BC dist is not dependant on the whole matrix so it is ok to subset them. It would not be ok to subset ordination scores.

dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")

summaries.ss<-Summaries[!rownames(Summaries)%in%dodgysquares,]


####ANALYSES and SUMMARIES####
#did richness go up?
t.test(summaries.ss$lich.rich1990, summaries.ss$lich.rich2015, paired=TRUE)
t.test(summaries.ss$bryo.rich1990, summaries.ss$bryo.rich2015, paired=TRUE)
t.test(summaries.ss$vasc.rich1990, summaries.ss$vasc.rich2015, paired=TRUE)

sapply(summaries.ss[,1:length(summaries.ss)-1],FUN=mean )
sapply(summaries.ss[1:length(summaries.ss)-1],FUN=sd)

#a long way to get the richnesses etc for the table in the whole crypto paper
dim(comp_old[!rownames(comp_old)%in%dodgysquares,colSums(comp_old[!rownames(comp_old)%in%dodgysquares]>0)])
dim(comp_new[!rownames(comp_new)%in%dodgysquares,colSums(comp_new[!rownames(comp_new)%in%dodgysquares]>0)])
dim(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,colSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,]>0)])
dim(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,colSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,]>0)])
dim(vascOld.fat[!rownames(vascOld.fat)%in%dodgysquares,colSums(vascOld.fat[!rownames(vascOld.fat)%in%dodgysquares]>0)])
dim(vascNew.fat[!rownames(vascNew.fat)%in%dodgysquares,colSums(vascNew.fat[!rownames(vascNew.fat)%in%dodgysquares]>0)])

#is the number of communities a good explainer? Of anything?
mapply(function(x){
  tempy<-aov(x~ncomms, data=summaries.ss)
  summary(tempy)
  },x=summaries.ss[,1:59])

#idea for another day: plot number of new-to-whole-crypto lichen spp against BC dist of only the existing list.

####winners and losers####
losers<-as.data.frame(head(sort(colSums(vascall.df.ss[128:254,]>0)-colSums(vascall.df.ss[1:127,]>0))))#losers: by total squares lost FIX THESE MAGIC NUMBERS
winners<-tail(sort(colSums(vascall.df.ss[128:254,]>0)-colSums(vascall.df.ss[1:127,]>0)))#winners: by total squares gained
write.csv2(as.data.frame(sort(colSums(vascall.df.ss[,(colSums(vascall.df.ss[128:254,])==0)&(colSums(vascall.df.ss[1:127,])>1)]>0))), file="vasc.extinction.csv")#July 2017: 6 extinctions, of
write.csv2(as.data.frame(sort(colSums(vascall.df.ss[,(colSums(vascall.df.ss[128:254,])>0)&(colSums(vascall.df.ss[1:127,])==0)]>0), decreasing = TRUE)), file="vasc colonisation.csv")#115 colonisations              
#having difficulty making sure we have the right turnover, it seems higher than before?


losers<-as.data.frame(head(sort(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",]>0)-colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="1990",]>0))))#losers: by total squares lost
winners<-tail(sort(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",]>0)-colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="1990",]>0)))#winners: by total squares gained
write.csv2(as.data.frame(sort(colSums(bryoall.df.ss[,(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",])==0)&(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="1990",])>1)]>0))), file="bryo.extinction.csv")
write.csv2(as.data.frame(sort(colSums(bryoall.df.ss[,(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",])>0)&(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="1990",])==0)]>0), decreasing = TRUE)), file="bryo colonisation.csv")


losers<-as.data.frame(head(sort(colSums(lichall.df.ss[128:254,]>0)-colSums(lichall.df.ss[1:127,]>0))))#losers: by total squares lost
winners<-tail(sort(colSums(lichall.df.ss[128:254,]>0)-colSums(lichall.df.ss[1:127,]>0)))#winners: by total squares gained
write.csv2(as.data.frame(sort(colSums(lichall.df.ss[,(colSums(lichall.df.ss[128:254,])==0)&(colSums(lichall.df.ss[1:127,])>1)]>0))), file="lich.extinction.csv")
write.csv2(as.data.frame(sort(colSums(lichall.df.ss[,(colSums(lichall.df.ss[128:254,])>0)&(colSums(lichall.df.ss[1:127,])==0)]>0), decreasing = TRUE)), file="lich colonisation.csv")

#post-hoc tests for richness vs community:
write.table(c(0,0), file="Posthoc of community rich etc.csv")
mapply(function(x, name.x){model.x<-aov(x~dominant, data=summaries.ss)
write.table(name.x, file="Posthoc of community rich etc.csv",append=TRUE)
write.table(round(TukeyHSD(model.x)[[1]], digits=4), file="Posthoc of community rich etc.csv", append=TRUE, sep=";")},x=summaries.ss[,c("lich.rich1990","lich.rich2015","lich.extinct","lich.colonise","lich.BCdiss", "bryo.rich1990", "bryo.rich2015","bryo.extinct","bryo.colonise","bryo.BCdiss", "vasc.rich1990","vasc.rich2015","vasc.extinct","vasc.colonise","vasc.BCdiss")], name.x=c("lich.rich1990","lich.rich2015","lich.extinct","lich.colonise","lich.BCdiss", "bryo.rich1990", "bryo.rich2015","bryo.extinct","bryo.colonise","bryo.BCdiss", "vasc.rich1990","vasc.rich2015","vasc.extinct","vasc.colonise","vasc.BCdiss")) 

##who got lost from the PP and where did they go?
dim(vascall.df.ss)
colSums(vascall.df.ss[substr(rownames(vascall.df.ss),1,3)%in%rownames(summaries.ss[summaries.ss$dominant=="PP",]),])#how to get the PP plots in a reliable but convoluted manner. trouble is that filter removed rownames (frickin hadley) so I have to use indexing.
rownames(summaries.ss)
