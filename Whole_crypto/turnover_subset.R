#this runs the same plots, then after that the summary analyses, as in turnover but without the squares which are either rectangles or have river boundaries
source("turnover.R")

#I checked, and the BC dist is not dependant on the whole matrix so it is ok to subset them. It would not be ok to subset ordination scores.

dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")
summaries.ss<-Summaries[!rownames(Summaries)%in%dodgysquares,]

x11();par(mfrow=c(3,3), xpd=NA)
mapply(function(x){hist (x, main=NULL, ylab=NULL, xlab=NULL)}, x= summaries.ss[,c("lich.BCdiss","lich.rich1992","lich.rich2015","bryo.BCdiss", "bryo.rich1992", "bryo.rich2015","vasc.BCdiss", "vasc.rich1992","vasc.rich2015")]) # OBS! column subsetting needs mapply.
text("Vegdist",x=-325, y=175, cex=1.4)
text("Richness in 1992",x=-125, y=175, cex=1.4)
text("Richness in 2015",x=75, y=175, cex=1.4)
text("Lichens",x=-415, y=150, cex=1.4, srt=90)
text("Bryophytes",x=-415, y=85, cex=1.4, srt=90)
text("Vascular plants",x=-415, y=15, cex=1.4, srt=90)
savePlot("Distributions of values_subset.emf", type="emf")

t.test(summaries.ss$lich.rich1992, summaries.ss$lich.rich2015, paired=TRUE)
t.test(summaries.ss$bryo.rich1992, summaries.ss$bryo.rich2015, paired=TRUE)
t.test(summaries.ss$vasc.rich1992, summaries.ss$vasc.rich2015, paired=TRUE)#this is very ns for unpaired data and very sig for paired data!

x11(7,3); par(mfrow=c(1,3), pin=c(1.6,1.6), mgp=c(1.8,0.5,0))
plot(summaries.ss$lich.extinct, summaries.ss$lich.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Lichens")
plot(summaries.ss$bryo.extinct, summaries.ss$bryo.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Bryophytes")
plot(summaries.ss$vasc.extinct, summaries.ss$vasc.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Vascular plants")
savePlot("Colonisations vs extinctions_subset.emf", type="emf")

x11()#this runs all the way to line 72
layout(matrix(c(1,4,7,10,13,2,5,8,11,14,3,6,9,12,15), 3, 5, byrow = TRUE))
par(mar=c(3,3,3,1), cex.axis=0.8, las=2, xpd=NA, mgp=c(2,0.5,0))
mapply(function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,150), ylab="Plot richness 1992", main=NA)
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=140, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=120, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
  text(x=3, y=-20, labels=colnames(x))
}, x=summaries.ss[,c("lich.rich1992","bryo.rich1992","vasc.rich1992")] ) #It would be great if I could get the P to display as stars or as >0.001 as well. And I should use an appropriate model, but poisson models with log links don't appear to give F or P values.

mapply(function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,150), ylab="Plot richness 2015")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=140, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=120, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}, x=summaries.ss[,c("lich.rich2015","bryo.rich2015","vasc.rich2015")]) 

mapply(function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,1), ylab="Proportion plot extinctions")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=1, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.9, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}, x=summaries.ss[,c("lich.extinct", "bryo.extinct","vasc.extinct")])

mapply(function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,1), ylab="Proportion plot colonisations")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=1, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.9, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}, x=summaries.ss[,c("lich.colonise","bryo.colonise","vasc.colonise")])

mapply(function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,0.4), ylab="Bray-Curtis distance")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=0.4, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.36, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}, x=summaries.ss[,c("lich.BCdiss","bryo.BCdiss","vasc.BCdiss")])

text(-22.5,2.05,"Lichens", cex=1.2)
text(-22.5,1.3,"Bryophytes", cex=1.2)
text(-22.5,0.5,"Vascular plants", cex=1.2)
savePlot("Turnover and richness by phytosoc_subset.emf", type="emf")

###for powerpoint###
x11(6,3);par(mfrow=c(1,3))
mapply(function(x, z, main, ylim){
  boxplot(x~dominant, data=summaries.ss, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="blue", las=2)
  boxplot(z~dominant, data=summaries.ss, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="yellow", xaxt="n", yaxt="n")}, 
  x = summaries.ss[,c("lich_threatened_1992","bryo_threatened_1992","vasc_threatened_1992")], z=summaries.ss[,c("lich_threatened_2015","bryo_threatened_2015","vasc_threatened_2015")] , main=c("Threatened lichens","Threatened bryophytes","Threatened vascular"), ylim=list(c(0,50),c(0,10),c(0,2)))#note, ylim can't be moved back
mapply(function(x, z, main, ylim){
  boxplot(x~dominant, data=summaries.ss, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="blue", las=2)
  boxplot(z~dominant, data=summaries.ss, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="yellow", xaxt="n", yaxt="n")}, 
  x = summaries.ss[,c("lich.rich1992","bryo.rich1992","vasc.rich1992")], z=summaries.ss[,c("lich.rich2015","bryo.rich2015","vasc.rich2015")] , main=c("Lichen richness","Bryophyte richness","Vascular plant richness"), ylim=list(c(0,150),c(0,150),c(0,150)))
legend("topright", fill=c("blue","yellow"), legend=c("1992","2015"), y.intersp=0.8)
savePlot("Richness by phytosoc for powerpoint.png", type="png")
x11(6,3);par(mfrow=c(1,3), las=2)
mapply(function(x,main){
  boxplot(x~dominant, data=summaries.ss, main=main)
  testy<-aov(x~dominant, data=summaries.ss)
  print(summary(testy))
  TukeyHSD(testy)
  }, 
  x = summaries.ss[,c("lich.colonise","bryo.colonise","vasc.colonise")],main=c("Lichen colonists","Bryophyte colonists","Vascular plant richness"))
savePlot("Colonists by community plain for ppt.png", type="png")

####ANALYSES and SUMMARIES####
#did richness go up?
t.test(summaries.ss$lich.rich1992, summaries.ss$lich.rich2015, paired=TRUE)
t.test(summaries.ss$bryo.rich1992, summaries.ss$bryo.rich2015, paired=TRUE)
t.test(summaries.ss$vasc.rich1992, summaries.ss$vasc.rich2015, paired=TRUE)

sapply(summaries.ss[1:21],FUN=mean )
sapply(summaries.ss[1:21],FUN=sd)

#a long way to get the richnesses etc for the table in the whole crypto paper
dim(comp_old[!rownames(comp_old)%in%dodgysquares,colSums(comp_old[!rownames(comp_old)%in%dodgysquares]>0)])
dim(comp_new[!rownames(comp_new)%in%dodgysquares,colSums(comp_new[!rownames(comp_new)%in%dodgysquares]>0)])
dim(easytabx[substr(rownames(easytabx),4,7)=="1992"&!substr(rownames(easytabx),1,3)%in%dodgysquares,colSums(easytabx[substr(rownames(easytabx),4,7)=="1992"&!substr(rownames(easytabx),1,3)%in%dodgysquares,]>0)])
dim(easytabx[substr(rownames(easytabx),4,7)=="2015"&!substr(rownames(easytabx),1,3)%in%dodgysquares,colSums(easytabx[substr(rownames(easytabx),4,7)=="2015"&!substr(rownames(easytabx),1,3)%in%dodgysquares,]>0)])
dim(vascOld.fat[!rownames(vascOld.fat)%in%dodgysquares,colSums(vascOld.fat[!rownames(vascOld.fat)%in%dodgysquares]>0)])
dim(vascNew.fat[!rownames(vascNew.fat)%in%dodgysquares,colSums(vascNew.fat[!rownames(vascNew.fat)%in%dodgysquares]>0)])

#is the number of communities a good explainer? Of anything?
mapply(function(x){
  tempy<-aov(x~ncomms, data=summaries.ss)
  summary(tempy)
  },x=summaries.ss[,1:59])

###observer effect?####
x11(8,3); par(mfrow=c(1,3), pin=c(2,2))
plot(summaries.ss$lich.colonise~summaries.ss$lich.BCdiss)
plot(summaries.ss$bryo.colonise~summaries.ss$bryo.BCdiss)
plot(summaries.ss$vasc.colonise~summaries.ss$vasc.BCdiss)

##idea for another day: plot number of new-to-whole-crypto lichen spp against BC dist of only the existing list.

####winners and losers####
losers<-as.data.frame(head(sort(colSums(vascall.df.ss[128:254,]>0)-colSums(vascall.df.ss[1:127,]>0))))#losers: by total squares lost
winners<-tail(sort(colSums(vascall.df.ss[128:254,]>0)-colSums(vascall.df.ss[1:127,]>0)))#winners: by total squares gained
write.csv2(as.data.frame(sort(colSums(vascall.df.ss[,(colSums(vascall.df.ss[128:254,])==0)&(colSums(vascall.df.ss[1:127,])>1)]>0))), file="vasc.extinction.csv")
write.csv2(as.data.frame(sort(colSums(vascall.df.ss[,(colSums(vascall.df.ss[128:254,])>0)&(colSums(vascall.df.ss[1:127,])==0)]>0), decreasing = TRUE)), file="vasc colonisation.csv")               

losers<-as.data.frame(head(sort(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",]>0)-colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="1992",]>0))))#losers: by total squares lost
winners<-tail(sort(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",]>0)-colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="1992",]>0)))#winners: by total squares gained
write.csv2(as.data.frame(sort(colSums(bryoall.df.ss[,(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",])==0)&(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="1992",])>1)]>0))), file="bryo.extinction.csv")
write.csv2(as.data.frame(sort(colSums(bryoall.df.ss[,(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",])>0)&(colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="1992",])==0)]>0), decreasing = TRUE)), file="bryo colonisation.csv")


losers<-as.data.frame(head(sort(colSums(lichall.df.ss[128:254,]>0)-colSums(lichall.df.ss[1:127,]>0))))#losers: by total squares lost
winners<-tail(sort(colSums(lichall.df.ss[128:254,]>0)-colSums(lichall.df.ss[1:127,]>0)))#winners: by total squares gained
write.csv2(as.data.frame(sort(colSums(lichall.df.ss[,(colSums(lichall.df.ss[128:254,])==0)&(colSums(lichall.df.ss[1:127,])>1)]>0))), file="lich.extinction.csv")
write.csv2(as.data.frame(sort(colSums(lichall.df.ss[,(colSums(lichall.df.ss[128:254,])>0)&(colSums(lichall.df.ss[1:127,])==0)]>0), decreasing = TRUE)), file="lich colonisation.csv")
