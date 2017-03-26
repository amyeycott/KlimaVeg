#this runs the same plots, then after that the summary analyses, as in turnover but without the squares which are either rectangles or have river boundaries
source("turnover.R")

#I checked, and the BC dist is not dependant on the whole matrix so it is ok to subset them. It would not be ok to subset ordination scores.

dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")
summaries.ss<-Summaries[!rownames(Summaries)%in%dodgysquares,]

x11();par(mfrow=c(3,3), xpd=NA)
mapply(function(x){hist (x, main=NULL, ylab=NULL, xlab=NULL)}, x= summaries.ss[,c("lich.BCdiss","lich.rich1990","lich.rich2015","bryo.BCdiss", "bryo.rich1990", "bryo.rich2015","vasc.BCdiss", "vasc.rich1990","vasc.rich2015")]) # OBS! column subsetting needs mapply.
text("Vegdist",x=-325, y=175, cex=1.4)
text("Richness in 1990",x=-125, y=175, cex=1.4)
text("Richness in 2015",x=75, y=175, cex=1.4)
text("Lichens",x=-415, y=150, cex=1.4, srt=90)
text("Bryophytes",x=-415, y=85, cex=1.4, srt=90)
text("Vascular plants",x=-415, y=15, cex=1.4, srt=90)
savePlot("Distributions of values_subset.emf", type="emf")

x11(7,3.5); par(mfrow=c(1,3), pin=c(1.6,1.6), mgp=c(1.8,0.5,0), xpd=NA)
plot(summaries.ss$lich.extinct, summaries.ss$lich.colonise, xlab="Proportion plot extinctions 1990-2015", ylab="Proportion plot colonisations 1990-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Lichens",col= coloury$Colour_bolder[as.factor(summaries.ss$dominant)], lwd=1.5, cex=0.8)# the colour only works while the factor for community is ordered the same way for both. Should be fixed, same as magic numbers (which I have written here to find the faults)
plot(summaries.ss$bryo.extinct, summaries.ss$bryo.colonise, xlab="Proportion plot extinctions 1990-2015", ylab="Proportion plot colonisations 1990-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Bryophytes",col= coloury$Colour_bolder[as.factor(summaries.ss$dominant)], lwd=1.5, cex=0.8)
legend(x=-1, y=-0.3, legend=coloury$Community_in_1990,fill=coloury$Colour_bolder, ncol=3)
plot(summaries.ss$vasc.extinct, summaries.ss$vasc.colonise, xlab="Proportion plot extinctions 1990-2015", ylab="Proportion plot colonisations 1990-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Vascular plants", col= coloury$Colour_bolder[as.factor(summaries.ss$dominant)], lwd=1.5, cex=0.8)
savePlot("Colonisations vs extinctions_subset.png", type="png")
savePlot("Colonisations vs extinctions_subset.emf", type="emf")

x11(7,8)#this runs all the way to line 69. Is currently Figure 4 in Whole Crypto ms
layout(matrix(c(1,4,7,10,13,2,5,8,11,14,3,6,9,12,15), 3, 5, byrow = TRUE))
par(mar=c(2,2.5,2,0.5), cex.axis=0.8, las=2, xpd=NA, mgp=c(1.5,0.3,0), tcl=-0.2)
mapply(function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,150), ylab="Plot richness 1990", main=NA)
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=150, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)#magic numbers extract the right bits of the aov objects.
  text(x=4, y=135, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
  text(x=3, y=-20, labels=colnames(x))
}, x=summaries.ss[,c("lich.rich1990","bryo.rich1990","vasc.rich1990")] ) #It would be great if I could get the P to display as stars or as >0.001 as well. And I should use an appropriate model, but poisson models with log links don't appear to give F or P values.

mapply(function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,150), ylab="Plot richness 2015")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=150, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=135, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
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
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,1), ylab="Bray-Curtis distance")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=1, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.9, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}, x=summaries.ss[,c("lich.BCdiss","bryo.BCdiss","vasc.BCdiss")])

text(-20,3.9,"Lichens", cex=1.2)
text(-20,2.5,"Bryophytes", cex=1.2)
text(-20,1.1,"Vascular plants", cex=1.2)
savePlot("Turnover and richness by phytosoc_subset.emf", type="emf")
savePlot("Turnover and richness by phytosoc_subset.pdf", type="pdf")
savePlot("Turnover and richness by phytosoc_subset.png", type="png")

###for powerpoint###
x11(6,3);par(mfrow=c(1,3))
mapply(function(x, z, main, ylim){
  boxplot(x~dominant, data=summaries.ss, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="blue", las=2)
  boxplot(z~dominant, data=summaries.ss, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="yellow", xaxt="n", yaxt="n")}, 
  x = summaries.ss[,c("lich_threatened_1990","bryo_threatened_1990","vasc_threatened_1990")], z=summaries.ss[,c("lich_threatened_2015","bryo_threatened_2015","vasc_threatened_2015")] , main=c("Threatened lichens","Threatened bryophytes","Threatened vascular"), ylim=list(c(0,50),c(0,10),c(0,2)))#note, ylim can't be moved back
mapply(function(x, z, main, ylim){
  boxplot(x~dominant, data=summaries.ss, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="blue", las=2)
  boxplot(z~dominant, data=summaries.ss, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="yellow", xaxt="n", yaxt="n")}, 
  x = summaries.ss[,c("lich.rich1990","bryo.rich1990","vasc.rich1990")], z=summaries.ss[,c("lich.rich2015","bryo.rich2015","vasc.rich2015")] , main=c("Lichen richness","Bryophyte richness","Vascular plant richness"), ylim=list(c(0,150),c(0,150),c(0,150)))
legend("topright", fill=c("blue","yellow"), legend=c("1990","2015"), y.intersp=0.8)
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
t.test(summaries.ss$lich.rich1990, summaries.ss$lich.rich2015, paired=TRUE)
t.test(summaries.ss$bryo.rich1990, summaries.ss$bryo.rich2015, paired=TRUE)
t.test(summaries.ss$vasc.rich1990, summaries.ss$vasc.rich2015, paired=TRUE)

sapply(summaries.ss[,1:length(summaries.ss)-1],FUN=mean )
sapply(summaries.ss[1:length(summaries.ss)-1],FUN=sd)

#a long way to get the richnesses etc for the table in the whole crypto paper
dim(comp_old[!rownames(comp_old)%in%dodgysquares,colSums(comp_old[!rownames(comp_old)%in%dodgysquares]>0)])
dim(comp_new[!rownames(comp_new)%in%dodgysquares,colSums(comp_new[!rownames(comp_new)%in%dodgysquares]>0)])
dim(bryo.fat[substr(rownames(bryo.fat),4,7)=="1992"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,colSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="1992"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,]>0)])
dim(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,colSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,]>0)])
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

#post-hoc tests: cannot coerce class "c("TukeyHSD", "multicomp")" to a data.frame write.table(x="The table of post hoc tests for figure 4", file="Posthoc of community rich etc.csv")
write.table(c(0,0), file="Posthoc of community rich etc.csv")
mapply(function(x, name.x){model.x<-aov(x~dominant, data=summaries.ss)
write.table(name.x, file="Posthoc of community rich etc.csv",append=TRUE)
write.table(round(TukeyHSD(model.x)[[1]], digits=4), file="Posthoc of community rich etc.csv", append=TRUE, sep=";")},x=summaries.ss[,c("lich.rich1990","lich.rich2015","lich.extinct","lich.colonise","lich.BCdiss", "bryo.rich1990", "bryo.rich2015","bryo.extinct","bryo.colonise","bryo.BCdiss", "vasc.rich1990","vasc.rich2015","vasc.extinct","vasc.colonise","vasc.BCdiss")], name.x=c("lich.rich1990","lich.rich2015","lich.extinct","lich.colonise","lich.BCdiss", "bryo.rich1990", "bryo.rich2015","bryo.extinct","bryo.colonise","bryo.BCdiss", "vasc.rich1990","vasc.rich2015","vasc.extinct","vasc.colonise","vasc.BCdiss"))#works for 'print' but not for 'write table' (cannot coerce class "c("TukeyHSD", "multicomp")" to a data.frame write.table(x="The table of post hoc tests for figure 4", file="Posthoc of community rich etc.csv"))

TukeyHSD(aov(vasc.BCdiss~dominant, data=summaries.ss))[[1]]

