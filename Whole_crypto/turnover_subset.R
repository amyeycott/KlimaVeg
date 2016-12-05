#this runs the same plots (not all analyses) as turnover but without the squares which are either rectangles or have river boundaries
source("turnover.R")

dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")
summaries.ss<-Summaries[!rownames(Summaries)%in%dodgysquares,]


x11();par(mfrow=c(3,3), xpd=NA)
sapply(summaries.ss[,c(1,2,3,6,7,8,11,12,13)], function(x){hist (x, main=NULL, ylab=NULL, xlab=NULL)}) #Bad magic numbers because of new columns
text("Vegdist",x=-450, y=175, cex=1.4)
text("Richness in 1992",x=-150, y=175, cex=1.4)
text("Richness in 2015",x=100, y=175, cex=1.4)
text("Lichens",x=-550, y=150, cex=1.4, srt=90)
text("Bryophytes",x=-550, y=85, cex=1.4, srt=90)
text("Vascular plants",x=-550, y=15, cex=1.4, srt=90)
savePlot("Distributions of values_subset.emf", type="emf")

t.test(summaries.ss$lich.rich1992, summaries.ss$lich.rich2015, paired=TRUE)
t.test(summaries.ss$bryo.rich1992, summaries.ss$bryo.rich2015, paired=TRUE)
t.test(summaries.ss$vasc.rich1992, summaries.ss$vasc.rich2015, paired=TRUE)#this is very ns for unpaired data and very sig for paired data!

x11(); par(mfrow=c(1,3), pin=c(1.6,1.6), mgp=c(1.8,0.5,0))
plot(summaries.ss$lich.extinct, summaries.ss$lich.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Lichens")
plot(summaries.ss$bryo.extinct, summaries.ss$bryo.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Bryophytes")
plot(summaries.ss$vasc.extinct, summaries.ss$vasc.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Vascular plants")
savePlot("Colonisations vs extinctions_subset.emf", type="emf")

x11()
layout(matrix(c(1,4,7,10,13,2,5,8,11,14,3,6,9,12,15), 3, 5, byrow = TRUE))
par(mar=c(3,3,3,1), cex.axis=0.8, las=2, xpd=NA, mgp=c(2,0.5,0))
sapply(summaries.ss[,c(2,7,12)], function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,200), ylab="Plot richness 1992", main=colnames(x))
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=200, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=180, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
  text(x=3, y=-20, labels=colnames(x))
}) #The titles aren't working with either of the two methods included here. It would be great if I could get the P to display as stars or as >0.001 as well. And I should use an appropriate model, but poisson models with log links don't appear to give F or P values.
sapply(summaries.ss[,c(3,8,13)], function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,200), ylab="Plot richness 2015")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=200, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=180, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}) 
sapply(summaries.ss[,c(4,9,14)], function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,1), ylab="Proportion plot extinctions")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=1, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.9, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}) 
sapply(summaries.ss[,c(5,10,15)], function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,1), ylab="Proportion plot colonisations")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=1, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.9, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}) 
sapply(summaries.ss[,c(1,6,11)], function (x) { 
  boxplot(x~dominant, data=summaries.ss, col=2:8, ylim=c(0,0.4), ylab="Bray-Curtis distance")
  model.x<-aov(x~dominant, data=summaries.ss)
  text(x=4, y=0.4, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.36, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
})
text(-17.5,1.8,"Lichens", cex=1.2)
text(-17.5,1.15,"Bryophytes", cex=1.2)
text(-17.5,0.5,"Vascular plants", cex=1.2)
savePlot("Turnover and richness by phytosoc_subset.emf", type="emf")