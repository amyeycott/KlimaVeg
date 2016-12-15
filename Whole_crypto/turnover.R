source("../Whole_crypto/source data and merging.R")
#turnover. First work out the dissimilaritiy scores. Any score that vegdist makes can be added in by changing the first line for each section - for example, options for binary dissimilarity indices. E.g. if you do Bray Curtis, binary= TRUE you get sørensen DISsimilarity.

####lichens###
BCdist.lichens<-as.matrix(vegdist(comp, method="bray"))
Sodiss.lichens<-as.matrix(vegdist(comp, method="bray", binary="TRUE"))
summ.lichens<-as.data.frame(cbind(0,0))#you have to set something up for the loop to feed into.
  for (i in 1:144)
  {summ.lichens[i,1]<-(BCdist.lichens[i, i+144])
    summ.lichens[i,2]<-(Sodiss.lichens[i, i+144])}
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
BCdist.lichens[1:5, 145:149]
summ.lichens[1:5,]
head(comp_old[,1:5])#shows that comp/comp new/comp old have the same row order
colnames(summ.lichens)<-c("lich.BCdiss", "lich.Sodiss")
summ.lichens$lich.rich1992<-rowSums(subset(comp_old>0, select=-Year))
summ.lichens$lich.rich2015<-rowSums(subset(comp_new>0, select=-Year))
summ.lichens$richchange<-summ.lichens$lich.rich2015-summ.lichens$lich.rich1992
rownames(summ.lichens)<-rownames(comp_old)

library(vegan)
extinctions.lichens<-as.matrix(designdist(comp, method = "(A-J)/A", terms = "binary", abcd=FALSE, alphagamma=FALSE, "extinctions")) #J for shared quantity, A and B for totals, N for the number of rows (sites) and P for the number of columns (species). I need to get someone to check this.
summ.lichens$lich.extinct<-0
for (i in 1:144)
{summ.lichens$lich.extinct[i]<-(extinctions.lichens[i, i+144])}
colonisations.lichens<-as.matrix(designdist(comp, method = "(B-J)/B", terms = "binary", abcd=FALSE, alphagamma=FALSE, "colonisations")) #J for shared quantity, A and B for totals, N for the number of rows (sites) and P for the number of columns (species)
summ.lichens$lich.colonise<-0
for (i in 1:144)
{summ.lichens$lich.colonise[i]<-(colonisations.lichens[i, i+144])}

###bryos###
BCdist.bryos<-as.matrix(vegdist(easytabx, method="bray"))
Sodist.bryos<-as.matrix(vegdist(easytabx, method="bray", binary=TRUE))
summ.bryos<-as.data.frame(cbind(0,0))#you have to set something up for the loop to feed into.
for (i in (1:144))
{summ.bryos[i,1]<-(BCdist.bryos[i*2-1, i*2])
  summ.bryos[i,2]<-(Sodist.bryos[i*2-1, i*2])}# Here I want odds combined with their folllowing evens.
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
BCdist.bryos[c(1,3,5,7,9), c(2,4,6,8,10)]
summ.bryos[1:5,]
head(easytabx[,1:5])#shows that bryo and the resulting objects have the same row order. BUT the are a different row order to lichens and vascular - goes A01, A02, A03 not A01, A10, A11.
rownames(summ.bryos)<-substr(rownames(easytabx[substr(rownames(easytabx),4,7)=="1992",]),1,3)
colnames(summ.bryos)<-c("bryo.BCdiss", "bryo.Sodiss")
summ.bryos$bryo.rich1992<-rowSums(easytabx[substr(rownames(easytabx),4,7)=="1992",]>0)
summ.bryos$bryo.rich2015<-rowSums(easytabx[substr(rownames(easytabx),4,7)=="2015",]>0)
summ.bryos$bryochange<-summ.bryos$bryo.rich2015-summ.bryos$bryo.rich1992
extinctions.bryos<-as.matrix(designdist(easytabx, method = "(A-J)/A", terms = "binary", abcd=FALSE, alphagamma=FALSE, "extinctions")) 
summ.bryos$bryo.extinct<-0
for (i in 1:144)
{summ.bryos$bryo.extinct[i]<-(extinctions.bryos[i*2-1, i*2])}
colonisations.bryos<-as.matrix(designdist(easytabx, method = "(B-J)/B", terms = "binary", abcd=FALSE, alphagamma=FALSE, "colonisations")) #J for shared quantity, A and B for totals, N for the number of rows (sites) and P for the number of columns (species)
summ.bryos$bryo.colonise<-0
for (i in 1:144)
{summ.bryos$bryo.colonise[i]<-(colonisations.bryos[i*2-1, i*2])}

##Vascular plants##
BCdist.vascs<-as.matrix(vegdist(Vascall.df, method="bray"))
Sodiss.vascs<-as.matrix(vegdist(Vascall.df, method="bray", binary=TRUE))
summ.vascs<-as.data.frame(cbind(0,0))#you have to set something up for the loop to feed into.
for (i in (1:144))
{summ.vascs[i,1]<-(BCdist.vascs[i, i+144])
  summ.vascs[i,2]<-(Sodiss.vascs[i, i+144])}# Like lichens, it goes 1-144 then 145-250
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
BCdist.vascs[1:5, 145:149]
summ.vascs[1:5,]
head(Vascall.df[,1:5])#shows that bryo and the resulting objects have the same row order. They go A1, A2, A3 not A1, A10, A11.
colnames(summ.vascs)<-c("Vasc.BCdiss", "Vasc.Sodiss")
summ.vascs$vasc.rich1992<-rowSums(VascOld.fat>0)
summ.vascs$vasc.rich2015<-rowSums(VascNew.fat>0)
summ.vascs$vascchange<-summ.vascs$vasc.rich2015-summ.vascs$vasc.rich1992
rownames(summ.vascs)<-rownames(VascOld.fat)

extinctions.vascs<-as.matrix(designdist(Vascall.df, method = "(A-J)/A", terms = "binary", abcd=FALSE, alphagamma=FALSE, "extinctions")) 
summ.vascs$vasc.extinct<-0
for (i in 1:144)
{summ.vascs$vasc.extinct[i]<-(extinctions.vascs[i, i+144])}
colonisations.vascs<-as.matrix(designdist(Vascall.df, method = "(B-J)/B", terms = "binary", abcd=FALSE, alphagamma=FALSE, "colonisations")) #J for shared quantity, A and B for totals, N for the number of rows (sites) and P for the number of columns (species)
summ.vascs$vasc.colonise<-0
for (i in 1:144)
{summ.vascs$vasc.colonise[i]<-(colonisations.vascs[i, i+144])}


##Final step is to merge them all. I can't just cbind because the row orders are different. This is a neat function off stack overflow (http://stackoverflow.com/questions/16666643/merging-more-than-2-dataframes-in-r-by-rownames) that does all three objects at once and gets rid of the superfluous row names-columns
HDRmerge<- function(x, y){
  df<- merge(x, y, by= "row.names", all.x= TRUE, all.y= TRUE)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}
Summaries<- Reduce(HDRmerge, list(summ.lichens, summ.bryos, summ.vascs))

x11();par(mfrow=c(3,3), xpd=NA)
sapply(Summaries[,c(1,3,4,8,10,11,15,17,18)], function(x){hist (x, main=NULL, ylab=NULL, xlab=NULL)}) #Bad magic numbers because of new columns
text("BCdist",x=-200, y=175, cex=1.4)#needs fixing another day
text("Richness in 1992",x=-150, y=175, cex=1.4)
text("Richness in 2015",x=100, y=175, cex=1.4)
text("Lichens",x=-550, y=150, cex=1.4, srt=90)
text("Bryophytes",x=-550, y=85, cex=1.4, srt=90)
text("Vascular plants",x=-550, y=15, cex=1.4, srt=90)
savePlot("Distributions of values.emf", type="emf")
t.test(Summaries$lich.rich1992, Summaries$lich.rich2015, paired=TRUE)
t.test(Summaries$bryo.rich1992, Summaries$bryo.rich2015, paired=TRUE)
t.test(Summaries$vasc.rich1992, Summaries$vasc.rich2015, paired=TRUE)#this is very ns for unpaired data and very sig for paired data!

x11(); par(mfrow=c(1,3), pin=c(1.6,1.6), mgp=c(1.8,0.5,0))
plot(Summaries$lich.extinct, Summaries$lich.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Lichens")
plot(Summaries$bryo.extinct, Summaries$bryo.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Bryophytes")
plot(Summaries$vasc.extinct, Summaries$vasc.colonise, xlab="Proportion plot extinctions 1992-2015", ylab="Proportion plot colonisations 1992-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Vascular plants")
savePlot("Colonisations vs extinctions.emf", type="emf")

sapply(Summaries, mean)
sapply(Summaries, sd)
sapply (list(comp_old, comp_new, VascOld.fat, VascNew.fat), dim)
sum(colSums(easytabx[substr(rownames(easytabx),4,7)=="1992",])>0)#that's a long-winded way to get bryo richness...
sum(colSums(easytabx[substr(rownames(easytabx),4,7)=="2015",])>0)

#is turnover higher in certain communities? Or plots with more communities in?
Summaries<-merge(Summaries, phytosoc, by.x=0, by.y=1)
rownames(Summaries)<-Summaries$Row.names
Summaries$Row.names<-NULL

x11()
layout(matrix(c(1,4,7,10,13,2,5,8,11,14,3,6,9,12,15), 3, 5, byrow = TRUE))
par(mar=c(3,3,3,1), cex.axis=0.8, las=2, xpd=NA, mgp=c(2,0.5,0))
sapply(Summaries[,c(3,10,17)], function (x) { 
  boxplot(x~dominant, data=Summaries, col=2:8, ylim=c(0,200), ylab="Plot richness 1992", main=colnames(x))
  model.x<-aov(x~dominant, data=Summaries)
    text(x=4, y=200, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
    text(x=4, y=180, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
   text(x=3, y=-20, labels=colnames(x))
  }) #The titles aren't working with either of the two methods included here. It would be great if I could get the P to display as stars or as >0.001 as well. And I should use an appropriate model, but poisson models with log links don't appear to give F or P values. See richard fix
sapply(Summaries[,c(4,11,18)], function (x) { 
  boxplot(x~dominant, data=Summaries, col=2:8, ylim=c(0,200), ylab="Plot richness 2015")
  model.x<-aov(x~dominant, data=Summaries)
  text(x=4, y=200, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=180, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}) 
sapply(Summaries[,c(6,13,20)], function (x) { 
  boxplot(x~dominant, data=Summaries, col=2:8, ylim=c(0,1), ylab="Proportion plot extinctions")
  model.x<-aov(x~dominant, data=Summaries)
  text(x=4, y=1, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.9, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}) 
sapply(Summaries[,c(7,14,21)], function (x) { 
  boxplot(x~dominant, data=Summaries, col=2:8, ylim=c(0,1), ylab="Proportion plot colonisations")
  model.x<-aov(x~dominant, data=Summaries)
  text(x=4, y=1, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.9, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
}) 
sapply(Summaries[,c(1,8,15)], function (x) { 
  boxplot(x~dominant, data=Summaries, col=2:8, ylim=c(0,0.4), ylab="Bray-Curtis distance")
  model.x<-aov(x~dominant, data=Summaries)
  text(x=4, y=0.4, labels=paste("F =", signif(summary(model.x)[[1]][1,4], 3)),cex=0.8)
  text(x=4, y=0.36, labels=paste("P =", signif(summary(model.x)[[1]][1,5], 3)),cex=0.8)
})
text(-17.5,1.8,"Lichens", cex=1.2)
text(-17.5,1.15,"Bryophytes", cex=1.2)
text(-17.5,0.5,"Vascular plants", cex=1.2)
savePlot("Turnover and richness by phytosoc.emf", type="emf")

#for cambs presentation
library(tidyr)
test<-gather(Summaries[,c(3,4,10,11,17,18,30)], key=facet, value=species_richness, -dominant)
test$Year<-substr(test$facet,10,13)
test$Plants<-substr(test$facet,1,4)

library(ggplot2)
library(ggthemes)
x11(5,4);
plotty<-ggplot(test, aes(dominant, species_richness))+geom_boxplot()
plotty+facet_grid(Plants~Year)+theme_igray()
savePlot("Facets for Phytosoc.emf", type="emf")

#some stuff for messing about or spare code for if we go back to models
#model.x<-as.data.frame(anova(glm(x~dominant, data=Summaries, family= poisson(link = "log"))))
#text(x=4, y=200, labels=paste("F =", signif(model.x[1,4], 3)),cex=0.8)
#text(x=4, y=180, labels=paste("P =", signif(model.x[1,5], 3)),cex=0.8)


#check that the models chosen behave themselves. This should be commented out once completed
#dev.off()#takes out the settings used for the previous figures, but note risks loss of unsaved figures
#sapply(Summaries[,c(2,7,12)], function (x) { 
#    model.x<-aov(x~dominant, data=Summaries)
#    plot(model.x)
#})#they look ok (rich1992) but is it ok to anova count data? It wouldn't be ok to glm it. There is really no risk here of the sd crossing zero.
