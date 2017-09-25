source("../Whole_crypto/turnover_subset.R")
names(envir)
names(lichall.df.ss)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}#adapted from help file for pairs, prints the correlation coefficient on the upper diagonals

pairs(~L_light+T_temperature+K_continentality+F_moisture+R_reaction+N_nitrogen,data=envir[envir$Species%in%names(lichall.df.ss),], lower.panel = panel.smooth, upper.panel = panel.cor, na.action = na.omit)#panel.smooth is fitting a lowess-smoothed fit line to the lower diagonals.

pairs(sapply(envir[envir$Species%in%names(lichall.df.ss),c("L_light","T_temperature","K_continentality","F_moisture","R_reaction","N_nitrogen")], jitter, amount=0.1),lower.panel=panel.smooth, upper.panel=NULL, main="Lichens")#you can have jitter OR panel cor, not both.

savePlot("Lichen_EIV_correlations.emf", type="emf")
savePlot("Lichen_EIV_correlations.png", type="png")

pairs(sapply(bryo.status[,c("L","T","K","F","R")], jitter, amount=0.1),lower.panel=panel.smooth, upper.panel=NULL, main="Bryophytes")#you can have jitter OR panel cor, not both.

savePlot("Bryo_EIV_correlations.emf", type="emf")
savePlot("Bryo_EIV_correlations.png", type="png")

pairs(sapply(vasc.ellen[,c("L","T","K","W","Tr","R")], jitter, amount=0.1),lower.panel=panel.smooth, upper.panel=NULL, main="Vascular plants")#you can have jitter OR panel cor, not both.

savePlot("Vasc_EIV_correlations.emf", type="emf")
savePlot("Vasc_EIV_correlations.png", type="png")


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

###observer effect?####
x11(8,3); par(mfrow=c(1,3), pin=c(2,2))
plot(summaries.ss$lich.colonise~summaries.ss$lich.BCdiss)
plot(summaries.ss$bryo.colonise~summaries.ss$bryo.BCdiss)
plot(summaries.ss$vasc.colonise~summaries.ss$vasc.BCdiss)