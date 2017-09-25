source("turnover.R")
#####Some plots####


x11();par(mfrow=c(3,3), xpd=NA)
mapply(function(x, ylab){hist (x, main=NULL, ylab=ylab, xlab=NULL)}, x=Summaries[,c("lich.BCdiss","lich.rich1990","lich.rich2015","bryo.BCdiss", "bryo.rich1990", "bryo.rich2015","vasc.BCdiss", "vasc.rich1990","vasc.rich2015")], ylab= c("BCdist","Richness in 1990","Richness in 2015")) # OBS! column subsetting needs mapply.
text("Lichens",x=-150, y=180, cex=1.4)
text("Bryophytes",x=-150, y=110, cex=1.4)
text("vascular plants",x=-150, y=35, cex=1.4)
savePlot("Distributions of values.emf", type="emf")
t.test(Summaries$lich.rich1990, Summaries$lich.rich2015, paired=TRUE)
t.test(Summaries$bryo.rich1990, Summaries$bryo.rich2015, paired=TRUE)
t.test(Summaries$vasc.rich1990, Summaries$vasc.rich2015, paired=TRUE)#this is very ns for unpaired data and very sig for paired data!

x11(); par(mfrow=c(1,3), pin=c(1.4,1.4), mgp=c(1.8,0.5,0))
plot(Summaries$lich.extinct, Summaries$lich.colonise, xlab="Proportion plot extinctions 1990-2015", ylab="Proportion plot colonisations 1990-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Lichens")
plot(Summaries$bryo.extinct, Summaries$bryo.colonise, xlab="Proportion plot extinctions 1990-2015", ylab="Proportion plot colonisations 1990-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Bryophytes")
plot(Summaries$vasc.extinct, Summaries$vasc.colonise, xlab="Proportion plot extinctions 1990-2015", ylab="Proportion plot colonisations 1990-2015", xlim=c(0,0.8), ylim=c(0,0.8), main="Vascular plants")
savePlot("Colonisations vs extinctions.emf", type="emf")
