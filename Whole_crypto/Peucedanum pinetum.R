source("../Whole_crypto/turnover.R")
str(Summaries)

#next step is essential for preserving row names
Picea1992<-subset(VascOld.fat, select="Picea_abies")
Picea2015<-subset(VascNew.fat, select="Picea_abies")
SummariesPP<-Reduce(HDRmerge, list(Summaries, Picea1992, Picea2015))# hdr merge is just a tidier version of merge, already composed in the source file. 
library(plyr)
SummariesPP<-rename(SummariesPP,c("Picea_abies.x"="Picea1992","Picea_abies.y"="Picea2015"))
rm(Picea1992, Picea2015)#just to avoid confusion

table(SummariesPP$Picea1992,SummariesPP$Picea2015)#there were no Picea-free plots in either year, according to this! I checked in the original thin files

SummariesPP$Transition<-paste(SummariesPP$Picea1992, "to", SummariesPP$Picea2015)
SummariesPP$Transition[SummariesPP$Transition=="3 to 3"]<-"Stayed 3"
SummariesPP$Transition[SummariesPP$Transition=="2 to 2"]<-"Stayed 2"
SummariesPP$Transition[SummariesPP$Transition=="1 to 1"]<-"Stayed 1"

x11(12,7); par(mfrow=c(3,7), las=2, mar=c(5,3,3,0.5), mgp=c(2,0.5,0))
mapply(function(x, main, ylim){boxplot(x~Transition, data=SummariesPP, main=main, cex.main=0.8, ylim=ylim)}, x = Summaries[, 1:21], main = c("Lichen BC diss", "Lichen Sörensen diss","Lichen plot richness 1992", "Lichen plot richness 2015","Lichen change in plot richness","Lichen plot extinctions since 1992", "Lichen plot colonisations 2015", "Bryophyte BC diss","Bryophyte Sörensen diss", "Bryophyte plot richness 1992", "Bryophyte plot richness 2015","Bryophyte change in plot richness","Bryophyte plot extinctions since 1992", "Bryophyte plot colonisations 2015", "Vascular BC diss","Vascular Sörensen diss", "Vascular plot richness 1992", "Vascular plot richness 2015","Vascular change in plot richness","Vascular plot extinctions since 1992", "Vascular plot colonisations 2015"), ylim=list(c(0,0.35),c(0,0.55),c(0,120),c(0,120),c(-10,50),c(0,0.8),c(0,0.8)))#data exploration. ylim argument is failing, have tried list, cbind, rbind..
savePlot("Fuckyeah.pdf", type="pdf")
savePlot("Fuckyeah.emf", type="emf")
#####ORDINATIONS#####

giantdataset<-Reduce(HDRmerge, list(comp, easytabx, Vascall.df))#broken
giantnmds<-metaMDS(giantdataset, try=100)#converges some days and not others! Maybe the standardisation/transformation is not appropriate.
giantnmds.sites<-as.data.frame(scores(giantnmds, display="sites"))
giantnmds.sites$plot<-substr(row.names(giantnmds.sites),1,3)
giantnmds.sites$year<-substr(row.names(giantnmds.sites),4,7)
ords.sites<-merge(giantnmds.sites, SummariesPP[,c(31,32)], by.x=3, by.y=0)#
giantdca<-decorana(giantdataset)
giantnmds.sites<-as.data.frame(scores(giantnmds, display="sites"))
giantnmds.sites$plot<-substr(row.names(giantnmds.sites),1,3)
giantnmds.sites$year<-substr(row.names(giantnmds.sites),4,7)

library(ggplot2)
x11()
plotty<-ggplot(ords.sites, aes(NMDS1, NMDS2, colour=year))+  geom_point()
plotty+facet_grid(-Picea1992~-Picea2015)
savePlot("Fancy-ass figure_not quite there yet.pdf", type="pdf")#still needs to be done: mess with the aesthetics to get crosshairs, get the faceting variable on each side not just the values of the faceting variable, can it make blank space for facets containing no data?
