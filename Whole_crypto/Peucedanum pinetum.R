source("../Whole_crypto/turnover_subset.R")
##this is now run on the subset of sites which are not rectangular or riverine

str(summaries.ss)

#next step is essential for preserving row names
Picea1992<-subset(VascOld.fat, select="Picea_abies")
Picea2015<-subset(VascNew.fat, select="Picea_abies")
Summaries.ss.PP<-Reduce(HDRmerge, list(summaries.ss, Picea1992, Picea2015))# hdr merge is just a tidier version of merge, already composed in the source file. 
library(plyr)
Summaries.ss.PP<-rename(Summaries.ss.PP,c("Picea_abies.x"="Picea1992","Picea_abies.y"="Picea2015"))
rm(Picea1992, Picea2015)#just to avoid confusion

table(Summaries.ss.PP$Picea1992,Summaries.ss.PP$Picea2015)#there were no Picea-free plots in either year, according to this! I checked in the original thin files

Summaries.ss.PP$Transition<-paste(Summaries.ss.PP$Picea1992, "to", Summaries.ss.PP$Picea2015)
Summaries.ss.PP$Transition[Summaries.ss.PP$Transition=="3 to 3"]<-"Stayed 3"
Summaries.ss.PP$Transition[Summaries.ss.PP$Transition=="2 to 2"]<-"Stayed 2"
Summaries.ss.PP$Transition[Summaries.ss.PP$Transition=="1 to 1"]<-"Stayed 1"

x11(12,7); par(mfrow=c(3,7), las=2, mar=c(5,3,3,0.5), mgp=c(2,0.5,0))
mapply(function(x, main, ylim){boxplot(x~Transition, data=Summaries.ss.PP, main=main, cex.main=0.8, ylim=ylim)}, x = Summaries.ss.PP[,c("lich.BCdiss","lich.Sodiss","lich.rich1992","lich.rich2015","richchange","lich.extinct","lich.colonise","bryo.BCdiss","bryo.Sodiss","bryo.rich1992","bryo.rich2015","bryochange","bryo.extinct","bryo.colonise","Vasc.BCdiss","Vasc.Sodiss","vasc.rich1992","vasc.rich2015","vascchange","vasc.extinct","vasc.colonise")], main = c("Lichen BC diss", "Lichen Sörensen diss","Lichen plot richness 1992", "Lichen plot richness 2015","Lichen change in plot richness","Lichen plot extinctions since 1992", "Lichen plot colonisations 2015", "Bryophyte BC diss","Bryophyte Sörensen diss", "Bryophyte plot richness 1992", "Bryophyte plot richness 2015","Bryophyte change in plot richness","Bryophyte plot extinctions since 1992", "Bryophyte plot colonisations 2015", "Vascular BC diss","Vascular Sörensen diss", "Vascular plot richness 1992", "Vascular plot richness 2015","Vascular change in plot richness","Vascular plot extinctions since 1992", "Vascular plot colonisations 2015"), ylim=list(c(0,0.35),c(0,0.55),c(0,120),c(0,120),c(-10,50),c(0,0.8),c(0,0.8)))#data exploration. 
savePlot("Fuckyeah.pdf", type="pdf")
savePlot("Fuckyeah.emf", type="emf")

##protected##
x11(); par(mfrow=c(3,2))
mapply(function(x, main, ylim){boxplot(x~Transition, data=Summaries.ss.PP, main=main, cex.main=0.8, ylim=ylim, las=2)}, x = Summaries.ss.PP[,c("lich_threatened_1992","lich_threatened_2015","bryo_threatened_1992","bryo_threatened_2015","vasc_threatened_1992","vasc_threatened_2015")], main=c("Threatened lichens 1992","Threatened lichens 2015","Threatened bryophytes 1992","Threatened bryophytes 2015","Threatened vascular 1992","Threatened vascular 2015"), ylim=list(c(0,50),c(0,50),c(0,10),c(0,10),c(0,2),c(0,2)))
savePlot("redlist_tentative_subsetted.pdf", type="pdf")
savePlot("redlist_tentative_subsetted.emf", type="emf")

#####ORDINATIONS#####

giantdataset<-Reduce(HDRmerge, list(comp, easytabx, Vascall.df))#broken
giantnmds<-metaMDS(giantdataset, try=100)#converges some days and not others! Maybe the standardisation/transformation is not appropriate.
giantnmds.sites<-as.data.frame(scores(giantnmds, display="sites"))
giantnmds.sites$plot<-substr(row.names(giantnmds.sites),1,3)
giantnmds.sites$year<-substr(row.names(giantnmds.sites),4,7)
ords.sites<-merge(giantnmds.sites, Summaries.ss.PP[,c(31,32)], by.x=3, by.y=0)#
giantdca<-decorana(giantdataset)
giantnmds.sites<-as.data.frame(scores(giantnmds, display="sites"))
giantnmds.sites$plot<-substr(row.names(giantnmds.sites),1,3)
giantnmds.sites$year<-substr(row.names(giantnmds.sites),4,7)

library(ggplot2)
x11()
plotty<-ggplot(ords.sites, aes(NMDS1, NMDS2, colour=year))+  geom_point()
plotty+facet_grid(-Picea1992~-Picea2015)
savePlot("Fancy-ass figure_not quite there yet.pdf", type="pdf")#still needs to be done: mess with the aesthetics to get crosshairs, get the faceting variable on each side not just the values of the faceting variable, can it make blank space for facets containing no data?

