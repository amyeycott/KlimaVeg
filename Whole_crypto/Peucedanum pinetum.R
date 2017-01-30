source("../Whole_crypto/turnover_subset.R")
##this is now run on the subset of sites which are not rectangular or riverine

str(summaries.ss)

#next step is essential for preserving row names
Picea1992<-subset(vascOld.fat, select="Picea_abies")
Picea2015<-subset(vascNew.fat, select="Picea_abies")
Summaries.ss.PP<-Reduce(HDRmerge, list(summaries.ss, Picea1992, Picea2015))# hdr merge is just a tidier version of merge, already composed in the source file. 

library(dplyr)
Summaries.ss.PP<-rename(Summaries.ss.PP, Picea1992=Picea_abies.x , Picea2015=Picea_abies.y)
rm(Picea1992, Picea2015)#just to avoid confusion

table(Summaries.ss.PP$Picea1992,Summaries.ss.PP$Picea2015)#there were no Picea-free plots in either year, according to this! I checked in the original thin files

Summaries.ss.PP$Transition<-paste(Summaries.ss.PP$Picea1992, "to", Summaries.ss.PP$Picea2015)
Summaries.ss.PP$Transition[Summaries.ss.PP$Transition=="3 to 3"]<-"Stayed 3"
Summaries.ss.PP$Transition[Summaries.ss.PP$Transition=="2 to 2"]<-"Stayed 2"
Summaries.ss.PP$Transition[Summaries.ss.PP$Transition=="1 to 1"]<-"Stayed 1"

x11(12,7); par(mfrow=c(3,7), las=2, mar=c(5,3,3,0.5), mgp=c(2,0.5,0))
mapply(function(x, main, ylim){boxplot(x~Transition, data=Summaries.ss.PP, main=main, cex.main=0.8, ylim=ylim)}, x = Summaries.ss.PP[,c("lich.BCdiss","lich.Sodiss","lich.rich1992","lich.rich2015","richchange","lich.extinct","lich.colonise","bryo.BCdiss","bryo.Sodiss","bryo.rich1992","bryo.rich2015","bryochange","bryo.extinct","bryo.colonise","vasc.BCdiss","vasc.Sodiss","vasc.rich1992","vasc.rich2015","vascchange","vasc.extinct","vasc.colonise")], main = c("Lichen BC diss", "Lichen Sörensen diss","Lichen plot richness 1992", "Lichen plot richness 2015","Lichen change in plot richness","Lichen plot extinctions since 1992", "Lichen plot colonisations 2015", "Bryophyte BC diss","Bryophyte Sörensen diss", "Bryophyte plot richness 1992", "Bryophyte plot richness 2015","Bryophyte change in plot richness","Bryophyte plot extinctions since 1992", "Bryophyte plot colonisations 2015", "vascular BC diss","vascular Sörensen diss", "vascular plot richness 1992", "vascular plot richness 2015","vascular change in plot richness","vascular plot extinctions since 1992", "vascular plot colonisations 2015"), ylim=list(c(0,0.35),c(0,0.55),c(0,120),c(0,120),c(-10,50),c(0,0.8),c(0,0.8)))#data exploration. 
savePlot("PP all the boxplots.pdf", type="pdf")
savePlot("PP all the boxplots.emf", type="emf")

##protected##
x11(); par(mfrow=c(3,1))
mapply(function(x, z, main, ylim){
  boxplot(x~Transition, data=Summaries.ss.PP, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="blue")
  boxplot(z~Transition, data=Summaries.ss.PP, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="yellow", xaxt="n", yaxt="n")}, 
  x = Summaries.ss.PP[,c("lich_threatened_1992","bryo_threatened_1992","vasc_threatened_1992")], z=Summaries.ss.PP[,c("lich_threatened_2015","bryo_threatened_2015","vasc_threatened_2015")] , main=c("Threatened lichens","Threatened bryophytes","Threatened vascular"), ylim=list(c(0,50),c(0,10),c(0,2)))#note, ylim can't be moved back
savePlot("PP_redlist_tentative_subsetted.pdf", type="pdf")
savePlot("PP_redlist_tentative_subsetted.emf", type="emf")

#Nice new verison of figure with 3x3 and side-by-sides
x11(7,7)
layout(matrix(c(1,2,3,4,5,6,7,8,9), 3, 3, byrow = FALSE))
par(las=2, mar=c(4,3,3,0.1), tcl=-0.2, mgp=c(1,0.3,0))
mapply(function(x, z, main, ylim){
  boxplot(x~Transition, data=Summaries.ss.PP, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="blue", cex.main=1)
    boxplot(z~Transition, data=Summaries.ss.PP, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="yellow", xaxt="n", yaxt="n")}, 
    x = Summaries.ss.PP[,c("lich.rich1992","bryo.rich1992","vasc.rich1992")], z=Summaries.ss.PP[,c("lich.rich2015","bryo.rich2015","vasc.rich2015")] , main=c("Lichen richness","Bryophyte richness","Vascular plant richness"), list(c(0,100),c(0,100),c(0,100)))

mapply(function(x, z, main, ylim){  
  boxplot(x~Transition, data=Summaries.ss.PP, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="blue", cex.main=1)
  boxplot(z~Transition, data=Summaries.ss.PP, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="yellow", xaxt="n", yaxt="n")}, 
  x = Summaries.ss.PP[,c("lich_threatened_1992","bryo_threatened_1992","vasc_threatened_1992")], z=Summaries.ss.PP[,c("lich_threatened_2015","bryo_threatened_2015","vasc_threatened_2015")] , main=c("Threatened lichens","Threatened bryophytes","Threatened vascular"), ylim=list(c(0,50),c(0,10),c(0,2)))

mapply(function(x, z, main, ylim){  
  boxplot(x~Transition, data=Summaries.ss.PP, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="blue", cex.main=1)
  boxplot(z~Transition, data=Summaries.ss.PP, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="yellow", xaxt="n", yaxt="n")}, 
  x = Summaries.ss.PP[,c("lich.extinct","bryo.extinct","vasc.extinct")], z=Summaries.ss.PP[,c("lich.colonise","bryo.colonise","vasc.colonise")] , main=c("Lichens ext. vs. col.","Bryophytes ext. vs. col","Vascular ext. vs. col"), ylim=list(c(0,1),c(0,1),c(0,1)))
legend("topright", fill=c("blue","yellow"), legend=c("1992","2015"), y.intersp=0.8)
savePlot("PP Compares years richness colnisations threatened.wmf", type="wmf")
savePlot("PP Compares years richness colnisations threatened.pdf", type="pdf")


#####ORDINATIONS#####
library(vegan)
giantdataset<-Reduce(HDRmerge, list(subset(comp, select=-Year), easytabx, vascall.df))

giantnmds<-metaMDS(giantdataset, try=100)#converges some days and not others! Maybe the standardisation/transformation is not appropriate.
giantnmds.sites<-as.data.frame(scores(giantnmds, display="sites"))
giantnmds.sites$plot<-substr(row.names(giantnmds.sites),1,3)
giantnmds.sites$year<-substr(row.names(giantnmds.sites),4,7)

ords.sites<-merge(giantnmds.sites, Summaries.ss.PP[,c("Picea abies.x", "Picea abies.y","Transition")], by.x=3, by.y=0)#this is bad, I need to use subsets for the input to the nmds
giantdca<-decorana(giantdataset)
giantnmds.sites<-as.data.frame(scores(giantnmds, display="sites"))
giantnmds.sites$plot<-substr(row.names(giantnmds.sites),1,3)
giantnmds.sites$year<-substr(row.names(giantnmds.sites),4,7)

lichnmds.ss<-metaMDS(lichall.df.ss)#this one reaches a solution... time is on axis 2, so what has it put on axis 1?
lichnmds.ss.sites<-as.data.frame(scores(lichnmds.ss, display="sites"))
lichnmds.ss.sites$plot<-substr(row.names(lichnmds.ss.sites),1,3)
lichnmds.ss.sites$year<-substr(row.names(lichnmds.ss.sites),4,7)
names(lichnmds.ss.sites)[1:2]<-c("lichnmds1","lichnmds2")
ords.ss.sites<-merge(lichennmds.ss.sites, Summaries.ss.PP[,c("Picea1992", "Picea2015","Transition")], by.x=3, by.y=0)#

set.seed(1)
bryonmds.ss<-metaMDS(bryoall.df.ss)#I set the seed because the solution was unstable and did not converge 1 in every 5 runs. I could test a range of seeds and take the lowest stress solution.
bryonmds.ss.sites<-as.data.frame(scores(bryonmds.ss, display="sites"))
bryonmds.ss.sites$plot<-substr(row.names(bryonmds.ss.sites),1,3)
bryonmds.ss.sites$year<-substr(row.names(bryonmds.ss.sites),4,7)
names(bryonmds.ss.sites)[1:2]<-c("bryonmds1","bryonmds2")
rownames(ords.ss.sites)<-paste(ords.ss.sites$plot, ords.ss.sites$year, sep="")
ords.ss.sites<-merge(ords.ss.sites, bryonmds.ss.sites, by=0)

set.seed(1)
vascnmds.ss<-metaMDS(vascall.df.ss)#I set the seed because the solution was unstable and did not converge 1 in every 5 runs. I could test a range of seeds and take the lowest stress solution.
vascnmds.ss.sites<-as.data.frame(scores(vascnmds.ss, display="sites"))
vascnmds.ss.sites$plot<-substr(row.names(vascnmds.ss.sites),1,3)
vascnmds.ss.sites$year<-substr(row.names(vascnmds.ss.sites),4,7)
names(vascnmds.ss.sites)[1:2]<-c("vascnmds1","vascnmds2")
rownames(ords.ss.sites)<-paste(ords.ss.sites$plot, ords.ss.sites$year, sep="")
ords.ss.sites<-merge(ords.ss.sites, vascnmds.ss.sites, by=0)



library(ggplot2)
x11()
plotty<-ggplot(ords.ss.sites, aes(NMDS1, NMDS2, colour=year))+  geom_point()
plotty+facet_grid(-Picea1992~-Picea2015)
savePlot("PP NMDS facets.emf", type="emf")
savePlot("PP NMDS facets.pdf", type="pdf")#still needs to be done: mess with the aesthetics to get crosshairs, get the faceting variable on each side not just the values of the faceting variable, can it make blank space for facets containing no data? Lichens wise, does it plot axis 1 as the dominant? Because for lichens it plots time as axis 2. 

