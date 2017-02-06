source("../Whole_crypto/turnover_subset.R")
##this is now run on the subset of sites which are not rectangular or riverine. The ordinations can be started after running only lines 1-20. The lichens are out of any figures used in the current version. The Picea map is in maps.R

str(summaries.ss)

#next step is essential for preserving row names
Picea1992 <- subset(vascOld.fat, select = "Picea_abies")
Picea2015 <- subset(vascNew.fat, select = "Picea_abies")
Summaries.ss.PP<-Reduce(HDRmerge, list(summaries.ss, Picea1992, Picea2015))# hdr merge is just a tidier version of merge, already composed in the source file. 

#set the colours for each year before making any plots. It'sthe klimaveg colours!
coloury<-cbind(c(1992,2015),c("#3873AE","#EF9335"))


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
  x = Summaries.ss.PP[,c("bryo_threatened_1992","vasc_threatened_1992")], z=Summaries.ss.PP[,c("bryo_threatened_2015","vasc_threatened_2015")] , main=c("Threatened bryophytes","Threatened vascular"), ylim=list(c(0,50),c(0,10),c(0,2)))#note, ylim can't be moved back
savePlot("PP_redlist_tentative_subsetted.pdf", type="pdf")
savePlot("PP_redlist_tentative_subsetted.emf", type="emf")

#Nice new verison of figure with 3x3 and side-by-sides
x11(7,7)
layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = FALSE))
par(las=2, mar=c(4,3,3,0.1), tcl=-0.2, mgp=c(1,0.3,0))
mapply(function(x, z, main, ylim){
  boxplot(x~Transition, data=Summaries.ss.PP, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="#3873AE", cex.main=1)
    boxplot(z~Transition, data=Summaries.ss.PP, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="#EF9335", xaxt="n", yaxt="n")}, 
    x = Summaries.ss.PP[,c("bryo.rich1992","vasc.rich1992")], z=Summaries.ss.PP[,c("bryo.rich2015","vasc.rich2015")] , main=c("Bryophyte richness","Vascular plant richness"), ylim=list(c(0,100),c(0,150)))

mapply(function(x, z, main, ylim, yaxp){  
  boxplot(x~Transition, data=Summaries.ss.PP, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="#3873AE", cex.main=1, yaxp=yaxp)
  boxplot(z~Transition, data=Summaries.ss.PP, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="#EF9335", xaxt="n", yaxt="n")}, 
  x = Summaries.ss.PP[,c("bryo_threatened_1992","vasc_threatened_1992")], z=Summaries.ss.PP[,c("bryo_threatened_2015","vasc_threatened_2015")] , main=c("Threatened bryophytes","Threatened vascular"), ylim=list(c(0,10),c(0,2)),yaxp=list(c(0,10,2),c(0,2,1)))

mapply(function(x, z, main, ylim){  
  boxplot(x~Transition, data=Summaries.ss.PP, main=main, ylim=ylim, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="#3873AE", cex.main=1)
  boxplot(z~Transition, data=Summaries.ss.PP, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="#EF9335", xaxt="n", yaxt="n")}, 
  x = Summaries.ss.PP[,c("bryo.extinct","vasc.extinct")], z=Summaries.ss.PP[,c("bryo.colonise","vasc.colonise")] , main=c("Bryophytes ext. vs. col","Vascular ext. vs. col"), ylim=list(c(0,1),c(0,1)))
legend("topright", fill=c("#3873AE","#EF9335"), legend=c("1992","2015"), y.intersp=0.8)
savePlot("PP Compares years richness colonisations threatened.wmf", type="wmf")
savePlot("PP Compares years richness colonisations threatened.pdf", type="pdf")


#####ORDINATIONS#####
library(vegan)
giantdataset<-Reduce(HDRmerge, list(subset(comp, select=-Year), easytabx, vascall.df))

giantnmds<-metaMDS(giantdataset, try=100)#converges some days and not others! Maybe the standardisation/transformation is not appropriate.
giantnmds.sites<-as.data.frame(scores(giantnmds, display="sites"))
giantnmds.sites$plot<-substr(row.names(giantnmds.sites),1,3)
giantnmds.sites$year<-substr(row.names(giantnmds.sites),4,7)
giantdca<-decorana(giantdataset)
giantnmds.sites<-as.data.frame(scores(giantnmds, display="sites"))
giantnmds.sites$plot<-substr(row.names(giantnmds.sites),1,3)
giantnmds.sites$year<-substr(row.names(giantnmds.sites),4,7)

nmds.siteout.fun<-function(df, name){
  set.seed=10
  nmds.ss<-metaMDS(x, try=100)
  #how to save te object with the name?
  ss.sites<-as.data.frame(scores(nmds.ss, display="sites"))
  ss.sites$plot<-substr(row.names(ss.sites),1,3)
  ss.sites$year<-substr(row.names(ss.sites),4,7)
  names(ss.sites)[1:2]<-c(paste(name, "nmds1"),"nmds2")
  
}

set.seed(10)#seed was taken from rnorm*10 to make the code a bit more replicable
lichnmds.ss<-metaMDS(lichall.df.ss, try=100)#may tries because solutions for bryo and vasc were unstable. time is on axis 2, so what has it put on axis 1?
lichnmds.ss.sites<-as.data.frame(scores(lichnmds.ss, display="sites"))
lichnmds.ss.sites$plot<-substr(row.names(lichnmds.ss.sites),1,3)
lichnmds.ss.sites$year<-substr(row.names(lichnmds.ss.sites),4,7)
names(lichnmds.ss.sites)[1:2]<-c("lichnmds1","lichnmds2")

set.seed(2)#taken from rnorm*10
bryonmds.ss<-metaMDS(bryoall.df.ss, try=100)#many tries because it was struggling to converge
bryonmds.ss.sites<-as.data.frame(scores(bryonmds.ss, display="sites"))
bryonmds.ss.sites$plot<-substr(row.names(bryonmds.ss.sites),1,3)
bryonmds.ss.sites$year<-substr(row.names(bryonmds.ss.sites),4,7)
names(bryonmds.ss.sites)[1:2]<-c("bryonmds1","bryonmds2")

set.seed(1)#taken from rnorm*10
vascnmds.ss<-metaMDS(vascall.df.ss, try=100)#many tries because it was struggling to converge.
vascnmds.ss.sites<-as.data.frame(scores(vascnmds.ss, display="sites"))
vascnmds.ss.sites$plot<-substr(row.names(vascnmds.ss.sites),1,3)
vascnmds.ss.sites$year<-substr(row.names(vascnmds.ss.sites),4,7)
names(vascnmds.ss.sites)[1:2]<-c("vascnmds1","vascnmds2")

ords.ss.sites<-merge(lichnmds.ss.sites, Summaries.ss.PP[,c("Picea1992", "Picea2015","Transition")], by.x="plot", by.y=0)
rownames(ords.ss.sites)<-paste(ords.ss.sites$plot, ords.ss.sites$year, sep="")
ords.ss.sites<-merge(ords.ss.sites, bryonmds.ss.sites, by=0)
#rownames(ords.ss.sites)<-paste(ords.ss.sites$plot, ords.ss.sites$year, sep="")#right now this doesn't run but the merge doesn't fail.
ords.ss.sites<-merge(ords.ss.sites, vascnmds.ss.sites, by.x="Row.names", by.y=0)
#We have to make it understand that 3 is first in order:
ords.ss.sites$Picea1992<-factor(ords.ss.sites$Picea1992, levels = 3:1)
ords.ss.sites$Picea2015<-factor(ords.ss.sites$Picea2015, levels = 3:1)

library(ggplot2)
#first, all at once. this is currently not the subset!!
x11()
plotty<-ggplot(giantnmds.sites, aes(NMDS1, NMDS2, colour=year))+  geom_point()
plotty+facet_grid(-Picea1992~-Picea2015)
savePlot("PP NMDS facets_all groups.emf", type="emf")
savePlot("PP NMDS facets_all groups.pdf", type="pdf")#still needs to be done: mess with the aesthetics to get crosshairs, get the faceting variable on each side not just the values of the faceting variable, can it make blank space for facets containing no data? Lichens wise, does it plot axis 1 as the dominant? Because for lichens it plots time as axis 2. 

#then a slightly crazy double-nested plot. 
#plot.lich<-ggplot(ords.ss.sites, aes(lichnmds1, lichnmds2, colour=year))+  geom_point()+facet_grid(-Picea1992~-Picea2015)+ coord_fixed()
library(cowplot)#cowplot is an extension to ggplot, particularly good for multiple plots

plot.bryo<-ggplot(ords.ss.sites, aes(bryonmds1, bryonmds2, colour=year))+  
  geom_point()+
  scale_colour_manual(values=c("#3873AE","#EF9335"))+
  facet_grid(Picea1992~Picea2015)+ 
  coord_fixed()+
  labs(x = "NMDS axis 1", y = "NMDS axis 2")+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5), legend.position="bottom")+
  geom_hline(aes(yintercept=0))+
  geom_vline(aes(xintercept=0))+
  labs(colour="Year of survey")

plot.vasc<-plot.bryo %+% ords.ss.sites+aes(vascnmds1, vascnmds2, colour=year)+
  labs(x="NMDS axis 1", y="NMDS axis 2") #%+% is a magic pipe which means 'take the code for this'



source("strippyfunction.R")

legend_b <- get_legend(plot.bryo + theme(legend.position="bottom"))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).

PP_two_nmds_ss<-plot_grid(strippyfunction(p=plot.bryo+theme_cowplot(font_size = 11)+ theme(legend.position="none")), strippyfunction(p=plot.vasc+theme_cowplot(font_size = 11)+ theme(legend.position="none")), ncol = 2, nrow = 1, labels=c("a) Bryophytes", "b) Vascular plants"), label_size=11)
p <- plot_grid(PP_two_nmds_ss, legend_b, ncol = 1, rel_heights = c(1, .03))


#does not plot visibly. This had to be used because facets overrides mfrow and it gets confused.
x11(12,6)
p
save_plot("PP_two_nmds_ss.png", plot = PP_two_nmds_ss, ncol=2, nrow=1)# save_plot is cowplot's version of savePlot. It's very clever, it assumes all plots are 1:1 x:y ratio (you can change that) then works out the aspect ratio for the whole thing from there.
save_plot("PP_two_nmds_ss.pdf", plot = PP_two_nmds_ss, ncol=2, nrow=1)


#I want to add strips for year, 
#gridExtra::grid.arrange(PP_two_nmds_ss, top = "1992 Picea abies frequency score", right = "2015 Picea abies frequency score")#works but is too quick and dirty: they are right on the edges of the plot area, above the plot titles
#this method is from http://stackoverflow.com/questions/36941197/overall-label-for-facets but doesn't work at the moment - right strip disappears and top strip covers the facet labels. This may get easier on a bigger screen
library(RColorBrewer)
library(grid)
library(gtable)

extrastrips<-function(z)





####exploratory analyses for ordination problems
lich.ss.pcoa<-capscale(BCdist.lichens~1, eig=TRUE)
barplot(as.vector(eigenvals(lich.ss.pcoa)))
screeplot(lich.ss.pcoa)
bryo.ss.pcoa<-capscale(BCdist.bryos~1, eig=TRUE)
screeplot(bryo.ss.pcoa)
vasc.ss.pcoa<-capscale(BCdist.vascs~1, eig=TRUE)
screeplot(vasc.ss.pcoa)
#suggests two axes. Increase iterations or use parametric ordination.
