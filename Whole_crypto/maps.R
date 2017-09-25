## ---- Maps

library("assertthat")

#this is an attempt to re-create the maps of Falinski.
source("turnover_subset.R")



#set up the coordinates for the squares
#mapbase for the subset
mapbase.ss<-summaries.ss#all columns so we can look at change in richness too
mapbase.ss$plot<-rownames(summaries.ss)
mapbase.ss$byrow<-as.numeric(as.factor(substr(mapbase.ss$plot,1,1)))#yuck
mapbase.ss$bycol<-as.numeric(substr(mapbase.ss$plot,2,3))
library(dplyr)
mapbase.ss<-left_join(mapbase.ss, coloury, by = c("dominant" = "Phytosociology_Latin"))
assert_that(all(mapbase.ss$plot == rownames(vascNew.fat[!rownames (vascNew.fat)%in%dodgysquares,])))#If this does not retunr 'true' then the next steps are illegal
mapbase.ss$Picea1990<-vascOld.fat$Picea_abies[!rownames (vascOld.fat)%in%dodgysquares]
mapbase.ss$Picea2015<-vascNew.fat$Picea_abies[!rownames (vascNew.fat)%in%dodgysquares]

#summaries.maps for not using the subset
summaries.maps<-Summaries[,c("dominant", "ncomms")]#this step preserves row names
summaries.maps$byrow<-as.numeric(as.factor(substr(rownames(summaries.maps),1,1)))#yuck
summaries.maps$bycol<-as.numeric(substr(rownames(summaries.maps),2,3))
summaries.maps$plot<-rownames(summaries.maps)
summaries.maps<-left_join(summaries.maps, coloury, by = c("dominant" = "Phytosociology_Latin"))#left_join is nicer than merge. But today is is losing row names.
assert_that(all(summaries.maps$plot == rownames(vascNew.fat))) # will throw error if rownames are not identical
summaries.maps$Viola<-vascNew.fat$Viola_riviniana#this is risky - it depends on there being the same order of plots between vascNew.fat and summaries.ss. Always check first that the previous line returns TRUE!
summaries.maps$Viscum_new<-vascNew.fat$Viscum_album
summaries.maps$Picea1990<-vascOld.fat$Picea_abies
summaries.maps$Picea2015<-vascNew.fat$Picea_abies

#For PP
x11(6,5);par(mfrow=c(1,2), pin=c(2.0,2.8), xpd=NA, mar=c(8,2,1,1), mgp=c(1,0.2,0), las=1, tcl=0)
plot(byrow~bycol, data=mapbase.ss, col=as.character(mapbase.ss$Colour_softer), pch=15, cex=2.5, cex.axis=0.8, xlab="", ylab="", yaxt="n", xaxp=c(1,max(mapbase.ss$bycol),9))
text(x=8.5, y=14, "1990", adj=0)
axis(side=2, at=1:max(mapbase.ss$byrow), labels=unique(substr(mapbase.ss$plot,1,1)), cex.axis=0.8)
points(byrow~bycol, data=mapbase.ss, cex=(mapbase.ss$Picea1990*0.66), pch=16)
plot(byrow~bycol, data=mapbase.ss[mapbase.ss$Picea1990!=mapbase.ss$Picea2015,], col=as.character(mapbase.ss$Colour_softer[mapbase.ss$Picea1990!=mapbase.ss$Picea2015]), pch=15, cex=2.4,cex.axis=0.8, xlab="", ylab="", ylim=c(1,max(mapbase.ss$byrow)), xlim=c(1,max(mapbase.ss$bycol)), yaxt="n", xaxp=c(1,max(mapbase.ss$bycol),9))
axis(side=2, at=1:max(mapbase.ss$byrow), labels=unique(substr(mapbase.ss$plot,1,1)), cex.axis=0.8)
points(byrow~bycol, data=mapbase.ss[mapbase.ss$Picea1990!=mapbase.ss$Picea2015,], cex=(mapbase.ss$Picea2015[mapbase.ss$Picea1990!=mapbase.ss$Picea2015]*0.66), pch=16)
text(x=4, y=14, "2015 (changes only)", adj=0)
legend(x=-13, y=-1, legend=coloury$Community_in_1990, fill=coloury$Colour_softer, ncol=2, title="Phytosociological classification in 1990", cex=0.8)
legend(x=4.5, y=-1, legend=c( "> 10 individuals", "6 - 10 individuals","5 or fewer individuals"), pch=16, title="Spruce frequency", cex=0.8, pt.cex=c(3*0.66, 2*0.66, 1*0.66))

savePlot("PP Picea map_new 5th feb 2017.png", type="png")
savePlot("PP Picea map_new 5th feb 2017.pdf", type="pdf")

#sanity check, are these the right plots?
rownames(vascOld.fat[vascOld.fat$Picea_abies-vascNew.fat$Picea_abies!=0,])

#For richness analyses
x11(6,4);par(mfrow=c(1,3), pin=c(2.0,2.8), xpd=NA, mar=c(8,2,1,1), mgp=c(1,0.2,0), las=1, tcl=0)
plot(byrow~bycol, data=mapbase.ss, col=as.character(mapbase.ss$Colour_softer), pch=15, cex=2.5, cex.axis=0.8, xlab="", ylab="", yaxt="n", xaxp=c(1,max(mapbase.ss$bycol),9))
text(x=5, y=14, "Lichens", adj=0, cex=1.2)
axis(side=2, at=1:max(mapbase.ss$byrow), labels=unique(substr(mapbase.ss$plot,1,1)), cex.axis=0.8)
points(byrow~bycol, data=mapbase.ss[(mapbase.ss$lich.rich2015/mapbase.ss$lich.rich1990)>1,], 
       cex=mapbase.ss$lich.rich2015[(mapbase.ss$lich.rich2015/mapbase.ss$lich.rich1990)>1]/mapbase.ss$lich.rich1990[(mapbase.ss$lich.rich2015/mapbase.ss$lich.rich1990)>1], pch=16)
points(byrow~bycol, data=mapbase.ss[(mapbase.ss$lich.rich2015/mapbase.ss$lich.rich1990)<1,], 
       cex=mapbase.ss$lich.rich1990[(mapbase.ss$lich.rich2015/mapbase.ss$lich.rich1990)<1]/mapbase.ss$lich.rich2015[(mapbase.ss$lich.rich2015/mapbase.ss$lich.rich1990)<1], pch=1)
plot(byrow~bycol, data=mapbase.ss, col=as.character(mapbase.ss$Colour_softer), pch=15, cex=2.5, cex.axis=0.8, xlab="", ylab="", yaxt="n", xaxp=c(1,max(mapbase.ss$bycol),9))
text(x=5, y=14, "Bryophytes", adj=0, cex=1.2)
axis(side=2, at=1:max(mapbase.ss$byrow), labels=unique(substr(mapbase.ss$plot,1,1)), cex.axis=0.8)
points(byrow~bycol, data=mapbase.ss[(mapbase.ss$bryo.rich2015/mapbase.ss$bryo.rich1990)>1,], 
       cex=mapbase.ss$bryo.rich2015[(mapbase.ss$bryo.rich2015/mapbase.ss$bryo.rich1990)>1]/mapbase.ss$bryo.rich1990[(mapbase.ss$bryo.rich2015/mapbase.ss$bryo.rich1990)>1], pch=16)
points(byrow~bycol, data=mapbase.ss[(mapbase.ss$bryo.rich2015/mapbase.ss$bryo.rich1990)<1,], cex=mapbase.ss$bryo.rich1990[(mapbase.ss$bryo.rich2015/mapbase.ss$bryo.rich1990)<1]/mapbase.ss$bryo.rich2015[(mapbase.ss$bryo.rich2015/mapbase.ss$bryo.rich1990)<1], pch=1)
plot(byrow~bycol, data=mapbase.ss, col=as.character(mapbase.ss$Colour_softer), pch=15, cex=2.5, cex.axis=0.8, xlab="", ylab="", yaxt="n", xaxp=c(1,max(mapbase.ss$bycol),9))
text(x=5, y=14, "Vascular plants", adj=0, cex=1.2)
axis(side=2, at=1:max(mapbase.ss$byrow), labels=unique(substr(mapbase.ss$plot,1,1)), cex.axis=0.8)
points(byrow~bycol, data=mapbase.ss[(mapbase.ss$vasc.rich2015/mapbase.ss$vasc.rich1990)>1,], cex=
         mapbase.ss$vasc.rich2015[(mapbase.ss$vasc.rich2015/mapbase.ss$vasc.rich1990)>1]/mapbase.ss$vasc.rich1990[(mapbase.ss$vasc.rich2015/mapbase.ss$vasc.rich1990)>1], pch=16)
points(byrow~bycol, data=mapbase.ss[(mapbase.ss$vasc.rich2015/mapbase.ss$vasc.rich1990)<1,], 
       cex=mapbase.ss$vasc.rich1990[(mapbase.ss$vasc.rich2015/mapbase.ss$vasc.rich1990)<1]/mapbase.ss$vasc.rich2015[(mapbase.ss$vasc.rich2015/mapbase.ss$vasc.rich1990)<1], pch=1)
legend(x=-20, y=-1, legend=coloury$Community_in_1990, fill=coloury$Colour_softer, ncol=2, title="Phytosociological classification in 1990", cex=1.5)
savePlot("Net richness change mapped.png", type="png")



#####loop to create all maps####
#possible ways to proceed: merge mapbase onto each year, then call a loop of one year then the other. But how will this handle species in only one dataset? Other option, use the both-year data and merge mapbase on with repeats, then call a loop on one year subset then the other.
comp$plot<-substr(rownames(comp), 1,3)
lichens.maps<-merge (comp, summaries.maps, by.x = "plot", by.y="plot")
head(lichens.maps[(length(lichens.maps)-5):(length(lichens.maps))])
lichens.maps$byrow<-substr(lichens.maps$plot,1,1)
#lichens.maps[lichens.maps==0]<-NA#needed to make the zero points disappear in scale_size_area
names(lichens.maps)<-gsub(" ","_",names(lichens.maps))

bryo.fat$plot<-substr(rownames(bryo.fat), 1,3)
bryos.maps<-merge(bryo.fat, summaries.maps, by.x="plot", by.y="plot")
bryos.maps$byrow<-substr(bryos.maps$plot,1,1)
names(bryos.maps)<-gsub(" ","_",gsub("-","__",names(bryos.maps)))
bryos.maps$Year<-substr(rownames(bryo.fat), 4,7)

vascall.df$plot<-substr(rownames(vascall.df), 1,3)
vascall.df$Year<-substr(rownames(vascall.df), 4,7)
vascs.maps<-merge(vascall.df, summaries.maps, by="plot")
vascs.maps$byrow<-substr(vascs.maps$plot,1,1)
names(vascs.maps)<-gsub(" ","_",gsub("-","__",names(vascs.maps)))

library(ggplot2)#g sets up plots with one year and then another.
g<-ggplot(lichens.maps, aes(x=as.factor(bycol), y=byrow, fill=dominant, size=as.factor(Acrocordia_gemmata)))+
  theme(panel.grid = element_blank(), panel.background = element_blank())+
  geom_tile(colour="black", size=0.5, linetype="dashed")+
  geom_point()+
  scale_fill_manual(limits=coloury$Phytosociology_Latin, values=coloury$Colour_softer, labels=coloury$Community_in_1990)+
  scale_size_manual(breaks = c(1,2,3), limits=c(1,2,3), values=c(0.5,1.5,3))+#this stops it from displaying zeros and allows us to build our own size for the dots - the default was too similar between 2 and 3
  facet_wrap(~Year)+
  coord_equal()+
  labs(x="",y="",fill="Forest type in 1990", size="Frequency")+
  ggtitle("Acrocordia gemmata")
g#sets up the plot, using the first species as an example



## ---- lichens_maps
for (i in gsub(" ","_", unique(c(new.harm.db$Species, old.harm.db$Species)))) {
h<-g+aes_string(size=paste0("as.factor(",i,")"))+
    ggtitle(gsub("_"," ", i))+
    labs(size="Frequency")
  #ggsave(paste0("Maps/",i,".png"))
  print(h)
}

## ---- bryophytes_maps

for (i in gsub(" ","_", gsub("-","__",unique(bryophytes$Species_name)))) {
  h<-g %+% bryos.maps+aes_string(size=paste0("as.factor(",i,")"))+
    ggtitle(gsub("_"," ", gsub("__","-",i)))+
    labs(size="Frequency")
  #ggsave(paste0("Maps/",i,".png"))
  print(h)
}

## ---- vascularplants_maps

for (i in gsub(" ","_", gsub("-","__",unique(c(vascOld.thin$Species_name, vascNew.thin$Species_name_2015))))) {
  h<-g %+% vascs.maps+aes_string(size=paste0("as.factor(",i,")"))+
    ggtitle(gsub("_"," ", gsub("__","-",i)))+
    labs(size="Frequency")
  #ggsave(paste0("Maps/",i,".png"))
  print(h)
}

#k sets up plots for lichens-bryos-vasculars (e.g. richchange)
library(tidyr)
for.k.plots<-cbind(gather(mapbase.ss[,c("byrow","bycol","dominant","plot","Community_in_1990","Colour_softer","Colour_bolder","lich.BCdiss", "bryo.BCdiss","vasc.BCdiss")],key=group, value=BCdiss,lich.BCdiss, bryo.BCdiss,vasc.BCdiss), gather(mapbase.ss[,c("lich.rich1990", "bryo.rich1990","vasc.rich1990")],key=notneeded, value=rich1990),gather(mapbase.ss[,c("lich.rich2015", "bryo.rich2015","vasc.rich2015")],key=ignoreme, value=rich2015))
for.k.plots$group<-substr(for.k.plots$group,1,4)
for.k.plots<-subset(for.k.plots,select=-c(notneeded,ignoreme))
for.k.plots$richdiff<-for.k.plots$rich2015-for.k.plots$rich1990
for.k.plots$group<-factor(for.k.plots$group)
levels(for.k.plots$group) <- list(Lichens="lich",Bryophytes="bryo", Vascular_plants="vasc")

x11(6,3)
k<-ggplot(for.k.plots, aes(x=as.factor(bycol), y=byrow, fill=dominant, size=BCdiss/2))+
  theme(panel.grid = element_blank(), panel.background = element_blank())+
  geom_tile(colour="black", size=0.5, linetype="dashed")+
  geom_point()+
  scale_fill_manual(limits=coloury$Phytosociology_Latin, values=coloury$Colour_softer, labels=coloury$Community_in_1990)+
  facet_wrap(~group)+
  coord_equal()+
  labs(x="",y="",fill="Forest type in 1990", size="Frequency")+
  ggtitle("Bray-Curtis dissimilarity")
k


x11(6,3)#plot m: with filled circles for positive numbers and hollow circles for negative ones. Negative ones were plotting fine in a previous version where I wasn't attempting unfilled circles. Comented out so that the markdown runs
#m<-ggplot(data=for.k.plots, aes(x=as.factor(bycol), y=byrow, fill=dominant)+
#  geom_point(aes(as.factor(bycol)[richdiff>0], byrow[richdiff>0], size=richdiff))+
#  geom_point(aes(as.factor(bycol)[richdiff<0], byrow[richdiff<0], size=richdiff, fill=NULL))+
#  theme(panel.grid = element_blank(), panel.background = element_blank())+
#  geom_tile(colour="black", size=0.5, linetype="dashed")+
#  scale_fill_manual(limits=coloury$Phytosociology_Latin, values=coloury$Colour_softer, labels=coloury$Community_in_1990)+
#  facet_wrap(~group)+
#  coord_equal()+
#  labs(x="",y="",fill="Forest type in 1990", size="Frequency")+
#  ggtitle("Change in species richness")) #Error in layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint. Was using code from http://stackoverflow.com/questions/8583594/modifying-the-shape-for-a-subset-of-points-with-ggplot2
#m

p <- ggplot(data = mtcars)+
  geom_point(aes(wt, mpg, shape=as.factor(cyl), colour=gear, size=carb))+
  geom_point(aes(wt[carb==8], mpg[carb==8]), colour="black", shape=1, size=7)  

k+aes(size=richdiff)+ggtitle("Change in species richness")
