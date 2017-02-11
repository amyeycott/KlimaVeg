## ---- Maps

library("assertthat")

#this is an attempt to re-create the maps of Falinski.
source("turnover_subset.R")

coloury <- data.frame(
  Phytosociology_Latin = c("CA","CelA","PP","PQ","QP","TC"),
  Colour_softer = c("lightskyblue1", "darkorchid","lightgoldenrod","indianred3","darkgoldenrod2","forestgreen"),
  Community_in_1992 = c("Streamside alder-ash forest", "Black alder bog forest","Mesotrophic pine forest","Meso-oligotrophic mixed for.", "Spruce forest", "Mixed deciduous forest"), 
  Colour_bolder = c("lightskyblue", "darkviolet","lightgoldenrod","firebrick","orange","green4"), 
  stringsAsFactors = FALSE
  )

#mapbase for the subset
mapbase.ss<-summaries.ss
mapbase.ss$plot<-rownames(mapbase.ss)
library(dplyr)
mapbase.ss<-left_join(mapbase.ss[,c("plot","dominant","byrow","bycol")], coloury, by = c("dominant" = "Phytosociology_Latin"))
assert_that(all(mapbase.ss$plot == rownames(vascNew.fat[!rownames (vascNew.fat)%in%dodgysquares,])))#If this does not retunr 'true' then the next steps are illegal
mapbase.ss$Picea1992<-vascOld.fat$Picea_abies[!rownames (vascOld.fat)%in%dodgysquares]
mapbase.ss$Picea2015<-vascNew.fat$Picea_abies[!rownames (vascNew.fat)%in%dodgysquares]

#summaries.maps for not using the subset
Summaries$byrow<-as.numeric(as.factor(substr(rownames(Summaries),1,1)))#yuck
Summaries$bycol<-as.numeric(substr(rownames(Summaries),2,3))
Summaries$plot<-rownames(Summaries)
summaries.maps<-left_join(Summaries[,c("dominant","byrow","bycol","plot")], coloury, by = c("dominant" = "Phytosociology_Latin"))#left_join is nicer than merge as it preserved the row order (at least in this case) This is broken

assert_that(all(summaries.maps$plot == rownames(vascNew.fat))) # will throw error if rownames are not identical
summaries.maps$Viola<-vascNew.fat$Viola_riviniana#this is risky - it depends on there being the same order of plots between vascNew.fat and summaries.ss. Always check first that the previous line returns TRUE!
summaries.maps$Viscum_new<-vascNew.fat$Viscum_album
summaries.maps$Picea1992<-vascOld.fat$Picea_abies
summaries.maps$Picea2015<-vascNew.fat$Picea_abies

x11();par(pin=c(2.2,3.0))    
plot(byrow~bycol, data=summaries.maps, col=summaries.maps$Colour_softer, pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.maps, cex=(summaries.maps$Viola*0.75), pch=16)
savePlot("Very basic viola map_wholecrypto.png", type="png")
plot(byrow~bycol, data=summaries.maps, col=as.character(summaries.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.maps, cex=(summaries.maps$Viscum*0.75), pch=16)
savePlot("Very basic viscum map_wholecrypto.png", type="png")

#For PP
x11(6,5);par(mfrow=c(1,2), pin=c(2.0,2.8), xpd=NA, mar=c(8,2,1,1), mgp=c(1,0.2,0), las=1, tcl=0)
plot(byrow~bycol, data=mapbase.ss, col=as.character(mapbase.ss$Colour_softer), pch=15, cex=2.5, cex.axis=0.8, xlab="", ylab="", yaxt="n", xaxp=c(1,max(mapbase.ss$bycol),9))
text(x=8.5, y=14, "1992", adj=0)
axis(side=2, at=1:max(mapbase.ss$byrow), labels=LETTERS[1:max(mapbase.ss$byrow)], cex.axis=0.8)
points(byrow~bycol, data=mapbase.ss, cex=(mapbase.ss$Picea1992*0.66), pch=16)
plot(byrow~bycol, data=mapbase.ss[mapbase.ss$Picea1992!=mapbase.ss$Picea2015,], col=as.character(mapbase.ss$Colour_softer[mapbase.ss$Picea1992!=mapbase.ss$Picea2015]), pch=15, cex=2.4,cex.axis=0.8, xlab="", ylab="", ylim=c(1,max(mapbase.ss$byrow)), xlim=c(1,max(mapbase.ss$bycol)), yaxt="n", xaxp=c(1,max(mapbase.ss$bycol),9))
axis(side=2, at=1:max(mapbase.ss$byrow), labels=LETTERS[1:max(mapbase.ss$byrow)], cex.axis=0.8)
points(byrow~bycol, data=mapbase.ss[mapbase.ss$Picea1992!=mapbase.ss$Picea2015,], cex=(mapbase.ss$Picea2015[mapbase.ss$Picea1992!=mapbase.ss$Picea2015]*0.66), pch=16)
text(x=4, y=14, "2015 (changes only)", adj=0)
legend(x=-13, y=-1, legend=coloury$Community_in_1992, fill=coloury$Colour_softer, ncol=2, title="Phytosociological classification in 1992", cex=0.8)
legend(x=4.5, y=-1, legend=c( "> 10 individuals", "6 - 10 individuals","5 or fewer individuals"), pch=16, title="Spruce frequency", cex=0.8, pt.cex=c(3*0.66, 2*0.66, 1*0.66))

savePlot("PP Picea map_new 5th feb 2017.png", type="png")
savePlot("PP Picea map_new 5th feb 2017.pdf", type="pdf")

#sanity check, are these the right plots?
rownames(vascOld.fat[vascOld.fat$Picea_abies-vascNew.fat$Picea_abies!=0,])
#####loop to create all maps####
#possible ways to proceed: merge mapbase onto each year, then call a loop of one year then the other. But how will this handle species in only one dataset? Other option, use the both-year data and merge mapbase on with repeats, then call a loop on one year subset then the other.
comp$plot<-substr(rownames(comp), 1,3)
lichens.maps<-merge (comp, summaries.maps, by.x = "plot", by.y="plot")
head(lichens.maps[(length(lichens.maps)-5):(length(lichens.maps))])
lichens.maps$byrow<-LETTERS[lichens.maps$byrow]
#lichens.maps[lichens.maps==0]<-NA#needed to make the zero points disappear in scale_size_area
names(lichens.maps)<-gsub(" ","_",names(lichens.maps))

library(ggplot2)
g<-ggplot(lichens.maps, aes(x=as.factor(bycol), y=byrow, fill=dominant, size=as.factor(Acrocordia_gemmata)))+
  theme(panel.grid = element_blank(), panel.background = element_blank())+
  geom_tile(colour="black", size=0.5, linetype="dashed")+
  geom_point()+
  scale_fill_manual(limits=coloury$Phytosociology_Latin, values=coloury$Colour_softer, labels=coloury$Community_in_1992)+
  scale_size_manual(breaks = c(1,2,3), limits=c(1,2,3), values=c(0.5,1.5,3))+#this stops it from displaying zeros and allows us to build our own size for the dots - the original had too similar between 2 and 3
  facet_wrap(~Year)+
  coord_equal()+
  labs(x="",y="",fill="Forest type in 1992", size="Frequency")+
  ggtitle("Acrocordia gemmata")
g
## ---- lichens_maps
for (i in gsub(" ","_", unique(c(new.harm.db$Species, old.harm.db$Species)))) {
g+aes_string(size=paste0("as.factor(",i,")"))+
    ggtitle(gsub("_"," ", i))+
    labs(size="Frequency")
  #ggsave(paste0("Maps/",i,".png"))
  print(g)
}
