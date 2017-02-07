#this is an attempt to re-create the maps of Falinski.
source("turover_subset.R")

coloury<-as.data.frame(cbind(c("CA","CelA","PP","PQ","QP","TC"),c("lightskyblue1", "darkorchid","lightgoldenrod","indianred3","darkgoldenrod2","forestgreen"),c("Streamside alder-ash forest", "Black alder bog forest","Mesotrophic pine forest","Meso-oligotrophic mixed for.", "Spruce forest", "Mixed deciduous forest"), c("lightskyblue", "darkviolet","lightgoldenrod","firebrick","orange","green4")))
names(coloury)<-c("Phytosociology_Latin", "Colour_softer","Community in 1992", "Colour_bolder")

#mapbase for the subset
mapbase.ss<-summaries.ss
mapbase.ss$plot<-rownames(mapbase.ss)
mapbase.ss<-merge(mapbase.ss[,c("plot","dominant","byrow","bycol")], coloury, by.x="dominant", by.y=1)
mapbase.ss$Picea1992<-vascOld.fat$Picea_abies[!rownames (vascOld.fat)%in%dodgysquares]
mapbase.ss$Picea2015<-vascNew.fat$Picea_abies[!rownames (vascOld.fat)%in%dodgysquares]

#summaries.maps for not using the subset
Summaries$byrow<-as.numeric(as.factor(substr(rownames(Summaries),1,1)))#yuck
Summaries$bycol<-as.numeric(substr(rownames(Summaries),2,3))
summaries.maps<-merge(Summaries, coloury, by.x="dominant", by.y=1)
summaries.maps$Viola<-vascNew.fat$Viola_riviniana#this is risky - it depends on there being the same order of plots between vascNew.fat and summaries.ss. Always check first!
summaries.maps$Viscum_new<-vascNew.fat$Viscum_album
summaries.maps$Picea1992<-vascOld.fat$Picea_abies
summaries.maps$Picea2015<-vascNew.fat$Picea_abies

x11();par(pin=c(2.2,3.0))    
plot(byrow~bycol, data=summaries.maps, col=as.character(summaries.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
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
plot(byrow~bycol, data=mapbase.ss[mapbase.ss$Picea1992!=mapbase.ss$Picea2015,], col=as.character(mapbase.ss$Colour_softer[mapbase.ss$Picea1992!=mapbase.ss$Picea2015]), pch=15, cex=2.4,cex.axis=0.8, xlab="", ylab="", ylim=c(1,max(mapbase.ss$byrow)), yaxt="n", xaxp=c(1,max(mapbase.ss$bycol),9))
axis(side=2, at=1:max(mapbase.ss$byrow), labels=LETTERS[1:max(mapbase.ss$byrow)], cex.axis=0.8)
points(byrow~bycol, data=mapbase.ss[mapbase.ss$Picea1992!=mapbase.ss$Picea2015,], cex=(mapbase.ss$Picea2015[mapbase.ss$Picea1992!=mapbase.ss$Picea2015]*0.66), pch=16)
text(x=4, y=14, "2015 (changes only)", adj=0)
legend(x=-13, y=-1, legend=coloury[,"Community in 1992"], fill=as.character(coloury[,"Colour_softer"]), ncol=2, title="Phytosociological classification in 1992", cex=0.8)
legend(x=4.5, y=-1, legend=c( "> 10 individuals", "6 - 10 individuals","5 or fewer individuals"), pch=16, title="Spruce frequency", cex=0.8, pt.cex=c(3*0.66, 2*0.66, 1*0.66))

savePlot("PP Picea map_new 5th feb 2017.png", type="png")
savePlot("PP Picea map_new 5th feb 2017.pdf", type="pdf")


#####loop to create all maps####
#possible ways to proceed: merge mapbase onto each year, then call a loop of one year then the other. But how will this handle species in only one dataset? Other option, use the both-year data and merge mapbase on with repeats, then call a loop on one year subset then the other.
comp$plot<-substr(rownames(comp), 1,3)
lichens.maps<-merge (comp, mapbase.ss, by.x = "plot", by.y="plot")
lichens.maps<-lichens.maps
head(lichens.maps[(length(lichens.maps)-5):(length(lichens.maps))]) 

#so the general idea is to draw the same background for each map, then add points of the sizes in each species' score, 1992 followed by 2015, save that plot before heading back into the loop to make the next pair. This currently saves the penultimate pair of maps...
par(mfrow=c(1,2))# is currently ignored. No columns argument in pdf function.
mapply(function(x, plotnames){
  pdf("allthelichenmaps.pdf", width=4.0, height=5.6)
  plot(byrow~bycol, data=lichens.maps, col=as.character(lichens.maps$Colour_softer), pch=15, cex=2.4, xlab="", ylab="", main=paste(plotnames, "1992"))
  points(byrow~bycol, data=lichens.maps[lichens.maps$Year=="1992",], cex=(x)*0.66, pch=16)#lichens.maps$x*0.66 didn't work (gave cex=1 for all point I think), x*0.66 or (x)*0.66 gives "Error: non numeric argument to binary operator"
  plot(byrow~bycol, data=lichens.maps, col=as.character(lichens.maps$Colour_softer), pch=15, cex=2.4, xlab="", ylab="", main=paste(plotnames, "2015"))
  points(byrow~bycol, data=lichens.maps[lichens.maps$Year=="2015",], cex=(x)*0.66, pch=16)}, x=lichens.maps[1:6], plotnames=names(lichens.maps[1:6]))#note short version of lichens for testing
 graphics.off()#dev.off wasn't shutting the pdf properly

#once working, x=lichens.maps[c(2:(length(lichens.maps)-6), length(lichens.maps)-4)], plotnames=names(lichens.maps[c(2:(length(lichens.maps)-6), length(lichens.maps)-4)])) #Note the weird subsetting for lichens.maps because the year column is in an annoying place. Maybe use %in% instead.

