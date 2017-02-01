#this is an attempt to re-create the maps of Falinski.
head(summaries.ss)
summaries.ss$byrow<-as.numeric(as.factor(substr(rownames(summaries.ss),1,1)))#yuck
summaries.ss$bycol<-as.numeric(substr(rownames(summaries.ss),2,3))
coloury<-cbind(c("CA","CelA","PP","PQ","QP","TC"),c("lightskyblue", "darkviolet","lightgoldenrod","firebrick2","orange","green4"))

#mapbase I'm using for 'proper' programming
mapbase.ss<-summaries.ss
mapbase.ss$plot<-rownames(mapbase.ss)
mapbase.ss<-merge(mapbase.ss[,c("plot","dominant","byrow","bycol")], coloury, by.x="dominant", by.y=1)
names(mapbase.ss)<-c("Dominant_community_1992","Plot","Row","Column","Colourcode")

#summaries.ss.maps$Viola<-vascNew.fat$Viola_riviniana[!rownames(vascNew.fat)%in%dodgysquares]#this is risky - it depends on there being the same order of plots between vascNew.fat and summaries.ss. Always check first!
#summaries.ss.maps$Viscum<-vascNew.fat$Viscum_album[!rownames(vascNew.fat)%in%dodgysquares]

x11();par(pin=c(2.0,2.8))               
plot(byrow~bycol, data=summaries.ss.maps, col=as.character(summaries.ss.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.ss.maps, cex=(summaries.ss.maps$Viola*0.75), pch=16)
savePlot("Very basic viola map_subset.png", type="png")
plot(byrow~bycol, data=summaries.ss.maps, col=as.character(summaries.ss.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.ss.maps, cex=(summaries.ss.maps$Viscum*0.75), pch=16)
savePlot("Very basic viscum map_subset.png", type="png")

#summaries.maps I'm using to play so that I don't break anything
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
x11();par(mfrow=c(1,2), pin=c(2.2,3.0), xpd=NA, mar=c(8,3,1,0.1), mgp=c(2,0.5,0))
plot(byrow~bycol, data=summaries.maps, col=as.character(summaries.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
text(x=10, y=15, "1992")
points(byrow~bycol, data=summaries.maps, cex=(summaries.maps$Picea1992*0.75), pch=16)
plot(byrow~bycol, data=summaries.maps, col=as.character(summaries.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.maps, cex=(summaries.maps$Picea2015*0.75), pch=16)
text(x=10, y=15, "2015")
legend(x=-6, y=-2, legend=coloury[,1], fill=coloury[,2], ncol=3, title="Phytosociological classification in 1992")

savePlot("PP Very basic Picea map_wholecrypto.png", type="png")


#####loop to create all maps####
#possible ways to proceed: merge mapbase onto each year, then call a loop of one year then the other. But how will this handle species in only one dataset? Other option, use the both-year data and merge mapbase on with repeats, then call a loop on one year subset then the other.
comp$plot<-substr(rownames(comp), 1,3)
lichens.maps<-merge (comp, mapbase.ss, by.x = "plot", by.y="Plot")
lichens.maps<-lichens.maps
head(lichens.maps[(length(lichens.maps)-5):(length(lichens.maps))]) 
x11();par(pin=c(2.0,2.8), mfrow=c(1,2))
#so the general idea is to draw the same background for each map, then add points of the sizes in each species' score, 1992 followed by 2015, save that plot before heading back into the loop to make the next pair.
mapply(function(x, plotnames){
  plot(Row~Column, data=lichens.maps, col=as.character(lichens.maps$Colourcode), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
  points(Row~Column, data=lichens.maps[lichens.maps$Year=="1992",], cex=lichens.maps$x, pch=16)
  plot(Row~Column, data=lichens.maps, col=as.character(lichens.maps$Colourcode), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
  points(Column~Row, data=lichens.maps[lichens.maps$Year=="2015",], cex=lichens.maps$x, pch=16)
  savePlot(paste("/Maps/",plotnames, ".png", sep=""), type="png")
  }, x=lichens.maps[2:6], plotnames=names(lichens.maps[2:6]))#short version for testing
#currently gives a popup impossible to open /Maps/Acrocordia gemmata.png so I think I need to work on specifying file location. I tried /, //, \ and \\!


paste("/Maps/", "Testy", ".png", sep="")

#once working, x=lichens.maps[c(2:(length(lichens.maps)-6), length(lichens.maps)-4)], plotnames=names(lichens.maps[c(2:(length(lichens.maps)-6), length(lichens.maps)-4)])) #Note the weird subsetting for lichens.maps because the year column is in an annoying place.

