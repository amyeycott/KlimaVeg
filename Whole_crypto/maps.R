#this is an attempt to re-create the maps of Falinski.
head(summaries.ss)
summaries.ss$byrow<-as.numeric(as.factor(substr(rownames(summaries.ss),1,1)))#yuck
summaries.ss$bycol<-as.numeric(substr(rownames(summaries.ss),2,3))
coloury<-cbind(c("CA","CelA","PP","PQ","QP","TC"),c("lightskyblue", "darkviolet","lightgoldenrod","firebrick2","orange","green4"))
summaries.ss.maps<-merge(summaries.ss, coloury, by.x="dominant", by.y=1)
summaries.ss.maps$Viola<-vascNew.fat$Viola_riviniana[!rownames(vascNew.fat)%in%dodgysquares]#this is risky - it depends on there being the same order of plots between vascNew.fat and summaries.ss. Always check first!
summaries.ss.maps$Viscum<-vascNew.fat$Viscum_album[!rownames(vascNew.fat)%in%dodgysquares]

x11();par(pin=c(2.0,2.8))               
plot(byrow~bycol, data=summaries.ss.maps, col=as.character(summaries.ss.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.ss.maps, cex=(summaries.ss.maps$Viola*0.75), pch=16)
savePlot("Very basic viola map_subset.png", type="png")
plot(byrow~bycol, data=summaries.ss.maps, col=as.character(summaries.ss.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.ss.maps, cex=(summaries.ss.maps$Viscum*0.75), pch=16)
savePlot("Very basic viscum map_subset.png", type="png")

Summaries$byrow<-as.numeric(as.factor(substr(rownames(Summaries),1,1)))#yuck
Summaries$bycol<-as.numeric(substr(rownames(Summaries),2,3))
summaries.maps<-merge(Summaries, coloury, by.x="dominant", by.y=1)
summaries.maps$Viola<-vascNew.fat$Viola_riviniana#this is risky - it depends on there being the same order of plots between vascNew.fat and summaries.ss. Always check first!
summaries.maps$Viscum<-vascNew.fat$Viscum_album

x11();par(pin=c(2.2,3.0))    
plot(byrow~bycol, data=summaries.maps, col=as.character(summaries.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.maps, cex=(summaries.maps$Viola*0.75), pch=16)
savePlot("Very basic viola map_wholecrypto.png", type="png")
plot(byrow~bycol, data=summaries.maps, col=as.character(summaries.maps$V2), pch=15, pin=c(2,2), cex=2.4, xlab="", ylab="")
points(byrow~bycol, data=summaries.maps, cex=(summaries.maps$Viscum*0.75), pch=16)
savePlot("Very basic viscum map_wholecrypto.png", type="png")

#####loop to create all maps####
#first check that in all the data frames we will use, the plots are in the same order. 
summaries.maps$plot<-rownames(Summaries)
#Was that step ok?
Summaries.maps$bycol!=
  
  
  




