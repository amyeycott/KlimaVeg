source("source data and merging.R")
set.seed()
library(vegan)
library(RColorBrewer)
dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")

#Lichens whole set.
lichen.nmds.1990<-metaMDS(subset(comp_old[!rownames(comp_old)%in%dodgysquares,colSums(comp_old>0)>1], select=-Year), trymax=100)# See notes on the not-subsetted version, but subset does not cure the non-convergence issue.
lichen.nmds.2015<-metaMDS(subset(comp_new[!rownames(comp_new)%in%dodgysquares,colSums(comp_new>0)>1], select=-Year), trymax=100)

####start of the rest of the code how it was intended to be - this script skips all the exploratory stuff
lichen.nmds.2015<-metaMDS(subset(comp[!rownames(comp_old)%in%dodgysquares,], Year==2015, select=-Year))#not converging
lichen.crusty<-protest(lichen.nmds.1990, lichen.nmds.2015)
#Bryophytes whole set
bryo.nmds.1990<-metaMDS(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,])#
bryo.nmds.2015<-metaMDS(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,])#
crusty.bryo<-protest(bryo.nmds.1990, bryo.nmds.2015)#very similar, r=0.98, P=0.001
#vascular whole set
vasc.nmds.1990<-metaMDS(vascOld.fat[!rownames(vascOld.fat)%in%dodgysquares,])#converges
vasc.nmds.2015<-metaMDS(vascNew.fat[!rownames(vascNew.fat)%in%dodgysquares,])#converges
crusty.vasc<-protest(vasc.nmds.1990, vasc.nmds.2015)#very similar
crusty.2015.vascbryo<-protest(vasc.nmds.2015, bryo.nmds.2015)# r=0.95, P=0.001
crusty.1990.vascbryo<-protest(vasc.nmds.1990, bryo.nmds.1990)#very very slightly less similar, but unlikely to be a serious thing: r=0.93, P=0.001

x11(); par(mfrow=c(2,2))
plot(bryo.nmds.1990, main="Bryophytes 1990")
plot(bryo.nmds.2015, main="Bryophytes 2015")
plot(vasc.nmds.1990, main="Vascular plants 1990")
plot(vasc.nmds.2015, main="Vascular plants 2015")


bryo.sppscores.1990<-as.data.frame(scores(bryo.nmds.1990, display="species"))
bryo.sppscores.1990$Light<-bryo.status$L
bryo.sppscores.1990$coloury<-rev(brewer.pal(9, "YlGn"))[bryo.sppscores.1990$Light]
bryo.sppscores.1990$coloury[is.na(bryo.sppscores.1990$coloury)]<-"#6A6C6E"#mid grey for species without an ellenberg for light - of course, this will fail if there's a black and white printed version, but it's easy enough to fix

bryo.sppscores.2015<-as.data.frame(scores(bryo.nmds.2015, display="species"))
bryo.sppscores.2015$Light<-bryo.status$L
bryo.sppscores.2015$coloury<-rev(brewer.pal(9, "YlGn"))[bryo.sppscores.2015$Light]
bryo.sppscores.2015$coloury[is.na(bryo.sppscores.2015$coloury)]<-"#6A6C6E"

vasc.sppscores.1990<-as.data.frame(scores(vasc.nmds.1990, display="species"))
vasc.sppscores.1990$Light<-vasc.status$L
vasc.sppscores.1990$coloury<-rev(brewer.pal(9, "YlGn"))[vasc.sppscores.1990$Light]
vasc.sppscores.1990$coloury[is.na(vasc.sppscores.1990$coloury)]<-"#6A6C6E"#mid grey for species without an ellenberg for light - of course, this will fail if there's a black and white printed version, but it's easy enough to fix

vasc.sppscores.2015<-as.data.frame(scores(vasc.nmds.2015, display="species"))
vasc.sppscores.2015$Light<-####NOT simple as the two vascs are separate objects. Need to merge or filter from vasc.ellen
vasc.sppscores.2015$coloury<-rev(brewer.pal(9, "YlGn"))[vasc.sppscores.2015$Light]
vasc.sppscores.2015$coloury[is.na(vasc.sppscores.2015$coloury)]<-"#6A6C6E"

plot(bryo.nmds.1990, main="Bryophytes 1990", type="n")
points(bryo.sppscores.1990$NMDS1, bryo.sppscores.1990$NMDS2, col=bryo.sppscores.1990$coloury, pch=16)
points(bryo.nmds.1990, display="sites")
plot(bryo.nmds.2015, main="Bryophytes 2015", type="n")
points(bryo.sppscores.2015$NMDS1, bryo.sppscores.2015$NMDS2, col=bryo.sppscores.2015$coloury, pch=16)
points(bryo.nmds.2015, display="sites")#plot has done an annoying flip. Must learn set seed.
plot(vasc.nmds.1990, main="vascphytes 1990", type="n")
points(vasc.sppscores.1990$NMDS1, vasc.sppscores.1990$NMDS2, col=vasc.sppscores.1990$coloury, pch=16)
points(vasc.nmds.1990, display="sites")
plot(vasc.nmds.2015, main="vascphytes 2015", type="n")
points(vasc.sppscores.2015$NMDS1, vasc.sppscores.2015$NMDS2, col=vasc.sppscores.2015$coloury, pch=16)
points(vasc.nmds.2015, display="sites")#plot has done an annoying flip. Must learn set seed.
       
savePlot("nmds bryo and vasc just for show_subset.emf", type="emf")
