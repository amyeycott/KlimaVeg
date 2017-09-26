source("source data and merging.R")
set.seed(5)# 5 seed makes vasc 2015 not converge, even thugh it used to be the most converge-able.
library(vegan)
library(RColorBrewer)
library(tidyverse)
#on a new computer
#library(devtools)
#install_github("gavinsimpson/ggvegan")#,
library(ggvegan)

dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")
set.seed(5)


lichen.nmds.1990.k3<-metaMDS(subset(comp_old[!rownames(comp_old)%in%dodgysquares,colSums(comp_old>0)>1], select=-Year), k=3, trymax=100)# See notes on the not-subsetted version, but subset does not cure the non-convergence issue. 3 axes does, though the stress isn't much lower than the 2 axis version. 

#showing Richard that arrows aren't the way forward
lichen.nmds.both<-metaMDS(subset(comp[!rownames(comp)%in%dodgysquares,colSums(comp>0)>1], select=-Year), trymax=100)#putting both years in together also helps.
lich.nmds.both.fortified<-fortify(lichen.nmds.both)
lich.nmds.both.sites<-filter(lich.nmds.both.fortified, Score=="sites")
lich.nmds.both.species<-filter(lich.nmds.both.fortified, Score=="species")
lich.nmds.both.sites$Year<-substr(lich.nmds.both.sites$Label,4,7)
g<-ggplot(lich.nmds.both.sites, aes(x=Dim1, y=Dim2, colour=Year))+
  geom_point()+
  coord_equal()



testy<-select(filter(lich.nmds.both.fortified, Score=="sites"), Label)
testy2<-substr(testy, 4,7)
          
####start of the rest of the code how it was intended to be - this script skips all the exploratory stuff
lichen.nmds.1990<-metaMDS(subset(comp_old[!rownames(comp_old)%in%dodgysquares,colSums(comp_old>0)>1], select=-Year), trymax=100)# See notes on the not-subsetted version, but subset does not cure the non-convergence issue.
lichen.nmds.2015<-metaMDS(subset(comp_new[!rownames(comp_new)%in%dodgysquares,colSums(comp_new>0)>1], select=-Year), trymax=100)
lichen.crusty<-protest(lichen.nmds.1990, lichen.nmds.2015)
lichen.crusty
#Bryophytes whole set
bryo.nmds.1990<-metaMDS(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,])#
bryo.nmds.2015<-metaMDS(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015"&!substr(rownames(bryo.fat),1,3)%in%dodgysquares,])#
crusty.bryo<-protest(bryo.nmds.1990, bryo.nmds.2015)#very similar, r=0.98, P=0.001
#vascular whole set
vasc.nmds.1990<-metaMDS(vascall.df.ss[substr(rownames(vascall.df.ss),4,7)==1990,])#converges
vasc.nmds.2015<-metaMDS(vascall.df.ss[substr(rownames(vascall.df.ss),4,7)==2015,])#converges
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
vasc.sppscores.1990<-merge(vasc.sppscores.1990, vasc.ellen, by.x=0, by.y="Species.name", all.x=TRUE, all.y=FALSE)
vasc.sppscores.1990$coloury<-brewer.pal(9, "Spectral")[vasc.sppscores.1990$R]
vasc.sppscores.1990$coloury[is.na(vasc.sppscores.1990$coloury)]<-"#6A6C6E"#mid grey for species without an ellenberg for light - of course, this will fail if there's a black and white printed version, but it's easy enough to fix

vasc.sppscores.2015<-as.data.frame(scores(vasc.nmds.2015, display="species"))
vasc.sppscores.2015<-merge(vasc.sppscores.2015, vasc.ellen, by.x=0, by.y="Species.name", all.x=TRUE, all.y=FALSE)
vasc.sppscores.2015$coloury<-brewer.pal(9, "Spectral")[vasc.sppscores.2015$R]
vasc.sppscores.2015$coloury[is.na(vasc.sppscores.2015$coloury)]<-"#6A6C6E"

plot(bryo.nmds.1990, main="Bryophytes 1990", type="n")
points(bryo.sppscores.1990$NMDS1, bryo.sppscores.1990$NMDS2, col=bryo.sppscores.1990$coloury, pch=16)
points(bryo.nmds.1990, display="sites")
plot(bryo.nmds.2015, main="Bryophytes 2015", type="n")
points(bryo.sppscores.2015$NMDS1, bryo.sppscores.2015$NMDS2, col=bryo.sppscores.2015$coloury, pch=16)
points(bryo.nmds.2015, display="sites")#plot has done an annoying flip.  This happens with all whole-number seeds 1-5.
plot(vasc.nmds.1990, main="Vascular plants 1990", type="n")
points(vasc.sppscores.1990$NMDS1, vasc.sppscores.1990$NMDS2, col=vasc.sppscores.1990$coloury, pch=16)
points(vasc.nmds.1990, display="sites")
plot(vasc.nmds.2015, main="Vascular plants 2015", type="n")
points(vasc.sppscores.2015$NMDS1, vasc.sppscores.2015$NMDS2, col=vasc.sppscores.2015$coloury, pch=16)
points(vasc.nmds.2015, display="sites")

savePlot("nmds bryo and vasc just for show_subset_col.png", type="png")

plot(bryo.sppscores.1990$NMDS1, bryo.sppscores.1990$NMDS2, col=bryo.sppscores.1990$coloury, pch=16)
points(bryo.nmds.1990, display="sites")
plot(bryo.sppscores.2015$NMDS1, bryo.sppscores.2015$NMDS2, col=bryo.sppscores.2015$coloury, pch=16)
points(bryo.nmds.2015, display="sites")#plot has done an annoying flip.  This happens with all whole-number seeds 1-5
plot(vasc.sppscores.1990$NMDS1, vasc.sppscores.1990$NMDS2, col=vasc.sppscores.1990$coloury, pch=16)
points(vasc.nmds.1990, display="sites")
plot(vasc.sppscores.2015$NMDS1, vasc.sppscores.2015$NMDS2, col=vasc.sppscores.2015$coloury, pch=16)
points(vasc.nmds.2015, display="sites")

nmdsofdoom<-function(nmdsob, ell, ellvar, sppname, flip=FALSE){
  nmdsout<-fortify(nmdsob)
  nmdsout<-left_join(nmdsout, ell, by=c("Label"=sppname))
  if(isTRUE(flip)){
    nmdsout<-mutate(nmdsout, Dim1=Dim1*-1)
  }
  ggplot(filter(nmdsout, Score=="species"), aes_string(x="Dim1", y="Dim2", fill=ellvar))+
    geom_point(shape=21, size=2, colour="white")+
    geom_point(data=filter(nmdsout, Score=="sites"), aes(colour=I("black")), shape=4)+
    theme_classic()+
    coord_equal()+
    labs(x = "NMDS1", y = "NMDS2")
}
###next step: flip argument isn't working - keep working on a good seed then set flip - or set seed in multiple places. If you set seed then run from partway down that starts the set seed sequence in a different place and the answers will be different.

bry1<-nmdsofdoom(bryo.nmds.1990, bryo.status, "L", "Species_name")+ggtitle("Bryophytes 1990")+labs(fill="EIV\nLight")+xlim(-1.5, 2.5)+ylim(-2, 2.5)
bry2<-nmdsofdoom(bryo.nmds.2015, bryo.status, "L", "Species_name", flip=TRUE)+ggtitle("Bryophytes 2015")+labs(fill="EIV\nLight")+xlim(-1.5, 2)+ylim(-2, 2.5)
vasc3<-nmdsofdoom(vasc.nmds.1990, vasc.ellen, "R", "Species.name")+ggtitle("Vascular plants 1990")+labs(fill="EIV\nReaction")+xlim(-1.5, 2.5)+ylim(-2, 2.5)
vasc4<-nmdsofdoom(vasc.nmds.2015, vasc.ellen, "R", "Species.name")+ggtitle("Vascular plants 2015")+labs(fill="EIV\nReaction")+xlim(-1.5, 2.5)+ylim(-2, 2.5)

x11()
gridExtra::grid.arrange(bry1,bry2,vasc3,vasc4)
