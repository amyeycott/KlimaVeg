source("source data and merging.R")
set.seed()
library(vegan)
dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")

#Lichens whole set.
lichen.nmds.1992<-metaMDS(subset(comp_old[!rownames(comp_old)%in%dodgysquares,colSums(comp_old>0)>1], select=-Year), trymax=100)# See notes on the not-subsetted version, but subset does not cure the non-convergence issue.
lichen.nmds.2015<-metaMDS(subset(comp_new[!rownames(comp_new)%in%dodgysquares,colSums(comp_new>0)>1], select=-Year), trymax=100)

####start of the rest of the code how it was intended to be - this script skips all the exploratory stuff
lichen.nmds.2015<-metaMDS(subset(comp[!rownames(comp_old)%in%dodgysquares,], Year==2015, select=-Year))#not converging
lichen.crusty<-protest(lichen.nmds.1992, lichen.nmds.2015)
#Bryophytes whole set
bryo.nmds.1992<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="1992"&!substr(rownames(easytabx),1,3)%in%dodgysquares,])#
bryo.nmds.2015<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="2015"&!substr(rownames(easytabx),1,3)%in%dodgysquares,])#
crusty.bryo<-protest(bryo.nmds.1992, bryo.nmds.2015)#very similar, r=0.98, P=0.001
#vascular whole set
vasc.nmds.1992<-metaMDS(vascOld.fat[!rownames(vascOld.fat)%in%dodgysquares,])#converges
vasc.nmds.2015<-metaMDS(vascNew.fat[!rownames(vascNew.fat)%in%dodgysquares,])#converges
crusty.vasc<-protest(vasc.nmds.1992, vasc.nmds.2015)#very similar
crusty.2015.vascbryo<-protest(vasc.nmds.2015, bryo.nmds.2015)# r=0.95, P=0.001
crusty.1992.vascbryo<-protest(vasc.nmds.1992, bryo.nmds.1992)#very very slightly less similar, but unlikely to be a serious thing: r=0.93, P=0.001

x11(); par(mfrow=c(2,2))
plot(bryo.nmds.1992, main="Bryophytes 1992")
plot(bryo.nmds.2015, main="Bryophytes 2015")
plot(vasc.nmds.1992, main="Vascular plants 1992")
plot(vasc.nmds.2015, main="Vascular plants 2015")
savePlot("nmds bryo and vasc just for show_subset.emf", type="emf")
