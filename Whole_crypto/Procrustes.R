source("source data and merging.R")
set.seed()
library(vegan)
#Lichens whole set.
lichen.nmds.1992<-metaMDS(subset(comp_old[,colSums(comp_old>0)>1], select=-Year), trymax=100)# Not converging, even after 100 tries. There are between 26 and 153 species per plot, so removing plots by low species count doesn't make sense. Removing species with fewer than five ocurrences doesn't help, even though it takes out 99 species of the 212. Do the outliers have particularly high or low species counts? But hard to find outliers on an nmds. 
###here follows a bunch of stuff trying to resolve the non-convergence issue. Starts with looking at the datasets, then goes on to trying other ordination configurations, first on this set then on the others.

hist(rowSums(comp>0))
hist(rowSums(easytabx[substr(rownames(easytabx),4,7)=="1992",]>0))
hist(rowSums(easytabx[substr(rownames(easytabx),4,7)=="2015",]>0))
hist(rowsums(VascNew.fat>0))
hist(rowsums(VascOld.fat>0))

lichen.dca.1992<-decorana(subset(comp_old, select=-Year))#axis 1 length is 2.5
lichen.ca.1992<-cca(subset(comp_old, select=-Year))
plot(lichen.ca.1992)#one outlier with a very high axis 2 score
lichen.ca.1992.sites<-as.data.frame(scores(lichen.ca.1992, display = "sites"))
lichen.ca.1992.sites[lichen.ca.1992.sites$CA2>3,]#F5 is the outlier. Looking by eye at F5 over the two years, the change isn't particularly great. Changes of 0 to 2 or 2 to 0 were Pertusaria albescens, Lecanora pulicaris, Lecanora expallens,  Cladonia chlorophaea s.l.

metaMDS(subset(comp_old[!(rownames(comp_old)=="F5"),colSums(comp_old>0)>5], select=-Year), trymax=100)#still doesn't help, and by this time it feels like torturing the data
test.hclust<-hclust(vegdist(subset(comp, Year==1992, select=-Year)), method = "single")
plot(test.hclust)#three plots drift off eary, but npt far from the next branch



#Questions: are procrustes rotations of DCAs valid or does the detrending destroy comparability? How do the CAs look (comp_old: one massive outlier)
lichen.ca.2015<-cca(subset(comp_new, select=-Year))
plot(lichen.ca.2015)#mercedes sign. No specifically horrifying outliers
bryo.ca.1992<-cca(easytabx[substr(rownames(easytabx),4,7)=="1992",])
plot(bryo.ca.1992)#like a cross between an arch and an arrowhead. No horrifying outliers
bryo.ca.2015<-cca(easytabx[substr(rownames(easytabx),4,7)=="2015",])
plot(bryo.ca.2015)#same as 1992, maybe a bit more heart-like and three very slight outliers on axis 2
vasc.ca.1992<-cca(VascOld.fat)
plot(vasc.ca.1992)#very arrowheady, some outlieryness but nothing horrifying
vasc.ca.2015<-cca(VascNew.fat)
plot(vasc.ca.2015)#same as 1992

####start of the rest of the code how it was intended to be.
lichen.nmds.2015<-metaMDS(subset(comp, Year==2015, select=-Year))#not converging
lichen.crusty<-protest(lichen.nmds.1992, lichen.nmds.2015)
#Bryophytes whole set
bryo.nmds.1992<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="1992",])#converges
bryo.nmds.2015<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="2015",])#converges
crusty.bryo<-protest(bryo.nmds.1992, bryo.nmds.2015)#very similar, r=0.98, P=0.001
#vascular whole set
vasc.nmds.1992<-metaMDS(VascOld.fat)#converges
vasc.nmds.2015<-metaMDS(VascNew.fat)#converges
crusty.vasc<-protest(vasc.nmds.1992, vasc.nmds.2015)#very similar
crusty.2015.vascbryo<-protest(vasc.nmds.2015, bryo.nmds.2015)#still pretty similar: r=0.95, P=0.001
crusty.1992.vascbryo<-protest(vasc.nmds.1992, bryo.nmds.1992)#very very slightly less similar, but unlikely to be a serious thing: r=0.94, P=0.001

x11(); par(mfrow=c(2,2))
plot(bryo.nmds.1992, main="Bryophytes 1992")
plot(bryo.nmds.2015, main="Bryophytes 2015")
plot(vasc.nmds.1992, main="Vascular plants 1992")
plot(vasc.nmds.2015, main="Vascular plants 2015")
savePlot("nmds bryo and vasc just for show.emf", type="emf")
