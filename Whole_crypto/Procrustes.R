source("source data and merging.R")

library(vegan)
#Lichens whole set. MAGIC NUMBERS ARE BAD.
lichen.nmds.1992<-metaMDS(subset(comp, Year==1992, select=-Year))#not converging

test.hclust<-hclust(vegdist(subset(comp, Year==1992, select=-Year)), method = "single")
plot(test.hclust)
#the distributions of plot richnesses are rather similar between the three taxon groups, which is nice and means that they are reasonably ok-ish to compare different ordinations
hist(rowSums(VascNew.fat>0))
hist(rowSums(easytabx[substr(rownames(easytabx),4,7)=="1992",]>0))


lichen.nmds.2015<-metaMDS(subset(comp, Year==2015, select=-Year))#not converging
lichen.crusty<-protest(lichen.nmds.1992, lichen.nmds.2015)
#Bryophytes whole set
nmds1992<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="1992",])
nmds2015<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="2015",])
moss.crusty<-protest(nmds1992, nmds2015)
#vascular whole set
vasc.nmds.1992<-metaMDS(VascOld.fat)#converges


vasc.nmds.2015<-metaMDS(VascNew.fat)#converges
