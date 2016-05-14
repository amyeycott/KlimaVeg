source("source data and merging.R")

library(vegan)
#Lichens whole set. MAGIC NUMBERS ARE BAD.
lichen.nmds.1992<-metaMDS(subset(comp, Year==1992, select=-Year))
lichen.nmds.2015<-metaMDS(subset(comp, Year==2015, select=-Year))
lichen.crusty<-protest(lichen.nmds.1992, lichen.nmds.2015)
#Bryophytes whole set
nmds1992<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="1992",])
nmds2015<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="2015",])
moss.crusty<-protest(nmds1992, nmds2015)