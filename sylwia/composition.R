library(vegan)
coloury<-rep(c(1,2), nrow(easytabx))
myfirstdca<-decorana(easytabx)
plot(myfirstdca, display="sites",type="n")
points(myfirstdca, col=coloury)
ordiarrows(myfirstdca, groups=substr(rownames(scores(myfirstdca)), 1,3), length=0.1)

nmds1992<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="1992",])
nmds2015<-metaMDS(easytabx[substr(rownames(easytabx),4,7)=="2015",])
moss.crusty<-protest(nmds1992, nmds2015)

t.test(scores(myfirstdca[(substr(rownames(scores(myfirstdca)), 1,3))=="1992",1]),scores(myfirstdca[(substr(rownames(scores(myfirstdca)), 1,3))=="2015"]))

plotswithviride2015<-bryophytes$Plot[bryophytes$Year==2015&bryophytes$Species_name=="Dicranum viride"]
D_viride_subset<-bryophytes[bryophytes$Plot%in%plotswithviride2015,]
str(D_viride_subset)
