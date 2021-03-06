source("Bryophyte data loading.R")
library(vegan)
coloury<-rep(c(1,2), nrow(bryo.fat))
myfirstdca<-decorana(bryo.fat)
plot(myfirstdca, display="sites",type="n")
points(myfirstdca, col=coloury)
ordiarrows(myfirstdca, groups=substr(rownames(scores(myfirstdca)), 1,3), length=0.1)

nmds1990<-metaMDS(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990",])
nmds2015<-metaMDS(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015",])
moss.crusty<-protest(nmds1990, nmds2015)
moss.crusty#rho=0.98, p=0.001 :-)

t.test(scores(myfirstdca)[(substr(rownames(scores(myfirstdca)), 4,7))=="1990",1],scores(myfirstdca)[(substr(rownames(scores(myfirstdca)), 4,7))=="2015",1])# no difference between years, t283=0.561, p=0.5753

plotswithviride2015<-bryophytes$Plot[bryophytes$Year==2015&bryophytes$Species_name=="Dicranum viride"]
D_viride_subset<-bryophytes[bryophytes$Plot%in%plotswithviride2015,]
str(D_viride_subset)
