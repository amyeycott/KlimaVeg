plot.by.year<-paste(bryophytes$Plot,bryophytes$Year,sep="_")

bry.matr<-table(plot.by.year,bryophytes$Species_name) #making a occurrence matrix for bryophyte species per plot

#not sure if this is needed...
a1<-dim(bryophytes)[1]
rn<-rownames(bry.matr)
cn<-colnames(bry.matr)

for (i in 1:a1){
  bry.matr[rn==bryophytes[i,1],cn==bryophytes[i,3]]<-bryophytes[i,4]
}
#down here...
######################

on.ls.ag<-bryophytes[(bryophytes$Ap_LS)>0,c(1:4,which(names(bryophytes)%in%S.in.LS), 55:68)]
occ.on.ash<-table(on.ls.ag[on.ls.ag$Year==1992,3]) 
occ.tot<-colSums(bry.matr[(substr(rownames(bry.matr), 5,8))==1992,]>0)


occ.tot0<-NULL
for(i in 1:length(occ.on.ash)){
  occ.tot1<-occ.tot[names(occ.on.ash)[i]==names(occ.tot)]
  occ.tot0<-c(occ.tot0,occ.tot1)
}

frac1992<-occ.on.ash/occ.tot0 #

occ.on.ash<-table(on.ls.ag[on.ls.ag$Year==2015,3])
occ.tot<-colSums(bry.matr[(substr(rownames(bry.matr), 5,8))==2015,]>0)

occ.tot0<-NULL
for(i in 1:length(occ.on.ash)){
  occ.tot1<-occ.tot[names(occ.on.ash)[i]==names(occ.tot)]
  occ.tot0<-c(occ.tot0,occ.tot1)
} 

frac2015<-occ.on.ash/occ.tot0

n1<-table(c(names(frac1992),names(frac2015)))
n2<-names(n1[n1==2])

frac0<-NULL
for(i in 1:length(n2)){
  o1<-c(frac2015[names(frac2015)==n2[i]],frac1992[names(frac1992)==n2[i]])
  
frac0<-rbind(frac0,o1)
}

plot(frac0[,2],frac0[,1],xlab="Fraction on tree in 1992",ylab="Fraction on tree in 2015",main="Ap")
abline(0,1)

x11(); par(mfrow=c(5,3), mar=c(3,3,1,1))
names(bryophytes[61:74])


occ.on.ash
