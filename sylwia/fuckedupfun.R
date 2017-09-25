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

fuckedup.fun<-function(x){
  
on.ls.x<-bryophytes[(bryophytes[[x]])>0,c(1:4,which(names(bryophytes)%in%S.in.LS), 55:68)]
occ.on.x<-table(on.ls.x[on.ls.x$Year==1990,3]) 
occ.tot<-colSums(bry.matr[(substr(rownames(bry.matr), 5,8))==1990,]>0)

occ.tot[names(occ.on.x)]
# line above replaces the following five lines:
#occ.tot0<-NULL
#  for(i in 1:length(occ.on.x)){
#    occ.tot1<-occ.tot[names(occ.on.x)[i]==names(occ.tot)]
#    occ.tot0<-c(occ.tot0,occ.tot1)
#  } 

frac1990<-occ.on.x/occ.tot0 #hypnum pallescens  is 2

occ.on.x<-table(on.ls.x[on.ls.x$Year==2015,3])
occ.tot<-colSums(bry.matr[(substr(rownames(bry.matr), 5,8))==2015,]>0)
occ.tot[names(occ.on.x)]
# line above replaces the following five lines:
#occ.tot0<-NULL
#  for(i in 1:length(occ.on.x)){
#    occ.tot1<-occ.tot[names(occ.on.x)[i]==names(occ.tot)]
#    occ.tot0<-c(occ.tot0,occ.tot1)
#  } 

frac2015<-occ.on.x/occ.tot0

n1<-table(c(names(frac1990),names(frac2015)))
n2<-names(n1[n1==2])

frac0<-NULL
  for(i in 1:length(n2)){
   o1<-c(frac2015[names(frac2015)==n2[i]],frac1990[names(frac1990)==n2[i]])
    frac0<-rbind(frac0,o1)
  }

plot(frac0[,2],frac0[,1],xlab="Fraction on tree in 1990",ylab="Fraction on tree in 2015",main=x, xlim=c(0,1), ylim=c(0,1))
abline(0,1)
}

x11(); par(mfrow=c(2,3))
allprops<-sapply(c("Ap_LS", "Ag_LS", "Cb_LS", "Fe_LS", "Qr_LS", "Tc_LS"), fuckedup.fun) # Error in frac0[, 2] : subscript out of bounds. The frac[0,2] part runs fine when not in a loop though (see original code in separate script).
