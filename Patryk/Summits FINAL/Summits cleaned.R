####FILES####
elev.df<-read.table("tops.csv", sep=";", dec=",", header=TRUE)
lifeforms.df<-read.table("lifeforms.csv", sep=";", dec=",", header=TRUE)
kotula.df<-read.table("kotula.csv", sep=';', dec=',', header=TRUE)
sagorski.df<-read.table("sagorski.csv", sep=';', dec=',', header=TRUE)
pawlowski.df<-read.table("pawlowski.csv", sep=';', dec=',', header=TRUE)
ecol.ind.df<-read.table("ecol.ind.csv", sep=';', dec=',', header=TRUE)
merg.old.df<-read.table("merg.old.csv", sep=';', dec=',', header=TRUE)
merg.new.df<-read.table("merg.new.csv", sep=';', dec=',', header=TRUE)
climate.kasprowy.df<-read.table("climate.kasprowy.csv", sep=';', dec=',', header=TRUE)
climate.lomnica.df<-read.table("climate.lomnica.csv", sep=';', dec=',', header=TRUE)

####LIBS#####
library(vegan)
library(permute)
library(Hmisc)
library(corrplot)
library(dplyr)

####IDENTIFY SPECIES ABOVE 10,20,50,100 M BELOW SUMMIT####
elev2.df <- elev.df[-1, ]
summit <- elev.df[1, ]
s50 <- NULL
for (i in 1:28) {
  a1 <- as.numeric(summit[i]) - elev2.df[, i]
  s50a <- a1 < 50 #change this number from 10 to 100
  s50 <- cbind(s50, s50a)
}
occ50 <- rowSums(s50)

####DCA####
###SITES
dca1<-decorana(t(s50[occ50>0,]))
s1<-summary(dca1)
site1<-s1[[3]]
plot(site1[,1],site1[,2],xlab="DCA1",ylab="DCA2", xlim=c(-2,2.5), ylim=c(-2,2.5), cex=1.8, pch=c(19,21), cex.axis=1.5, cex.lab=1.5)
ordiarrows(dca1, levels=length(site1)/2, replicates=2, length=0.13)
text(site1[,1],site1[,2],labels=names(elev.df), col=rep(1:2, length(site1)/2), cex=0.75)

###SPECIES AND PLANTS' FORMS OF GROWTH
lifeforms2.df<-lifeforms2.df[,c(1,3)]
hulls<-ordihull(dca1, lifeforms2.df[occ50>0,2], display="species", draw="lines")
x11()
par(mfrow=c(1,2))
plot(dca1, type="n", cex.axis=1.5, cex.lab=1.5, xlim=c(-3.0,3.0)) 
points(dca1, display="species", col="grey", pch=19)
lines(hulls$M,col='red',lwd=2,lty=1)
lines(hulls$N, col='blue', lwd=2, lty=2)
lines(hulls$Ch, col='darkgreen', lwd=2, lty=3)
legend('topright', c('Megaphaneropytes','Nanophaneroprytes', 'Woody chamaephytes'), lwd=2, lty=c(1,2,3), cex=0.8, col=c('red', 'blue', 'darkgreen'))

plot(dca1, type="n", cex.axis=1.5, cex.lab=1.5, xlim=c(-3.0,3.0)) 
points(dca1, display="species", col="grey", pch=19)
lines(hulls$C, col='red',lwd=2, lty=1)
lines(hulls$H, col='blue', lwd=2, lty=2)
lines(hulls$T, col='darkgreen', lwd=2, lty=3)
lines(hulls$G, col='orange', lwd=2, lty=4)
legend('topright', c('Non-woody chamaephytes','Hemicryptophytes', 'Terophytes','Geophytes'),lwd=2, lty=c(1,2,3,4), col=c('red', 'blue', 'darkgreen', 'orange'), cex=0.8)

####CCA####
###RESTRICTED PERMUTATION TEST
n_mountains <- 14
mountains <- gl(n = n_mountains, k = 2)# must be factor - safer to use real mountain names
old_new <- gl(n = 2, k = 1, length = n_mountains * 2)#should be factor

##BLOCKS
CTRL1 <- how(blocks = mountains, complete=TRUE, maxperm=20000)
CTRL2 <- how(blocks = mountains, nperm=999)
check(1:length(mountains), control = CTRL1) # how many possible permutations #now it is 16384

##SOME EXAMPLE PERMUTATIONS
set.seed(42)
shuffleSet(1:length(mountains), nset = 10, control = CTRL)

###ORDINATION CODE
mod <- rda(t(s50[occ50>0,]) ~ old_new + Condition(mountains))#partial out effect of mountain
anova(mod, permutations = CTRL1)

###STRATA IN PLOTS - GIVES SIMILAR PERMUTATIONS
CTRL1 <- how(plots = Plots(strata = mountains))
check(1:length(mountains), control = CTRL1)
set.seed(42)
shuffleSet(1:length(mountains), nset = 10, control = CTRL1)

####CCA FOR DIFFERENT THRESHOLDS OF ELEVATION FOR EACH AUTHOR####
###MERGED DATASET
#s100<-NULL #temporary code for making cca with time for different threshold of elevation	
#for(i in 1:28){
#a1<-as.numeric(summit[i])-elev2.df[,i]
#s100a<-a1<100 ##change these numbers from 10 to 100
#s100<-cbind(s100,s100a)
#}
#occ100<-rowSums(s100)

rda10merged<-rda(t(s10[occ10>0,]) ~ old_new + Condition(mountains))
rda20merged<-rda(t(s20[occ20>0,]) ~ old_new + Condition(mountains))
rda50merged<-rda(t(ss50[occc50>0,]) ~ old_new + Condition(mountains))
rda100merged<-rda(t(s100[occ100>0,]) ~ old_new + Condition(mountains))
anova(rda10merged, permutations = CTRL1)
anova(rda20merged, permutations = CTRL1)
anova(rda50merged, permutations = CTRL1)
anova(rda100merged, permutations = CTRL1)

###SAGORSKI
sagorski1.df<-sagorski.df[-1,]
summit.sag<-sagorski.df[1,]
n_mountains_sag <- 12
mountains_sag <- gl(n = n_mountains_sag, k = 2)
old_new_sag <- gl(n = 2, k = 1, length = n_mountains_sag * 2)
CTRL_sag <- how(blocks = mountains_sag, complete=TRUE, maxperm=20000)
check(1:length(mountains_sag), control = CTRL_sag)
set.seed(42)
shuffleSet(1:length(mountains_sag), nset = 10, control = CTRL_sag)

#s100.sag<-NULL#temporary code for making cca with time for different threshold of elevation	
#for(i in 1:24){
#a1.sag<-as.numeric(summit.sag[i])-sagorski1.df[,i]
#s100a.sag<-a1.sag<100 #change these numbers from 10 to 100
#s100.sag<-cbind(s100.sag,s100a.sag)
#}
#occ100.sag<-rowSums(s100.sag)

rda10sag<-rda(t(s10.sag[occ10.sag>0,]) ~ old_new_sag + Condition(mountains_sag))
rda20sag<-rda(t(s20.sag[occ20.sag>0,]) ~ old_new_sag + Condition(mountains_sag))
rda50sag<-rda(t(s50.sag[occ50.sag>0,]) ~ old_new_sag + Condition(mountains_sag))
rda100sag<-rda(t(s100.sag[occ100.sag>0,]) ~ old_new_sag + Condition(mountains_sag))
anova(rda10sag, permutations = CTRL_sag)
anova(rda20sag, permutations = CTRL_sag)
anova(rda50sag, permutations = CTRL_sag)
anova(rda100sag, permutations = CTRL_sag)

###KOTULA
kotula1.df<-kotula.df[-1,]
summit.kot<-kotula.df[1,]
n_mountains_kot <- 9
mountains_kot <- gl(n = n_mountains_kot, k = 2)
old_new_kot <- gl(n = 2, k = 1, length = n_mountains_kot * 2)
CTRL_kot <- how(blocks = mountains_kot, complete=TRUE, maxperm=20000)
check(1:length(mountains_kot), control = CTRL_kot)
set.seed(42)
shuffleSet(1:length(mountains_kot), nset = 10, control = CTRL_kot)

#s100.kot<-NULL#temporary code for making cca with time for different threshold of elevation	
#for(i in 1:18){
#a1.kot<-as.numeric(summit.kot[i])-kotula1.df[,i]
#s100a.kot<-a1.kot<100 #change these numbers from 10 to 100
#s100.kot<-cbind(s100.kot,s100a.kot)
#}
#occ100.kot<-rowSums(s100.kot)

rda10.kot<-rda(t(s10.kot[occ10.kot>0,]) ~ old_new_kot + Condition(mountains_kot))
rda20.kot<-rda(t(s20.kot[occ20.kot>0,]) ~ old_new_kot + Condition(mountains_kot))
rda50.kot<-rda(t(s50.kot[occ50.kot>0,]) ~ old_new_kot + Condition(mountains_kot))
rda100.kot<-rda(t(s100.kot[occ100.kot>0,]) ~ old_new_kot + Condition(mountains_kot))
anova(rda10.kot, permutations = CTRL_kot)
anova(rda20.kot, permutations = CTRL_kot)
anova(rda50.kot, permutations = CTRL_kot)
anova(rda100.kot, permutations = CTRL_kot)

###PAWLOWSKI
pawlowski1.df<-pawlowski.df[-1,]
summit.paw<-pawlowski.df[1,]
n_mountains_paw <- 6
mountains_paw <- gl(n = n_mountains_paw, k = 2)
old_new_paw <- gl(n = 2, k = 1, length = n_mountains_paw * 2)
CTRL_paw <- how(blocks = mountains_paw, complete=TRUE, maxperm=20000)
check(1:length(mountains_paw), control = CTRL_paw)
set.seed(42)
shuffleSet(1:length(mountains_paw), nset = 10, control = CTRL_paw)

#s100.paw<-NULL#temporary code for making cca with time for different threshold of elevation	
#for(i in 1:12){
#a1.paw<-as.numeric(summit.paw[i])-pawlowski1.df[,i]
#s100a.paw<-a1.paw<100 #change these numbers from 10 to 100
#s100.paw<-cbind(s100.paw,s100a.paw)
#}
#occ100.paw<-rowSums(s100.paw)

rda10.paw<-rda(t(s10.paw[occ10.paw>0,]) ~ old_new_paw + Condition(mountains_paw))
rda20.paw<-rda(t(s20.paw[occ20.paw>0,]) ~ old_new_paw + Condition(mountains_paw))
rda50.paw<-rda(t(s50.paw[occ50.paw>0,]) ~ old_new_paw + Condition(mountains_paw))
rda100.paw<-rda(t(s100.paw[occ100.paw>0,]) ~ old_new_paw + Condition(mountains_paw))
anova(rda10.paw, permutations = CTRL_paw)
anova(rda20.paw, permutations = CTRL_paw)
anova(rda50.paw, permutations = CTRL_paw)
anova(rda100.paw, permutations = CTRL_paw)

####ECOLOGICAL INDICATOR VALUES####
####CORRELATION MATRIX####
options(scipen=999)
res <- cor(ecol.ind.df)
round(res, 2)
res2 <- rcorr(as.matrix(ecol.ind.df))
res2
rr1<-res2$r
pp1<-res2$P
flattenCorrMatrix <- function(rr1, pp1){
  ut <- upper.tri(rr1)
  data.frame(
    row = rownames(rr1)[row(rr1)[ut]],
    column = rownames(rr1)[col(rr1)[ut]],
    cor  =(rr1)[ut],
    p = pp1[ut]
  )
}
f.cor.matr<-flattenCorrMatrix(rr1, pp1)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

####CHANGE OF ECOLOGICAL INDICATOR VALUES IN TIME####
###MERGED DATASET
merged.old<-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)
merged.new<-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)
#trait1<-NULL
#for(i in 1:28) #Temporal code for calculate mean values
#{
#a1<-s50[,i]
#t1<-ecol.ind.df[a1,5] #change from 1-5 for all ecological indicators
#t0<-mean(na.omit(t1))
#trait1<-c(trait1,t0)
#}
#Light.merged<-trait1
#Temperature.merged<-trait1
#Moisture.merged<-trait1
#Trophism.merged<-trait1
#Reaction.merged<-trait1
Zarzycki.merged<-as.data.frame(cbind(Light.merged,Temperature.merged,Moisture.merged,Trophism.merged,Reaction.merged))
t.test(Zarzycki.merged$Light.merged[merged.old],Zarzycki.merged$Light.merged[merged.new],paired=TRUE)
t.test(Zarzycki.merged$Temperature.merged[merged.old],Zarzycki.merged$Temperature.merged[merged.new],paired=TRUE)
t.test(Zarzycki.merged$Moisture.merged[merged.old],Zarzycki.merged$Moisture.merged[merged.new],paired=TRUE)
t.test(Zarzycki.merged$Trophism.merged[merged.old],Zarzycki.merged$Trophism.merged[merged.new],paired=TRUE)
t.test(Zarzycki.merged$Reaction.merged[merged.old],Zarzycki.merged$Reaction.merged[merged.new],paired=TRUE)

###SAGORSKI
sagorski.old<-c(1,3,5,7,9,11,13,15,17,19,21,23)
sagorski.new<-c(2,4,6,8,10,12,14,16,18,20,22,24)
#trait1.sag<-NULL
#for(i in 1:24) #Temporal code for calculate mean values
#{
#a1.sag<-s50.sag[,i]
#t1.sag<-ecol.ind.df[a1.sag,5] #change from 1-5 for all ecological indicators
#t0.sag<-mean(na.omit(t1.sag))
#trait1.sag<-c(trait1.sag,t0.sag)
#}
#Light.sag<-trait1.sag
#Temperature.sag<-trait1.sag
#Moisture.sag<-trait1.sag
#Trophism.sag<-trait1.sag
#Reaction.sag<-trait1.sag
Zarzycki.sag<-as.data.frame(cbind(Light.sag,Temperature.sag,Moisture.sag,Trophism.sag,Reaction.sag))
t.test(Zarzycki.sag$Light.sag[sagorski.old],Zarzycki.sag$Light.sag[sagorski.new],paired=TRUE)
t.test(Zarzycki.sag$Temperature.sag[sagorski.old],Zarzycki.sag$Temperature.sag[sagorski.new],paired=TRUE)
t.test(Zarzycki.sag$Moisture.sag[sagorski.old],Zarzycki.sag$Moisture.sag[sagorski.new],paired=TRUE)
t.test(Zarzycki.sag$Trophism.sag[sagorski.old],Zarzycki.sag$Trophism.sag[sagorski.new],paired=TRUE)
t.test(Zarzycki.sag$Reaction.sag[sagorski.old],Zarzycki.sag$Reaction.sag[sagorski.new],paired=TRUE)

###KOTULA
kotula.old<-c(1,3,5,7,9,11,13,15,17)
kotula.new<-c(2,4,6,8,10,12,14,16,18)
#trait1.kot<-NULL
#for(i in 1:18) #Temporal code for calculate mean values
#{
#a1.kot<-s50.kot[,i]
#t1.kot<-ecol.ind.df[a1.kot,5] #change from 1-5 for all ecological indicators
#t0.kot<-mean(na.omit(t1.kot))
#trait1.kot<-c(trait1.kot,t0.kot)
#}
Light.kot<-trait1.kot
Temperature.kot<-trait1.kot
Moisture.kot<-trait1.kot
Trophism.kot<-trait1.kot
Reaction.kot<-trait1.kot
Zarzycki.kot<-as.data.frame(cbind(Light.kot,Temperature.kot,Moisture.kot,Trophism.kot,Reaction.kot))
t.test(Zarzycki.kot$Light.kot[kotula.old],Zarzycki.kot$Light.kot[kotula.new],paired=TRUE)
t.test(Zarzycki.kot$Temperature.kot[kotula.old],Zarzycki.kot$Temperature.kot[kotula.new],paired=TRUE)
t.test(Zarzycki.kot$Moisture.kot[kotula.old],Zarzycki.kot$Moisture.kot[kotula.new],paired=TRUE)
t.test(Zarzycki.kot$Trophism.kot[kotula.old],Zarzycki.kot$Trophism.kot[kotula.new],paired=TRUE)
t.test(Zarzycki.kot$Reaction.kot[kotula.old],Zarzycki.kot$Reaction.kot[kotula.new],paired=TRUE)

###PAWLOWSKI
pawlowski.old<-c(1,3,5,7,9,11)
pawlowski.new<-c(2,4,6,8,10,12)
#trait1.paw<-NULL
#for(i in 1:12) #Temporal code for calculate mean values
#{
#a1.paw<-s50.paw[,i]
#t1.paw<-ecol.ind.df[a1.paw,4] #change from 1-5 for all ecological indicators
#t0.paw<-mean(na.omit(t1.paw))
#trait1.paw<-c(trait1.paw,t0.paw)
#}
#Light.paw<-trait1.paw
#Temperature.paw<-trait1.paw
#Moisture.paw<-trait1.paw
#Trophism.paw<-trait1.paw
#Reaction.paw<-trait1.paw
Zarzycki.paw<-as.data.frame(cbind(Light.paw,Temperature.paw,Moisture.paw,Trophism.paw,Reaction.paw))
t.test(Zarzycki.paw$Light.paw[pawlowski.old],Zarzycki.paw$Light.paw[pawlowski.new],paired=TRUE)
t.test(Zarzycki.paw$Temperature.paw[pawlowski.old],Zarzycki.paw$Temperature.paw[pawlowski.new],paired=TRUE)
t.test(Zarzycki.paw$Moisture.paw[pawlowski.old],Zarzycki.paw$Moisture.paw[pawlowski.new],paired=TRUE)
t.test(Zarzycki.paw$Trophism.paw[pawlowski.old],Zarzycki.paw$Trophism.paw[pawlowski.new],paired=TRUE)
t.test(Zarzycki.paw$Reaction.paw[pawlowski.old],Zarzycki.paw$Reaction.paw[pawlowski.new],paired=TRUE)

####ERROR BARS####
###ACCESSIBILITY FOR SHEEP
acc.merged<-c(2,3,4,2,4,4,4,3,3,1,1,4,3,2)
acc.sagorski<-c(2,3,4,2,4,4,4,3,3,1,4,2)
acc.kotula<-c(2,4,3,3,1,1,4,3,2)
acc.pawlowski<-c(3,3,1,1,4,2)

###TOURISM INTENSITY
tour.merged<-c(1,1,2,2,2,1,1,1,2,3,1,2,1,3)
tour.sagorski<-c(1,1,2,2,2,1,1,2,2,3,2,3)
tour.kotula<-c(2,1,1,2,3,1,2,1,3)
tour.pawlowski<-c(1,2,3,1,2,3)

###BRAY-CURTIS DISSIMILARITY
##MERGED
elev.trans.merged<-t(s50[occ50>0,])
veg1.merged<-vegdist(elev.trans.merged, method="bray")
v3.merged<-as.matrix(veg1.merged)
veg2.merged<-v3.merged[seq(1,ncol(v3.merged),by=2), seq(2,ncol(v3.merged), by=2)] 
bray.merged<-diag(veg2.merged)
##SAGORSKI
elev.trans.sag<-t(s50.sag[occ50.sag>0,])
veg1.sag<-vegdist(elev.trans.sag, method="bray")
v3.sag<-as.matrix(veg1.sag)
veg2.sag<-v3.sag[seq(1,ncol(v3.sag),by=2), seq(2,ncol(v3.sag), by=2)] 
bray.sag<-diag(veg2.sag)
##KOTULA
elev.trans.kot<-t(s50.kot[occ50.kot>0,])
veg1.kot<-vegdist(elev.trans.kot, method="bray")
v3.kot<-as.matrix(veg1.kot)
veg2.kot<-v3.kot[seq(1,ncol(v3.kot),by=2), seq(2,ncol(v3.kot), by=2)] 
bray.kot<-diag(veg2.kot)
##PAWLOWSKI
elev.trans.paw<-t(s50.paw[occ50.paw>0,])
veg1.paw<-vegdist(elev.trans.paw, method="bray")
v3.paw<-as.matrix(veg1.paw)
veg2.paw<-v3.paw[seq(1,ncol(v3.paw),by=2), seq(2,ncol(v3.paw), by=2)] 
bray.paw<-diag(veg2.paw)

###CREATING TABLE
##MERGED
change.per.summit.merged<-cbind(acc.merged,tour.merged,bray.merged,as.data.frame(Light.merged[merged.old]),Light.merged[merged.new],
                                Temperature.merged[merged.old],Temperature.merged[merged.new],
                                Moisture.merged[merged.old],Moisture.merged[merged.new],
                                Trophism.merged[merged.old],Trophism.merged[merged.new],
                                Reaction.merged[merged.old],Reaction.merged[merged.new])
names(change.per.summit.merged)<-c("Access","Tourism","BCdist","Lold","Lnew","Told","Tnew","Mold","Mnew","Nold","Nnew","Rold", "Rnew")

##SAGORSKI
change.per.summit.sagorski<-cbind(acc.sagorski,tour.sagorski,bray.sag,as.data.frame(Light.sag[sagorski.old]),Light.sag[sagorski.new],
                                  Temperature.sag[sagorski.old],Temperature.sag[sagorski.new],
                                  Moisture.sag[sagorski.old],Moisture.sag[sagorski.new],
                                  Trophism.sag[sagorski.old],Trophism.sag[sagorski.new],
                                  Reaction.sag[sagorski.old],Reaction.sag[sagorski.new])
names(change.per.summit.sagorski)<-c("Access","Tourism","BCdist","Lold","Lnew","Told","Tnew","Mold","Mnew","Nold","Nnew","Rold", "Rnew")

##KOTULA
change.per.summit.kotula<-cbind(acc.kotula,tour.kotula,bray.kot,as.data.frame(Light.kot[kotula.old]),Light.kot[kotula.new],
                                Temperature.kot[kotula.old],Temperature.kot[kotula.new],
                                Moisture.kot[kotula.old],Moisture.kot[kotula.new],
                                Trophism.kot[kotula.old],Trophism.kot[kotula.new],
                                Reaction.kot[kotula.old],Reaction.kot[kotula.new])
names(change.per.summit.kotula)<-c("Access","Tourism","BCdist","Lold","Lnew","Told","Tnew","Mold","Mnew","Nold","Nnew","Rold", "Rnew")

##PAWLOWSKI
change.per.summit.pawlowski<-cbind(acc.pawlowski,tour.pawlowski,bray.paw,as.data.frame(Light.paw[pawlowski.old]),Light.paw[pawlowski.new],
                                   Temperature.paw[pawlowski.old],Temperature.paw[pawlowski.new],
                                   Moisture.paw[pawlowski.old],Moisture.paw[pawlowski.new],
                                   Trophism.paw[pawlowski.old],Trophism.paw[pawlowski.new],
                                   Reaction.paw[pawlowski.old],Reaction.paw[pawlowski.new])
names(change.per.summit.pawlowski)<-c("Access","Tourism","BCdist","Lold","Lnew","Told","Tnew","Mold","Mnew","Nold","Nnew","Rold", "Rnew")

###CREATING PLOTS

summitsummary<-change.per.summit.merged%>%
  group_by(Tourism)%>%
  mutate(changeL=Lnew-Lold, changeT=Tnew-Told, changeM=Mnew-Nold, changeN=Nnew-Nold, changeR=Rnew-Rold)%>%
  select(-(Lold:Rnew))%>%
  summarize_each(funs(mean,sd))
acc.sumary<-summitsummary
tou.sumary<-summitsummary

x11()
par(mfrow=c(2,3))

##ACCESSIBILITY
errbar(acc.sumary$Access, acc.sumary$BCdist_mean, acc.sumary$BCdist_mean-acc.sumary$BCdist_sd, acc.sumary$BCdist_mean+acc.sumary$BCdist_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Bray-Curtis dissimilarity")
errbar(acc.sumary$Access, acc.sumary$changeL_mean, acc.sumary$changeL_mean-acc.sumary$changeL_sd, acc.sumary$changeL_mean+acc.sumary$changeL_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-L")
errbar(acc.sumary$Access, acc.sumary$changeT_mean, acc.sumary$changeT_mean-acc.sumary$changeT_sd, acc.sumary$changeT_mean+acc.sumary$changeL_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-T")
errbar(acc.sumary$Access, acc.sumary$changeM_mean, acc.sumary$changeM_mean-acc.sumary$changeL_sd, acc.sumary$changeM_mean+acc.sumary$changeM_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-F")
axis(1, at=1:4)
errbar(acc.sumary$Access, acc.sumary$changeN_mean, acc.sumary$changeN_mean-acc.sumary$changeL_sd, acc.sumary$changeN_mean+acc.sumary$changeN_sd, cap=0.05, cex=1.5, xaxt="n", xlab="Accessibility for sheep", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-N")
axis(1, at=1:4)
errbar(acc.sumary$Access, acc.sumary$changeR_mean, acc.sumary$changeR_mean-acc.sumary$changeR_sd, acc.sumary$changeR_mean+acc.sumary$changeR_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-R")
axis(1, at=1:4)

##TOURISM
errbar(tou.sumary$Tourism, tou.sumary$BCdist_mean, tou.sumary$BCdist_mean-tou.sumary$BCdist_sd, tou.sumary$BCdist_mean+tou.sumary$BCdist_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Bray-Curtis dissimilarity")
errbar(tou.sumary$Tourism, tou.sumary$changeL_mean, tou.sumary$changeL_mean-tou.sumary$changeL_sd, tou.sumary$changeL_mean+tou.sumary$changeL_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-L")
errbar(tou.sumary$Tourism, tou.sumary$changeT_mean, tou.sumary$changeT_mean-tou.sumary$changeT_sd, tou.sumary$changeT_mean+tou.sumary$changeL_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-T")
errbar(tou.sumary$Tourism, tou.sumary$changeM_mean, tou.sumary$changeM_mean-tou.sumary$changeL_sd, tou.sumary$changeM_mean+tou.sumary$changeM_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-F")
axis(1, at=1:3)
errbar(tou.sumary$Tourism, tou.sumary$changeN_mean, tou.sumary$changeN_mean-tou.sumary$changeL_sd, tou.sumary$changeN_mean+tou.sumary$changeN_sd, cap=0.05, cex=1.5, xaxt="n", xlab="Tourism intensity", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-N")
axis(1, at=1:3)
errbar(tou.sumary$Tourism, tou.sumary$changeR_mean, tou.sumary$changeR_mean-tou.sumary$changeR_sd, tou.sumary$changeR_mean+tou.sumary$changeR_sd, cap=0.05, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Change in mean EIV-R")
axis(1, at=1:3)

###ANOVA
##ACCESSIBILITY FOR SHEEP
#MERGED
summary(aov(bray.merged~acc.merged, data=change.per.summit.merged))
summary(aov(Light.merged[merged.old]-Light.merged[merged.new]~acc.merged, data=change.per.summit.merged))
summary(aov(Temperature.merged[merged.old]-Temperature.merged[merged.new]~acc.merged, data=change.per.summit.merged))
summary(aov(Moisture.merged[merged.old]-Moisture.merged[merged.new]~acc.merged, data=change.per.summit.merged))
summary(aov(Trophism.merged[merged.old]-Trophism.merged[merged.new]~acc.merged, data=change.per.summit.merged))
summary(aov(Reaction.merged[merged.old]-Reaction.merged[merged.new]~acc.merged, data=change.per.summit.merged))

#SAGORSKI
summary(aov(bray.sag~acc.sagorski, data=change.per.summit.sagorski))
summary(aov(Light.sag[sagorski.old]-Light.sag[sagorski.new]~acc.sagorski, data=change.per.summit.sagorski))
summary(aov(Temperature.sag[sagorski.old]-Temperature.sag[sagorski.new]~acc.sagorski, data=change.per.summit.sagorski))
summary(aov(Moisture.sag[sagorski.old]-Moisture.sag[sagorski.new]~acc.sagorski, data=change.per.summit.sagorski))
summary(aov(Trophism.sag[sagorski.old]-Trophism.sag[sagorski.new]~acc.sagorski, data=change.per.summit.sagorski))
summary(aov(Reaction.sag[sagorski.old]-Reaction.sag[sagorski.new]~acc.sagorski, data=change.per.summit.sagorski))

#KOTULA
summary(aov(bray.kot~acc.kotula, data=change.per.summit.kotula))
summary(aov(Light.kot[kotula.old]-Light.kot[kotula.new]~acc.kotula, data=change.per.summit.kotula))
summary(aov(Temperature.kot[kotula.old]-Temperature.kot[kotula.new]~acc.kotula, data=change.per.summit.kotula))
summary(aov(Moisture.kot[kotula.old]-Moisture.kot[kotula.new]~acc.kotula, data=change.per.summit.kotula))
summary(aov(Trophism.kot[kotula.old]-Trophism.kot[kotula.new]~acc.kotula, data=change.per.summit.kotula))
summary(aov(Reaction.kot[kotula.old]-Reaction.kot[kotula.new]~acc.kotula, data=change.per.summit.kotula))

plot(Light.kot[kotula.new]-Light.kot[kotula.old]~acc.kotula)
plot(Temperature.kot[kotula.new]-Temperature.kot[kotula.old]~acc.kotula)
plot(Trophism.kot[kotula.new]-Trophism.kot[kotula.old]~acc.kotula)

#PAWLOWSKI
summary(aov(bray.paw~acc.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Light.paw[pawlowski.old]-Light.paw[pawlowski.new]~acc.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Temperature.paw[pawlowski.old]-Temperature.paw[pawlowski.new]~acc.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Moisture.paw[pawlowski.old]-Moisture.paw[pawlowski.new]~acc.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Trophism.paw[pawlowski.old]-Trophism.paw[pawlowski.new]~acc.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Reaction.paw[pawlowski.old]-Reaction.paw[pawlowski.new]~acc.pawlowski, data=change.per.summit.pawlowski))

##TOURISM INTENSITY
#MERGED
summary(aov(bray.merged~tour.merged, data=change.per.summit.merged))
summary(aov(Light.merged[merged.old]-Light.merged[merged.new]~tour.merged, data=change.per.summit.merged))
summary(aov(Temperature.merged[merged.old]-Temperature.merged[merged.new]~tour.merged, data=change.per.summit.merged))
summary(aov(Moisture.merged[merged.old]-Moisture.merged[merged.new]~tour.merged, data=change.per.summit.merged))
summary(aov(Trophism.merged[merged.old]-Trophism.merged[merged.new]~tour.merged, data=change.per.summit.merged))
summary(aov(Reaction.merged[merged.old]-Reaction.merged[merged.new]~tour.merged, data=change.per.summit.merged))

#SAGORSKI
summary(aov(bray.sag~tour.sagorski, data=change.per.summit.sagorski))
summary(aov(Light.sag[sagorski.old]-Light.sag[sagorski.new]~tour.sagorski, data=change.per.summit.sagorski))
summary(aov(Temperature.sag[sagorski.old]-Temperature.sag[sagorski.new]~tour.sagorski, data=change.per.summit.sagorski))
summary(aov(Moisture.sag[sagorski.old]-Moisture.sag[sagorski.new]~tour.sagorski, data=change.per.summit.sagorski))
summary(aov(Trophism.sag[sagorski.old]-Trophism.sag[sagorski.new]~tour.sagorski, data=change.per.summit.sagorski))
summary(aov(Reaction.sag[sagorski.old]-Reaction.sag[sagorski.new]~tour.sagorski, data=change.per.summit.sagorski))

#KOTULA
summary(aov(bray.kot~tour.kotula, data=change.per.summit.kotula))
summary(aov(Light.kot[kotula.old]-Light.kot[kotula.new]~tour.kotula, data=change.per.summit.kotula))
summary(aov(Temperature.kot[kotula.old]-Temperature.kot[kotula.new]~tour.kotula, data=change.per.summit.kotula))
summary(aov(Moisture.kot[kotula.old]-Moisture.kot[kotula.new]~tour.kotula, data=change.per.summit.kotula))
summary(aov(Trophism.kot[kotula.old]-Trophism.kot[kotula.new]~tour.kotula, data=change.per.summit.kotula))
summary(aov(Reaction.kot[kotula.old]-Reaction.kot[kotula.new]~tour.kotula, data=change.per.summit.kotula))

#PAWLOWSKI
summary(aov(bray.paw~tour.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Light.paw[pawlowski.old]-Light.paw[pawlowski.new]~tour.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Temperature.paw[pawlowski.old]-Temperature.paw[pawlowski.new]~tour.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Moisture.paw[pawlowski.old]-Moisture.paw[pawlowski.new]~tour.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Trophism.paw[pawlowski.old]-Trophism.paw[pawlowski.new]~tour.pawlowski, data=change.per.summit.pawlowski))
summary(aov(Reaction.paw[pawlowski.old]-Reaction.paw[pawlowski.new]~tour.pawlowski, data=change.per.summit.pawlowski))

plot(Light.paw[pawlowski.new]-Light.paw[pawlowski.old]~tour.pawlowski)
plot(Moisture.paw[pawlowski.new]-Moisture.paw[pawlowski.old]~tour.pawlowski)

####CHANGE IN FREQUENCY####
merg.pres.old<-decostand(merg.old.df, method='pa')
merg.pres.new<-decostand(merg.new.df, method='pa')
old<-as.data.frame(rowSums(merg.pres.old))
new<-as.data.frame(rowSums(merg.pres.new))
n.spec<-414
no.sp <- gl(n = n.spec, k = 1)
freq<-cbind(old,new)
freq2<-cbind(old,new,no.sp)
chisq.test(freq[22,])
chisq.test(freq[263,])
chisq.test(freq[269,])
chisq.test(freq[398,])
chisq.test(freq[400,])

####CLIMATE CHANGE####
###KASPROWY WIERCH
lm1.ka<-lm(T~Year, data=climate.kasprowy.df)
lm2.ka<-lm(PP~Year, data=climate.kasprowy.df)
lm3.ka<-lm(RA~Year, data=climate.kasprowy.df)
lm4.ka<-lm(SN~Year, data=climate.kasprowy.df)
summary(lm1.ka)
summary(lm2.ka)
summary(lm3.ka)
summary(lm4.ka)

###LOMNICA
lm1.lo<-lm(T~Year, data=climate.lomnica.df)
lm2.lo<-lm(PP~Year, data=climate.lomnica.df)
lm3.lo<-lm(RA~Year, data=climate.lomnica.df)
lm4.lo<-lm(SN~Year, data=climate.lomnica.df)
summary(lm1.lo)
summary(lm2.lo)
summary(lm3.lo)
summary(lm4.lo)

###PLOTS
##KASPROWY WIERCH
x11()
par(mfrow=c(2,2))
plot(climate.kasprowy.df$T~climate.kasprowy.df$Year, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Mean annual temperature")
abline(lm(climate.kasprowy.df$T~climate.kasprowy.df$Year))
legend("topleft", c("R = 0.22**", "F = 9.01"), cex=1.2)
plot(climate.kasprowy.df$PP~climate.kasprowy.df$Year, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Mean annual precipitation")
abline(lm(climate.kasprowy.df$PP~climate.kasprowy.df$Year))
legend("topleft", c("R < 0.01 n.s.", "F = 0.07"), cex=1.2)
plot(climate.kasprowy.df$RA~climate.kasprowy.df$Year, cex=1.5, cex.axis=1.2, cex.lab=1.2, ylab="Number of days with rain", xlab="Year")
abline(lm(climate.kasprowy.df$RA~climate.kasprowy.df$Year))
legend("topleft", c("R = 0.19**", "F = 8.24"), cex=1.2)
plot(climate.kasprowy.df$SN~climate.kasprowy.df$Year, cex=1.5, cex.axis=1.2, cex.lab=1.2, ylab="Number of days with snow", xlab="")
abline(lm(climate.kasprowy.df$SN~climate.kasprowy.df$Year))
legend("bottomleft", c("R = 0.02 n.s.", "F = 1.68"), cex=1.2)

##LOMNICA
par(mfrow=c(2,2))
plot(climate.lomnica.df$T~climate.lomnica.df$Year, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Mean annual temperature")
abline(lm(climate.lomnica.df$T~climate.lomnica.df$Year))
legend("topleft", c("R = 0.46***", "F = 30.86"), cex=1.2)
plot(climate.lomnica.df$PP~climate.lomnica.df$Year, cex=1.5, xaxt="n", xlab="", cex.axis=1.2, cex.lab=1.2, ylab="Mean annual precipitation")
abline(lm(climate.lomnica.df$PP~climate.lomnica.df$Year))
legend("topleft", c("R = 0.49***", "F = 34.98"), cex=1.2)
plot(climate.lomnica.df$RA~climate.lomnica.df$Year, cex=1.5, cex.axis=1.2, cex.lab=1.2, ylab="Number of days with rain", xlab="Year")
abline(lm(climate.lomnica.df$RA~climate.lomnica.df$Year))
legend("bottomleft", c("R < 0.01 n.s.", "F = 0.07"), cex=1.2)
plot(climate.lomnica.df$SN~climate.lomnica.df$Year, cex=1.5, cex.axis=1.2, cex.lab=1.2, ylab="Number of days with snow", xlab="")
abline(lm(climate.lomnica.df$SN~climate.lomnica.df$Year))
legend("topright", c("R = 0.17**", "F = 8.25"), cex=1.2)