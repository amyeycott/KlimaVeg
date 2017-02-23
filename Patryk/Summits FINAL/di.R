patryk.df <- read.table("clipboard", header = TRUE)
elev.df <- read.table("clipboard", header = T)
impact.df <- read.table("clipboard", header = T)
traits.df <- read.table("clipboard", header = T)
#setwd("C:\\Users\\abc\\Desktop")
names(patryk.df)

dim(patryk.df)

summit <- patryk.df[1, ]
spec <- patryk.df[-1, ]
a20 <- (1:14) * 2
a19 <- a20 - 1
spec19 <- spec[, a19]
spec20 <- spec[, a20]

na19<-NULL
for(i in 1:14){
a1<-as.vector(na.omit(spec19[,i]))
na19<-c(na19,a1)
}


na20<-NULL
for(i in 1:14){
a1<-as.vector(na.omit(spec20[,i]))
na20<-c(na20,a1)
}

plot(na19,na20,xlab="m below summmit in old data",ylab="Change in elevation")
abline(0,1)
abline(0,-1)


#######################################################

s50<-NULL			#s50 this identifies the species at ecah summit above 50 meters below summit
for(i in 1:28){
a1<-as.numeric(summit[i])-elev.df[,i]
s50a<-a1<50
s50<-cbind(s50,s50a)
}


spp<-colSums(s50)
plot(spp[a19],spp[a20],xlab="Species richness in old survey",ylab="Species richness in new survey")
abline(0,1)
text(spp[a19],spp[a20],labels=names(elev.df)[a19])
hist(spp [a19], breaks=12)
t.test(spp[a19],spp[a20],paired=T)

diff.sp<-s50[,a20]-s50[,a19]

col1<-colSums(diff.sp==1)
ext1<-colSums(diff.sp==-1)
pers1<-colSums(diff.sp==0&s50[,a19]==1)

names(elev.df)[a20]

library(vegan)

occ50<-rowSums(s50)
x11()

dca1<-decorana(t(s50[occ50>0,]))
plot(dca1, choice=c(1,2))
dca1
s1<-summary(dca1)
site1<-s1[[3]]

wilcox.test(site1[c(TRUE,FALSE),1], site1[c(FALSE,TRUE),1])  #ns
wilcox.test(site1[c(TRUE,FALSE),2], site1[c(FALSE,TRUE),2]) #nearly sig
wilcox.test(site1[c(TRUE,FALSE),3], site1[c(FALSE,TRUE),3]) #ns

#next week: test altitude against axis 1



##########dca1 vs dca2
plot(site1[,1],site1[,2],xlab="DCA1",ylab="DCA2")
text(site1[,1],site1[,2],labels=names(elev.df), col=rep(1:2, length(site1)/2), cex=0.75)
ordiarrows(dca1, levels=length(site1)/2, replicates=2)

###for(i in a19){
#lines(rbind(c(site1[i,1],site1[i,2]),c(site1[i+1,1],site1[i+1,2])))
#}
savePlot("dca1vs2.pdf",type="pdf")
######DCA1 vs DCA3
plot(site1[,1],site1[,3],xlab="DCA1",ylab="DCA3")
text(site1[,1],site1[,3],labels=names(elev.df), col=rep(1:2, length(site1)/2), cex=0.75)
ordiarrows(dca1, choices=c(1,3), levels=length(site1)/2, replicates=2)


#for(i in a19){
#lines(rbind(c(site1[i,1],site1[i,3]),c(site1[i+1,1],site1[i+1,3])))
#}
savePlot("dca1vs3.pdf",type="pdf")
######DCA2 vs DCA3

plot(site1[,2],site1[,3],xlab="DCA2",ylab="DCA3")
text(site1[,2],site1[,3],labels=names(elev.df), col=rep(1:2, length(site1)/2), cex=0.75)
ordiarrows(dca1, choices=c(2,3), levels=length(site1)/2, replicates=2)

#for(i in a19){
#  lines(rbind(c(site1[i,2],site1[i,3]),c(site1[i+1,2],site1[i+1,3])))
#}
savePlot("dca2vs3.pdf",type="pdf")
############################
sp1<-s1[[1]]
plot(sp1[,1],sp1[,2],xlab="DCA1",ylab="DCA2",type="n")
text(sp1[,1],sp1[,2],rownames(elev.df)[occ50>0],cex=0.5)


####CCA with time as a predictor#####123
elev.df<-read.table("tops.csv", sep=";", dec=",", header=TRUE) ###dataframe equal to elev.df from clipboard#
elev2.df<-elev.df[-1,]###dataframe equal to elev.df from clipboard#
summit <- elev.df[1, ]#

s50<-NULL			#s50 this identifies the species at ecah summit above 50 meters below summit#
for(i in 1:28){#
  a1<-as.numeric(summit[i])-elev2.df[,i] #
  s50a<-a1<50#
  s50<-cbind(s50,s50a)#
}#

occ50<-rowSums(s50)#

old.new<-rep(c(0,1),14)#1
summit.number<-c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14)#
cca1<-cca(t(s50[occ50>0,])~old.new)#

set.seed(42)#
h<-how(within=Within(type="series", constant=TRUE), plots=Plots(strata=summit.number, type="free"))#
anova(cca1, permutations=h)#
####end of cca######123




rownames(elev.df)[1:25]


################################
#change in tourism

t.change<-impact.df[a20,2]-impact.df[a19,2]
dca.change<-site1[a20,2]-site1[a19,2]

plot(t.change,dca.change)


spp.change<-spp[a20]-spp[a19]
plot(t.change,spp.change)

################################
#change in grazing

g.change<-impact.df[a20,1]-impact.df[a19,1]
dca.change<-site1[a20,1]-site1[a19,1]

plot(g.change,dca.change,type="n",xlab="Change in grazing",ylab="Change in species composition (DCA)")
text(g.change,dca.change,labels=names(elev.df)[a20],cex=0.8)
cor.test(g.change,dca.change)

plot(g.change,spp.change)
cor.test(g.change,spp.change)

##################################
#trait values

trait1<-NULL
for(i in 1:28)
{
a1<-s50[,i]
t1<-traits.df[a1,2] #chabge from 1-5 for all traits
t0<-mean(na.omit(t1))
trait1<-c(trait1,t0)
}

t.test(trait1[a19],trait1[a20],paired=T)

trait.change<-trait1[a20]-trait1[a19]
plot(g.change,trait.change,type="n",xlab="Change in grazing",ylab="Change in trait (Tro)") #change name of x-axis
text(g.change,trait.change,labels=names(elev.df)[a20],cex=0.8)


fit1<-lm(trait.change~t.change) #change to t.change for tourism
anova(fit1,test="F")
fit1

fit1<-lm(trait.change~g.change)
anova(fit1,test="F")
fit1
#############NEW

setwd("C:\\Users\\abc\\Desktop\\tops authors")
lifeforms.df<-read.table("lifeforms.csv", sep=";", dec=",", header=TRUE)
head(lifeforms.df)
life1<-lifeforms.df$Lifeform
sp.df<-lifeforms.df[,3:53]
life1
author1<-read.table("clipboard", header=TRUE)

table(author1$Author)
au1<-author1$Author
su1<-author1$mount
head(author1)
czo<-au1=="Czortek.et.al.2014"
kot<-au1=="Kotula.1889-90"
sag<-au1=="Sagorski.1891"
paw<-au1=="Pawlowski.1956"

kot.df<-sp.df[,kot==1]
sag.df<-sp.df[,sag==1]
paw.df<-sp.df[,paw==1]
czo.df<-sp.df[,czo==1]
kot.s<-su1[kot==1]
sag.s<-su1[sag==1]
paw.s<-su1[paw==1]
czo.s<-su1[czo==1]
kot.el<-author1$elev.summit[kot==1]
sag.el<-author1$elev.summit[sag==1]
paw.el<-author1$elev.summit[paw==1]
czo.el<-author1$elev.summit[czo==1]
########## DIVERSITY ##########33

a1<-length(sag.s) #change to paw, sag 
matr1<-NULL
for(i in 1:a1){
b0<-czo.df[,czo.s==sag.s[i]] #change kot.s to sag.s and paw.s
b1<-sag.el[i]-b0
matr1<-cbind(matr1,b1)
}

czo.sag<-matr1<200 # czo.kot to czo.sag and paw.sag and change 50 to 20 and 100

a1<-length(sag.s) #change to paw, sag 
matr1<-NULL
for(i in 1:a1){
  b0<-sag.df[,i] #change kot.df to sag.df and paw.df
  b1<-sag.el[i]-b0
  matr1<-cbind(matr1,b1)
}
sag.sum<-matr1<200


spp.sag<-colSums(sag.sum>0,na.rm = T) #the same kot to paw and sag
spp.czo<-colSums(czo.sag>0,na.rm = T) #the same kot to sag and paw
x11()
plot(spp.sag,spp.czo) #the same
text(spp.sag,spp.czo,labels=names(sag.df),cex=0.8)
abline(0,1)

##############
a1<-length(kot.s)
matr1<-NULL
for(i in 1:a1){
  b1<-czo.s[czo.s==kot.s[i]]
  matr1<-c(matr1,b1)
}

czo.kot<-matr1
####################

