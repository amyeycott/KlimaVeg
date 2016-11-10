source("Bryophyte data loading.R")
on.ls.ash<-bryophytes[(bryophytes$Fe_LS)>0,c(1:4,which(names(bryophytes)%in%S.in.LS), 55:68)] #this makes a subset where all the ocurrences recorded on living ash are selected
on.ls.ash$Species_name<-droplevels(on.ls.ash$Species_name)
on.cwd.ash<-bryophytes[(bryophytes$Fe_CWD)>0,c(1:4,which(names(bryophytes)%in%S.in.CWD))]
on.cwd.ash$Species_name<-droplevels(on.cwd.ash$Species_name)

FeLS2015table.byplot<-table(on.ls.ash$Species_name[on.ls.ash$Year==2015], on.ls.ash$Plot[on.ls.ash$Year==2015])
FeLS1992table.byplot<-table(on.ls.ash$Species_name[on.ls.ash$Year==1992], on.ls.ash$Plot[on.ls.ash$Year==1992])
FeCWD2015table.byplot<-table(on.cwd.ash$Species_name, on.cwd.ash$Plot) #makes plot x species table for FeCWD2015
summary(colSums(FeCWD2015table.byplot))#the summary of species richnesses of different plots which had something growing on ash CWD

#Mean epiphytic/epixylic species richness per plot on Fe:
means.Fe<-c(mean(colSums(FeLS1992table.byplot)), mean(colSums(FeLS2015table.byplot)), mean(colSums(FeCWD2015table.byplot)))
sd.Fe<-c(sd(colSums(FeLS1992table.byplot)), sd(colSums(FeLS2015table.byplot)), sd(colSums(FeCWD2015table.byplot)))
plot(means.Fe, ylab="mean plot species richness", xaxt="n", ylim=c(0,30), xlab=NA)
axis(1, labels=c("LS1992","LS2015","CWD2015"), at=c(1,2,3))
#but better with error bars!
library(Hmisc)
x11(4,3); par(mar=c(3,4,0.1,0.1))
errbar(c(1.2,2,2.8),means.Fe, (means.Fe+sd.Fe) , (means.Fe-sd.Fe), ylim=c(0,40), pch=16,ylab="mean plot species richness", xlab=NA, xaxt="n", xlim=c(1,3), cex.axis=1.2, cex.lab=1.2, cex=1.2, col=c("forestgreen", "forestgreen", "chocolate4"), errbar.col=c("forestgreen", "forestgreen", "chocolate4"), lwd=2)
axis(1, labels=c("LS1992","LS2015","CWD2015"), at=c(1.2,2,2.8), cex=1.5) #this is figure 1 in the summary document
savePlot("Figure 1 richness per plot for presentation.emf", type="emf")

#how many plots have something reported from living ash? This is table 1 in the summary document.
dim(FeLS1992table.byplot) #66 x 64 sixty six species in 64 plots
dim(FeLS2015table.byplot)#66 x 52 sixty seven species in 52 plots, note 60 in common with FeLS1992
dim(FeCWD2015table.byplot) #104 x 69 a hundred and four species in sixty nine plots

####HOSTS stuff starts here - also some hosts stuff later, currently around lines 150-180####


#trying to make the number of other species a bryophyte is found on, per plot
hosts.LS2015<-on.ls.ash[on.ls.ash$Year==2015, 1:4] #has to be set up year by year then merged as diff lengths per year
hosts.LS2015$hosts.LS2015<-rowSums(on.ls.ash[on.ls.ash$Year==2015, 8:length(on.ls.ash)], na.rm=TRUE)# note to myself: this is ok order in at least the first 12 lines

hosts.LS1992<-on.ls.ash[on.ls.ash$Year==1992, 1:4] 
hosts.LS1992$hosts.LS1992<-rowSums(on.ls.ash[on.ls.ash$Year==1992, 8:length(on.ls.ash)], na.rm=TRUE)# note to myself: ordering is ok in at least the first 12 lines.

hosts.CWD2015<-on.cwd.ash[1:4] 
hosts.CWD2015$hosts.CWD2015<-rowSums(on.cwd.ash[, 8:length(on.cwd.ash)], na.rm=TRUE)

#error-bar plot of mean and sd of number of hosts per plot for Fe epiphytic and epixylic species:
by.LS1992<-by(hosts.LS1992$hosts.LS1992, hosts.LS1992$Plot, mean)
by.LS2015<-by(hosts.LS2015$hosts.LS2015, hosts.LS2015$Plot, mean)
by.CWD2015<-by(hosts.CWD2015$hosts.CWD2015, hosts.CWD2015$Plot, mean)

errbar(c(1.2,2,2.8),c(mean(by.LS1992), mean(by.LS2015), mean(by.CWD2015)),c(mean(by.LS1992)+sd(by.LS1992),mean(by.LS2015)+sd(by.LS2015),mean(by.CWD2015)+sd(by.CWD2015)),c(mean(by.LS1992)-sd(by.LS1992),mean(by.LS2015)-sd(by.LS2015),mean(by.CWD2015)-sd(by.CWD2015)),xaxt="n", xlab=NA, xlim=c(1,3), main="Hosts per ash-epiphytic bryophyte species per plot", ylab="n hosts +- 1 sd", cex.axis=1.2, cex.lab=1.2, cex=1.2, col=c("forestgreen", "forestgreen", "chocolate4"), errbar.col=c("forestgreen", "forestgreen", "chocolate4"), lwd=2)
axis(1, labels=c("LS1992","LS2015","CWD2015"), at=c(1.2,2,2.8), cex=1.5) #this is Figure 2 in the summary document.
savePlot("Figure 2 alt hosts per plot for presentation.emf", type="emf")

#who is on CWD and NOT on LS? #This is Table 2 in the summary document.
they.like.their.ash.dead<-unique(on.cwd.ash$Species_name[!on.cwd.ash$Species_name%in%on.ls.ash$Species_name])#don't stress about learning this, the syntax is a bastard
#are they only only on CWD?
colSums(bryophytes[bryophytes$Species_name%in%they.like.their.ash.dead, colnames(bryophytes)%in%S.in.inorganic], na.rm=TRUE)#tthey.like.their.ash.dead... and S4 mineral soil. There are 146 ocurrences incorporating 37 species of those on ash CWD but not ash LS. These 37 species are recorded a total 1414 times on inorganic substrates (1000 of these on soil).
colSums(bryophytes[bryophytes$Species_name%in%they.like.their.ash.dead, colnames(bryophytes)%in%s.in.fineorganic], na.rm=TRUE)# tthey.like.their.ash.dead... and conifer litter 
colSums(bryophytes[bryophytes$Species_name%in%they.like.their.ash.dead, colnames(bryophytes)%in%S.in.LS], na.rm=TRUE)# they like dead ash... and other species of tree living.

#finding out where else they grow for each theyliketheirashdead species
colSums(do.call(rbind, by(bryophytes[bryophytes$Species_name%in%they.like.their.ash.dead, c(5:9,11,12,16,18,20, 22:30)], bryophytes$Species_name[bryophytes$Species_name%in%they.like.their.ash.dead], colSums)))#they like S4 best, by a long way
do.call(rbind,by(bryophytes[bryophytes$Species_name%in%they.like.their.ash.dead, colnames(bryophytes)%in%S.in.fineorganic], bryophytes$Species_name[bryophytes$Species_name%in%they.like.their.ash.dead], colSums))#it is the Sphagna who like conifer litter
do.call(rbind, by(bryophytes[bryophytes$Species_name%in%they.like.their.ash.dead, colnames(bryophytes)%in%S.in.inorganic], bryophytes$Species_name[bryophytes$Species_name%in%they.like.their.ash.dead], colSums))

#who is on Fe LS and not Fe CWD*?
they.like.their.ash.alive<-unique(on.ls.ash$Species_name[!on.ls.ash$Species_name%in%on.cwd.ash$Species_name])

#####composition stuff starts here####
library(vegan)
str(on.ls.ash)
ashtabx<-xtabs(Frequency~(paste(Plot,Year, sep=""))+Species_name,  data=on.ls.ash[,1:4], drop.unused.levels=TRUE) #plots x species, because that is what ordinations need
unique(ashtabx) #check there's no silly numbers in the result

#had a look with decorana first
ash.dca<-decorana(ashtabx)
plot(ash.dca)#very very strong first axis
summary(ash.dca)
sitescores.ash.dca<-as.data.frame(scores(ash.dca, display="sites"))#Gives site and species scores. 
sitescores.ash.dca[(sitescores.ash.dca$DCA1)>1,]#big outliers are C11992, E012015, G081992
speciesscores.ash.dca<-as.data.frame(scores(ash.dca, display="species"))
speciesscores.ash.dca[(speciesscores.ash.dca$DCA1)>1,]#outlier species are ground species that happen to come on stems. Maybe look up weighting instead? There's a blog post on this.

#but nmds is considered more robust
ash.nmds<-metaMDS(ashtabx)
plot(ash.nmds)
ash.nmds$points
ash.nmds$species

x11();
plot(ash.nmds, display="sites",type="n")
points(ash.nmds, col=as.factor(substr(rownames(ashtabx),4,7)))# 1992 is black and 2015 is red. nice - this is Figure 3 in the summary document. 
ordiarrows(ash.nmds, groups=substr(rownames(scores(ash.nmds)), 1,3)) #crashycrash because some of the arrows have no length as not all the sites are paired
#savePlot("whywecanthavearrows.jpg", type="jpg")
points(ash.nmds, display="species", col="green")
orditorp(ash.nmds,display="species",col="red",air=0.01)
fig<-ordiplot(ash.nmds)
#identify(fig, what="species")
#identify(fig, what="sites")

t.test(scores(ash.nmds)[(substr(rownames(scores(ash.nmds)), 4,7))=="1992",1],scores(ash.nmds)[(substr(rownames(scores(ash.nmds)), 4,7))=="2015",1])# Difference between years, t94=2.202, p=0.0301. This is interesting because there is no consistent change in composition at the whole area level. But what about at the level of just the LS2015 plots?
FeLSsites2015<-colnames(FeLS2015table.byplot)
FeLSsites.allspp.nmds<-metaMDS(easytabx[which(substr(rownames(easytabx),1,3)%in%FeLSsites2015),])
t.test(scores(FeLSsites.allspp.nmds)[(substr(rownames(scores(FeLSsites.allspp.nmds)), 4,7))=="1992",1],scores(FeLSsites.allspp.nmds)[(substr(rownames(scores(FeLSsites.allspp.nmds)), 4,7))=="2015",1])

#is 2015LS less diverse than 1992LS?
betatest<-betadisper(vegdist(ashtabx), group=substr(rownames(ashtabx),4,7))
anova(betatest)#no it's not, F1,113=0.295, p= 0.5883.

#is 2015 CWD more like 1992 FE_LS or like 2015 Fe_LS? 
#Start by finding sites which appear in all of the datasets. 
constantsites<-intersect(colnames(FeLS2015table.byplot), colnames(FeCWD2015table.byplot))
constantsites<-intersect(constantsites,colnames(FeLS1992table.byplot))
  
CWDnmds<-metaMDS(t(FeCWD2015table.byplot[,which(colnames(FeCWD2015table.byplot)%in%constantsites)]))
FeLS1992nmds<-metaMDS(t(FeLS1992table.byplot[,which(colnames(FeLS1992table.byplot)%in%constantsites)]))
FeLS2015nmds<-metaMDS(t(FeLS2015table.byplot[,which(colnames(FeLS2015table.byplot)%in%constantsites)]))

protest(CWDnmds, FeLS2015nmds)#correlation 0.39, sig 0.032(unstable, consider more permutations)
protest(CWDnmds, FeLS1992nmds)# correlation 0.037, P=0.944!!!
# but mantel tests are more direct.

CWD2015dist<-vegdist(t(FeCWD2015table.byplot[,which(colnames(FeCWD2015table.byplot)%in%constantsites)]))
FeLS1992dist<-vegdist(t(FeLS1992table.byplot[,which(colnames(FeLS1992table.byplot)%in%constantsites)]))#72 35
FeLS2015dist<-vegdist(t(FeLS2015table.byplot[,which(colnames(FeLS2015table.byplot)%in%constantsites)]))
mantel(CWD2015dist, FeLS2015dist) #r=0.2753, P=0.056
mantel(CWD2015dist, FeLS1992dist)# r=0.1205, P=0.195

####Rank-ocurrence#####
occurrencesnew<-as.data.frame(rowSums(FeLS2015table.byplot))
occurrencesold<-as.data.frame(rowSums(FeLS1992table.byplot))#not perfectly overlapping, depsite containing the same number of species they are different
occurrences<-merge(occurrencesold, occurrencesnew, by=0, all=TRUE)
names(occurrences)<-c("Species","FeLS1992","FeLS2015")
head(occurrences)
occurrences[is.na(occurrences)]<-0
occurrencesDEADW<-as.data.frame(rowSums(FeCWD2015table.byplot)) #can't be joined into the other one because they are different sizes

#plot where species are seperately ranked for each year, so two bars overlapping may be for different actual species.
x11(5,8); par(mfrow=c(2,1))
barplot(sort(occurrences$FeLS1992, decreasing=TRUE), ylim=c(0,60), xlab="Species ranked by occurrences", ylab="Total occurrences", main="both years sorted")
barplot(sort(occurrences$FeLS2015, decreasing=TRUE), col="yellow", add=TRUE)
legend("topright", legend=c("1992", "2015"), fill=c("grey","yellow"))

#plot where each bar represents a specific species, regardless of year
occurrences.ordered<-occurrences[(order(occurrences$FeLS1992, decreasing=TRUE)),]#this is the line to be suspicious of.
barplot(occurrences.ordered$FeLS1992, ylim=c(0,60), xlab="Species ranked by occurrence in 1992", ylab="Total occurrences", main="each bar is one species")
barplot(occurrences.ordered$FeLS2015, col="yellow", add=TRUE)
#legend("topright", legend=c("1992", "2015"), fill=c("grey","yellow"))
savePlot("rank abundance strangeness.pdf", type="pdf")
savePlot("rank abundance strangeness.emf", type="emf")

#winners, those who increased from fewer than 5 plots in 1992 to more than 5 plots in 2015
occurrences[(occurrences$FeLS1992)<5&(occurrences$FeLS2015)>5,]
#losers, those who decreased from more than 5 plots in 1992 to fewer than 5 plots in 2015
occurrences[(occurrences$FeLS1992)>5&(occurrences$FeLS2015)<5,]
#who's on most deadwood in 2015?
occurrencesDEADW[(occurrencesDEADW$`rowSums(FeCWD2015table.byplot)`)>40,]


#how many hosts does each epiphytic bryophyte have? How many hosts do ash bryophytes have?
hostsall.2015<-aggregate(bryophytes[bryophytes$Year==2015, 61:74], by=list(bryophytes$Species_name[bryophytes$Year==2015]), max)
hostsall.2015$max<-rowSums((hostsall.2015[,2:length(hostsall.2015)]>0), na.rm=T)
hostsall.2015<-hostsall.2015[(hostsall.2015$max)>0,]
hist(hostsall.2015$max[(hostsall.2015$max)>0])#show the distribution of nhosts
str(hosts.LS2015)
mean(hostsall.2015$max)
mean(hostsall.2015$max[(hostsall.2015$Fe_LS==1)])

t.test(hostsall.2015$max[(hostsall.2015$Group.1%in%hosts.LS2015$Species_name)], hostsall.2015$max[(!hostsall.2015$Group.1%in%hosts.LS2015$Species_name)])

#other hosts
generalisty.2015<-sapply(hostsall.2015[, 2:(length(hostsall.2015)-1)], function(x){
  mean(hostsall.2015$max[x==1])
})
generalisty.2015
generalisty.testy<-sapply(hostsall.2015[, 2:(length(hostsall.2015)-1)], function(x){
  t.test(hostsall.2015$max[x==1],hostsall.2015$max[x==1])
})
generalisty.testy #not enough observations

#what about 1992?
hostsall.1992<-aggregate(bryophytes[bryophytes$Year==1992, 61:74], by=list(bryophytes$Species_name[bryophytes$Year==1992]), max)
hostsall.1992$max<-rowSums((hostsall.1992[,2:length(hostsall.1992)]>0), na.rm=T)
hostsall.1992<-hostsall.1992[(hostsall.1992$max)>0,]
generalisty.1992<-sapply(hostsall.1992[, 2:(length(hostsall.1992)-1)], function(x){
  mean(hostsall.1992$max[x==1])
})
generalisty.1992
t.test(hostsall.2015$max[hostsall.2015$Fe_LS==1], hostsall.1992$max[hostsall.1992$Fe_LS==1])

#simple stuff :-)
#n epiphytic hosts per plot plotted against ash epiphyte richness per plot
#plot n epiphytic hosts
bryophytes[bryophytes$Plot=="A01"&bryophytes$Year==2015,]
library(plyr)
#plotrich<-ddply(bryophytes[,c(1:3,61:74)], .variables = ~Year+Plot, nrow)
plothostrich<-ddply(bryophytes[,c(1:3,61:74)], .variables = ~Year+Plot, function(x){
  sum(colSums(x[,-(1:3)])>0)
})
plotFe_LSrich<-ddply(bryophytes[,c("Plot","Year","Species_name","Fe_LS")], .variables = ~Year+Plot, function(x)sum(x$Fe_LS))
x11()
plot(plothostrich$V1, plotFe_LSrich$V1)
totalhosts<-ddply(bryophytes[,c(1:3,61:74)], .variables = ~Year+Species_name, function(x){
  sum(colSums(x[,-(1:3)])>0)
})
hostsocurrences<-ddply(bryophytes[,c(1:3,61:74)], .variables = ~Year, function(x){
  colSums(x[,-(1:3)])
})

hostsocurrences<-ddply(bryophytes[,c(1:3,61:74)], .variables = ~Year, function(x){
  colSums(x[,-(1:3)])
})

library(ggplot2)
ggplot(cbind(plothostrich, Fe_LS= plotFe_LSrich$V1), aes(x=V1, y = Fe_LS)) +
  geom_jitter() +facet_wrap(~Year)+xlab("Number of epiphyte-hosting tree species")+ylab("Number of ash-epiphytic species")+theme_bw()
#there are a bunch on the left in 1992 not in 2015 - are these the plots which lost their Fe???
#next thing to plot is number of hosts per plot against ash-specialists (IndVal? More than 10% of ocurrences are on ash?) or things that can live on ash. 


#Figure on hosts for ms. Lines 30-40 have it per plot, and then line 150 onwards 
#asked for: Histogram of total n alternative hosts and barplot of ash epiphytes per host. Also for CWD? (3x2 – LS 1992, LS2015, CWD 2015).
x11(8,6); par(mfrow=c(2,3))
hist(hostsall.1992$max[hostsall.1992$Fe_LS==1], xlab="Number of hosts for F. excelsior epiphyte species", main="1992", xlim=c(0,14), ylim=c(0,25), las=1)
hist(hostsall.2015$max[(hostsall.2015$Group.1%in%hosts.LS2015$Species_name)], xlab="Number of hosts for F. excelsior epiphyte species", main="2015", xlim=c(0,14), ylim=c(0,25), las=1) #two different ways of the same thing... why?
hist(hostsall.2015$max[(hostsall.2015$Group.1%in%hosts.CWD2015$Species_name)], xlab="Number of hosts for F. excelsior saproxylic species", main="CWD2015", xlim=c(0,14), ylim=c(0,25), breaks=14, las=1)
#something seems wrong with hostsall.2015 - there can't be 70 epiphytic species on alnus glutinosa. But there is, there's 84, I checked using "bryophytes"!
barplot(colSums(hostsall.1992[,substr(colnames(hostsall.1992),4,5)=="LS"]), names.arg =substr(colnames(hostsall.1992[,substr(colnames(hostsall.1992),4,5)=="LS"]),1,2), las=2, xlab="Host tree", ylab="Number of epiphyte species")
barplot(colSums(hostsall.2015[,substr(colnames(hostsall.2015),4,5)=="LS"]), names.arg =substr(colnames(hostsall.2015[,substr(colnames(hostsall.2015),4,5)=="LS"]),1,2), las=2, xlab="Host tree", ylab="Number of epiphyte species")
hostsall.CWD2015<-aggregate(bryophytes[bryophytes$Year==2015, substr(colnames(bryophytes),4,6)=="CWD"], by=list(bryophytes$Species_name[bryophytes$Year==2015]), max)
barplot(colSums(hostsall.CWD2015[,substr(colnames(hostsall.CWD2015),4,6)=="CWD"]), names.arg =substr(colnames(hostsall.CWD2015[,substr(colnames(hostsall.CWD2015),4,6)=="CWD"]),1,2), las=2, xlab="Host tree", ylab="Number of epiphyte species")#hostsall.CWD2015 needs making
savePlot("Figure Hosts 3x2.emf", type="emf")
savePlot("Figure Hosts 3x2.pdf", type="pdf")
