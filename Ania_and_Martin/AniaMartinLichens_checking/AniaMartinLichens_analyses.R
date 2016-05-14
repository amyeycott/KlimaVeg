source("AniaMartinLichens_dataloading.R")#this runs the same code as in that file

rich<-aggregate(olddb$Species, by=list(olddb$Site), FUN=length)
temp<-(aggregate(trimmednewdb$Species, by=list(trimmednewdb$Site), FUN=length))[2]
rich$trimmednew<-temp$x
names(rich)<-c("Site","old","trimmednew")
plot(rich$old, rich$trimmednew, xlab="Plot richness CRYPTO", ylab="Plot richness KlimaVeg")
cor.test(rich$trimmednew, rich$old, method="spearman")

library(vegan)
lichens.nmds<-metaMDS(subset(comp,select=-Year))
plot(lichens.nmds, display="sites")
points(lichens.nmds, display="sites", col=comp$Year)#totally separate subsets for each year. Taxonomy?

lichen.nmds.genus<-metaMDS(subset(genus_comp,select=-Year))
plot(lichen.nmds.genus, display="sites")
points(lichen.nmds.genus, display="sites", col=genus_comp$Year)#reducing to genus doesn't help


#############################
colSumsP <- function(x){
  x <- x > 0
  colSums(x)
}
  
abun <- subset(ddply(comp, .(Year), colSumsP), select=-Year)
plot(t(abun), xlab = 1992, ylab = 2015, main = "N occurences 215 vs 1992" )
abline(0,1)

gabun <- subset(ddply(genus_comp, .(Year), colSumsP), select=-Year)
plot(t(gabun))
abline(0,1)

table(comp$Year)
