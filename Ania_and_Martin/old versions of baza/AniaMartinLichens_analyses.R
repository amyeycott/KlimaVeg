source("AniaMartinLichens_dataloading.R")#this runs the same code as in that file

rich<-aggregate(olddb$Species, by=list(olddb$Site), FUN=length)
temp<-(aggregate(trimmednewdb$Species, by=list(trimmednewdb$Site), FUN=length))[2]
rich$trimmednew<-temp$x
names(rich)<-c("Site","old","trimmednew")
plot(rich$old, rich$trimmednew, xlab="Plot richness CRYPTO", ylab="Plot richness KlimaVeg")
cor.test(rich$trimmednew, rich$old, method="spearman")

library(vegan)
lichens.nmds<-metaMDS(subset(comp,select=-Year))
lichen.nmds.genus<-metaMDS(subset(genus_comp,select=-Year))

table(names(comp_old)%in%names(comp_trimnew))#147 species in common (212 species in trimnew, 159 species in old)

commonspp<-names(comp_old)[names(comp_old)%in%names(comp_trimnew)]#147 species in common (212 species in trimnew, 159 species in old)
comp_commonspp<-comp[,names(comp)%in%commonspp]

nmds.commons<-metaMDS(subset(comp_commonspp,select= -Year))
nmds.commons01<-metaMDS(subset((comp_commonspp)>0,select= -Year))


x11(); par(mfrow=c(2,2), mar=c(2,2,2,0))

plot(lichens.nmds, display="sites", main="all in Ania's subset")
points(lichens.nmds, display="sites", col=comp$Year)#totally separate subsets for each year. Taxonomy?

plot(lichen.nmds.genus, display="species", main="Genus level")
points(lichen.nmds.genus, display="sites", col=genus_comp$Year)#reducing to genus doesn't help
legend("topright", fill=c("yellow","grey"), legend=c("new","old"))

plot(nmds.commons, display="species", main="species in common")
points(nmds.commons, display="sites", col=comp_commonspp$Year)
plot(nmds.commons01, display="species", main="species in common pres-abs")
points(nmds.commons01, display="sites", col=comp_commonspp$Year)

savePlot("Lichen ordinations.pdf", type="pdf")
x11()
plot(nmds.commons, type="n", main="species in common")
text(nmds.commons, display = "species", cex=0.5)
savePlot("Species in common.pdf", type="pdf")
