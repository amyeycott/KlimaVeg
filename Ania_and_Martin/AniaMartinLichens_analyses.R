source("AniaMartinLichens_dataloading.R")#this runs the same code as in that file

length(unique(olddb$Species))
length(olddb$Species)
length(old.harm.db$Species)
length(unique(old.harm.db$Species))
length(unique(newdb$Species))
length(newdb$Species)
length(new.harm.db$Species)
length(unique(new.harm.db$Species))

rich<-aggregate(old.harm.db$Species, by=list(old.harm.db$Site), FUN=length)
temp<-(aggregate(new.harm.db$Species, by=list(new.harm.db$Site), FUN=length))[2]
rich$new.harm.<-temp$x
names(rich)<-c("Site","old.harm","new.harm.")
plot(rich$old.harm, rich$new.harm., xlab="Plot richness CRYPTO", ylab="Plot richness KlimaVeg")
cor.test(rich$new.harm., rich$old, method="spearman")

library(vegan)
lichens.nmds<-metaMDS(subset(comp,select=-Year))
plot(lichens.nmds, display="sites")
points(lichens.nmds, display="sites", col=comp$Year)#totally separate subsets for each year. Am using the harmonised data from both years.

lichen.nmds.genus<-metaMDS(subset(genus_comp,select=-Year))
plot(lichen.nmds.genus, display="sites")
points(lichen.nmds.genus, display="sites", col=genus_comp$Year)#reducing to genus doesn't help

lichen.nmds.1990<-metaMDS(comp[comp$Year==1990,-225])
lichen.nmds.2015<-metaMDS(comp[comp$Year==2015,-225])
protest(lichen.nmds.1990, lichen.nmds.2015)

#############################
colSumsP <- function(x){
  x <- x > 0
  colSums(x)
}
  
abun <- subset(ddply(comp, .(Year), colSumsP), select=-Year)
plot(t(abun), xlab = 1990, ylab = 2015, main = "N occurences 2015 vs 1990" )
abline(0,1)

gabun <- subset(ddply(genus_comp, .(Year), colSumsP), select=-Year)
plot(t(gabun))
abline(0,1)

############new 18th April - Wirth values
numcolwise(median)(old.wirths, na.rm=TRUE)
numcolwise(median)(new.wirths, na.rm=TRUE)
wilcox.test(old.wirths$L_light, new.wirths$L_light)
wilcox.test(old.wirths$T_temperature, new.wirths$T_temperature)
wilcox.test(old.wirths$K_continentality, new.wirths$K_continentality)
wilcox.test(old.wirths$F_moisture, new.wirths$F_moisture)
wilcox.test(old.wirths$R_reaction, new.wirths$R_reaction)
wilcox.test(old.wirths$N_nitrogen, new.wirths$N_nitrogen)
numcolwise(mean)(old.wirths, na.rm=TRUE)
numcolwise(mean)(new.wirths, na.rm=TRUE)
