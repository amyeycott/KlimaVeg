#next steps: epiphytes only, dataset minus PP squares, then maybe both of those. Also consider 'obvious' species only.
source("AniaMartinLichens_dataloading.R")
library(Hmisc)
library(ggplot2)
library(plyr)
envir$Trentepohlia_photobiont [is.na(envir$Trentepohlia_photobiont)]<-0#use NA for missing data, not as no in yes/no
hastrent<-envir$Species[envir$Trentepohlia_photobiont==1]
  
###the relationship between Tentapohliod-ness and Wierz values.N is lower for Tp, Temp is higher, and F humidity is not different.
ggplot(envir, aes(x = Trentepohlia_photobiont, y = N_nitrogen)) + gnoPPm_violin()#not normally distributed, use Wilcoxon 
ggplot(envir, aes(x = Trentepohlia_photobiont, y = T_temperature)) + gnoPPm_violin()#replace with boxplot
ggplot(envir, aes(x = Trentepohlia_photobiont, y = F_moisture)) + gnoPPm_violin()
tapply(envir$N_nitrogen, envir$Trentepohlia_photobiont,median, na.rm=TRUE)
wilcox.test(N_nitrogen~Trentepohlia_photobiont, data = envir)#sig lower N for Tp
tapply(envir$T_temperature, envir$Trentepohlia_photobiont,median, na.rm=TRUE)
wilcox.test(T_temperature~Trentepohlia_photobiont, data = envir)#sig higher temp for Tp
wilcox.test(F_moisture~Trentepohlia_photobiont, data = envir)#NS and mean diff is relatively small too (0.22)
tapply(envir$F_moisture, envir$Trentepohlia_photobiont,median, na.rm=TRUE)



#Count and frequency analyses. This is repeated many times but with the same object names, just on different subsets. The harmonised versions are always used. The epiphytes only starts on line 88, PP only on line 153:
#####looking at how the numbers and proportions of tentraphloid species have changed over time######
length(unique(new.harm.db$Species[new.harm.db$Species%in%hastrent]))/length(unique(new.harm.db$Species))
length(unique(old.harm.db$Species[old.harm.db$Species%in%hastrent]))/length(unique(old.harm.db$Species))

newfrq <- as.data.frame.matrix(table(new.harm.db$Frequency, new.harm.db$Species %in% hastrent))
oldfrq <- as.data.frame.matrix(table(old.harm.db$Frequency, old.harm.db$Species %in% hastrent))#needs a more sensible way of looking at it. Proportion of 3's that were Tp? Actually a teeny bit higher in old set (need a suitable test)

x11(7,5); par(mfrow=c(1,2), mar=c(4,4,3,0.1), las=1, xpd=NA)
barplot(c(table(!old.harm.db$Species%in%hastrent),table(!new.harm.db$Species%in%hastrent)), names.arg=c("Tp", "Not Tp","Tp","Not Tp"), main="Total records", ylab="Count")
text(c(1.2,3.7), -1200, c("1990", "2015"))

barplot(as.matrix(cbind(oldfrq[2]/colSums(oldfrq[2]), oldfrq[1]/colSums(oldfrq[1]),newfrq[2]/colSums(newfrq[2]), newfrq[1]/colSums(newfrq[1]))), names.arg=c("Tp", "Not Tp","Tp","Not Tp"), main="Assigned frequency scores", ylab="Proportion")
text(c(1.2,3.7), -0.17, c("1990", "2015"))
savePlot("Counts and assigned scores side by side.emf", type="emf")

Tpspp_per_plot.1990<-aggregate(old.harm.db$Frequency[old.harm.db$Species%in%hastrent], by=list(old.harm.db$Site[old.harm.db$Species%in%hastrent]), NROW)
NotTpspp_per_plot.1990<-aggregate(old.harm.db$Frequency[!old.harm.db$Species%in%hastrent], by=list(old.harm.db$Site[!old.harm.db$Species%in%hastrent]), NROW)

Tpspp_per_plot.2015<-aggregate(new.harm.db$Frequency[new.harm.db$Species%in%hastrent], by=list(new.harm.db$Site[new.harm.db$Species%in%hastrent]), NROW)
NotTpspp_per_plot.2015<-aggregate(new.harm.db$Frequency[!new.harm.db$Species%in%hastrent], by=list(new.harm.db$Site[!new.harm.db$Species%in%hastrent]), NROW)

Tpcount.perplot.1990<-merge(NotTpspp_per_plot.1990, Tpspp_per_plot.1990, all=TRUE, by=1)
Tpcount.perplot.1990[is.na(Tpcount.perplot.1990)]<-0
Tpcount.perplot.1990$proportion1990<-Tpcount.perplot.1990$x.y/Tpcount.perplot.1990$x.x
Tpcount.perplot.2015<-merge(NotTpspp_per_plot.2015, Tpspp_per_plot.2015, all=TRUE, by=1)
Tpcount.perplot.2015[is.na(Tpcount.perplot.2015)]<-0
Tpcount.perplot.2015$proportion2015<-Tpcount.perplot.2015$x.y/Tpcount.perplot.2015$x.x
wilcox.test(Tpcount.perplot.2015$proportion2015, Tpcount.perplot.1990$proportion1990) #ns

boxplot(list(Tpcount.perplot.1990$proportion1990, Tpcount.perplot.2015$proportion2015), names = c(1990, 2015), ylab="Proportion of Tp-containg species per plot")
boxplot(list(Tpcount.perplot.1990$x.y, Tpcount.perplot.2015$x.y), names = c(1990, 2015), ylab="Number of Tp-containg species per plot")
wilcox.test(Tpcount.perplot.2015$x.y, Tpcount.perplot.1990$x.y)#overall counts of Tp per plot higher in 2015, but due to large increase in n. records.
savePlot("Tp per plot counts and proportions_harmonised.emf", type="emf")

numcolwise(mean, na.rm=TRUE)(Tpcount.perplot.1990)
numcolwise(mean, na.rm=TRUE)(Tpcount.perplot.2015)

Tp.freqperspp1990<-aggregate(old.harm.db$Frequency[old.harm.db$Species%in%hastrent], by=list(old.harm.db$Species[old.harm.db$Species%in%hastrent]), NROW)
NotTp.freqperspp1990<-aggregate(old.harm.db$Frequency[!old.harm.db$Species%in%hastrent], by=list(old.harm.db$Species[!old.harm.db$Species%in%hastrent]), NROW)
Tp.freqperspp2015<-aggregate(new.harm.db$Frequency[new.harm.db$Species%in%hastrent], by=list(new.harm.db$Species[new.harm.db$Species%in%hastrent]), NROW)
NotTp.freqperspp2015<-aggregate(new.harm.db$Frequency[!new.harm.db$Species%in%hastrent], by=list(new.harm.db$Species[!new.harm.db$Species%in%hastrent]), NROW)

c(mean(Tp.freqperspp1990$x, na.rm=TRUE), mean(NotTp.freqperspp1990$x, na.rm=TRUE), mean(Tp.freqperspp2015$x, na.rm=TRUE), mean(NotTp.freqperspp2015$x, na.rm=TRUE))



#######The same again, but with only the easy, common Trentapoloids. Needs repeating with the easy, common everythings!######
hastrent.limited<-hastrent[envir$Is_it_easy_to_id==1]
table(new.harm.db$Frequency, new.harm.db$Species%in%hastrent.limited)
table(old.harm.db$Frequency, old.harm.db$Species%in%hastrent.limited)

x11()
#barplot(c(NROW(new.harm.db$Frequency[new.harm.db$Species%in%hastrent.limited]), NROW(new.harm.db$Frequency[!new.harm.db$Species%in%hastrent.limited]), NROW(old.harm.db$Frequency[old.harm.db$Species%in%hastrent.limited]), NROW(new.harm.db$Frequency[!new.harm.db$Species%in%hastrent.limited])), names.arg=c("Tp 2015","Not Tp 2015", "Tp 1990", "Not Tp 1990"))#, log="y")

barplot(c(table(!new.harm.db$Species%in%hastrent.limited), table(!old.harm.db$Species%in%hastrent.limited)), names.arg=c("Tp 2015","Not Tp 2015", "Tp 1990", "Not Tp 1990"))#, log="y")

length(new.harm.db)
Tpspp_per_plot.1990<-aggregate(old.harm.db$Frequency[old.harm.db$Species%in%hastrent.limited], by=list(old.harm.db$Site[old.harm.db$Species%in%hastrent.limited]), NROW)
NotTpspp_per_plot.1990<-aggregate(old.harm.db$Frequency[!old.harm.db$Species%in%hastrent.limited], by=list(old.harm.db$Site[!old.harm.db$Species%in%hastrent.limited]), NROW)
Tpspp_per_plot.2015<-aggregate(new.harm.db$Frequency[new.harm.db$Species%in%hastrent.limited], by=list(new.harm.db$Site[new.harm.db$Species%in%hastrent.limited]), NROW)
NotTpspp_per_plot.2015<-aggregate(new.harm.db$Frequency[!new.harm.db$Species%in%hastrent.limited], by=list(new.harm.db$Site[!new.harm.db$Species%in%hastrent.limited]), NROW)
Tpcount.perplot.1990<-merge(NotTpspp_per_plot.1990, Tpspp_per_plot.1990, all=TRUE, by=1)#equivalent code for 2015 mising
Tpcount.perplot.1990[is.na(Tpcount.perplot.1990)]<-0
Tpcount.perplot.1990$proportion1990<-Tpcount.perplot.1990$x.y/Tpcount.perplot.1990$x.x

wilcox.test(Tpcount.perplot.2015$proportion2015, Tpcount.perplot.1990$proportion1990)

x11(7,5); par(mfrow=c(1,2), mar=c(4,4,3,0.1), las=1, xpd=NA)
barplot(c(table(!old.harm.db$Species%in%hastrent),table(!new.harm.db$Species%in%hastrent)), names.arg=c("Tp", "Not Tp","Tp","Not Tp"), main="Total records", ylab="Count")
text(c(1.2,3.7), -1200, c("1990", "2015"))

barplot(as.matrix(cbind(oldfrq[2]/colSums(oldfrq[2]), oldfrq[1]/colSums(oldfrq[1]),newfrq[2]/colSums(newfrq[2]), newfrq[1]/colSums(newfrq[1]))), names.arg=c("Tp", "Not Tp","Tp","Not Tp"), main="Assigned frequency scores", ylab="Proportion")
text(c(1.2,3.7), -0.17, c("1990", "2015"))
savePlot("Counts and assigned scores side by side.emf", type="emf")

wilcox.test(Tpcount.perplot.2015$x.y, Tpcount.perplot.1990$x.y)
numcolwise(mean, na.rm=TRUE)(Tpcount.perplot.1990)
numcolwise(mean, na.rm=TRUE)(Tpcount.perplot.2015)

#####Have the Tentrapohloid species expanded their ocurrrnces in the site?
Tp.freqperspp1990<-aggregate(old.harm.db$Frequency[old.harm.db$Species%in%hastrent], by=list(old.harm.db$Species[old.harm.db$Species%in%hastrent]), NROW)
NotTp.freqperspp1990<-aggregate(old.harm.db$Frequency[!old.harm.db$Species%in%hastrent], by=list(old.harm.db$Species[!old.harm.db$Species%in%hastrent]), NROW)
Tp.freqperspp2015<-aggregate(new.harm.db$Frequency[new.harm.db$Species%in%hastrent], by=list(new.harm.db$Species[new.harm.db$Species%in%hastrent]), NROW)
NotTp.freqperspp2015<-aggregate(new.harm.db$Frequency[!new.harm.db$Species%in%hastrent], by=list(new.harm.db$Species[!new.harm.db$Species%in%hastrent]), NROW)

c(mean(Tp.freqperspp1990$x, na.rm=TRUE), mean(NotTp.freqperspp1990$x, na.rm=TRUE), mean(Tp.freqperspp2015$x, na.rm=TRUE), mean(NotTp.freqperspp2015$x, na.rm=TRUE))

############################################################
######starts again from line 22 but with epiphytes only (skips the bit with just the easy spp)#####
new.harm.db.EO<-new.harm.db[(rowSums(new.harm.db[,names(new.harm.db)%in%S.in.LS]))>0,]
old.harm.db.EO<-old.harm.db[(rowSums(old.harm.db[,names(old.harm.db)%in%S.in.LS]))>0,]
length(unique(new.harm.db.EO$Species[new.harm.db.EO$Species%in%hastrent]))/length(unique(new.harm.db.EO$Species))
length(unique(old.harm.db.EO$Species[old.harm.db.EO$Species%in%hastrent]))/length(unique(old.harm.db.EO$Species))


table(new.harm.db.EO$Frequency, new.harm.db.EO$Species %in% hastrent)
table(old.harm.db.EO$Frequency, old.harm.db.EO$Species %in% hastrent)
newfrq.EO<-as.data.frame.matrix(table(new.harm.db.EO$Frequency, new.harm.db.EO$Species %in% hastrent))
oldfrq.EO<-as.data.frame.matrix(table(old.harm.db.EO$Frequency, old.harm.db.EO$Species %in% hastrent))

x11(7,5); par(mfrow=c(1,2), mar=c(4,4,3,0.1), las=1, xpd=NA)
barplot(c(table(!old.harm.db.EO$Species%in%hastrent),table(!new.harm.db.EO$Species%in%hastrent)), names.arg=c("Tp", "Not Tp","Tp","Not Tp"), main="Total records", ylab="Count")
text(c(1.2,3.7), -1200, c("1990", "2015"))

barplot(as.matrix(cbind(oldfrq.EO[2]/colSums(oldfrq.EO[2]), oldfrq.EO[1]/colSums(oldfrq.EO[1]),newfrq.EO[2]/colSums(newfrq.EO[2]), newfrq.EO[1]/colSums(newfrq.EO[1]))), names.arg=c("Tp", "Not Tp","Tp","Not Tp"), main="Assigned frequency scores", ylab="Proportion")
text(c(1.2,3.7), -0.17, c("1990", "2015"))
savePlot("Counts and assigned scores side by side_epiphytes only.emf", type="emf")


Tpspp_per_plot.1990<-aggregate(old.harm.db.EO$Frequency[old.harm.db.EO$Species%in%hastrent], by=list(old.harm.db.EO$Site[old.harm.db.EO$Species%in%hastrent]), NROW)
NotTpspp_per_plot.1990<-aggregate(old.harm.db.EO$Frequency[!old.harm.db.EO$Species%in%hastrent], by=list(old.harm.db.EO$Site[!old.harm.db.EO$Species%in%hastrent]), NROW)

Tpspp_per_plot.2015<-aggregate(new.harm.db.EO$Frequency[new.harm.db.EO$Species%in%hastrent], by=list(new.harm.db.EO$Site[new.harm.db.EO$Species%in%hastrent]), NROW)
NotTpspp_per_plot.2015<-aggregate(new.harm.db.EO$Frequency[!new.harm.db.EO$Species%in%hastrent], by=list(new.harm.db.EO$Site[!new.harm.db.EO$Species%in%hastrent]), NROW)

Tpcount.perplot.1990<-merge(NotTpspp_per_plot.1990, Tpspp_per_plot.1990, all=TRUE, by=1)
Tpcount.perplot.1990[is.na(Tpcount.perplot.1990)]<-0
Tpcount.perplot.1990$proportion1990<-Tpcount.perplot.1990$x.y/Tpcount.perplot.1990$x.x
Tpcount.perplot.2015<-merge(NotTpspp_per_plot.2015, Tpspp_per_plot.2015, all=TRUE, by=1)
Tpcount.perplot.2015[is.na(Tpcount.perplot.2015)]<-0
Tpcount.perplot.2015$proportion2015<-Tpcount.perplot.2015$x.y/Tpcount.perplot.2015$x.x
wilcox.test(Tpcount.perplot.2015$proportion2015, Tpcount.perplot.1990$proportion1990)#NS

boxplot(list(Tpcount.perplot.1990$proportion1990, Tpcount.perplot.2015$proportion2015), names = c(1990, 2015), ylab="Proportion of Tp-containg species per plot")
boxplot(list(Tpcount.perplot.1990$x.y, Tpcount.perplot.2015$x.y), names = c(1990, 2015), ylab="Number of Tp-containg species per plot")
wilcox.test(Tpcount.perplot.2015$x.y, Tpcount.perplot.1990$x.y)

numcolwise(mean, na.rm=TRUE)(Tpcount.perplot.1990)
numcolwise(mean, na.rm=TRUE)(Tpcount.perplot.2015)

#####Have the Tentrapohloid species expanded their ocurrrnces in the site?
Tp.freqperspp1990<-aggregate(old.harm.db.EO$Frequency[old.harm.db.EO$Species%in%hastrent], by=list(old.harm.db.EO$Species[old.harm.db.EO$Species%in%hastrent]), NROW)
NotTp.freqperspp1990<-aggregate(old.harm.db.EO$Frequency[!old.harm.db.EO$Species%in%hastrent], by=list(old.harm.db.EO$Species[!old.harm.db.EO$Species%in%hastrent]), NROW)
Tp.freqperspp2015<-aggregate(new.harm.db.EO$Frequency[new.harm.db.EO$Species%in%hastrent], by=list(new.harm.db.EO$Species[new.harm.db.EO$Species%in%hastrent]), NROW)
NotTp.freqperspp2015<-aggregate(new.harm.db.EO$Frequency[!new.harm.db.EO$Species%in%hastrent], by=list(new.harm.db.EO$Species[!new.harm.db.EO$Species%in%hastrent]), NROW)

c(mean(Tp.freqperspp1990$x, na.rm=TRUE), mean(NotTp.freqperspp1990$x, na.rm=TRUE), mean(Tp.freqperspp2015$x, na.rm=TRUE), mean(NotTp.freqperspp2015$x, na.rm=TRUE))

#############Peucedanum-pinetum plots removed (otherwise normal)#########################
##############
new.harm.db.noPP<-new.harm.db[!new.harm.db$Site%in%PP,]
old.harm.db.noPP<-old.harm.db[!old.harm.db$Site%in%PP,]
#how many records does this affect?
(NROW(old.harm.db[old.harm.db$Site%in%PP,]))/(NROW(old.harm.db[!old.harm.db$Site%in%PP,]))
(NROW(new.harm.db[new.harm.db$Site%in%PP,]))/(NROW(new.harm.db[!new.harm.db$Site%in%PP,])) #takes 23% of new records and 25% of old records.
length(unique(new.harm.db.noPP$Species[new.harm.db.noPP$Species%in%hastrent]))/length(unique(new.harm.db.noPP$Species))
length(unique(old.harm.db.noPP$Species[old.harm.db.noPP$Species%in%hastrent]))/length(unique(old.harm.db.noPP$Species))

#####looking at how the numbers and proportions of trentapohlioid species have changed over time
table(new.harm.db.noPP$Frequency, new.harm.db.noPP$Species %in% hastrent)
table(old.noPP$Frequency, old.noPP$Species %in% hastrent)#needs a more sensible way of looking at it. Proportion of 3's that were Tp? Actually a teeny bit higher in old set (need a suitable test)

x11(7,5); par(mfrow=c(1,2), mar=c(4,4,3,0.1), las=1, xpd=NA)
barplot(c(table(!old.harm.db$Species%in%hastrent),table(!new.harm.db$Species%in%hastrent)), names.arg=c("Tp", "Not Tp","Tp","Not Tp"), main="Total records", ylab="Count")
text(c(1.2,3.7), -1200, c("1990", "2015"))

barplot(as.matrix(cbind(oldfrq[2]/colSums(oldfrq[2]), oldfrq[1]/colSums(oldfrq[1]),newfrq[2]/colSums(newfrq[2]), newfrq[1]/colSums(newfrq[1]))), names.arg=c("Tp", "Not Tp","Tp","Not Tp"), main="Assigned frequency scores", ylab="Proportion")
text(c(1.2,3.7), -0.17, c("1990", "2015"))
savePlot("Counts and assigned scores side by side_noPP.emf", type="emf")

Tpspp_per_plot.1990<-aggregate(old.noPP$Frequency[old.noPP$Species%in%hastrent], by=list(old.noPP$Site[old.noPP$Species%in%hastrent]), NROW)
NotTpspp_per_plot.1990<-aggregate(old.noPP$Frequency[!old.noPP$Species%in%hastrent], by=list(old.noPP$Site[!old.noPP$Species%in%hastrent]), NROW)

Tpspp_per_plot.2015<-aggregate(new.harm.db.noPP$Frequency[new.harm.db.noPP$Species%in%hastrent], by=list(new.harm.db.noPP$Site[new.harm.db.noPP$Species%in%hastrent]), NROW)
NotTpspp_per_plot.2015<-aggregate(new.harm.db.noPP$Frequency[!new.harm.db.noPP$Species%in%hastrent], by=list(new.harm.db.noPP$Site[!new.harm.db.noPP$Species%in%hastrent]), NROW)

Tpcount.perplot.1990<-merge(NotTpspp_per_plot.1990, Tpspp_per_plot.1990, all=TRUE, by=1)
Tpcount.perplot.1990[is.na(Tpcount.perplot.1990)]<-0
Tpcount.perplot.1990$proportion1990<-Tpcount.perplot.1990$x.y/Tpcount.perplot.1990$x.x
Tpcount.perplot.2015<-merge(NotTpspp_per_plot.2015, Tpspp_per_plot.2015, all=TRUE, by=1)
Tpcount.perplot.2015[is.na(Tpcount.perplot.2015)]<-0
Tpcount.perplot.2015$proportion2015<-Tpcount.perplot.2015$x.y/Tpcount.perplot.2015$x.x
wilcox.test(Tpcount.perplot.2015$proportion2015, Tpcount.perplot.1990$proportion1990) #sig

boxplot(list(Tpcount.perplot.1990$proportion1990, Tpcount.perplot.2015$proportion2015), names = c(1990, 2015), ylab="Proportion of Tp-containg species per plot")#sig, slightly higher Tp proportion in 1990.
boxplot(list(Tpcount.perplot.1990$x.y, Tpcount.perplot.2015$x.y), names = c(1990, 2015), ylab="Number of Tp-containg species per plot")
savePlot("proportions tp per plot PP only.emf", type="emf")
wilcox.test(Tpcount.perplot.2015$x.y, Tpcount.perplot.1990$x.y)#overall counts of Tp per plot higher in 2015, but due to large increase in n. records.
numcolwise(mean, na.rm=TRUE)(Tpcount.perplot.1990)
numcolwise(mean, na.rm=TRUE)(Tpcount.perplot.2015)

#####Have the Tentrapohloid species expanded their ocurrrnces in the site?
Tp.freqperspp1990<-aggregate(old.harm.db.noPP$Frequency[old.harm.db.noPP$Species%in%hastrent], by=list(old.harm.db.noPP$Species[old.harm.db.noPP$Species%in%hastrent]), NROW)
NotTp.freqperspp1990<-aggregate(old.harm.db.noPP$Frequency[!old.harm.db.noPP$Species%in%hastrent], by=list(old.harm.db.noPP$Species[!old.harm.db.noPP$Species%in%hastrent]), NROW)
Tp.freqperspp2015<-aggregate(new.harm.db.noPP$Frequency[new.harm.db.noPP$Species%in%hastrent], by=list(new.harm.db.noPP$Species[new.harm.db.noPP$Species%in%hastrent]), NROW)
NotTp.freqperspp2015<-aggregate(new.harm.db.noPP$Frequency[!new.harm.db.noPP$Species%in%hastrent], by=list(new.harm.db.noPP$Species[!new.harm.db.noPP$Species%in%hastrent]), NROW)

c(mean(Tp.freqperspp1990$x, na.rm=TRUE), mean(NotTp.freqperspp1990$x, na.rm=TRUE), mean(Tp.freqperspp2015$x, na.rm=TRUE), mean(NotTp.freqperspp2015$x, na.rm=TRUE))


##########Newstuff 18th april 2016 - mostly about wirth values
x11(9,4); par(mfrow=c(1,3))
hist(envir$T_temperature, main="Temperature by Species", xlab="Wirth T value", ylab="Number of species")
hist(envir$T_temperature[envir$Trentepohlia_photobiont==1], col="red",add=TRUE)
legend("topright", legend=c("All species", "Trentapohlioid"), fill=c("white", "red"))
hist(old.wirths$T_temperature, main="1990", xlab="Wirth T value", ylab="Occurrences")
hist(old.wirths$T_temperature[old.wirths$Trentepohlia_photobiont==1], col="red",add=TRUE)
hist(new.wirths$T_temperature, main="2015", xlab="Wirth T value", ylab="Occurrences")
hist(new.wirths$T_temperature[new.wirths$Trentepohlia_photobiont==1], col="red",add=TRUE)
savePlot("Temp values for Tp.emf", type="emf")

