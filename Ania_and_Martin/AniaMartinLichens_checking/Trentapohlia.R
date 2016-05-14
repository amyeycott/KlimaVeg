source("AniaMartinLichens_dataloading.R")
###the relationship between Tentapohliod-ness and Wierz values. Means are in t-test outputs. N is lover for Tp, Temp is higher, and F humidity is not different.
t.test(envir$N_nitrogen[envir$Trentepohlia_photobiont==1],envir$N_nitrogen[is.na(envir$Trentepohlia_photobiont==1)])
t.test(envir$T_temperature[envir$Trentepohlia_photobiont==1],envir$T_temperature[is.na(envir$Trentepohlia_photobiont)])
t.test(envir$F_moisture [envir$Trentepohlia_photobiont==1],envir$F_moisture[is.na(envir$Trentepohlia_photobiont)])

#####looking at how the numbers and proportions of tentraphloid species have changed over time######
table(trimmednewdb$Frequency[trimmednewdb$Species%in%hastrent])
table(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent])
table(olddb$Frequency[olddb$Species%in%hastrent] )
table(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent])
x11()
barplot(c(NROW(trimmednewdb$Frequency[trimmednewdb$Species%in%hastrent]), NROW(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent]), NROW(olddb$Frequency[olddb$Species%in%hastrent]), NROW(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent])), names.arg=c("Tp 2015","Not Tp 2015", "Tp 1992", "Not Tp 1992"), log="y")

Tpspp_per_plot.1992<-aggregate(olddb$Frequency[olddb$Species%in%hastrent], by=list(olddb$Site[olddb$Species%in%hastrent]), NROW)
NotTpspp_per_plot.1992<-aggregate(olddb$Frequency[!olddb$Species%in%hastrent], by=list(olddb$Site[!olddb$Species%in%hastrent]), NROW)
Tpspp_per_plot.2015<-aggregate(trimmednewdb$Frequency[trimmednewdb$Species%in%hastrent], by=list(trimmednewdb$Site[trimmednewdb$Species%in%hastrent]), NROW)
NotTpspp_per_plot.2015<-aggregate(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent], by=list(trimmednewdb$Site[!trimmednewdb$Species%in%hastrent]), NROW)
Tpcount.perplot.1992<-merge(NotTpspp_per_plot.1992, Tpspp_per_plot.1992, all=TRUE, by=1)
Tpcount.perplot.1992[is.na(Tpcount.perplot.1992)]<-0
Tpcount.perplot.1992$proportion1992<-Tpcount.perplot.1992$x.y/Tpcount.perplot.1992$x.x
t.test(Tpcount.perplot.2015$proportion2015, Tpcount.perplot.1992$proportion1992)

library(Hmisc)
x11()
errbar(c(1,2), c(mean(Tpcount.perplot.1992$proportion1992), mean(Tpcount.perplot.2015$proportion2015)), c(mean(Tpcount.perplot.1992$proportion1992)+sd(Tpcount.perplot.1992$proportion1992), mean(Tpcount.perplot.2015$proportion2015)+sd(Tpcount.perplot.2015$proportion2015)), c(mean(Tpcount.perplot.1992$proportion1992)-sd(Tpcount.perplot.1992$proportion1992), mean(Tpcount.perplot.2015$proportion2015)-sd(Tpcount.perplot.2015$proportion2015)), xlab="1992            2015", ylab="Proportion of Tp-containg species per plot")
x11()
errbar(c(1,2), c(mean(Tpcount.perplot.1992$x.y), mean(Tpcount.perplot.2015$x.y)), c(mean(Tpcount.perplot.1992$x.y)+sd(Tpcount.perplot.1992$x.y), mean(Tpcount.perplot.2015$x.y)+sd(Tpcount.perplot.2015$x.y)), c(mean(Tpcount.perplot.1992$x.y)-sd(Tpcount.perplot.1992$x.y), mean(Tpcount.perplot.2015$x.y)-sd(Tpcount.perplot.2015$x.y)), xlab="1992            2015", ylab="Number of Tp-containg species per plot")
t.test(Tpcount.perplot.2015$x.y, Tpcount.perplot.1992$x.y)

mean(envir$N_nitrogen[envir$`Trentepohlia_photobiont`==1], na.rm=TRUE)
mean(envir$N_nitrogen[is.na(envir$Trentepohlia_photobiont==1)], na.rm=TRUE)
t.test(envir$N_nitrogen[envir$Trentepohlia_photobiont==1],envir$N_nitrogen[is.na(envir$Trentepohlia_photobiont==1)])
t.test(envir$T_temperature[envir$Trentepohlia_photobiont==1],envir$T_temperature[is.na(envir$Trentepohlia_photobiont)])

#######The same again, but with only the easy, common Trentapoloids. Needs repeating with the easy, common everythings!######
hastrent.limited<-hastrent[envir$Is_it_easy_to_id==1]
table(trimmednewdb$Frequency[trimmednewdb$Species%in%hastrent.limited])
table(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent.limted])
table(olddb$Frequency[olddb$Species%in%hastrent.limited])
table(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent.limited])
x11()
barplot(c(NROW(trimmednewdb$Frequency[trimmednewdb$Species%in%hastrent.limited]), NROW(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent.limited]), NROW(olddb$Frequency[olddb$Species%in%hastrent.limited]), NROW(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent.limited])), names.arg=c("Tp 2015","Not Tp 2015", "Tp 1992", "Not Tp 1992"), log="y")

length(trimmednewdb)
Tpspp_per_plot.1992<-aggregate(olddb$Frequency[olddb$Species%in%hastrent.limited], by=list(olddb$Site[olddb$Species%in%hastrent.limited]), NROW)
NotTpspp_per_plot.1992<-aggregate(olddb$Frequency[!olddb$Species%in%hastrent.limited], by=list(olddb$Site[!olddb$Species%in%hastrent.limited]), NROW)
Tpspp_per_plot.2015<-aggregate(trimmednewdb$Frequency[trimmednewdb$Species%in%hastrent.limited], by=list(trimmednewdb$Site[trimmednewdb$Species%in%hastrent.limited]), NROW)
NotTpspp_per_plot.2015<-aggregate(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent.limited], by=list(trimmednewdb$Site[!trimmednewdb$Species%in%hastrent.limited]), NROW)
Tpcount.perplot.1992<-merge(NotTpspp_per_plot.1992, Tpspp_per_plot.1992, all=TRUE, by=1)
Tpcount.perplot.1992[is.na(Tpcount.perplot.1992)]<-0
Tpcount.perplot.1992$proportion1992<-Tpcount.perplot.1992$x.y/Tpcount.perplot.1992$x.x

t.test(Tpcount.perplot.2015$proportion2015, Tpcount.perplot.1992$proportion1992)

library(Hmisc)
x11()
errbar(c(1,2), c(mean(Tpcount.perplot.1992$proportion1992), mean(Tpcount.perplot.2015$proportion2015)), c(mean(Tpcount.perplot.1992$proportion1992)+sd(Tpcount.perplot.1992$proportion1992), mean(Tpcount.perplot.2015$proportion2015)+sd(Tpcount.perplot.2015$proportion2015)), c(mean(Tpcount.perplot.1992$proportion1992)-sd(Tpcount.perplot.1992$proportion1992), mean(Tpcount.perplot.2015$proportion2015)-sd(Tpcount.perplot.2015$proportion2015)), xlab="1992            2015", ylab="Proportion of Tp-containg species per plot")
x11()
errbar(c(1,2), c(mean(Tpcount.perplot.1992$x.y), mean(Tpcount.perplot.2015$x.y)), c(mean(Tpcount.perplot.1992$x.y)+sd(Tpcount.perplot.1992$x.y), mean(Tpcount.perplot.2015$x.y)+sd(Tpcount.perplot.2015$x.y)), c(mean(Tpcount.perplot.1992$x.y)-sd(Tpcount.perplot.1992$x.y), mean(Tpcount.perplot.2015$x.y)-sd(Tpcount.perplot.2015$x.y)), xlab="1992            2015", ylab="Number of Tp-containg species per plot")
t.test(Tpcount.perplot.2015$x.y, Tpcount.perplot.1992$x.y)

#####Have the Tentrapohloid species expanded their ocurrrnces in the site?
Tp.freqperspp1992<-aggregate(olddb$Frequency[olddb$Species%in%hastrent], by=list(olddb$Species[olddb$Species%in%hastrent]), NROW)
NotTp.freqperspp1992<-aggregate(olddb$Frequency[!olddb$Species%in%hastrent], by=list(olddb$Species[!olddb$Species%in%hastrent]), NROW)
Tp.freqperspp2015<-aggregate(trimmednewdb$Frequency[trimmednewdb$Species%in%hastrent], by=list(trimmednewdb$Species[trimmednewdb$Species%in%hastrent]), NROW)
NotTp.freqperspp2015<-aggregate(trimmednewdb$Frequency[!trimmednewdb$Species%in%hastrent], by=list(trimmednewdb$Species[!trimmednewdb$Species%in%hastrent]), NROW)

c(mean(Tp.freqperspp1992$x, na.rm=TRUE), mean(NotTp.freqperspp1992$x, na.rm=TRUE), mean(Tp.freqperspp2015$x, na.rm=TRUE), mean(NotTp.freqperspp2015$x, na.rm=TRUE))
