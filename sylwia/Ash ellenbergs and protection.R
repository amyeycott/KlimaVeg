library(readxl)
source("Ash script recovery.R")#this runs the same code as in that file

bryo.status$Red.coded[bryo.status$Red.coded==""]<-NA
bryo.status$Any.status<-bryo.status$Red.coded
bryo.status$Any.status[!is.na(bryo.status$Red.coded)]<-1
bryo.status$Any.status[!is.na(bryo.status$Protected.species)]<-1
bryo.status$Any.status[is.na(bryo.status$Any.status)]<-0

#what I need is a total number of ocurrences of all the species in the on.ls.ash list.
bryo.status$LS.Fe.2015<-0
bryo.status$LS.Fe.2015[bryo.status$Species.name%in%hosts.LS2015$Species_name]<-1
table(bryo.status$LS.Fe.2015, bryo.status$Any.status)#Of the 53 species which are protected or redlisted, 22 (41%, or two-fifths) were recorded on living ash in 2015. Of the 66 species found on living ash in 2015, 22 (33%, or one-third) were redlisted or protected.
chisq.test(bryo.status$LS.Fe.2015, bryo.status$Any.status)#somehow not working

#not sure how useful the next bit is. First, it might reproduce code from the load data file but by a diufferent name, second it gives n alternative hosts per plot whereas maybe we just want n alternative hosts/substrates overall? 
FeLS.byspp<-(unique(hosts.LS2015[,2:3]))
Fetemp.byspp<-as.data.frame(unclass(by(hosts.LS2015[5], hosts.LS2015$Species_name, max)))
FeLS.byspp<-merge(FeLS.byspp, Fetemp.byspp, by.x=2, by.y=0)
names(FeLS.byspp)<-c("Species","Year","maxhosts")
FeLS.byspp2<-merge(FeLS.byspp, bryo.status, by.x=1, by.y=2)
barplot(sort(FeLS.byspp2$maxhosts), col=FeLS.byspp2$Protected.species[order(FeLS.byspp2$maxhosts)], las=2)#The number of alternative hosts seems unrelated to protected status.
barplot(sort(FeLS.byspp2$maxhosts), col=FeLS.byspp2$Any.status[order(FeLS.byspp2$maxhosts)], las=2)#The number of laternative hosts seems unrelated to protected status.
# I pull out FeLS but if they're in a stand where they're not FeLS, they could have other hosts. This needs checking for other analyses.

Ellens<-merge(bryophytes[c(1:4,66)], bryo.status, by.x=3, by.y=2, all=TRUE)

#weighted mean L value in 2015 for plots which had Fraxinus epiphytes in 1992 kept to test the order is coming out as expected:
mean(rep(Ellens$L[!is.na(Ellens$L) &Ellens$Plot%in%hosts.LS1992$Plot &Ellens$Year==2015], Ellens$Frequency[!is.na(Ellens$L)&Ellens$Plot%in%hosts.LS1992$Plot &Ellens$Year==2015]))#5.134


cases<-expand.grid(year = c(1992, 2015), FeLS1992 = c(FALSE, TRUE))#sets up the subset possibilities for the sapply to loop through

wt.mean.ell.fun<-function(x,param){
  E<-Ellens[Ellens$Year == x[1], ]#do one subsetting strategy on each row. This starts with the first column of the grid we fed sapply
  E<-E[(E$Plot%in%hosts.LS1992$Plot)==x[2],]#this chooses plots which had Fe epiphytes in 1992 (so, we assume, some big ash trees which have a  75 %change of having died since).
   out <- by(E, IND = E$Plot, function(x){       
    weighted.mean(x[[param]], x$Frequency, na.rm = TRUE)
    })
  mean(out)
}
wt.sd.ell.fun<-function(x,param){
  E<-Ellens[Ellens$Year == x[1], ]#do one subsetting strategy on each row. This starts with the first column of the grid we fed sapply
  E<-E[(E$Plot%in%hosts.LS1992$Plot)==x[2],]#this chooses plots which had Fe epiphytes in 1992 (so, we assume, some big ash trees which have a  75 %change of having died since).
  out <- by(E, IND = E$Plot, function(x){       
    weighted.mean(x[[param]], x$Frequency, na.rm = TRUE)
  })
  sd(out)
}



Ellensummary<-cases
Ellensummary$meanL<-apply(cases,1,wt.mean.ell.fun,param="L")#the x bit doesn't make sense. Just trust it. So, it runs ellenbergfun over each row of the little choices grid made two steps above, that is, it runs the function according to the correct subsets.
Ellensummary$meanT<-apply(cases,1,wt.mean.ell.fun,param="T")
Ellensummary$meanK<-apply(cases,1,wt.mean.ell.fun,param="K")
Ellensummary$meanF<-apply(cases,1,wt.mean.ell.fun,param="F")
Ellensummary$meanR<-apply(cases,1,wt.mean.ell.fun,param="R") 

Ellensummary$sdL<-apply(cases,1,wt.sd.ell.fun,param="L")
Ellensummary$sdT<-apply(cases,1,wt.sd.ell.fun,param="T")
Ellensummary$sdK<-apply(cases,1,wt.sd.ell.fun,param="K")
Ellensummary$sdF<-apply(cases,1,wt.sd.ell.fun,param="F")
Ellensummary$sdR<-apply(cases,1,wt.sd.ell.fun,param="R")

wt.ttest.ell.fun<-function(x,param){  
  E<-Ellens[(Ellens$Plot%in%hosts.LS1992$Plot)==x[2],]
  out.1992 <- by(E[E$Year == 1992,], IND = E$Plot[E$Year == 1992], function(x){       weighted.mean(x[[param]], x$Frequency, na.rm = TRUE)
  })
  out.2015<-by(E[E$Year == 2015,], IND = E$Plot[E$Year == 2015], function(x){           weighted.mean(x[[param]], x$Frequency, na.rm = TRUE)
  })
  t.test(out.1992,out.2015)
}

apply(cases[c(1,3),],1,wt.ttest.ell.fun,param="L") #sig for both FELS1992 and not
apply(cases[c(1,3),],1,wt.ttest.ell.fun,param="T") #sig for no-FELS1992! Went up by 0.06...
apply(cases[c(1,3),],1,wt.ttest.ell.fun,param="K") #same! Went up by 0.01...
apply(cases[c(1,3),],1,wt.ttest.ell.fun,param="F") #ns
apply(cases[c(1,3),],1,wt.ttest.ell.fun,param="R") #ns
#note that you can't use this if the Ellenbegs have non-integer values. Hmisc has a wt.sd

library(Hmisc)
x11(); par(mfrow=c(2,2), xpd=NA)
with(Ellensummary, errbar(c(1.2,2,2.8, 3.6),meanL, (meanL+sdL), (meanL-sdL), pch=16,ylab="Mean of plot weighted means +- 1 sd", xlab=NA, xaxt="n", xlim=c(1,4), cex.axis=1.2, cex.lab=1.2, ylim=c(4.8, 5.4)))
axis(1, labels=c("1992","2015","1992", "2015"), at=c(1.2,2,2.8,3.6),cex.axis=1.2)
mtext("Light (L)", side =3, line=2, cex=1.2)
text(x=1.6, y=4.6, "No Ash in 1992", cex=1.2)
text(x=3.2, y=4.6, "Ash in 1992", cex=1.2)

with(Ellensummary, errbar(c(1.2,2,2.8, 3.6),meanF, (meanF+sdF), (meanF-sdF), pch=16,ylab="Mean of plot weighted means +- 1 sd", xlab=NA, xaxt="n", xlim=c(1,4), cex.axis=1.2, cex.lab=1.2, ylim=c(4.6, 5.6)))
axis(1, labels=c("1992","2015","1992", "2015"), at=c(1.2,2,2.8,3.6), cex.axis=1.2) 
mtext("Humidity (F)", side =3, line=2, cex=1.2)
text(x=1.6, y=4.3, "No Ash in 1992", cex=1.2)
text(x=3.2, y=4.3, "Ash in 1992", cex=1.2)

with(Ellensummary, errbar(c(1.2,2,2.8, 3.6),meanR, (meanR+sdR), (meanR-sdR), pch=16,ylab="Mean of plot weighted means +- 1 sd", xlab=NA, xaxt="n", xlim=c(1,4), cex.axis=1.2, cex.lab=1.2))
axis(1, labels=c("1992","2015","1992", "2015"), at=c(1.2,2,2.8,3.6), cex.axis=1.2) 
mtext("Soil pH (R)", side =3, line=2, cex=1.2)
text(x=1.6, y=2.3, "No Ash in 1992", cex=1.2)
text(x=3.2, y=2.3, "Ash in 1992", cex=1.2)

with(Ellensummary, errbar(c(1.2,2,2.8, 3.6),meanT, (meanT+sdT), (meanT-sdT), pch=16,ylab="Mean of plot weighted means +- 1 sd", xlab=NA, xaxt="n", xlim=c(1,4), cex.axis=1.2, cex.lab=1.2))
axis(1, labels=c("1992","2015","1992", "2015"), at=c(1.2,2,2.8,3.6), cex.axis=1.2) 
mtext("Temperature (T)", side =3, line=2, cex=1.2)
text(x=1.6, y=3.02, "No Ash in 1992", cex=1.2)
text(x=3.2, y=3.02, "Ash in 1992", cex=1.2)
savePlot("Compound plot Ellenbergs for Ash.emf", type="emf")

x11(5,5);par(xpd=NA)
with(Ellensummary, errbar(c(1.2,2,2.8, 3.6),meanL, (meanL+sdL), (meanL-sdL), pch=16,ylab="Mean of plot weighted means +- 1 sd", xlab=NA, xaxt="n", xlim=c(1,4), cex.axis=1.2, cex.lab=1.2, ylim=c(4.8, 5.4)))
axis(1, labels=c("1992","2015","1992", "2015"), at=c(1.2,2,2.8,3.6),cex.axis=1.2)
mtext("Light (L)", side =3, line=2, cex=1.2)
text(x=1.6, y=4.6, "No Ash in 1992", cex=1.2)
text(x=3.2, y=4.6, "Ash in in 1992", cex=1.2)
savePlot("Ellenberg for Ash Light.emf", type="emf")
with(Ellensummary, errbar(c(1.2,2,2.8, 3.6),meanF, (meanF+sdF), (meanF-sdF), pch=16,ylab="Mean of plot weighted means +- 1 sd", xlab=NA, xaxt="n", xlim=c(1,4), cex.axis=1.2, cex.lab=1.2, ylim=c(4.6, 5.6)))
axis(1, labels=c("1992","2015","1992", "2015"), at=c(1.2,2,2.8,3.6), cex.axis=1.2) 
mtext("Humidity (F)", side =3, line=2, cex=1.2)
text(x=1.6, y=4.3, "No Ash in 1992", cex=1.2)
text(x=3.2, y=4.3, "Ash in 1992", cex=1.2)
savePlot("Ellenberg for Ash Humidity.emf", type="emf")
with(Ellensummary, errbar(c(1.2,2,2.8, 3.6),meanR, (meanR+sdR), (meanR-sdR), pch=16,ylab="Mean of plot weighted means +- 1 sd", xlab=NA, xaxt="n", xlim=c(1,4), cex.axis=1.2, cex.lab=1.2))
axis(1, labels=c("1992","2015","1992", "2015"), at=c(1.2,2,2.8,3.6), cex.axis=1.2) 
mtext("Soil pH (R)", side =3, line=2, cex=1.2)
text(x=1.6, y=2.3, "No Ash in 1992", cex=1.2)
text(x=3.2, y=2.3, "Ash in 1992", cex=1.2)
savePlot("Ellenberg for Ash pH.emf", type="emf")
with(Ellensummary, errbar(c(1.2,2,2.8, 3.6),meanT, (meanT+sdT), (meanT-sdT), pch=16,ylab="Mean of plot weighted means +- 1 sd", xlab=NA, xaxt="n", xlim=c(1,4), cex.axis=1.2, cex.lab=1.2))
axis(1, labels=c("1992","2015","1992", "2015"), at=c(1.2,2,2.8,3.6), cex.axis=1.2) 
mtext("Temperature (T)", side =3, line=2, cex=1.2)
text(x=1.6, y=3.02, "No Ash in 1992", cex=1.2)
text(x=3.2, y=3.02, "Ash in 1992", cex=1.2)
savePlot("Ellenberg for Ash Temparature.emf", type="emf")
