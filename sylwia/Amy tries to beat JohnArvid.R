library(readxl)
str(bryophytes)
bryophytes1992<-bryophytes[bryophytes$Year==1992,]
bryophytes2015<-bryophytes[bryophytes$Year==2015,]

proportions.fun<-function(x){
  allplots1992<-as.data.frame(tapply(as.vector((bryophytes1992$Frequency)>0), bryophytes1992$Species_name, sum))
  allplots2015<-as.data.frame(tapply(as.vector((bryophytes2015$Frequency)>0), bryophytes2015$Species_name, sum))
  x1992<-as.data.frame(tapply(bryophytes1992[[x]], bryophytes1992$Species_name, sum))
  x2015<-as.data.frame(tapply(bryophytes2015[[x]], bryophytes2015$Species_name, sum))
  forprop1992<-merge(allplots1992, x1992, by=0, all=TRUE)
  forprop1992$prop<-forprop1992[3]/forprop1992[2]
  forprop1992$prop[forprop1992$prop=="Inf"]<-0
  forprop2015<-merge(allplots2015, x2015, by=0, all=TRUE)
  forprop2015$prop<-forprop2015[3]/forprop2015[2]
  forprop2015$prop[forprop2015$prop=="Inf"]<-0
  props<-merge(forprop1992[1:2], forprop2015[1:2], by=1, all=TRUE)#not working
  plot(props[,2],props[,3],xlab="Fraction on tree in 1992",ylab="Fraction on tree in 2015",main=x)
  abline(0,1)
}
x11(); par(mfrow=c(2,3))
allprops<-sapply(c("Ap_LS", "Ag_LS", "Cb_LS", "Fe_LS", "Qr_LS", "Tc_LS"), proportions.fun) #plotsa the same data over and over again. Which ranges from 0 to 140, not 0 to 1.

