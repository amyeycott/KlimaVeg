#turnover script
source("../Whole_crypto/turnover.R")#check that 
on.ls.ash<-bryophytes[(bryophytes$Fe_LS)>0,c(1:4,which(names(bryophytes)%in%S.in.LS), 55:68)] #this makes a subset where all the ocurrences recorded on living ash are selected
plots.withlsash<-unique(on.ls.ash$Plot)