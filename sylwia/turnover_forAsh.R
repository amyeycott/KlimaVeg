#turnover script
source("../Whole_crypto/turnover.R")#
on.ls.ash<-bryophytes[(bryophytes$Fe_LS)>0,c(1:4,which(names(bryophytes)%in%S.in.LS), 55:68)]
plotswithlsFe<-unique(on.ls.ash$Plot[!on.ls.ash$Plot=="0"])
x11(); par(mfrow=c(3,2))
hist(Summaries$bryo.extinct[rownames(Summaries)%in%plotswithlsFe])
hist(Summaries$bryo.extinct[!rownames(Summaries)%in%plotswithlsFe])
hist(Summaries$bryo.colonise[rownames(Summaries)%in%plotswithlsFe])
hist(Summaries$bryo.colonise[!rownames(Summaries)%in%plotswithlsFe])#all reasonably normal distributions
hist(Summaries$bryo.colonise[rownames(Summaries)%in%plotswithlsFe]/Summaries$bryo.extinct[rownames(Summaries)%in%plotswithlsFe], breaks=20)

t.test(Summaries$bryo.extinct[rownames(Summaries)%in%plotswithlsFe],Summaries$bryo.extinct[!rownames(Summaries)%in%plotswithlsFe]) #sig. Mean is 0.234 for with Fe, 0.301 without. So Fe-LS plots have lower extinction!
t.test(Summaries$bryo.colonise[rownames(Summaries)%in%plotswithlsFe],Summaries$bryo.colonise[!rownames(Summaries)%in%plotswithlsFe])# Just ns. 0.169 mean colonisations with FeLS, 0.2089 without. So pplots with FE_LS are more stable on both counts.

t.test(Summaries$bryo.colonise[!rownames(Summaries)%in%plotswithlsFe]/Summaries$bryo.extinct[!rownames(Summaries)%in%plotswithlsFe], breaks=20) #both somewhat leptokurtic... Sqrt fixes it, I should look up proper turnover indices.
