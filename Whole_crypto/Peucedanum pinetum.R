source("../Whole_crypto/turnover.R")
str(Summaries)
rownames(VascOld.fat)<-paste(substr(rownames(VascOld.fat), 1,1), substr(rownames(VascOld.fat), 3,4), sep="")#put this here for now, to make the row names match, but could consider moving it to the data loading file.
rownames(VascNew.fat)<-paste(substr(rownames(VascNew.fat), 1,1), substr(rownames(VascNew.fat), 3,4), sep="")
#next step is essential for preserving row names
Picea1992<-subset(VascOld.fat, select="Picea_abies")
Picea2015<-subset(VascNew.fat, select="Picea_abies")
SummariesPP<-Reduce(HDRmerge, list(Summaries, Picea1992, Picea2015))# hdr merge is just a tidier version of merge, already composed in the source file. 
library(plyr)
SummariesPP<-rename(SummariesPP,c("Picea_abies.x"="Picea1992","Picea_abies.y"="Picea2015"))
rm(Picea1992, Picea2015)#just to avoid confusion

table(SummariesPP$Picea1992,SummariesPP$Picea2015)#there were no Picea-free plots in either year, according to this! I checked in the original thin files

SummariesPP$Transition<-paste(SummariesPP$Picea1992, "to", SummariesPP$Picea2015)
SummariesPP$Transition[SummariesPP$Transition=="3 to 3"]<-"Stayed 3"
SummariesPP$Transition[SummariesPP$Transition=="2 to 2"]<-"Stayed 2"
SummariesPP$Transition[SummariesPP$Transition=="1 to 1"]<-"Stayed 1"

x11(); par(mfrow=c(3,5), las=2, mar=c(5,3,3,0.5), mgp=c(2,0.5,0))
mapply(function(x, main){boxplot(x~Transition, data=SummariesPP, main=main)}, x = Summaries[, 1:15], main = colnames(Summaries[,1:15]))#data exploration
savePlot("Fuckyeah.pdf", type="pdf")



