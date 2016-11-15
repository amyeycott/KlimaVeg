#turnover script
#steps: build list for each plot for each year then do length not in for each comparison. What about not in?

#or start with Fe_LS 2015 by plot?
plotnames<-list(unique(bryophytes$Plot))

FeLS2015df.byplot<-as.data.frame.matrix(FeLS2015table.byplot)
FeLS1992df.byplot<-as.data.frame.matrix(FeLS1992table.byplot)
turnoverfunction<-function(x){rownames}

newlist<-rownames(FeLS2015df.byplot[FeLS2015df.byplot$P01==1,])
oldlist<-rownames(FeLS1992df.byplot[FeLS1992df.byplot$P01==1,])


lapply(plotnames, turnoverfunction)
