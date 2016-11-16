source("source data and merging.R")
#turnover. First work out the dissimilaritiy scores. Any score that vegdist makes can be added in by changing the first line for each section - for example, options for binary dissimilarity indices. 

####lichens###
vegdist.lichens<-as.matrix(vegdist(comp))
summ.lichens<-as.data.frame(0)#you have to set something up for the loop to feed into.
  for (i in 1:144)
  {summ.lichens[i,]<-(vegdist.lichens[i, i+144])}
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
vegdist.lichens[1:5, 145:149]
summ.lichens[1:5,]
head(comp_old[,1:5])#shows that comp/comp new/comp old have the same row order
colnames(summ.lichens)<-"lich.dist"
summ.lichens$lich.rich1992<-rowSums(subset(comp_old, select=-Year))
summ.lichens$lich.rich2015<-rowSums(subset(comp_new, select=-Year))
rownames(summ.lichens)<-rownames(comp_old)


###bryos###
vegdist.bryos<-as.matrix(vegdist(easytabx))
summ.bryos<-as.data.frame(0)#you have to set something up for the loop to feed into.
for (i in (1:144))
{summ.bryos[i,]<-(vegdist.bryos[i*2-1, i*2])}# Here I want odds combined with their folllowing evens.
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
vegdist.bryos[c(1,3,5,7,9), c(2,4,6,8,10)]
summ.bryos[1:5,]
head(easytabx[,1:5])#shows that bryo and the resulting objects have the same row order. BUT the are a different row order to lichens and vascular - goes A01, A02, A03 not A01, A10, A11.
rownames(summ.bryos)<-substr(rownames(easytabx[substr(rownames(easytabx),4,7)=="1992",]),1,3)
colnames(summ.bryos)<-"bryo.dist"
summ.bryos$bryo.rich1992<-rowSums(easytabx[substr(rownames(easytabx),4,7)=="1992",])
summ.bryos$bryo.rich2015<-rowSums(easytabx[substr(rownames(easytabx),4,7)=="2015",])


##Vascular plants##
vegdist.vascs<-as.matrix(vegdist(Vascall.df))
summ.vascs<-as.data.frame(0)#you have to set something up for the loop to feed into.
for (i in (1:144))
{summ.vascs[i,]<-(vegdist.vascs[i, i+144])}# Like lichens, it goes 1-144 then 145-250
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
vegdist.vascs[1:5, 145:149]
summ.vascs[1:5,]
head(Vascall.df[,1:5])#shows that bryo and the resulting objects have the same row order. They go A1, A2, A3 not A1, A10, A11.
colnames(summ.vascs)<-"vasc.dist"
summ.vascs$vasc.rich1992<-rowSums(VascOld.fat)
summ.vascs$vasc.rich2015<-rowSums(VascNew.fat)
rownames(summ.vascs)<-paste(substr(rownames(VascOld.fat), 1,1), substr(rownames(VascOld.fat), 3,4), sep="")

##Final step will be to merge them all. I like merge (nb there is a different merge in rioja library!) but Richard likes something from dplyr. I can't just cbind because the row orders are different. This is a neat function off stack overflow (http://stackoverflow.com/questions/16666643/merging-more-than-2-dataframes-in-r-by-rownames)
HDRmerge<- function(x, y){
  df<- merge(x, y, by= "row.names", all.x= TRUE, all.y= TRUE)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}
Summaries<- Reduce(HDRmerge, list(summ.lichens, summ.bryos, summ.vascs))#264 lines.... because lichens have P4 and bryos and vascs P04.... this may call for a regular expression and should be fixed way back in lichens data prep, but I'm not sure if that will mess anything else up.




