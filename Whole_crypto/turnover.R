source("source data and merging.R")
#turnover. First work out the dissimilaritiy scores. vascs there is a merged data set called comp, where year is the second-last column and the two years are in the row names as "  .x" for 1992 and "  .y" for 2015

#ultimate ambition is to know enough about row orders to feed everything into one summary df instead of three separate ones.

####vascs###
vegdist.vascs<-as.matrix(vegdist(comp))
summ.vascs<-as.data.frame(0)#you have to set something up for the loop to feed into.
  for (i in 1:144)
  {summ.vascs[i,]<-(vegdist.vascs[i, i+144])}
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
vegdist.vascs[1:5, 145:149]
summ.vascs[1:5,]
head(comp_old[,1:5])#shows that comp/comp new/comp old have the same row order
colnames(summ.vascs)<-"vasc.dist"
summ.vascs$vasc.rich1992<-rowSums(subset(comp_old, select=-Year))
summ.vascs$vasc.rich2015<-rowSums(subset(comp_new, select=-Year))

###bryos###
vegdist.bryos<-as.matrix(vegdist(easytabx))
summ.bryos<-as.data.frame(0)#you have to set something up for the loop to feed into.
for (i in (1:144))
{summ.bryos[i,]<-(vegdist.bryos[i*2-1, i*2])}# Here I want odds combined with their folllowing evens.
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
vegdist.bryos[c(1,3,5,7,9), c(2,4,6,8,10)]
summ.bryos[1:5,]
head(easytabx[,1:5])#shows that bryo and the resulting objects have the same row order. BUT the are a different row order to vascs - goes A01, A02, A03 not A01, A10, A11.
rownames(summ.bryos)<-substr(rownames(easytabx[substr(rownames(easytabx),4,7)=="1992",]),1,3)
colnames(summ.bryos)<-"bryo.dist"
summ.bryos$bryo.rich1992<-rowSums(easytabx[substr(rownames(easytabx),4,7)=="1992",])
summ.bryos$bryo.rich2015<-rowSums(easytabx[substr(rownames(easytabx),4,7)=="2015",])


##Vascular plants##
#I need a combined object of all years veg.