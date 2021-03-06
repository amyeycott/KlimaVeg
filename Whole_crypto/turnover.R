source("../Whole_crypto/source data and merging.R")
#this file prepares the plot-level summaries (richness, tunover, n threatened species, etc), merges them with the dominant vegetation community from 1990 and weighted mean ellenberg values. There were a few plots here, they now have their own script so that this doesn't produce plots when being run as source.
library(vegan)
####lichens###
#turnover. First work out the dissimilaritiy scores. Any score that vegdist makes can be added in by changing the first line for each section - for example, options for binary dissimilarity indices. E.g. if you do Bray Curtis, binary= TRUE you get sørensen DISsimilarity.
BCdist.lichens<-as.matrix(vegdist(comp[,!names(comp)=="Year"], method="bray"))
Sodiss.lichens<-as.matrix(vegdist(comp[,!names(comp)=="Year"], method="bray", binary="TRUE"))
summ.lichens<-as.data.frame(cbind(0,0))#you have to set something up for the loop to feed into.
  for (i in 1:144)
  {summ.lichens[i,1]<-(BCdist.lichens[i, i+144])
    summ.lichens[i,2]<-(Sodiss.lichens[i, i+144])}
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after - this should produce five zeros. It has.
diag(BCdist.lichens[1:5, 145:149])-summ.lichens[1:5,1]

head(comp_old[,1:5])#shows that comp/comp new/comp old have the same row order
colnames(summ.lichens)<-c("lich.BCdiss", "lich.Sodiss")
summ.lichens$lich.rich1990<-rowSums(subset(comp_old>0, select=-Year))
summ.lichens$lich.rich2015<-rowSums(subset(comp_new>0, select=-Year))
summ.lichens$richchange<-summ.lichens$lich.rich2015-summ.lichens$lich.rich1990
rownames(summ.lichens)<-rownames(comp_old)

extinctions.lichens<-as.matrix(designdist(comp, method = "(A-J)/A", terms = "binary", abcd=FALSE, alphagamma=FALSE, "extinctions")) #J for shared quantity, A and B for totals, N for the number of rows (sites) and P for the number of columns (species).
summ.lichens$lich.extinct<-0
for (i in 1:144)
{summ.lichens$lich.extinct[i]<-(extinctions.lichens[i, i+144])}
colonisations.lichens<-as.matrix(designdist(comp, method = "(B-J)/B", terms = "binary", abcd=FALSE, alphagamma=FALSE, "colonisations")) #J for shared quantity, A and B for totals, N for the number of rows (sites) and P for the number of columns (species)
summ.lichens$lich.colonise<-0
for (i in 1:144)
{summ.lichens$lich.colonise[i]<-(colonisations.lichens[i, i+144])}
summ.lichens$lich_threatened_1990<-rowSums(comp_old[,names(comp_old)%in%lich.protect$Species_name[lich.protect$Is_Redlisted==1]]>0)
summ.lichens$lich_threatened_2015<-rowSums(comp_new[,names(comp_new)%in%lich.protect$Species_name[lich.protect$Is_Redlisted==1]]>0)

###bryos###
BCdist.bryos<-as.matrix(vegdist(bryo.fat, method="bray"))
Sodist.bryos<-as.matrix(vegdist(bryo.fat, method="bray", binary=TRUE))
summ.bryos<-as.data.frame(cbind(0,0))#you have to set something up for the loop to feed into.
for (i in (1:144))
{summ.bryos[i,1]<-(BCdist.bryos[i*2-1, i*2])
  summ.bryos[i,2]<-(Sodist.bryos[i*2-1, i*2])}# Here I want odds combined with their folllowing evens.
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. GSould be a row of zeros. It has.
diag(BCdist.bryos[c(1,3,5,7,9), c(2,4,6,8,10)])-summ.bryos[1:5,1]
head(bryo.fat[,1:5])#shows that bryo and the resulting objects have the same row order. BUT the are a different row order to lichens and vascular - goes A01, A02, A03 not A01, A10, A11.
rownames(summ.bryos)<-substr(rownames(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990",]),1,3)
colnames(summ.bryos)<-c("bryo.BCdiss", "bryo.Sodiss")
summ.bryos$bryo.rich1990<-rowSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990",]>0)
summ.bryos$bryo.rich2015<-rowSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015",]>0)
summ.bryos$bryochange<-summ.bryos$bryo.rich2015-summ.bryos$bryo.rich1990
extinctions.bryos<-as.matrix(designdist(bryo.fat, method = "(A-J)/A", terms = "binary", abcd=FALSE, alphagamma=FALSE, "extinctions")) 
summ.bryos$bryo.extinct<-0
for (i in 1:144)
{summ.bryos$bryo.extinct[i]<-(extinctions.bryos[i*2-1, i*2])}
colonisations.bryos<-as.matrix(designdist(bryo.fat, method = "(B-J)/B", terms = "binary", abcd=FALSE, alphagamma=FALSE, "colonisations")) #J for shared quantity, A and B for totals, N for the number of rows (sites) and P for the number of columns (species)
summ.bryos$bryo.colonise<-0
for (i in 1:144)
{summ.bryos$bryo.colonise[i]<-(colonisations.bryos[i*2-1, i*2])}

summ.bryos$bryo_threatened_1990<-rowSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990",names(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990",])%in%bryo.status$Species_name[!bryo.status$Red_coded=="NA"]]>0)
summ.bryos$bryo_threatened_2015<-rowSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015",names(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015",])%in%bryo.status$Species_name[!bryo.status$Red_coded=="NA"]]>0)

##Vascular plants##
BCdist.vascs<-as.matrix(vegdist(vascall.df, method="bray"))
Sodiss.vascs<-as.matrix(vegdist(vascall.df, method="bray", binary=TRUE))
summ.vascs<-as.data.frame(cbind(0,0))#you have to set something up for the loop to feed into.
for (i in (1:144))
{summ.vascs[i,1]<-(BCdist.vascs[i, i+144])
  summ.vascs[i,2]<-(Sodiss.vascs[i, i+144])}# Like lichens, it goes 1-144 then 145-250
#How do I know it preserved the row order? Compare the diagonals of the next line with the values of the one after. It has.
diag(BCdist.vascs[1:5, 145:149])-summ.vascs[1:5,1]
head(vascall.df[,1:5])#shows that bryo and the resulting objects have the same row order. They go A1, A2, A3 not A1, A10, A11.
colnames(summ.vascs)<-c("vasc.BCdiss", "vasc.Sodiss")
summ.vascs$vasc.rich1990<-rowSums(vascOld.fat>0)
summ.vascs$vasc.rich2015<-rowSums(vascNew.fat>0)
summ.vascs$vascchange<-summ.vascs$vasc.rich2015-summ.vascs$vasc.rich1990
rownames(summ.vascs)<-rownames(vascOld.fat)

extinctions.vascs<-as.matrix(designdist(vascall.df, method = "(A-J)/A", terms = "binary", abcd=FALSE, alphagamma=FALSE, "extinctions")) 
summ.vascs$vasc.extinct<-0
for (i in 1:144)
{summ.vascs$vasc.extinct[i]<-(extinctions.vascs[i, i+144])}
colonisations.vascs<-as.matrix(designdist(vascall.df, method = "(B-J)/B", terms = "binary", abcd=FALSE, alphagamma=FALSE, "colonisations")) #J for shared quantity, A and B for totals, N for the number of rows (sites) and P for the number of columns (species)
summ.vascs$vasc.colonise<-0
for (i in 1:144)
{summ.vascs$vasc.colonise[i]<-(colonisations.vascs[i, i+144])}
summ.vascs$vasc_threatened_1990<-rowSums(vascOld.fat[,names(vascOld.fat)%in%vasc.protected$Species.name[!vasc.protected$Red_coded=="NA"]]>0)
summ.vascs$vasc_threatened_2015<-rowSums(vascNew.fat[,names(vascNew.fat)%in%vasc.protected$Species.name[!vasc.protected$Red_coded=="NA"]]>0)

##Final step is to merge them all. I can't just cbind because the row orders are different. This is a neat function off stack overflow (http://stackoverflow.com/questions/16666643/merging-more-than-2-dataframes-in-r-by-rownames) that does all three objects at once and gets rid of the superfluous row names-columns
HDRmerge<- function(x, y){
  df<- merge(x, y, by= "row.names", all.x= TRUE, all.y= TRUE)
  rownames(df)  <- df$Row.names
  df$Row.names  <- NULL
  return(df)
}
Summaries<- Reduce(HDRmerge, list(summ.lichens, summ.bryos, summ.vascs))

####Add in Ellenbergs####
####weighted mean ellenbergs for different species groups in the plots####

#merge thin tables with ellenberg values (already done for lichens)
vascoldthin.withellens<-merge(vascOld.thin[,c("Species_name","Plot_number","frequency_score")], vasc.ellen, by.x="Species_name", by.y="Species.name")
vascnewthin.withellens<-merge(vascNew.thin[,c("Species_name_2015","Plot_number_2015","frequency_score")], vasc.ellen, by.x="Species_name_2015", by.y="Species.name")
bryothin.withellens<-merge(bryophytes[1:4], bryo.status, by.x="Species_name", by.y="Species_name")

#weighted averages by plot
lich.weightmeanold<-old.wirths %>% 
  group_by(Site) %>%
  summarise_at(vars(L_light:N_nitrogen),weighted.mean, z=.$Frequency, na.rm=TRUE)#the dot is because it means "use the object named in the previous line" in hadlyspeak. Think of pipe as "feed into", where a little robot on the previous line delivers its completed task to the next robot
lich.weightmeannew<-new.wirths %>% 
  group_by(Site) %>%
  summarise_at(vars(L_light:N_nitrogen),weighted.mean, z=.$Frequency, na.rm=TRUE)
bryo.weightmean<-bryothin.withellens %>% 
  group_by(Plot, Year) %>%
  summarise_at(vars(L:R),weighted.mean, z=.$Frequency, na.rm=TRUE)
vasc.weightmeanold<-vascoldthin.withellens %>% 
  group_by(Plot_number) %>%
  summarise_at(vars(L:R),weighted.mean, z=.$frequency_score, na.rm=TRUE)#broken: there's x's in the ellenberg values file. Emailed Bogdan 25th Sep 2017
vasc.weightmeannew<-vascnewthin.withellens %>% 
  group_by(Plot_number_2015) %>%
  summarise_at(vars(L:R),weighted.mean, z=.$frequency_score, na.rm=TRUE)

#make the plot names the row names in A01, A02 format. You can't manipulate rownames in vascoldell, and I want to leave it as rownames to be able to use the HDRmerge.
lich.weightmeanold<-as.data.frame(lich.weightmeanold)
rownames(lich.weightmeanold)<-lich.weightmeanold$Site

names(lich.weightmeanold)<-paste("lich.old.",names(lich.weightmeanold), sep="")
lich.weightmeannew<-as.data.frame(lich.weightmeannew)
rownames(lich.weightmeannew)<-lich.weightmeannew$Site
names(lich.weightmeannew)<-paste("lich.new.",names(lich.weightmeannew), sep="")
bryo.weightmeanold<-as.data.frame(bryo.weightmean[bryo.weightmean$Year=="1990",])
rownames(bryo.weightmeanold)<-bryo.weightmeanold$Plot
names(bryo.weightmeanold)<-paste("bryo.old.",names(bryo.weightmeanold), sep="")
bryo.weightmeannew<-as.data.frame(bryo.weightmean[bryo.weightmean$Year=="2015",])
rownames(bryo.weightmeannew)<-bryo.weightmeannew$Plot
names(bryo.weightmeannew)<-paste("bryo.new.",names(bryo.weightmeannew), sep="")

vasc.weightmeanold<-as.data.frame(vasc.weightmeanold)
rownames(vasc.weightmeanold)<-substr(vasc.weightmeanold$Plot_number,1,3)
names(vasc.weightmeanold)<-paste("vasc.old.",names(vasc.weightmeanold), sep="")
vasc.weightmeannew<-as.data.frame(vasc.weightmeannew)
rownames(vasc.weightmeannew)<-substr(vasc.weightmeannew$Plot_number_2015,1,3)
names(vasc.weightmeannew)<-paste("vasc.new.",names(vasc.weightmeannew), sep="")


#Merging taxa summaries and removing unnecessary columns.
Summaries<- Reduce(HDRmerge, list(Summaries, select(lich.weightmeanold,-lich.old.Site), select(lich.weightmeannew,-lich.new.Site), select(bryo.weightmeanold,-bryo.old.Plot, -bryo.old.Year), select(bryo.weightmeannew,-bryo.new.Plot, -bryo.new.Year), select(vasc.weightmeanold, -vasc.old.Plot_number), select(vasc.weightmeannew, -vasc.new.Plot_number_2015)))



##summary statistics
sapply(Summaries, mean)
sapply(Summaries, sd)
sapply (list(comp_old, comp_new, vascOld.fat, vascNew.fat), dim)
sum(colSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="1990",])>0)#that's a long-winded way to get bryo richness...
sum(colSums(bryo.fat[substr(rownames(bryo.fat),4,7)=="2015",])>0)
#is turnover higher in certain communities? Or plots with more communities in?
Summaries<-merge(Summaries, phytosoc, by.x=0, by.y=1)
rownames(Summaries)<-Summaries$Row.names 
Summaries$Row.names<-NULL
Summaries$dominant<-factor(Summaries$dominant, levels=c("PP", "PQ", "QP", "TC", "CelA", "CA"))