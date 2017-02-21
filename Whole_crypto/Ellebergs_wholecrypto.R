source("turnover.R")
#the datasets are prepared in turnover.r. Here we use a subset excluding rectangles and rivers. The first two lines are exactly the same as in turnover_subset.r
dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")
summaries.ss<-Summaries[!rownames(Summaries)%in%dodgysquares,]

mapply(function(x,y){t.test(x,y, paired=TRUE)}, x= summaries.ss[,c("lich.old.L_light" ,"lich.old.T_temperature",   "lich.old.K_continentality","lich.old.F_moisture","lich.old.R_reaction","lich.old.N_nitrogen")], y=summaries.ss[,c("lich.new.L_light" ,"lich.new.T_temperature","lich.new.K_continentality", "lich.new.F_moisture" ,"lich.new.R_reaction","lich.new.N_nitrogen")])# Negative difference means value went UP. Light goes up significantly but only very slightly, temperature is ns (just - goes up by almost two points), continentality is ns, moisture goes up but only slightly, reaction goes up slightly but significantly, and nitrogen goes up significantly but still less than half a point.

#histograms of ellenberg changes by EIV and functional group
x11(12,7);par(mfrow=c(3,6))
mapply(function(x,y, main){
  hist(x-y, main=main, xlim=c(-1,1), ylim=c(0,50))
  abline(v=0, col="red")
  }, 
  y= summaries.ss[,c("lich.old.L_light" ,"lich.old.T_temperature",   "lich.old.K_continentality","lich.old.F_moisture","lich.old.R_reaction","lich.old.N_nitrogen")], x=summaries.ss[,c("lich.new.L_light" ,"lich.new.T_temperature","lich.new.K_continentality", "lich.new.F_moisture", "lich.new.R_reaction", "lich.new.N_nitrogen")] , main=c("Light (up**)","Temperature (ns)", "Continentality (ns)","Moisture (up***)","Reaction (up***)","Nitrogen (up***)"))  

 
mapply(function(x,y){t.test(x,y, paired=TRUE)}, x= summaries.ss[,c("bryo.old.L" ,"bryo.old.T",   "bryo.old.K","bryo.old.F","bryo.old.R")], y=summaries.ss[,c("bryo.new.L" ,"bryo.new.T",   "bryo.new.K","bryo.new.F","bryo.new.R")])# Light goes up significantly but only very slightly, temperature up significantly but only very slightly, continentality goes up slightly, moisture is doing absolutely nothing, reaction goes up slightly but significantly.

mapply(function(x,y, main){
  hist(x-y, main=main, xlim=c(-1,1), ylim=c(0,50))
  abline(v=0, col="red")
  }, 
  y= summaries.ss[,c("bryo.old.L" ,"bryo.old.T",   "bryo.old.K","bryo.old.F","bryo.old.R")], x=summaries.ss[,c("bryo.new.L" ,"bryo.new.T",   "bryo.new.K","bryo.new.F","bryo.new.R")], main=c("Light (up***)","Temperature (up***)","Continentality (up**)","Moisture (ns)","Reaction (up***)"))#  
plot(0,type='n',axes=FALSE,ann=FALSE)#puts in a blank plot because there is no trophism for bryophytes

mapply(function(x,y){t.test(x,y, paired=TRUE)}, x= summaries.ss[,c("vasc.old.L" ,"vasc.old.T", "vasc.old.K","vasc.old.W", "vasc.old.R","vasc.old.Tr")], y=summaries.ss[,c("vasc.new.L" ,"vasc.new.T","vasc.new.K","vasc.new.W", "vasc.new.R","vasc.new.Tr")])# Light ns, temperature up just significantly* but only very slightly, moisture is doing absolutely nothing, reaction goes up slightly but significantly, trophism goes up slightly but significantly.

mapply(function(x,y, main){
  hist(x-y, main=main, xlim=c(-1,1), ylim=c(0,50))
  abline(v=0, col="red") 
  }, 
  y= summaries.ss[,c("vasc.old.L" ,"vasc.old.T", "vasc.old.K","vasc.old.W", "vasc.old.R","vasc.old.Tr")], x=summaries.ss[,c("vasc.new.L" ,"vasc.new.T", "vasc.new.K","vasc.new.W", "vasc.new.R","vasc.new.Tr")], main=c("Light (up***)","Temperature (up**)","Continentality (down***)","Moisture (ns)","Reaction (up***)", "Nutrients (up***)"))

savePlot("Ellenbergs all groups.emf", type="emf")
savePlot("Ellenbergs all groups.pdf", type="pdf")
savePlot("Ellenbergs hists all groups.png", type="png")



###anovas of richness/ellenberg by dominant community
mapply(function(x,y,z){
  testy<-aov((x-y)~z)
  print(testy)
  print(TukeyHSD(testy))
  }, x= summaries.ss[,c("vasc.old.L" ,"vasc.old.T", "vasc.old.K","vasc.old.W", "vasc.old.R","vasc.old.Tr")], y=summaries.ss[,c("vasc.new.L" ,"vasc.new.T", "vasc.new.K","vasc.new.W", "vasc.new.R","vasc.new.Tr")], z=summaries.ss$dominant)# Error in model.frame.default(formula = (x - y) ~ z, drop.unused.levels = TRUE) : variable lengths differ (found for 'z') 
mapply(function(x,y){
  testy<-aov((x-y)~z)
  print(testy)
  print(TukeyHSD(testy))
}, x= summaries.ss$vasc.old.L, y=summaries.ss$vasc.new.L, z=summaries.ss$dominant)# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : contrasts can be applied only to factors with 2 or more levels 

#will have to do it the old way until I can work it out. Note! Multiple testing, 16 tests. Bonferroni would make p=0.003
testing1<-aov(summaries.ss$lich.old.L_light-summaries.ss$lich.new.L_light~summaries.ss$dominant)#works fine
testing2<-aov(summaries.ss$lich.old.T_temperature-summaries.ss$lich.new.T_temperature~summaries.ss$dominant)
testing3<-aov(summaries.ss$lich.old.K_continentality-summaries.ss$lich.new.K_continentality~summaries.ss$dominant)
testing4<-aov(summaries.ss$lich.old.F_moisture-summaries.ss$lich.new.F_moisture~summaries.ss$dominant)
testing5<-aov(summaries.ss$lich.old.R_reaction-summaries.ss$lich.new.R_reaction~summaries.ss$dominant)
testing6<-aov(summaries.ss$lich.old.N_nitrogen-summaries.ss$lich.new.N_nitrogen~summaries.ss$dominant)
lapply(list(testing1, testing2, testing3,testing4,testing5, testing6),FUN=summary)#significant after Bonferroni are light and continentality (significant without multiple testing correction are temperature and moisture)
lapply(list(testing1, testing2, testing3,testing4),FUN=function(x)TukeyHSD(x, bonferroni=TRUE))#light is PP-CA contrast at P=0.003, temperature is PP-CA P=.03 and TC-PP p=0.04, continentality PP-CA P=0.0012 and PP-TC 0.02, and moisture doesn't have anything, suggesting that anova is bogus here.
#Strictly, a global bonferroni on these tests (lichens significant ANOVAS only ie 0.05/12*4) is 0.0010 which nothing hits.
testing7<-aov(summaries.ss$bryo.old.L-summaries.ss$bryo.new.L~summaries.ss$dominant)
testing8<-aov(summaries.ss$bryo.old.T-summaries.ss$bryo.new.T~summaries.ss$dominant)
testing9<-aov(summaries.ss$bryo.old.K-summaries.ss$bryo.new.K~summaries.ss$dominant)
testing10<-aov(summaries.ss$bryo.old.F-summaries.ss$bryo.new.F~summaries.ss$dominant)
testing11<-aov(summaries.ss$bryo.old.R-summaries.ss$bryo.new.R~summaries.ss$dominant)
lapply(list(testing7, testing8, testing9,testing10,testing11),FUN=summary)#L is NS, T is at P=0.00005!, K and F are ns, R is at P=0.0004
lapply(list(testing7, testing8, testing11),FUN=function(x)TukeyHSD(x, bonferroni=TRUE))# Temp is PP-CA P=0.002, PP-CelA P=0.0002, PP_PQ P=0.04, PP-TC P=0.0003, R is PP-CA at 0.04, PP-TC at 0.002, QP-CA at P=0.03, 
#Bonferroni would be 0.05/(2*15) 0.0017
testing12<-aov(summaries.ss$vasc.old.L-summaries.ss$vasc.new.L~summaries.ss$dominant)
testing13<-aov(summaries.ss$vasc.old.T-summaries.ss$vasc.new.T~summaries.ss$dominant)
testing14<-aov(summaries.ss$vasc.old.K-summaries.ss$vasc.new.K~summaries.ss$dominant)
testing15<-aov(summaries.ss$vasc.old.W-summaries.ss$vasc.new.W~summaries.ss$dominant)
testing16<-aov(summaries.ss$vasc.old.R-summaries.ss$vasc.new.R~summaries.ss$dominant)
testing17<-aov(summaries.ss$vasc.old.Tr-summaries.ss$vasc.new.Tr~summaries.ss$dominant)
lapply(list(testing12, testing13, testing14, testing15,testing16,testing17),FUN=summary)#L is ns, T is P=0.0003, W is P=0.002, R is P=0.0000000000015 and Tr even stronger.
lapply(list(testing12,testing13, testing14, testing15, testing16,testing17),FUN=function(x)TukeyHSD(x, bonferroni=TRUE))# T is PP-CA P=0.008, pp-PQ P=0.0004287 (just misses), TC-PP P=0.005. W is PP-CelA 0.03, TC-PP 0.002. R is PP-CA 0.0009, PP-CelA 0.00006, QP-PP 0.000056, TC-PP <0.0000001, an TC-PQ 0.0006. Tr is PP-CA, PP-CelA, PP-QP and PP-TC <0.0000001, plus PP-PQ 0.0002, QP-PQ 0.03 and TC-PQ 0.02. Bonferroni on this set is 0.0010.
#Bonferroni on all of them is 0.000417
0.05/((12*4)+(12*2)+(12*4))
#P values 'beat' bonferroni in bryophytes for temperature, vascular soil reaction and vascular trophism.

#tidy up!
rm(testing1, testing2, testing3,testing4,testing5,testing6,testing7,testing8,testing9,testing10,testing11,testing12,testing13,testing14,testing15,testing16)


#for Checiny powerpoint
x11(6,3); par(mfrow=c(1,3), cex.lab=1.5, cex.axis=1.2)
mapply(function(x, z, main, ylab){
  boxplot(x~dominant, data=summaries.ss, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="#3873AE", main=main, las=2, ylab=ylab)
  boxplot(z~dominant, data=summaries.ss, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="#EF9335", xaxt="n", yaxt="n")},
  x = summaries.ss[,c("bryo.old.T","vasc.old.R","vasc.old.Tr")], 
  z=summaries.ss[,c("bryo.new.T","vasc.new.R","vasc.new.Tr")], 
    main=c("Bryophytes","Vascular plants","Vascular plants"),
  ylab=c("Temperature Indicator","Soil Reaction Indicator", "Soil Trophism Indicator"))

legend("bottomright", fill=c("#3873AE","#EF9335"), legend=c("1992","2015"), y.intersp=0.8)
savePlot("Ellenbergs by community after bonferroni for presentation.png", type="png")

#Figure: all the ellenbergs by community
x11(10,6); par(mfrow=c(3,6), mar=c(3,4,2,0.1), tcl=-0.2, mgp=c(2,0.25,0))
mapply(function(x,z, ylab, main){boxplot(x~dominant, data=summaries.ss, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="#3873AE", las=2, ylab=ylab, main=main)
  boxplot(z~dominant, data=summaries.ss, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="#EF9335", xaxt="n", yaxt="n")}, x= summaries.ss[,c("lich.old.L_light" ,"lich.old.T_temperature",   "lich.old.K_continentality","lich.old.F_moisture","lich.old.R_reaction","lich.old.N_nitrogen")], z=summaries.ss[,c("lich.new.L_light" ,"lich.new.T_temperature","lich.new.K_continentality", "lich.new.F_moisture", "lich.new.R_reaction", "lich.new.N_nitrogen")] , ylab=c("Light","Temperature", "Continentality","Moisture","Reaction","Nitrogen"), main=c("","","Lichens","","",""))

mapply(function(x,z, ylab, main){boxplot(x~dominant, data=summaries.ss, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="#3873AE", las=2, ylab=ylab, main=main)
  boxplot(z~dominant, data=summaries.ss, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="#EF9335", xaxt="n", yaxt="n")}, x= summaries.ss[,c("bryo.old.L" ,"bryo.old.T",   "bryo.old.K","bryo.old.F","bryo.old.R")], z=summaries.ss[,c("bryo.new.L" ,"bryo.new.T",   "bryo.new.K","bryo.new.F","bryo.new.R")], ylab=c("Light","Temperature","Continentality","Moisture ","Reaction"), main=c("","","Bryophytes","",""))
plot(0,type='n',axes=FALSE,ann=FALSE)

mapply(function(x,z, ylab, main){boxplot(x~dominant, data=summaries.ss, at=c(1,3,5,7,9,11), xlim=c(0.5,12.5), col="#3873AE", las=2, ylab=ylab, main=main)
  boxplot(z~dominant, data=summaries.ss, main=NULL,add=TRUE, at=c(1.8,3.8,5.8,7.8,9.8,11.8), col="#EF9335", xaxt="n", yaxt="n")}, x= summaries.ss[,c("vasc.old.L" ,"vasc.old.T", "vasc.old.K","vasc.old.W", "vasc.old.R","vasc.old.Tr")], z=summaries.ss[,c("vasc.new.L" ,"vasc.new.T", "vasc.new.K","vasc.new.W", "vasc.new.R","vasc.new.Tr")], ylab=c("Light","Temperature","Continentality","Moisture ","Reaction","Trophism"), main=c("","","Vascular plants","","",""))
 
savePlot("Ellenbergs by community ALL.png", type="png")

                                                                                          
#Summaries
write.csv2(aggregate(summaries.ss, by=list(summaries.ss$dominant), FUN=mean), file="summaries for Bogdan.csv")
