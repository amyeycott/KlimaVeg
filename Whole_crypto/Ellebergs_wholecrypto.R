source("turnover.R")
#the datasets are prepared in turnover.r. Here we use a subset excluding rectangles and rivers. The first two lines are exactly the same as in turnover_subset.r
dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")
summaries.ss<-Summaries[!rownames(Summaries)%in%dodgysquares,]

mapply(function(x,y){t.test(x,y, paired=TRUE)}, x= summaries.ss[,c("lich.old.L_light" ,"lich.old.T_temperature",   "lich.old.K_continentality","lich.old.F_moisture","lich.old.R_reaction","lich.old.N_nitrogen")], y=summaries.ss[,c("lich.new.L_light" ,"lich.new.T_temperature","lich.new.K_continentality", "lich.new.F_moisture" ,"lich.new.R_reaction","lich.new.N_nitrogen")])# Negative difference means value went UP. Light goes up significantly but only very slightly, temperature is ns (just - goes up by almost two points), continentality is ns, moisture goes up but only slightly, reaction goes up slightly but significantly, and nitrogen goes up significantly but still less than half a point.
x11(12,7);par(mfrow=c(3,6))
mapply(function(x,y, main){
  hist(x-y, main=main, xlim=c(-1,1), ylim=c(0,50))
  abline(v=0, col="red")
  }, 
  y= summaries.ss[,c("lich.old.L_light" ,"lich.old.T_temperature",   "lich.old.K_continentality","lich.old.F_moisture","lich.old.R_reaction","lich.old.N_nitrogen")], x=summaries.ss[,c("lich.new.L_light" ,"lich.new.T_temperature","lich.new.K_continentality", "lich.new.F_moisture", "lich.new.R_reaction", "lich.new.N_nitrogen")] , main=c("Light (up**)","Temperature (ns)", "Continentality(ns)","Moisture (up***)","Reaction (up***)","Nitrogen (up***)"))  

mapply(function(x,y){t.test(x,y, paired=TRUE)}, x= summaries.ss[,c("bryo.old.L" ,"bryo.old.T",   "bryo.old.K","bryo.old.F","bryo.old.R")], y=summaries.ss[,c("bryo.new.L" ,"bryo.new.T",   "bryo.new.K","bryo.new.F","bryo.new.R")])# Light goes up significantly but only very slightly, temperature up significantly but only very slightly, continentality goes up slightly, moisture is doing absolutely nothing, reaction goes up slightly but significantly.

mapply(function(x,y, main){
  hist(x-y, main=main, xlim=c(-1,1), ylim=c(0,50))
  abline(v=0, col="red")
  }, 
  y= summaries.ss[,c("bryo.old.L" ,"bryo.old.T",   "bryo.old.K","bryo.old.F","bryo.old.R")], x=summaries.ss[,c("bryo.new.L" ,"bryo.new.T",   "bryo.new.K","bryo.new.F","bryo.new.R")], main=c("Light (up***)","Temperature (up***)","Continentality (up**)","Moisture (ns)","Reaction (up***)"))#  
plot(0,type='n',axes=FALSE,ann=FALSE)

mapply(function(x,y){t.test(x,y, paired=TRUE)}, x= summaries.ss[,c("vasc.old.L" ,"vasc.old.T", "vasc.old.W", "vasc.old.R","vasc.old.Tr")], y=summaries.ss[,c("vasc.new.L" ,"vasc.new.T","vasc.new.W", "vasc.new.R","vasc.new.Tr")])# Light ns, temperature up just significantly* but only very slightly, moisture is doing absolutely nothing, reaction goes up slightly but significantly, trophism goes up slightly but significantly.

mapply(function(x,y, main){
  hist(x-y, main=main, xlim=c(-1,1), ylim=c(0,50))
  abline(v=0, col="red") 
  }, 
  y= summaries.ss[,c("vasc.old.L" ,"vasc.old.T")], x=summaries.ss[,c("vasc.new.L" ,"vasc.new.T")], main=c("Light (na)","Temperature (up*)"))# 
plot(0,type='n',axes=FALSE,ann=FALSE)
mapply(function(x,y, main){
  hist(x-y, main=main, xlim=c(-1,1), ylim=c(0,50))
  abline(v=0, col="red")
  }, y= summaries.ss[,c("vasc.old.W", "vasc.old.R","vasc.old.Tr")], x=summaries.ss[,c("vasc.new.W", "vasc.new.R","vasc.new.Tr")], main=c("Moisture (ns)","Reaction (up***)", "Nutrients (up***)"))# 
savePlot("Ellenbergs all groups.emf", type="emf")
savePlot("Ellenbergs all groups.pdf", type="pdf")
savePlot("Ellenbergs all groups.png", type="png")