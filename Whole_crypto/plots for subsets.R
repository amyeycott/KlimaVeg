source("../Whole_crypto/turnover_subset.R")
names(envir)
names(lichall.df.ss)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}#adapted from help file for pairs, prints the correlation coefficient on the upper diagonals

pairs(~L_light+T_temperature+K_continentality+F_moisture+R_reaction+N_nitrogen,data=envir[envir$Species%in%names(lichall.df.ss),], lower.panel = panel.smooth, upper.panel = panel.cor, na.action = na.omit)#panel.smooth is fitting a lowess-smoothed fit line to the lower diagonals.

pairs(sapply(envir[envir$Species%in%names(lichall.df.ss),c("L_light","T_temperature","K_continentality","F_moisture","R_reaction","N_nitrogen")], jitter, amount=0.1),lower.panel=panel.smooth, upper.panel=NULL, main="Lichens")#you can have jitter OR panel cor, not both.

savePlot("Lichen_EIV_correlations.emf", type="emf")
savePlot("Lichen_EIV_correlations.png", type="png")

pairs(sapply(bryo.status[,c("L","T","K","F","R")], jitter, amount=0.1),lower.panel=panel.smooth, upper.panel=NULL, main="Bryophytes")#you can have jitter OR panel cor, not both.

savePlot("Bryo_EIV_correlations.emf", type="emf")
savePlot("Bryo_EIV_correlations.png", type="png")

pairs(sapply(vasc.ellen[,c("L","T","K","W","Tr","R")], jitter, amount=0.1),lower.panel=panel.smooth, upper.panel=NULL, main="Vascular plants")#you can have jitter OR panel cor, not both.

savePlot("Vasc_EIV_correlations.emf", type="emf")
savePlot("Vasc_EIV_correlations.png", type="png")

