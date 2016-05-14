# First run "KlimaVegRedBog.R" until I work out how source works in RStudio
library(vegan)
testit<-metaMDS(mergedplants)
plot(testit)
testitDCA<-decorana(mergedplants)
plot(testitDCA)
plot(testitDCA, display ="sites", cols=as.factor(as.character(envall$Year)))
       
       str(envall$Year)

head(envall)

envall[1:4,4]
class(envall)
levels(envall$Year)
attributes(envall$Year)
attr(envall$Year, which=c())<-NULL
envall$Year<-unname(envall$Year, force=FALSE)

names(mergedplants)

