library(readxl)
bryophytes<-read_excel("Baza Styczen 2016.xls", sheet=1,col_names = TRUE) # przypisałam do zbioru funcje
status<-read.table("Ellenberg09022016.txt", sep="\t")#I don't know why read xls wasn't working, but it wasn't.
str(status)
str(bryophytes) # teraz robię badanie konstrukcji całyej zawartości arkusza, który został wczytany
bryophytes<-as.data.frame(bryophytes)# we do this because otherwise read.excel makes three objects and code won't work.
bryophytes[is.na(bryophytes)] <-0#replace NAs with zeros. this makes some later code a lot easier but I always put dataset preparation at the top
bryophytes<-bryophytes[!(rowSums(bryophytes[,5:length(bryophytes)]))==0,]#removes the blank rows. 
colnames(bryophytes)<-gsub(" ", "_", colnames(bryophytes))# this is a special line to make the colnames not have spaces in. Then you can get rid of those stupid ` things.
bryophytes$Species_name<-as.factor(bryophytes$Species_name)

S.in.LS<-c("S14", "S16","S18","S19")
S.in.CWD<-c("S6", "S9", "S10", "S11", "S13", "S15", "S17")
S.in.inorganic<-c("S1", "S2","S3", "S4","S5","s12", "S25")
S.in.fineorganic<-c("S7","S8","S20", "S21","S22", "S23","S24","S26")

easytab<-bryophytes[1:4]
easytabx<-xtabs(Frequency~(paste(Plot,Year, sep=""))+Species_name, data=easytab)
easytabx<-as.data.frame.matrix(easytabx)
head(easytabx[1:6])
