library(readxl)
bryophytes<-read_excel("../sylwia/Baza Styczen 2016.xls", sheet=1,col_names = TRUE) # przypisałam do zbioru funcje
bryo.status<-read_excel("../sylwia/Baza Styczen 2016.xls", sheet="Ellenberg.protected")#works only if you index the sheet number by name, not by position.
str(bryo.status)
names(bryo.status)<-gsub(" ", "_", names(bryo.status))
bryo.status$Red_coded[bryo.status$Red_coded==""]<-0
bryo.status$Any.status<-bryo.status$Red_coded
bryo.status$Any.status[!is.na(bryo.status$Red_coded)]<-1
bryo.status$Any.status[!is.na(bryo.status$Protected_species)]<-1
bryo.status$Any.status[is.na(bryo.status$Any.status)]<-0
bryo.status$Protected_species<-as.factor(bryo.status$Protected_species)
bryo.status$Red_coded<-as.factor(bryo.status$Red_coded)


#str(bryophytes) # teraz robię badanie konstrukcji całyej zawartości arkusza, który został wczytany #commented out so that it's tidier when using this as source code
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
