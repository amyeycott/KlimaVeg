library(readxl)
bryophytes<-read_excel("Baza Styczen 2016.xls", sheet=1,col_names = TRUE) # przypisałam do zbioru funcje
str(bryophytes) # teraz robię badanie konstrukcji całyej zawartości arkusza, który został wczytany
bryophytes<-as.data.frame(bryophytes)# we do this because otherwise read.excel makes three objects and code won't work.
bryophytes[is.na(bryophytes)] <-0#replace NAs with zeros. this makes some later code a lot easier but I always put dataset preparation at the top
bryophytes<-bryophytes[!(rowSums(bryophytes[,5:length(bryophytes)]))==0,]
colnames(bryophytes)<-gsub(" ", "_", colnames(bryophytes))
bryophytes$Species_name<-as.factor(bryophytes$Species_name)



#all the places where there is a record but no frequency.
write.table(bryophytes[bryophytes$Frequency==0&!(rowSums(bryophytes[,5:length(bryophytes)]))==0, 1:4], file="strange things of zero frequency.tab", sep="\t")

unique(bryophytes$Frequency)#checking for silly (like 22) values - unique gives a listof all the values in an element

#the table for checking that the right species is on the right substrate, overall
speciesbysubstrate<-aggregate(bryophytes[,5:length(bryophytes)], by=list(bryophytes$Species_name), FUN=sum)
write.table(speciesbysubstrate, "species by substrate.csv", sep=";", quote=FALSE)

#saving a table 
sapply(unique(bryophytes$Year),function(Y) {
  sapply(unique(bryophytes$Plot), function(P){
    print(c(Y,P))
    x<-bryophytes[bryophytes$Year==Y&bryophytes$Plot==P,]
    write.table(x, file=paste0("bryophytes",Y,P,".csv"), sep=";", quote=FALSE, row.names=FALSE)
  })
})

sapply(unique(bryophytes$Year),function(Y) {
  sapply(unique(bryophytes$Plot), function(P){
    print(c(Y,P))
    x<-bryophytes[bryophytes$Year==Y&bryophytes$Plot==P,1:4]
    write.table(x, file=paste0("bryophytes",Y,P,"justfreq.csv"), sep=";", quote=FALSE, row.names=FALSE)
  })
})

#how can AG have 1.5 proportion of epeihpytic to overall ocurrences? checking for doubles
str(bryophytes [1:6])
bryophytes[duplicated(with(bryophytes, paste(Plot, Year, Species_name)))==TRUE,1:4] #if this comes up with stuff, it's usually because the line of removing empty lines has not run
unique(with (bryophytes, duplicated(paste(Plot, Year, Species_name))))
