#this code tests whether the changes in ellenberg values are the same for different plant groups.
source("../Whole_crypto/source data and merging.R")

#what is loaded and useful? vasc.ellen is a thin table by species. vasc composition is in five different objects: vascall.df (sites are rows, species are columns), vascnew.fat, vascnew.thin, vascold.fat and vascold.thin. Lichens Wirth values are in old.wirths and new.wirths, which are thin tables of species and plot and wirth value. They are also in envir which is a table of species as rows. Lichen composition files are olddb and old.harm.db, newdb and new.harm.db - use the harmonised ones - for thin files, and comp, comp_new and comp_old for fat type. Bryophyte ellenbergs are in bryo.status. Bryophyte composition is in bryophytes for full thin data, easytab for trimmed thin data, easytabx for fat data.
#Less useful are compz, vasc.protected and lich.protect. 

#first make the subset of squares that are rectangles or are rivers.
dodgysquares<-c("P01", "O03", "N08", "M09", "M10", "M11", "A11", "B11", "C11", "D11", "E11", "F11", "G11", "H11", "I11", "J11", "K11", "L11")

####weighted mean ellenbergs for different species groups in the plots####

#this could be done with a loop and weighted.mean but never mind.
#merge thin tables with ellenberg values (already done for lichens)
vascoldthin.withellens<-merge(VascOld.thin[,c("Species_name","Plot_number","frequency_score")], vasc.ellen, by.x="Species_name", by.y="Species.name")
vascnewthin.withellens<-merge(VascNew.thin[,c("Species_name_2015","Plot_number_2015","frequency_score")], vasc.ellen, by.x="Species_name_2015", by.y="Species.name")
bryothin.withellens<-merge(easytab, bryo.status, by.x="Species_name", by.y="Species_name")

#make several new columns at once
vascoldthin.withellens[c("L.x.freq","T.x.freq","W.x.freq","Tr.x.freq","R.x.freq")] <- NA

#multiply frequency by ellenberg to get a 'contribution' for that species in that plot. 
vascoldthin.withellens[c("L.x.freq","T.x.freq","W.x.freq","Tr.x.freq","R.x.freq")]<-
mapply(function(x)x*vascoldthin.withellens$frequency_score, vascoldthin.withellens[c("L","T","W","Tr","R")])

#aggregate or ddply to make the plot weighted sum then divide the sum by the plot sum of frequencies to get a weighted average
plotsums.vascoldell<-ddply(vascoldthin.withellens[,c("Plot_number","L.x.freq","T.x.freq","W.x.freq","Tr.x.freq","R.x.freq")], "Plot_number", function(x) colMeans(x, na.rm=TRUE) )# Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric 

test<-0
test$L<-ddply(vascoldthin.withellens[,c("Plot_number","L.x.freq")], "Plot_number", FUN=colMeans)#that's not right either, it just returns the input. 


#pull out the unused plots(doing this last in case we want them back)

