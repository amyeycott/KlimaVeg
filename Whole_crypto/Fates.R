###fates of species which disappeared from PP plots after 1990:
source("turnover_subset.r")
##who got lost from the PP and where did they go?
#Important!! No PP in dodgysquares. The list of PP squares is just called PP. I don't know where it loads but it does.


vascall.df.ss.PP<-vascall.df.ss[substr(rownames(vascall.df.ss),1,3)%in%PP,]

changes.vasc.ss.PP<-colSums((vascall.df.ss.PP[substr(rownames(vascall.df.ss.PP),4,7)=="2015",]))/(colSums(vascall.df.ss.PP[substr(rownames(vascall.df.ss.PP),4,7)=="1990",]))
lostfromppvasc<-unique(names(changes.vasc.ss.PP[changes.vasc.ss.PP==0]))#output is an odd thing, the na has to be tolerated

PPvascwentextinct<-colSums(vascall.df.ss[substr(rownames(vascall.df.ss),4,7)=="2015",colnames(vascall.df.ss)%in%lostfromppvasc])#Zeros went extinct
PPvascwentextinct<-names(PPvascwentextinct[PPvascwentextinct==0])
PPvascwentextinct.onlyeverpp<-(colSums(vascall.df.ss[(substr(rownames(vascall.df.ss),4,7)=="1990")&!(substr(rownames(vascall.df.ss),1,3)%in%PP),colnames(vascall.df.ss)%in%PPvascwentextinct]))  #were they elsewhere?
PPvascwentextinct.onlyeverpp<-names(PPvascwentextinct.onlyeverpp[PPvascwentextinct.onlyeverpp==0]) 
PPvascwentextinct.notjustfrompp<-PPvascwentextinct[!(PPvascwentextinct%in%PPvascwentextinct.onlyeverpp)]

PPvascwentelsewhere<-lostfromppvasc[!(lostfromppvasc%in%PPvascwentextinct)]
PPvascwentelsewhere.startedPPonly<-(colSums(vascall.df.ss[(substr(rownames(vascall.df.ss),4,7)=="1990")&!(substr(rownames(vascall.df.ss),1,3)%in%PP),colnames(vascall.df.ss)%in%PPvascwentelsewhere]))
PPvascwentelsewhere.startedPPonly<-PPvascwentelsewhere.startedPPonly[PPvascwentelsewhere.startedPPonly==0]
PPvascwentelsewhere.alwayswas<-PPvascwentelsewhere[!PPvascwentelsewhere%in%PPvascwentelsewhere.startedPPonly]

#of all these possible fates of lostfromPP, how many plots were they in in 1990?
colSums(vascall.df.ss[(substr(rownames(vascall.df.ss),4,7)=="1990"),colnames(vascall.df.ss)%in%PPvascwentextinct.notjustfrompp])
colSums(vascall.df.ss[(substr(rownames(vascall.df.ss),4,7)=="1990"),colnames(vascall.df.ss)%in%PPvascwentextinct.onlyeverpp])
colSums(vascall.df.ss[(substr(rownames(vascall.df.ss),4,7)=="1990"),colnames(vascall.df.ss)%in%PPvascwentelsewhere.alwayswas])

##lichens
lichall.df.ss.PP<-lichall.df.ss[substr(rownames(lichall.df.ss),1,3)%in%PP,]

changes.lich.ss.PP<-colSums((lichall.df.ss.PP[substr(rownames(lichall.df.ss.PP),4,7)=="2015",]))/(colSums(lichall.df.ss.PP[substr(rownames(lichall.df.ss.PP),4,7)=="1990",]))
lostfrompplich<-unique(names(changes.lich.ss.PP[changes.lich.ss.PP==0]))#somehow hates drop levels

PPlichwentextinct<-colSums(lichall.df.ss[substr(rownames(lichall.df.ss),4,7)=="2015",colnames(lichall.df.ss)%in%lostfrompplich])#Zeros went extinct
PPlichwentextinct<-names(PPlichwentextinct[PPlichwentextinct==0])
PPlichwentextinct.onlyeverpp<-(colSums(lichall.df.ss[(substr(rownames(lichall.df.ss),4,7)=="1990")&!(substr(rownames(lichall.df.ss),1,3)%in%PP),colnames(lichall.df.ss)%in%PPlichwentextinct]))  #were they elsewhere?
PPlichwentextinct.onlyeverpp<-names(PPlichwentextinct.onlyeverpp[PPlichwentextinct.onlyeverpp==0]) 
PPlichwentextinct.notjustfrompp<-PPlichwentextinct[!(PPlichwentextinct%in%PPlichwentextinct.onlyeverpp)]

PPlichwentelsewhere<-lostfrompplich[!(lostfrompplich%in%PPlichwentextinct)]
PPlichwentelsewhere.startedPPonly<-(colSums(lichall.df.ss[(substr(rownames(lichall.df.ss),4,7)=="1990")&!(substr(rownames(lichall.df.ss),1,3)%in%PP),colnames(lichall.df.ss)%in%PPlichwentelsewhere]))
PPlichwentelsewhere.startedPPonly<-PPlichwentelsewhere.startedPPonly[PPlichwentelsewhere.startedPPonly==0]
PPlichwentelsewhere.alwayswas<-PPlichwentelsewhere[!PPlichwentelsewhere%in%PPlichwentelsewhere.startedPPonly]

#of all these possible fates of lostfromPP, how many plots were they in in 1990?
colSums(lichall.df.ss[(substr(rownames(lichall.df.ss),4,7)=="1990"),colnames(lichall.df.ss)%in%PPlichwentextinct.notjustfrompp])
colSums(lichall.df.ss[(substr(rownames(lichall.df.ss),4,7)=="1990"),colnames(lichall.df.ss)%in%PPlichwentextinct.onlyeverpp])
as.data.frame(colSums(lichall.df.ss[(substr(rownames(lichall.df.ss),4,7)=="1990"),colnames(lichall.df.ss)%in%PPlichwentelsewhere.alwayswas]))
as.data.frame(colSums(lichall.df.ss[(substr(rownames(lichall.df.ss),4,7)=="1990"),colnames(lichall.df.ss)%in%PPlichwentelsewhere.startedPPonly]))

#bryos
bryoall.df.ss.PP<-bryoall.df.ss[substr(rownames(bryoall.df.ss),1,3)%in%PP,]

changes.bryo.ss.PP<-colSums((bryoall.df.ss.PP[substr(rownames(bryoall.df.ss.PP),4,7)=="2015",]))/(colSums(bryoall.df.ss.PP[substr(rownames(bryoall.df.ss.PP),4,7)=="1990",]))
lostfromppbryo<-unique(names(changes.bryo.ss.PP[changes.bryo.ss.PP==0]))#somehow hates drop levels

PPbryowentextinct<-colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",colnames(bryoall.df.ss)%in%lostfromppbryo])#Zeros went extinct
PPbryowentextinct<-names(PPbryowentextinct[PPbryowentextinct==0])
PPbryowentextinct.onlyeverpp<-(colSums(bryoall.df.ss[(substr(rownames(bryoall.df.ss),4,7)=="1990")&!(substr(rownames(bryoall.df.ss),1,3)%in%PP),colnames(bryoall.df.ss)%in%PPbryowentextinct]))  #were they elsewhere?
PPbryowentextinct.onlyeverpp<-names(PPbryowentextinct.onlyeverpp[PPbryowentextinct.onlyeverpp==0]) 
PPbryowentextinct.notjustfrompp<-PPbryowentextinct[!(PPbryowentextinct%in%PPbryowentextinct.onlyeverpp)]

PPbryowentelsewhere<-lostfromppbryo[!(lostfromppbryo%in%PPbryowentextinct)]
PPbryowentelsewhere.startedPPonly<-(colSums(bryoall.df.ss[(substr(rownames(bryoall.df.ss),4,7)=="1990")&!(substr(rownames(bryoall.df.ss),1,3)%in%PP),colnames(bryoall.df.ss)%in%PPbryowentelsewhere]))
PPbryowentelsewhere.startedPPonly<-PPbryowentelsewhere.startedPPonly[PPbryowentelsewhere.startedPPonly==0]#none
PPbryowentelsewhere.alwayswas<-PPbryowentelsewhere[!PPbryowentelsewhere%in%PPbryowentelsewhere.startedPPonly]

#of all these possible fates of lostfromPP, how many plots were they in in 1990?
bryoall.df.ss$`Eurhynchium striatum`[(substr(rownames(bryoall.df.ss),4,7)=="1990")]#normal filter gets sad because it's only one species, Eurhynchium striatum
as.data.frame(colSums(bryoall.df.ss[(substr(rownames(bryoall.df.ss),4,7)=="1990"),colnames(bryoall.df.ss)%in%PPbryowentextinct.onlyeverpp]))
as.data.frame(colSums(bryoall.df.ss[(substr(rownames(bryoall.df.ss),4,7)=="1990"),colnames(bryoall.df.ss)%in%PPbryowentelsewhere.alwayswas]))
as.data.frame(colSums(bryoall.df.ss[(substr(rownames(bryoall.df.ss),4,7)=="1990"),colnames(bryoall.df.ss)%in%PPbryowentelsewhere.startedPPonly]))

rm(lostfromppvasc, PPlichwentextinct, PPlichwentextinct.notjustfrompp, PPlichwentextinct.onlyeverpp, PPlichwentelsewhere, PPlichwentelsewhere.startedPPonly, PPlichwentelsewhere.alwayswas, lostfrompplich, PPlichwentelsewhere, PPlichwentelsewhere.alwayswas, PPlichwentelsewhere.startedPPonly, PPlichwentextinct, PPlichwentextinct.onlyeverpp, PPlichwentextinct.notjustfrompp, lostfromppbryo, PPbryowentelsewhere, PPbryowentelsewhere.alwayswas, PPbryowentelsewhere.startedPPonly, PPbryowentextinct, PPbryowentextinct.onlyeverpp, PPbryowentextinct.notjustfrompp)

#### fates of species which disappeared from the extiction-y-est communities. From crypto ms"the greatest proportions of bryophytes disappeared in Peucedanum-Pinetum or Pino-Quercetum  plots and the lowest proportion in Circaeao-Alnetum, while the highest rate of vascular plant extinctions took place in Querco-Piceetum . "####
QP<-rownames(summaries.ss[summaries.ss$dominant==("QP"),])

vascall.df.ss.QP<-vascall.df.ss[substr(rownames(vascall.df.ss),1,3)%in%QP,]

changes.vasc.ss.QP<-colSums((vascall.df.ss.QP[substr(rownames(vascall.df.ss.QP),4,7)=="2015",]))/(colSums(vascall.df.ss.QP[substr(rownames(vascall.df.ss.QP),4,7)=="1990",]))
lostfromQPvasc<-unique(names(changes.vasc.ss.QP[changes.vasc.ss.QP==0]))#output is an odd thing, the na has to be tolerated

QPvascwentextinct<-colSums(vascall.df.ss[substr(rownames(vascall.df.ss),4,7)=="2015",colnames(vascall.df.ss)%in%lostfromQPvasc])# #Only one, Carex loliacea

QPvascwentelsewhere<-lostfromQPvasc[!(lostfromQPvasc%in%QPvascwentextinct)]
QPvascwentelsewhere.startedQPonly<-(colSums(vascall.df.ss[(substr(rownames(vascall.df.ss),4,7)=="1990")&!(substr(rownames(vascall.df.ss),1,3)%in%QP),colnames(vascall.df.ss)%in%QPvascwentelsewhere]))
QPvascwentelsewhere.startedQPonly<-QPvascwentelsewhere.startedQPonly[QPvascwentelsewhere.startedQPonly==0]#0
QPvascwentelsewhere.alwayswas<-QPvascwentelsewhere[!QPvascwentelsewhere%in%QPvascwentelsewhere.startedQPonly]#21

#of all these possible fates of lostfromQP, how many plots were they in in 1990?
vascall.df.ss[(substr(rownames(vascall.df.ss),4,7)=="1990"),colnames(vascall.df.ss)=="Carex_loliacea"]

colSums(vascall.df.ss[(substr(rownames(vascall.df.ss),4,7)=="1990"),colnames(vascall.df.ss)%in%QPvascwentelsewhere.alwayswas])


#bryos
PPPQ<-c(PP, rownames(summaries.ss[summaries.ss$dominant==("PQ"),]))

bryoall.df.ss.PPPQ<-bryoall.df.ss[substr(rownames(bryoall.df.ss),1,3)%in%PPPQ,]

changes.bryo.ss.PPPQ<-colSums((bryoall.df.ss.PPPQ[substr(rownames(bryoall.df.ss.PPPQ),4,7)=="2015",]))/(colSums(bryoall.df.ss.PPPQ[substr(rownames(bryoall.df.ss.PPPQ),4,7)=="1990",]))
lostfromPPPQbryo<-unique(names(changes.bryo.ss.PPPQ[changes.bryo.ss.PPPQ==0]))#14. This is just the PP list and misses Ceratodon purpureus, Dicranella cerviculata.

PPPQbryowentextinct<-colSums(bryoall.df.ss[substr(rownames(bryoall.df.ss),4,7)=="2015",colnames(bryoall.df.ss)%in%lostfromPPPQbryo])#Zeros went extinct
PPPQbryowentextinct<-names(PPPQbryowentextinct[PPPQbryowentextinct==0])#11
PPPQbryowentextinct.onlyeverPPPQ<-(colSums(bryoall.df.ss[(substr(rownames(bryoall.df.ss),4,7)=="1990")&!(substr(rownames(bryoall.df.ss),1,3)%in%PPPQ),colnames(bryoall.df.ss)%in%PPPQbryowentextinct]))  #were they elsewhere?
PPPQbryowentextinct.onlyeverPPPQ<-names(PPPQbryowentextinct.onlyeverPPPQ[PPPQbryowentextinct.onlyeverPPPQ==0]) #10
PPPQbryowentextinct.notjustfromPPPQ<-PPPQbryowentextinct[!(PPPQbryowentextinct%in%PPPQbryowentextinct.onlyeverPPPQ)]#Eurhynchium striatum

PPPQbryowentelsewhere<-lostfromPPPQbryo[!(lostfromPPPQbryo%in%PPPQbryowentextinct)]
PPPQbryowentelsewhere.startedPPPQonly<-(colSums(bryoall.df.ss[(substr(rownames(bryoall.df.ss),4,7)=="1990")&!(substr(rownames(bryoall.df.ss),1,3)%in%PPPQ),colnames(bryoall.df.ss)%in%PPPQbryowentelsewhere]))
PPPQbryowentelsewhere.startedPPPQonly<-PPPQbryowentelsewhere.startedPPPQonly[PPPQbryowentelsewhere.startedPPPQonly==0]#none
PPPQbryowentelsewhere.alwayswas<-PPPQbryowentelsewhere[!PPPQbryowentelsewhere%in%PPPQbryowentelsewhere.startedPPPQonly]#Zygodon rupestris

#of all these possible fates of lostfromPPPQ, how many plots were they in in 1990?
bryoall.df.ss$`Eurhynchium striatum`[(substr(rownames(bryoall.df.ss),4,7)=="1990")]#normal filter gets sad because it's only one species, Eurhynchium striatum. 3 plots
as.data.frame(colSums(bryoall.df.ss[(substr(rownames(bryoall.df.ss),4,7)=="1990"),colnames(bryoall.df.ss)%in%PPPQbryowentextinct.onlyeverPPPQ]))
sum(bryoall.df.ss$`Zygodon rupestris`)#14


rm(lostfromppvasc, PPlichwentextinct, PPlichwentextinct.notjustfrompp, PPlichwentextinct.onlyeverpp, PPlichwentelsewhere, PPlichwentelsewhere.startedPPonly, PPlichwentelsewhere.alwayswas, lostfrompplich, PPlichwentelsewhere, PPlichwentelsewhere.alwayswas, PPlichwentelsewhere.startedPPonly, PPlichwentextinct, PPlichwentextinct.onlyeverpp, PPlichwentextinct.notjustfrompp, lostfromppbryo, PPbryowentelsewhere, PPbryowentelsewhere.alwayswas, PPbryowentelsewhere.startedPPonly, PPbryowentextinct, PPbryowentextinct.onlyeverpp, PPbryowentextinct.notjustfrompp)
