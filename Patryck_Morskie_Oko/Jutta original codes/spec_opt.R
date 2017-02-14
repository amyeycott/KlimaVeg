###########################
# Species optimum analysis
# following Kapfer J, Grytnes J-A, Gunnarsson U & Birks HJB (2011) Fine-scale changes in vegetation
#	composition in a boreal mire over 50 years. JEcol 99:1179-1189
###########################


#lnb is a vector with the indicator values for all species
#sp.df is the species/samples data frame. The order of species must be equal to the order of species in the lnb
#time0 is a vector with the period identifier (either 0 (old) or 1 (new)). Equal length to number of samples
#perm is number of permutations that should be run

emp.fun<-function(lnb,sp.df,time0,perm)
{
  
  #identifies the common species (all species that has occur>1 have >5 occurrences for each time period)
  allsp.pa<-sp.df>0
  occur.d<-apply(allsp.pa[time0==0,],2,sum)
  occur.f<-apply(allsp.pa[time0==1,],2,sum)
  occur<-(occur.d>5)+(occur.f>5)
  
  sp.nr<-dim(sp.df)[2]
  sample.nr<-dim(sp.df)[1]
  obs.res<-NULL
  sp1<-(1:sp.nr)[occur>1]
  sp.id<-1:sp.nr
  for(m in sp1)
  {
    
    
    #find weighted average (wa) S A M P L E   S C O R E for the indicator value for all samples (i)
    waN0<-NULL
    
    for(i in 1:sample.nr)
    {
      #makes a matrix out of the scores and the abundances for all species except the one in focus 
      # (i.e. the one for which the change is estimated)
      b1<-cbind(as.numeric(lnb[!sp.id==m]),as.numeric(sp.df[i,sp.id[!sp.id==m]]))
      
      #takes away those species that does not have an indicator value
      b2<-na.omit(b1)
      
      #estimate the wa scores
      wa1<-sum(b2[,1]*b2[,2])/sum(b2[,2])
      waN0<-c(waN0,wa1)
    }
    
    #makes new species data set, time and wa variables without the samples that could not be given a sample score
    time1<-na.omit(cbind(waN0,time0))[,2]
    sp.df1<-cbind(waN0,sp.df)
    sp.df1<-sp.df1[,2:dim(sp.df1)[2]]
    waN<-na.omit(cbind(waN0,sp.df))[,1]
    
    
    # P R U N E  data set so that they have similar distribution
    
    #first take away extremes
    #identify the second most extreme values at both times
    
    l0<-length(waN[time1==0])
    l1<-length(waN[time1==1])
    
    xtr0.l<-sort(waN[time1==0])[2]
    xtr1.l<-sort(waN[time1==1])[2]
    #the highest value of the lowest extreme from the two datasets
    xtr.l<-max(c(xtr0.l,xtr1.l))
    
    xtr0.h<-sort(waN[time1==0])[l0-1]
    xtr1.h<-sort(waN[time1==1])[l1-1]
    #the lowest value of the highest extreme from the two datasets
    xtr.h<-min(c(xtr0.h,xtr1.h))
    
    id1<-1:(length(waN))
    
    #make a new data set (wa and time, pluss a sample id for the species data set) taking out values that are 
    # higher or lower than these extreme values
    prun.wan<-waN[waN>xtr.l&waN<xtr.h]
    prun.time<-time1[waN>xtr.l&waN<xtr.h]
    prun.id<-id1[waN>xtr.l&waN<xtr.h]
    
    #divide these variables (wa and id) into two data sets based on the two time periods
    prun0.wan<-prun.wan[prun.time==0]
    prun1.wan<-prun.wan[prun.time==1]
    
    prun0.id<-prun.id[prun.time==0]
    prun1.id<-prun.id[prun.time==1]
    
    
    #divide in three sections to equalize the distribution
    q0<-quantile(c(prun1.wan,prun0.wan),prob=c(0.33,0.66,1))
    
    div0.1<-prun0.id[prun0.wan<=q0[1]]
    div0.2<-prun0.id[prun0.wan>q0[1]&prun0.wan<=q0[2]]
    div0.3<-prun0.id[prun0.wan>q0[2]]
    
    div1.1<-prun1.id[prun1.wan<=q0[1]]
    div1.2<-prun1.id[prun1.wan>q0[1]&prun1.wan<=q0[2]]
    div1.3<-prun1.id[prun1.wan>q0[2]]
    
    #make  the two time periods have equal relationship in number of plots used
    rel0.tot<-length(prun0.id)/length(prun.id)
    rel1.tot<-length(prun1.id)/length(prun.id)
    
    tot.1<-length(div1.1)+length(div0.1)
    tot.2<-length(div1.2)+length(div0.2)
    tot.3<-length(div1.3)+length(div0.3)
    
    rel0.1<-length(div0.1)/tot.1
    rel1.1<-length(div1.1)/tot.1
    rel0.2<-length(div0.2)/tot.2
    rel1.2<-length(div1.2)/tot.2
    rel0.3<-length(div0.3)/tot.3
    rel1.3<-length(div1.3)/tot.3
    
    
    
    #run permutations from here
    
    
    permut<-NULL
    obs.diff0<-NULL
    obs.diff.rel0 <- NULL
    
    for (w in 1:perm)
    {
      
      #a1 finds the number that should be drawn from the original sample to make the correct relationship 
      # between the two groups
      #divX.Yny gives the new sample that have the correct relative number of samples
      
      a1<-round((rel1.1*tot.1)/rel1.tot-(rel1.1*tot.1))
      if(rel0.1>rel0.tot){div0.1ny<-sample(div0.1,size=a1)}else {div0.1ny<-div0.1}
      
      a1<-round((rel0.1*tot.1)/rel0.tot-(rel0.1*tot.1))
      if(rel1.1>rel1.tot){div1.1ny<-sample(div1.1,size=a1)}else {div1.1ny<-div1.1}
      
      
      a1<-round((rel1.2*tot.2)/rel1.tot-(rel1.2*tot.2))
      if(rel0.2>rel0.tot){div0.2ny<-sample(div0.2,size=a1)}else {div0.2ny<-div0.2}
      
      a1<-round((rel0.2*tot.2)/rel0.tot-(rel0.2*tot.2))
      if(rel1.2>rel1.tot){div1.2ny<-sample(div1.2,size=a1)}else {div1.2ny<-div1.2}
      
      
      a1<-round((rel1.3*tot.3)/rel1.tot-(rel1.3*tot.3))
      if(rel0.3>rel0.tot){div0.3ny<-sample(div0.3,size=a1)}else {div0.3ny<-div0.3}
      
      a1<-round((rel0.3*tot.3)/rel0.tot-(rel0.3*tot.3))
      if(rel1.3>rel1.tot){div1.3ny<-sample(div1.3,size=a1)}else {div1.3ny<-div1.3}
      
      #now we have the sample id for each sample that we want to use to estimate the wa species scores.
      
      
      
      ###########################
      #calculate the observed wa S P E C I E S   S C O R E S  for time period 0 (d) and 1 (f)
      ###########################
      
      #only interested in making wa for the species with >5 (= at least 6) occurrences
      allsp.com<-sp.df1[,occur>1]
      
      #take out the data selected above from the total data set
      #makes the two data set where the wa species scores are found
      
      #species data for the pruned data set
      sp.d<-allsp.com[c(div0.1ny,div0.2ny,div0.3ny),]
      sp.f<-allsp.com[c(div1.1ny,div1.2ny,div1.3ny),]
      
      #wa sample scores for the pruned data set
      waN.d<-waN[c(div0.1ny,div0.2ny,div0.3ny)]
      waN.f<-waN[c(div1.1ny,div1.2ny,div1.3ny)]
      
      #estimate the wa species score for the old data set for the species in focus (m1)
      m1<-(1:length(sp1))[sp1==m]
      wasp.d<-sum(waN.d*sp.d[,m1])/sum(sp.d[,m1])
      
      
      #estimate the wa species score for the new data set
      wasp.f<-sum(waN.f*sp.f[,m1])/sum(sp.f[,m1])
      
      #observed difference between the sample scores in the pruned data set
      obs.diff<-wasp.f-wasp.d
      obs.diff.rel <- (wasp.f-wasp.d)/sd(waN)
      
      
      
      ##################
      #permutation test#
      ##################
      
      #puts the samples from the same group along the ellenberg gradient in the same basket
      gr1<-c(div0.1ny,div1.1ny)
      gr2<-c(div0.2ny,div1.2ny)
      gr3<-c(div0.3ny,div1.3ny)
      
      lgr0.1<-length(div0.1ny)
      lgr1.1<-length(div1.1ny)
      lgr0.2<-length(div0.2ny)
      lgr1.2<-length(div1.2ny)
      lgr0.3<-length(div0.3ny)
      lgr1.3<-length(div1.3ny)
      
      #and randomise these groups
      ss1<-sample(gr1)
      ss2<-sample(gr2)
      ss3<-sample(gr3)
      
      #divide each of the three groups in two time periods with teh same sample size as in the original 
      # pruned data set
      ss0.1<-ss1[1:lgr0.1]
      ss1.1<-ss1[(lgr0.1+1):length(gr1)]
      ss0.2<-ss2[1:lgr0.2]
      ss1.2<-ss2[(lgr0.2+1):length(gr2)]
      ss0.3<-ss3[1:lgr0.3]
      ss1.3<-ss3[(lgr0.3+1):length(gr3)]
      
      #and again combine the three groups so that they are now two data sets from the two time periods
      sp.d<-allsp.com[c(ss0.1,ss0.2,ss0.3),]
      sp.f<-allsp.com[c(ss1.1,ss1.2,ss1.3),]
      
      wasN.d<-waN[c(ss0.1,ss0.2,ss0.3)]
      wasN.f<-waN[c(ss1.1,ss1.2,ss1.3)]
      
      #estimate the wa species score for the randomised old data set
      wasp.ds<-sum(wasN.d*sp.d[,m1])/sum(sp.d[,m1])
      
      #estimate the wa species score for the randomised new data set
      wasp.fs<-sum(wasN.f*sp.f[,m1])/sum(sp.f[,m1])
      
      #difference between the two randomised (pruned) data sets
      perm.diff<-wasp.fs-wasp.ds
      perm.diff.rel <- (wasp.fs-wasp.ds)/sd(waN)
      
      #gives a 1 if the absolute value of the randomised difference is larger than (or equal) 
      #	the observed value and a 0 otherwise
      permut2<-abs(obs.diff)<abs(perm.diff)
      permut.rel2 <- abs(obs.diff.rel)<abs(perm.diff.rel)
      
      #collects the value for the different permutations
      #NOTE: the observed value is also permuted, because the pruning result in a different subset is 
      #	taken out each time.
      permut<-c(permut,permut2)
      permut.rel <- c(permut,permut.rel2)
      obs.diff0<-c(obs.diff0,obs.diff)
      obs.diff.rel0 <- c(obs.diff.rel0, obs.diff.rel)
      
      #Here ends the permutation test for each species
    }
    
    
    #making a  P - V A L U E   after taking out the permutations that give missing value in case of
    #	one of the time periods did not have enough samples in the randomisations
    permut1<-na.omit(permut)
    pval<-sum(permut1)/length(permut1)
    
    #averaging the difference. Again, the missing values are taken out
    obs.diff9<-mean(na.omit(obs.diff0))
    obs.diff.rel9 <- mean(na.omit(obs.diff.rel0))
    abs.diff.rel9 <- abs(obs.diff.rel9)
    
    ##This decides the output
    res.sp<-c(names(sp.df)[m],round(obs.diff.rel9,4),round(pval,3),sum(sp.df[,m]>0))
    obs.res<-rbind(obs.res,res.sp)
    
    #Here all the species has been tested
  }
  
  obs.res
  
  #Here ends the function
}



##########################################
# Run the function for all Indicator values and for all vegetation types at once (to study overall change in the area):
  #the output table (res1) will consist of 4 columns for each indicator value containing the species, the change in the species' 
  #optimum, the p-value, and the number of species occurrence in plot.


res0<-NULL
res1<-NULL

for(i in 1:dim(lnb)[2]){
  res0<-emp.fun(lnb[,i],sp.df,time0,999)
  res1<-cbind(res1,res0)
}
  

# To study species optimum changes for all vegetation types separately you just need to replace the 
  #input vegetation dataset (sp.df) in the "run-the-function" (lines 281:287) with the dataset of 
  #the vegetation type you want to study.

# If you want to see the weighted averaged site scores then you need to add/combine the "waN" in line 268 
  #together with the "obs.res".
  #Or alternatively: run the script from line 17 to 52 manually and separately for each Indicator value. 
  #To do so, remember to define the column for each indicator value in line 38, i.e. instead of "lnb[!sp.id==m])" 
  #for the first indicator value you write "lnb[!sp.id==m,1])". This is not done in the original script because 
  #the indicator values to be used are defined by "i" in the loop (see run-the-function in lines 281:287).

#endend.