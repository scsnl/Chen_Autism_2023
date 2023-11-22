##BPSO Stats (the Chick and Gary method) - Lure Level	
source("R_functions.R") # loading packages for analysis

require(tidyverse)
require(effsize)

data_match<-as.data.frame(read.csv("ASD_BPSO_finalsample.csv",header=T,na.strings = c("","NA")))
data_match$PID<-as.factor(data_match$PID)
data_match$group<-as.factor(data_match$group)
data_match$Registration.and.Demographics.gender<-as.factor(data_match$Registration.and.Demographics.gender)

##create demographic table for ASD and TD group
#exclude the ADI part
data_match1<-data_match[,-grep("ADI",colnames(data_match))] 
#exclude the ADOS part
data_match1<-data_match1[,-grep("ADOS",colnames(data_match1))] 

mk_ttable(data_match1,3,c("TD","ASD"),4,"BPSO_ASD_TD_Match_sample_t-test.csv",2,3,equalvar=T)
chisq.test(data_match1$group,data_match1$Registration.and.Demographics.gender, correct=FALSE)
##descriptives for two groups, create two separate files
mk_destable(data_match,3,4,"BPSO",2)

####create measures for the BPSO task
##get overall performance
##overall acc (un-corrected; including NR trials)
data_match$OverallAcc<-((data_match$Old.Target.Raw.Count+data_match$Similar.Lure.Raw.Count+data_match$New.Foil.Raw.Count)/60)
##acc based on types (corrected)
data_match$OldAcc<-data_match$Old.Target.Raw.Count/data_match$Response.totals.Target
data_match$Old_lure_err<-data_match$Similar.Target.Raw.Count/data_match$Response.totals.Target
data_match$Old_new_err<-data_match$New.Target.Raw.Count/data_match$Response.totals.Target
data_match$LureAcc<-data_match$Similar.Lure.Raw.Count/data_match$Response.totals.Lure
data_match$Lure_old_err<-data_match$Old.Lure.Raw.Count/data_match$Response.totals.Lure
data_match$Lure_new_err<-data_match$New.Lure.Raw.Count/data_match$Response.totals.Lure
data_match$NewAcc<-data_match$New.Foil.Raw.Count/data_match$Response.totals.Foil
data_match$New_old_err<-data_match$Old.Foil.Raw.Count/data_match$Response.totals.Foil
data_match$New_lure_err<-data_match$Similar.Foil.Raw.Count/data_match$Response.totals.Foil

##recognition ability (hit-false alarm: old and new)
data_match$rec_acc<-data_match$OldAcc-data_match$New_old_err
##Pattern separation score
data_match$ps_score<-data_match$LureAcc-data_match$New_lure_err
##traditional recognition memory
data_match$dprime_old_new<-(qnorm(ifelse(data_match$Old.Target.Corrected==0,data_match$Old.Target.Corrected+0.001,data_match$Old.Target.Corrected-0.001))-qnorm(data_match$Old.Foil.Corrected+0.001))
data_match$dprime_old_lure<-(qnorm(ifelse(data_match$Old.Target.Corrected==0,data_match$Old.Target.Corrected+0.001,data_match$Old.Target.Corrected-0.001))-qnorm(data_match$Old.Lure.Corrected+0.001))
##pattern separation measure
data_match$dprime_lure_new<-(qnorm(data_match$Similar.Lure.Corrected-0.001)-qnorm(data_match$Similar.Foil.Corrected+0.001))

##pattern separation measure (1 - p(old)) at repitition, L1-L5, new for a tuning curve
data_match$Sep_Rep<-100-data_match$Old.Target.Corrected*100
data_match$Sep_L1<-100-data_match$L1O/(data_match$L1O+data_match$L1S+data_match$L1N)*100
data_match$Sep_L2<-100-data_match$L2O/(data_match$L2O+data_match$L2S+data_match$L2N)*100
data_match$Sep_L3<-100-data_match$L3O/(data_match$L3O+data_match$L3S+data_match$L3N)*100
data_match$Sep_L4<-100-data_match$L4O/(data_match$L4O+data_match$L4S+data_match$L4N)*100
data_match$Sep_L5<-100-data_match$L5O/(data_match$L5O+data_match$L5S+data_match$L5N)*100
data_match$Sep_New<-100-data_match$Old.Foil.Corrected*100

##Create table for BPSO performance
#exclude the ADI part
data_match1<-data_match[,-grep("ADI",colnames(data_match))] 
#exclude the ADOS part
data_match1<-data_match1[,-grep("ADOS",colnames(data_match1))] 
# data_match1<-data_match1[,-grep("SRS",colnames(data_match1))] 
mk_ttable(data_match1,3,c("TD","ASD"),8,"BPSO_ASD_TD_Task_Performance_t-test.csv",2,3,equalvar=T)

# table 2 cohen's d
c1=data_match1$OverallAcc[data_match1$group=="ASD"]
c2=data_match1$OverallAcc[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$OldAcc[data_match1$group=="ASD"]
c2=data_match1$OldAcc[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$LureAcc[data_match1$group=="ASD"]
c2=data_match1$LureAcc[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$NewAcc[data_match1$group=="ASD"]
c2=data_match1$NewAcc[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$rec_acc[data_match1$group=="ASD"]
c2=data_match1$rec_acc[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$ps_score[data_match1$group=="ASD"]
c2=data_match1$ps_score[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$Old_lure_err[data_match1$group=="ASD"]
c2=data_match1$Old_lure_err[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$Old_new_err[data_match1$group=="ASD"]
c2=data_match1$Old_new_err[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$Lure_old_err[data_match1$group=="ASD"]
c2=data_match1$Lure_old_err[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$Lure_new_err[data_match1$group=="ASD"]
c2=data_match1$Lure_new_err[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$New_old_err[data_match1$group=="ASD"]
c2=data_match1$New_old_err[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$New_lure_err[data_match1$group=="ASD"]
c2=data_match1$New_lure_err[data_match1$group=="TD"]
cohen.d(c1,c2)

c1=data_match1$Sep_Rep[data_match1$group=="ASD"]
c2=data_match1$Sep_Rep[data_match1$group=="TD"]
cohen.d(c1,c2)
c1=data_match1$Sep_L1[data_match1$group=="ASD"]
c2=data_match1$Sep_L1[data_match1$group=="TD"]
cohen.d(c1,c2)
c1=data_match1$Sep_L2[data_match1$group=="ASD"]
c2=data_match1$Sep_L2[data_match1$group=="TD"]
cohen.d(c1,c2)
c1=data_match1$Sep_L3[data_match1$group=="ASD"]
c2=data_match1$Sep_L3[data_match1$group=="TD"]
cohen.d(c1,c2)
c1=data_match1$Sep_L4[data_match1$group=="ASD"]
c2=data_match1$Sep_L4[data_match1$group=="TD"]
cohen.d(c1,c2)
c1=data_match1$Sep_L5[data_match1$group=="ASD"]
c2=data_match1$Sep_L5[data_match1$group=="TD"]
cohen.d(c1,c2)
c1=data_match1$Sep_New[data_match1$group=="ASD"]
c2=data_match1$Sep_New[data_match1$group=="TD"]
cohen.d(c1,c2)

##for clustering analysis

##clustering
require(NbClust)

data_match$PID<-as.numeric(as.character(data_match$PID))
data_ASD<-with(data_match[data_match$group=="ASD",],cbind(PID,Sep_Rep,Sep_L1,Sep_L2,Sep_L3,Sep_L4,Sep_L5,Sep_New))
clust.ASD<-NbClust(data=data_ASD[,-1],distance="euclidean",min.nc=2,max.nc=8,method="ward.D2",index="alllong")
data_TD<-with(data_match[data_match$group=="TD",],cbind(PID,Sep_Rep,Sep_L1,Sep_L2,Sep_L3,Sep_L4,Sep_L5,Sep_New))
clust.TD<-NbClust(data=data_TD[,-1],distance="euclidean",min.nc=2,max.nc=8,method="ward.D2",index="alllong")

clust.ASD_nc<-clust.ASD[["Best.nc"]];
write.csv(clust.ASD_nc, "clust_ASD_nc.csv")
clust.TD_nc<-clust.TD[["Best.nc"]];
write.csv(clust.TD_nc, "clust_TD_nc.csv")

data_ASD<-as.data.frame(data_ASD)
data_ASD$hclust<-as.factor(clust.ASD$Best.partition)
data_TD<-as.data.frame(data_TD)
data_TD$hclust<-as.factor(clust.TD$Best.partition)
data_short<-rbind(data_ASD,data_TD)
data_match<-merge(data_match,data_short[,c("PID","hclust")],by="PID")
data_match$PID<-as.factor(data_match$PID) 

data_match<-data_match[,c(1:3,ncol(data_match),4:(ncol(data_match)-1))]
data_match$hclust<-as.factor(data_match$hclust)
write.csv(data_match, "data_match_hclust.csv")

###Make tables to compare ASD subgroups and TD
# #exclude the ADI part
data_match1<-data_match[,-grep("ADI",colnames(data_match))] 
# #exclude the ADOS part
data_match1<-data_match1[,-grep("ADOS",colnames(data_match1))] 
mk_ttable(data_match1[((data_match1$group=="ASD" & data_match1$hclust==1) |data_match1$group=="TD"),],
          3,c("ASD","TD"),5,"BPSO_ASD_subgroup1vsTD",2,3,equalvar=T)
mk_ttable(data_match1[((data_match1$group=="ASD" & data_match1$hclust==2) |data_match1$group=="TD"),],
          3,c("ASD","TD"),5,"BPSO_ASD_subgroup2vsTD",2,3,equalvar=T)
mk_ttable(data_match1[((data_match1$group=="ASD" & data_match1$hclust==3) |data_match1$group=="TD"),],
          3,c("ASD","TD"),5,"BPSO_ASD_subgroup3vsTD",2,3,equalvar=T)

## Table 3 cohen's d
c1=data_match$rec_acc[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$rec_acc[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$rec_acc[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$rec_acc[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$rec_acc[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$rec_acc[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$ps_score[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$ps_score[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$ps_score[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$ps_score[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$ps_score[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$ps_score[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Sep_Rep[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Sep_Rep[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_Rep[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Sep_Rep[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_Rep[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Sep_Rep[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Sep_L1[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Sep_L1[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L1[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Sep_L1[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L1[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Sep_L1[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Sep_L2[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Sep_L2[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L2[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Sep_L2[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L2[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Sep_L2[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Sep_L3[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Sep_L3[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L3[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Sep_L3[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L3[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Sep_L3[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Sep_L4[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Sep_L4[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L4[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Sep_L4[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L4[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Sep_L4[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Sep_L5[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Sep_L5[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L5[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Sep_L5[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_L5[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Sep_L5[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Sep_New[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Sep_New[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_New[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Sep_New[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Sep_New[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Sep_New[data_match$group=="TD"]
cohen.d(c1,c2)

## Table S3 cohen's d
c1=data_match$Old_lure_err[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Old_lure_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Old_lure_err[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Old_lure_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Old_lure_err[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Old_lure_err[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Old_new_err[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Old_new_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Old_new_err[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Old_new_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Old_new_err[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Old_new_err[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Lure_old_err[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Lure_old_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Lure_old_err[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Lure_old_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Lure_old_err[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Lure_old_err[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$Lure_new_err[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$Lure_new_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Lure_new_err[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$Lure_new_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$Lure_new_err[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$Lure_new_err[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$New_old_err[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$New_old_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$New_old_err[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$New_old_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$New_old_err[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$New_old_err[data_match$group=="TD"]
cohen.d(c1,c2)

c1=data_match$New_lure_err[(data_match$group=="ASD" & data_match$hclust==1)]
c2=data_match$New_lure_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$New_lure_err[(data_match$group=="ASD" & data_match$hclust==2)]
c2=data_match$New_lure_err[data_match$group=="TD"]
cohen.d(c1,c2)
c1=data_match$New_lure_err[(data_match$group=="ASD" & data_match$hclust==3)]
c2=data_match$New_lure_err[data_match$group=="TD"]
cohen.d(c1,c2)

###Correlation analysis
mk_rtable(data_match[data_match$group=="ASD",],5,2,"BPSO_ASD_match_group")

mk_rtable(data_match1[data_match1$group=="TD",],5,2,"BPSO_TD_match_group")








