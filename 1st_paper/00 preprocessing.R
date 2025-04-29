rm(list = ls())
library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)


####create data.frame from raw files-----------------------------------------------------------------------------------------------
filedir='muddled_models-master/results/magic_carpet/choices/'
df     =do.call(rbind,lapply(dir(filedir,pattern = '_game')   ,function(i) {cbind(subj=as.numeric(substr(i,1,5)),
                                                                                  read.csv(paste(filedir,i,sep="")))}))
colnames(df)
df<-data.frame(subj  =factor(df$subj),
               trl   =df$trial+1,
               rw    =df$reward,
               ch1   =df$choice1,
               ch2   =df$choice2,
               sr1   =df$isymbol_lft,
               sr2   =df$fsymbol_lft,
               trn   =(1-df$common),
               state2=df$final_state,
               rt1   =df$rt1,
               rt2   =df$rt2)

#####add  columns  -----
#stimulus-response mapping
df$key1<-(df$sr1==df$ch1)*1+1 
df$key2<-(df$sr2==df$ch2)*1+1 

#bandit and key repetition (note that for second stage you will need to filter stg2_stat_rep)
df%<>%mutate(#bandit repetition 
             stay1_bnd   =(ch1==lag(df$ch1, n = 1,default = 0))*1, 
             stay2_bnd   =(ch2==lag(df$ch2, n = 1,default = 0))*1,
             
             #response-key repetition 
             stay1_key   =(key1==lag(df$key1, n = 1,default = 0))*1,
             stay2_key   =(key2==lag(df$key2, n = 1,default = 0))*1,
             stay_key2to1=(key1==lag(df$key2, n = 1,default = 0))*1,
             
             #reward and transition at n-1 
             
             rw_pv        =shift(df$rw, n=1, fill=1, type=c("lag"), give.names=FALSE),
             trn_pv       =shift(df$trn, n=1, fill=1, type=c("lag"), give.names=FALSE),
             
             #s-r mapping repetition 
             pair_rep2    =factor(df$sr2==lag(df$sr2, n = 1,default = 0),levels=c(T,F),labels=c('repeat','switch')),
             
             #second-stage state repetition
             stg2_stat_rep=(df$state==lag(df$state, n = 1,default = 0)))

#### add abort col for quick rts and first trials
df%<>%mutate(abort=(rt1<0.2 | rt2<0.2 | trl==1))

####save
save(df,file="df.Rdata")



