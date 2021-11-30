#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 22/10/2021


# Use CreateSpells to determine overall or overlapping periods 
# Assume that a gap of max 7 days can be ignored - and spells are concatenated.
# We take the most recent spell. If DAPs observe in their data a large loss of spells (loss of observation time within the study period), we will reconsider taking the longest observation period. 

#25/10 Roel: make script into function using data.table
#26/10 Vjola: apply merge_gaps after applying Create_Spells
#27/10 merging last to first is difficult
#28/10 Create Spells takes most recent observation period, so don't need to apply that here


merge_gap<-function(mydata=OBSERVATION_PERIODS, ID="person_id", startdate="op_start_date", enddate="op_end_date", allowed_gap=7){

mydata<- split(mydata, f = mydata[[ID]] )

for (i in 1:length(mydata)){
  mydata[[i]][order(mydata[[i]][[startdate]]),]
  obs<-nrow(mydata[[i]])
  if(obs>1){
    for(j in nrow(mydata[[i]]:1) ){
      while(j>=2){
      my_gap<- (mydata[[i]][j-1][[enddate]])-(mydata[[i]][j][[startdate]])
        if(between(my_gap, 1,allowed_gap)){
          mydata[[i]][j][[startdate]]<-mydata[[i]][j-1][[startdate]]
          mydata[[i]][j-1][[startdate]]<-NA}
          j<-(j-1)}
  }}}
return(mydata)
  }

testdat<-merge_gap(OBSERVATION_PERIODS)


testdat
