#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 22/10/2021

# Assume that a gap of max 7 days can be ignored - and spells are concatenated.

# Use CreateSpells to determine overall or overlapping periods 

# We take the most recent spell. If DAPs observe in their data a large loss of spells (loss of observation time within the study period), we will reconsider taking the longest observation period. 

#merge_gap OBSERVATION_PERIOD if 1<gap<7 --> CREATE SPELLS --> take_recent

#25/10 Roel: make script into function using data.table
#26/10 Vjola: apply merge_gaps after applying Create_Spells
#27/10 merging last to first is difficult
#28/10 Create Spells takes most recent observation period, so don't need to apply that here
#28/10- went in circles... big problem catch: order of observations isn;t chronological because of different op_meanings (which are ignored for now (romin 28/10))

#29/10- DECISION- just merging gaps in observations BEFORE Create-Spells- not selecting most recent- doesn't matter going backward or forward 
#BUT ORDER is very important. ADDED TO CREATESPELLS script

#29/10 Also- what we want is gap BETWEEN 1-7 days MOST gaps are negative (overlapping coverage)

# OBSERVATION_PERIODS[OBSERVATION_PERIODS$person_id=="ConCDM_SIM_200421_00005",]
# OBSERVATION_PERIODS_merge[OBSERVATION_PERIODS_merge$person_id=="ConCDM_SIM_200421_00005",]
# OBSERVATION_PERIODS[order(OBSERVATION_PERIODS$person_id, OBSERVATION_PERIODS$op_start_date)]

merge_gap<-function(mydata=OBSERVATION_PERIODS, ID="person_id", startdate="op_start_date", enddate="op_end_date", gap=7){

# group data without "split" to avoid memory useage from data duplication
  
ID_vec<-unique(mydata[[ID]])

for (i in 1:length(ID_vec)){
  j<-1
  
  while(j<nrow(mydata[mydata[[ID]]==ID_vec[i],])){
   obs_gap<-mydata[mydata[[ID]]==ID_vec[i],][j+1][[startdate]]-mydata[mydata[[ID]]==ID_vec[i],][j][[enddate]]
    if(between(obs_gap,1,gap)){
      mydata[mydata[[ID]]==ID_vec[i],][j][[enddate]]<-mydata[mydata[[ID]]==ID_vec[i],][j+1][[enddate]]
      #if row is merged- then add an NA to remove the row in the next step
      mydata[mydata[[ID]]==ID_vec[i],][j+1][[startdate]]<-NA
      
    }
    j<-j+1}
}
#remove merged rows
mydata<-mydata[complete.cases(mydata[[startdate]])==T, ]
  return(mydata)
  }


