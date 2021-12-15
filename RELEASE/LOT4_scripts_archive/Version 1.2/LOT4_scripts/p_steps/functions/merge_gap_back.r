#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 22/10/2021


# Use CreateSpells to determine overall or overlapping periods 
# Assume that a gap of max 7 days can be ignored - and spells are concatenated.
# We take the most recent spell. If DAPs observe in their data a large loss of spells (loss of observation time within the study period), we will reconsider taking the longest observation period. 

#25/10 Roel: make script into function using data.table
#26/10 Vjola: apply merge_gaps after applying Create_Spells

#28/10 Create Spells takes most recent observation period, so don't need to apply that here
# 28/10 - made while loop count down instead of up, so we merge from most recent backwards

merge_gap<-function(mydata=OBSERVATION_PERIODS1, ID="person_id", startdate="entry_spell_category", enddate="exit_spell_category", gap=7){
  
ID_vec<-unique(mydata[[ID]])

for (i in 1:length(ID_vec)){
  if(nrow(mydata[(mydata[[ID]]==ID_vec[i]),]>1)){
    j<-nrow(mydata[mydata[[ID]]==ID_vec[i],])
      while(j>1){
        #starting from last row and working backwards, if gap<7 merge and impute missing for intermediate enddate
      if((mydata[mydata[[ID]]==ID_vec[i],][j-1,][[enddate]]-mydata[mydata[[ID]]==ID_vec[i],][j,][[startdate]])<=gap)
        {mydata[mydata[[ID]]==ID_vec[i],][j,][[startdate]]<-mydata[mydata[[ID]]==ID_vec[i],][j-1,][[startdate]]
          mydata[mydata[[ID]]==ID_vec[i],][j-1,][[enddate]]<-NA}
      j<-j-1}}}
  
  mydata<-mydata[complete.cases(mydata[[enddate]])==T, ]

return(mydata)}

merge_gap<-function(mydata=OBSERVATION_PERIODS, ID="person_id", startdate="op_start_date", enddate="op_end_date", gap=7){
  
  ID_vec<-unique(mydata[[ID]])
  
  for (i in 1:length(ID_vec)){
    j<-nrow(mydata[mydata[[ID]]==ID_vec[i],])
  if(j>1){
    while(j>1){
      
      if((mydata[mydata[[ID]]==ID_vec[i],][j-1][[enddate]]-mydata[mydata[[ID]]==ID_vec[i],][j][[startdate]])<=gap){
        mydata[mydata[[ID]]==ID_vec[i],][j][[startdate]]<-mydata[mydata[[ID]]==ID_vec[i],][j-1][[startdate]]
        mydata[mydata[[ID]]==ID_vec[i],][j-1][[startdate]]<-NA
        
      }
      j<-j-1}}else{break}
  }
  return(mydata)
}

trial_backward1<-merge_gap(mydata=OBSERVATION_PERIODS)




new_obs<- split(OBSERVATION_PERIODS, f = OBSERVATION_PERIODS$person_id )

final_obs<-list()

for (i in 1:length(new_obs)){
  j<-1
  while(j<nrow(new_obs[[i]])){
    
    if((new_obs[[i]][j+1]$op_start_date-new_obs[[i]][j]$op_end_date)<=7){
      new_obs[[i]][j]$op_end_date<-new_obs[[i]][j+1]$op_end_date
      j<-j+1
    }
    else {break}}
  final_obs[[i]]<-new_obs[[i]][1]
}


merge_obs<-do.call(rbind.data.frame, final_obs)

merge_obs$obs_length<-merge_obs$op_end_date-merge_obs$op_start_date

hist(merge_obs$obs_length)


merge_gap(mydata=OBSERVATION_PERIODS1)

library(data.table)
setDT(mydf)[,.SD[which.max(mydates)],keyby=myids]

# take most recent

most_recent<-function(mydata=OBSERVATION_PERIODS1, ID="person_id", startdate="entry_spell_category", enddate="exit_spell_category", gap=7){
  
  ID_vec<-unique(mydata[[ID]])
  
  for (i in 1:length(ID_vec)){
    if(nrow(mydata[(mydata[[ID]]==ID_vec[i]),]>1))
      for(j in 1:nrow(mydata[(mydata[[ID]]==ID_vec[i]),])){
        startmax<-max(mydata[(mydata[[ID]]==ID_vec[i]),])[[startdate]]
      if(mydata[(mydata[[ID]]==ID_vec[i]),][j][[startdate]]<startmax) {mydata[(mydata[[ID]]==ID_vec[i]),][j][[startdate]]<-NA}}}
  mydata<-mydata[complete.cases(mydata[[startdate]])==T, ]}
  
      

most_recent(OBSERVATION_PERIODS1)



