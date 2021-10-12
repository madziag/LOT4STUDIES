#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 11/10/2021



# READ.ME
#LOT 4 preselection application onto multiple table subsets (especially MEDICINES)

# set path
path<-"C://Users//Acer//OneDrive//Documents//GitHub//LOT4//"

#get tables 
#VJOLA please insert the code here for loading the lists of tables




#get functions
source(paste0(path, "preselection_DAP.r"))

firstfilter_ID<-vector()
for(i in 1:length(PERSONS{
 output<- firstfilter(personstable = PERSONS[[i]])
 firstfilter_ID<-c(firstfilter_ID, output)
 firstfilter_ID<-unique(firstfilter_ID)
}))
  
  ATCfilter_ID<-vector()
for(i in 1:length(PERSONS{
  output<- ATCfilter(medtable = MEDICINES[[i]])
  ATCfilter_ID<-c(ATCfilter_ID, output)
  ATCfilter_ID<-unique(ATCfilter_ID)
}))
  
  final_ID<-(ATCfilter_ID%in%firstfilter_ID)

for(i in 1:length(tables)){
  #need to name each new table the same as the old table, then write in the new folder
  tables[[i]]<-tables[[i]][tables[[i]]$person_id%in%final_ID]
  write.csv(tables[[i]], paste0(path,"CDMInstances\\CDMInstances_preselect\\"))
}