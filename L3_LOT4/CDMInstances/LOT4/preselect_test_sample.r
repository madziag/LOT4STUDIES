#READ.ME
#this script should be run AFTER the preselect script
#it samples the preselect data to create a test set that can be used for debugging
#user can set the percentage of the preselected script that they want


#THIS SCRIPT MUST BE SAVED IN THE CDMINSTANCES/PRESELECTION FOLDER

# default 10% of preselect_person_id will be sampled and used to generate a test set

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
if(!require(data.table)){install.packages("data.table")}
library(rstudioapi)
library(data.table)

#should be CDMInstances/preselect
projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)


#set up test_sample folder
dir.create(paste0(projectFolder,"_sample_test"))

sample_folder<-(paste0(projectFolder,"_sample_test"))

my_perc<-0.1

tables_preselect<-list()
tables_preselect$EVENTS<-list.files(paste0(projectFolder,"/"), pattern="^EVENTS")
tables_preselect$MEDICAL_OBSERVATIONS<-list.files(paste0(projectFolder,"/"), pattern="^MEDICAL_OBSERVATIONS")
tables_preselect$SURVEY_OBSERVATIONS<-list.files(paste0(projectFolder,"/"), pattern="^SURVEY_OBSERVATIONS")
tables_preselect$MEDICINES<-list.files(paste0(projectFolder,"/"), pattern="^MEDICINES")
tables_preselect$VACCINES<-list.files(paste0(projectFolder,"/"), pattern="^VACCINES")
tables_preselect$SURVEY_ID<-list.files(paste0(projectFolder,"/"), pattern="^SURVEY_ID")
tables_preselect$EUROCAT<-list.files(paste0(projectFolder,"/"), pattern="^EUROCAT")
tables_preselect$PERSONS<-list.files(paste0(projectFolder,"/"), pattern="^PERSONS")

#get persons table
PERSONS<- fread(paste0(projectFolder,"/", tables_preselect$PERSONS[1]))
person_id<-PERSONS$person_id
num_sample<-round((length(person_id)*my_perc),0)
sample_id<-sample(person_id, num_sample)

tables_preselect<-unlist(tables_preselect)

for(i in 1:length(tables_preselect)){
  tablename<-(tables_preselect[i])
  mytable<-fread(paste0(projectFolder,"/",tablename))
  sample_table<-mytable[mytable$person_id%in%sample_id,]
  fwrite(sample_table, paste0(sample_folder, "/", tablename), row.names = F)
}

