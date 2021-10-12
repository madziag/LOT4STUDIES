#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 11/10/2021



# READ.ME
#LOT 4 preselection application onto multiple table subsets (especially MEDICINES)

# set path
path<-"C://Users//Acer//OneDrive//Documents//GitHub//LOT4//"
path_dir<-"C://Users//Acer//OneDrive//Documents//GitHub//LOT4//CDMInstances//LOT4//"
#get tables 
#VJOLA please insert the code here for loading the lists of tables
#Get EVENTS, MO, SO, MEDICINES, VACCINES tables
actual_tables_preselect<-list()
actual_tables_preselect$EVENTS<-list.files(path_dir, pattern="^EVENTS")
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(path_dir, pattern="^MEDICAL_OBSERVATIONS")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(path_dir, pattern="^SURVEY_OBSERVATIONS")
actual_tables_preselect$MEDICINES<-list.files(path_dir, pattern="^MEDICINES")
actual_tables_preselect$VACCINES<-list.files(path_dir, pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(path_dir, pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(path_dir, pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(path_dir, pattern="^PERSONS")

all_actual_tables<-list.files(path_dir)


#get functions
source(paste0(path, "preselection_DAP.r"))


#run personsfilter on PERSONS table (PERSONS USUALLY one table)

  PERSONS<-read.csv(paste0(path_dir, actual_tables_preselect$PERSONS))
  personsfilter_ID<-as.vector((personsfilter(personstable = PERSONS))[[1]])

  
ATCfilter_ID<- list()
for(i in 1:length(actual_tables_preselect$MEDICINES)) {
  MEDS<-read.csv(paste0(path_dir, actual_tables_preselect$MEDICINES[[i]]))
  output<- ATCfilter(medtable = MEDS)
  ATCfilter_ID[[i]]<-unique(output[[1]])
}

ATCfilter_ID_unique<-as.vector(unique(unlist(ATCfilter_ID)))

#combine filters for final preselction IDs

final_ID<-ATCfilter_ID_unique[(ATCfilter_ID_unique%in%personsfilter_ID)==T]

#subset data using presection IDs and write new files
#need to name each new table the same as the old table, then write in the new folder
for(i in 1:length(actual_tables_preselect)){
  for(j in 1:length(actual_tables_preselect[[i]]))
    tablename<-as.character(actual_tables_preselect[[i]][[j]])
    mytable<-read.csv(paste0(path_dir, actual_tables_preselect[[i]][[j]]))
    preselect_table<-mytable[mytable$person_id%in%final_ID,]
    write.csv(preselect_table, paste0(path_dir,"CDMInstances_preselection//", tablename))
}

#only wrote 8 files (one for each section of the actual files list)... It's only keeping the last one in each sublist...