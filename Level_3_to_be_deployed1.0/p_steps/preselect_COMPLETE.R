#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 11/10/2021



# READ.ME
#LOT 4 preselection application onto multiple table subsets (especially MEDICINES)

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

all_actual_tables<-list.files(path_dir, pattern = "\\.csv$")

########################################################################
#get functions

#firstflter function selects females within age range from the persons table and stores the selected IDs to use for subsequent filtering
personsfilter<-function(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female="F", dob= "year_of_birth", dobmin=1954, dobmax=2008) {
  newdata<-personstable[(personstable[,get(sex)]==female),]
  flowchart_gender<-as.numeric(c(nrow(personstable), nrow(newdata)))
  newdata<-newdata[(newdata[,get(dob)]>=dobmin),]
  filtered_data<-newdata[(newdata[,get(dob)]<=dobmax),]
  flowchart_age<-as.numeric(c(flowchart_gender, nrow(filtered_data)))
  flowchart_steps<-c("original", "females only", "1954<=DOB<=2008")
  filter_ID<-(filtered_data[[caseid]])
  flowchart_filter<-as.data.frame(cbind(flowchart_steps, flowchart_age))
  colnames(flowchart_filter)<-c("filter_step","cases_number" )
  persons_filter_output<-list(filter_ID, flowchart_filter, filtered_data)
  return(persons_filter_output)
}



#ATC filter
#match on the first 4 cijfers in the code

#need to 
#define ATC codes vector
#manually extract ATC codes from SAP --> ATClib
#select first 4 cijfers
# select first 4 cijfers with substr()
#use %in% to select rows 

#establish ATC library


ATCfilter<-function(medtable=MEDICINES, ID="person_id", ATC="medicinal_product_atc_code", Lot4ATC= 
                      c( "N03A","N05B", "N05A", "C07A", "N06A", "N07C", "N02C", "C02A", "G03A",
                         "G03D", "G02B", "B03B", "B03A", "D10A", "J01F", "S01A", "J01A",
                         "D07A", "H02A", "D11A", "L04A", "D05A", "D05B")){
  medATC4<-substr((medtable[,get(ATC)]), 1,4)
  newMED<-medtable[(medATC4%in%Lot4ATC),]
  
  medID<-unique(newMED[[ID]])
  flowchart_ATC<-c(nrow(medtable), nrow(newMED))
  ATC_filter_output<-list(medID, flowchart_ATC, newMED)
  return(ATC_filter_output)
}          



#last step, combine

combine_filter<-function(person_filter_ID= filtered_data[[1]], med_filter_data= med_ID[[3]])
{final_med_data<- med_filter_data[(med_filter_data[["person_id"]]%in%person_filter_ID==T),]
final_med_ID<-unique(final_med_data[["person_id"]])
final_flowchart<-c(nrow(med_filter_data), nrow(final_med_data))
final_output<-list(final_med_ID, final_flowchart, final_med_data)
return(final_output)}

##############################################################
#run personsfilter on PERSONS table (PERSONS USUALLY one table)

if(length(actual_tables_preselect$PERSONS)>1){
  PERSONS<-lapply(paste0(path_dir,actual_tables_preselect$PERSONS), fread)
  PERSONS<-do.call(rbind,PERSONS)
  PERSONS<-as.data.table(PERSONS)
}else {
  PERSONS<-fread(paste0(path_dir, actual_tables_preselect$PERSONS))
}


personsfilter_ID<-as.vector((personsfilter(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female="F", dob= "year_of_birth", dobmin=1954, dobmax=2008))[[1]])

  
ATCfilter_ID<- list()
for(i in 1:length(actual_tables_preselect$MEDICINES)) {
  MEDS<-fread(paste0(path_dir, actual_tables_preselect$MEDICINES[[i]]))
  output<- ATCfilter(medtable = MEDS)
  ATCfilter_ID[[i]]<-unique(output[[1]])
}

ATCfilter_ID_unique<-as.vector(unique(unlist(ATCfilter_ID)))

#combine filters for final preselction IDs

final_ID<-ATCfilter_ID_unique[(ATCfilter_ID_unique%in%personsfilter_ID)==T]

#write preselected files into new folder

tables_df<-as.data.frame(unlist(actual_tables_preselect))
colnames(tables_df)<-"CDMtableName"
tables_vec_all<-unique(as.vector(tables_df$CDMtableName))


#subset data using presection IDs and write new files
#need to name each new table the same as the old table, then write in the new folder
for(i in 1:length(tables_vec_all)){
  tablename<-(tables_vec_all[i])
    mytable<-fread(paste0(path_dir,tablename))
    preselect_table<-mytable[mytable$person_id%in%final_ID,]
    write.csv(preselect_table, paste0(path_dir,"CDMInstances_preselect/", tablename))
}

actual_tables_preselect_changed<-list.files(paste0(path_dir,"CDMInstances_preselect/"), pattern = "\\.csv$")
to_be_copied<-setdiff(all_actual_tables,actual_tables_preselect_changed)

for(fil_ind in 1:length(to_be_copied)){
  tablename<-(to_be_copied[fil_ind])
  mytable<-fread(paste0(path_dir,tablename))
  write.csv(mytable, paste0(path_dir,"CDMInstances_preselect/", tablename))
}
