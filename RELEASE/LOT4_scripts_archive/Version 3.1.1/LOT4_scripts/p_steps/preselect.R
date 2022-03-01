#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 26/1/2022

#this script reduces data size for subsequent scripts by removing those who are inelligible due to age and gender
#MEDICINES is filtered on  ATC of interest

if(!require(data.table)){install.packages("data.table")}
library(data.table)

#set up folders

# Deletes CDMInstances_preselect folder if it exists
if ("CDMInstances_preselect" %in% list.files(dir_base)){unlink(paste0(dir_base,"/CDMInstances_preselect"), recursive = TRUE)}
# Creates CDMInstances_preselect folder and sets paths
dir.create(paste0(dir_base,"/CDMInstances_preselect"))
preselect_folder<-(paste0(dir_base,"/CDMInstances_preselect/"))
data_folder<-(paste0(dir_base,"/CDMInstances/LOT4"))

# READ.ME
#LOT 4 preselection application onto multiple table subsets (especially MEDICINES)

#get tables 
#Get EVENTS, MO, SO, MEDICINES, VACCINES tables
actual_tables_preselect<-list()
actual_tables_preselect$EVENTS<-list.files(paste0(data_folder,"/"), pattern="^EVENTS")
actual_tables_preselect$MEDICAL_OBSERVATIONS<-list.files(paste0(data_folder,"/"), pattern="^MEDICAL_OBSERVATIONS")
actual_tables_preselect$SURVEY_OBSERVATIONS<-list.files(paste0(data_folder,"/"), pattern="^SURVEY_OBSERVATIONS")
actual_tables_preselect$MEDICINES<-list.files(paste0(data_folder,"/"), pattern="^MEDICINES")
actual_tables_preselect$VACCINES<-list.files(paste0(data_folder,"/"), pattern="^VACCINES")
actual_tables_preselect$SURVEY_ID<-list.files(paste0(data_folder,"/"), pattern="^SURVEY_ID")
actual_tables_preselect$EUROCAT<-list.files(paste0(data_folder,"/"), pattern="^EUROCAT")
actual_tables_preselect$PERSONS<-list.files(paste0(data_folder,"/"), pattern="^PERSONS")

all_actual_tables<-list.files(paste0(data_folder,"/"), pattern = "\\.csv$")

##############
#get functions
##############

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
#match on the first 5 digits in the code


#establish ATC library


ATCfilter<-function(medtable=MEDICINES, 
                    ID="person_id", 
                    ATC="medicinal_product_atc_code", 
                    Lot4ATC= 
                      c("B03BB","B03AE",
                        "C02AC","C07AA","C07AB","C07AG",
                        "D05AC","D05AD","D05BA","D05BB","D05BX","D07AA","D07AB","D07AC","D07AD",
                        "D10AA","D10AB","D10AE","D10AF","D10BA","D11AH",
                        "G02BA","G02BB","G03AA","G03AB","G03AC","G03DB",
                        "H02AA","H02AB",
                        "J01AA","J01FA",
                        "L04AA","L04AB","L04AC","L04AD","L04AX",
                        "N02CX","N03AA","N03AB","N03AD","N03AE","N03AF", "N03AG","N03AX",
                        "N05BA","N05AH","N05AN","N06AA","N07CA",
                        "S01AA")){
  medATC5<-substr((medtable[,get(ATC)]), 1,5)
  newMED<-medtable[(medATC5%in%Lot4ATC),]
  
  medID<-unique(newMED[[ID]])
  flowchart_ATC<-c(nrow(medtable), nrow(newMED))
  ATC_filter_output<-list(medID, flowchart_ATC, newMED)
  return(ATC_filter_output)
}          


##############################################################
#run personsfilter on PERSONS table (PERSONS USUALLY* one table)

if(length(actual_tables_preselect$PERSONS)>1){
  PERSONS<-lapply(paste0(data_folder,"/",actual_tables_preselect$PERSONS), fread)
  PERSONS<-do.call(rbind,PERSONS)
  PERSONS<-as.data.table(PERSONS)

}else {
  PERSONS<-fread(paste0(data_folder,"/",actual_tables_preselect$PERSONS))
  
}


personsfilter_output<-as.vector((personsfilter(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female="F", dob= "year_of_birth", dobmin=1954, dobmax=2008)))
personsfilter_ID<-personsfilter_output[[1]]

# #write preselected files into new folder
# 
person_preselect_tables<-actual_tables_preselect[names(actual_tables_preselect) %in% "MEDICINES" == FALSE]
tables_df<-as.data.frame(unlist(person_preselect_tables))
colnames(tables_df)<-"CDMtableName"
tables_vec_all<-unique(as.vector(tables_df$CDMtableName))

#subset data using preselection IDs and write new files
#need to name each new table the same as the old table, then write in the new folder
for(i in 1:length(tables_vec_all)){
  tablename<-(tables_vec_all[i])
  mytable<-fread(paste0(data_folder,"/",tablename))
  preselect_table<-mytable[mytable$person_id%in%personsfilter_ID,]
  fwrite(preselect_table, paste0(preselect_folder, tablename), row.names = F)
}

actual_tables_preselect_changed<-list.files(preselect_folder, pattern = "\\.csv$")
to_be_copied<-setdiff(all_actual_tables,actual_tables_preselect_changed)

for(fil_ind in 1:length(to_be_copied)){
  tablename<-(to_be_copied[fil_ind])
  mytable<-fread(paste0(data_folder,"/",tablename))
  fwrite(mytable, paste0(preselect_folder, tablename), row.names = F)
}

##########################################################################
  
#filter MEDICINES tables to include only ATCs of interest and write new files into preselect folder

##########################################################################

for(i in 1:length(actual_tables_preselect$MEDICINES)) {
  MEDS<-fread(paste0(data_folder,"/",actual_tables_preselect$MEDICINES[[i]]))
  MEDS_select<-MEDS[MEDS$person_id%in%personsfilter_ID,]
  output<- ATCfilter(medtable = MEDS_select)
  fwrite(output[[3]], paste0(preselect_folder, actual_tables_preselect$MEDICINES[[i]]), row.names = F)
}



