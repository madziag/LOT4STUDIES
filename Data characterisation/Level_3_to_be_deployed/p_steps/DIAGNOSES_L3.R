#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

###############################################################################################
source(paste0(pre_dir, "create_conceptsets.R")) #for diagnoses
###############################################################################################
source(paste0(pre_dir, "DAP_info.R")) 
source(paste0(pre_dir, "info.R")) 
########################################################
#Create output folders
########################################################
#########################
#Diagnoses
#########################
if (subpopulations_present=="No"){
  #output folder for DIAGNOSES report in g_output
  if ("DIAGNOSES" %in% list.files(output_dir)){
   unlink(paste0(output_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    dir.create(paste(diag_dir,"Masked", sep=""))
    diag_less<-paste(diag_dir, "Masked/", sep="")
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    dir.create(paste(diag_dir,"Masked", sep=""))
    diag_less<-paste(diag_dir, "Masked/", sep="")
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("DIAGNOSES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
  }
  
  #EVENTS_tmp/EVENTS folder where all intermediary files are saved
  if ("EVENTS" %in% list.files(tmp)){
    events_tmp<-paste(tmp, "EVENTS/", sep="")
    do.call(file.remove, list(list.files(events_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the EVENTS folder in the output dir
    dir.create(paste(tmp, "EVENTS", sep=""))
    events_tmp<-paste(tmp, "EVENTS/", sep="")
  }
  
  #MO_tmp/MO folder where all intermediary files are saved
  if ("MO" %in% list.files(tmp)){
    mo_tmp<-paste(tmp, "MO/", sep="")
    do.call(file.remove, list(list.files(mo_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "MO", sep=""))
    mo_tmp<-paste(tmp, "MO/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("SO" %in% list.files(tmp)){
    so_tmp<-paste(tmp, "SO/", sep="")
    do.call(file.remove, list(list.files(so_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "SO", sep=""))
    so_tmp<-paste(tmp, "SO/", sep="")
  }
  
} else {
  #output folder for DIAGNOSES report in g_output
  if ("DIAGNOSES" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i],"/Masked"))
    }
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(output_dir, "DIAGNOSES", sep=""))
    diag_dir<-paste(output_dir, "DIAGNOSES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("DIAGNOSES" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"DIAGNOSES"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/",sep="")

    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_pop, subpopulations_names[i]))
    }
  } else {
    #Create the DIAGNOSES folder in the output dir
    dir.create(paste(populations_dir, "DIAGNOSES", sep=""))
    diag_pop<-paste(populations_dir, "DIAGNOSES/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(diag_pop, subpopulations_names[i]))
    }
  }
  
  #EVENTS_tmp/EVENTS folder where all intermediary files are saved
  if ("EVENTS" %in% list.files(tmp)){
    events_tmp<-paste(tmp, "EVENTS/", sep="")
    do.call(file.remove, list(list.files(events_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the EVENTS folder in the output dir
    dir.create(paste(tmp, "EVENTS", sep=""))
    events_tmp<-paste(tmp, "EVENTS/", sep="")
  }
  
  #MO_tmp/MO folder where all intermediary files are saved
  if ("MO" %in% list.files(tmp)){
    mo_tmp<-paste(tmp, "MO/", sep="")
    do.call(file.remove, list(list.files(mo_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "MO", sep=""))
    mo_tmp<-paste(tmp, "MO/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("SO" %in% list.files(tmp)){
    so_tmp<-paste(tmp, "SO/", sep="")
    do.call(file.remove, list(list.files(so_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "SO", sep=""))
    so_tmp<-paste(tmp, "SO/", sep="")
  }
  
}

###############################################################

if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS))>0){
####################################################################################################
#Main script
####################################################################################################
if (subpopulations_present=="Yes"){
for (s in 1: length(subpopulations_names)){
  study_sub_population<-study_population_dir[grepl(study_population_dir, pattern=paste0(subpopulations_names[s],"_study_population"), fixed=T)]
  study_sub_population<-study_sub_population[grepl(study_sub_population, pattern=paste0("^", subpopulations_names[s]))]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_sub_population))[,c("person_id","sex_at_instance_creation","birth_date","op_end_date","start_follow_up","age_start_follow_up")]
  nr_std<-study_population[,.N]
  nr_std_preg<-study_population[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,.N]
  #MEANINGS TO BE EXCLUDED
  meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS" & other==subpopulations_names[s],values], pattern = " "))
  meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
  meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))

  source(paste0(pre_dir, "DIAGNOSES_L3_pre_script.R"))
  
  if(!is.null(tab20)){
  tab20<-data.table(tab20, data_access_provider= data_access_provider_name, data_source=data_source_name)
  }
  if(!is.null(tab21)){
  tab21<-data.table(tab21, data_access_provider= data_access_provider_name, data_source=data_source_name)
  }
  write.csv(flowchart, paste0(diag_dir,subpopulations_names[s], "/", subpopulations_names[s],"_flowchart.csv"), row.names = F)
  write.csv(description, paste0(diag_dir,subpopulations_names[s], "/", subpopulations_names[s],"_description.csv"), row.names = F)
  
  if(!is.null(tab20)){
  write.csv(tab20, paste0(diag_dir,subpopulations_names[s], "/", subpopulations_names[s],"_tab20.csv"), row.names = F)
  }
  if(!is.null(tab21)){
    write.csv(tab21, paste0(diag_dir,subpopulations_names[s], "/", subpopulations_names[s],"_tab21.csv"), row.names = F)
  }
  
  if(length(actual_tables$EVENTS)>0){
    flowchart[, EVENTS:= as.character(EVENTS)][as.numeric(EVENTS) > 0 & as.numeric(EVENTS) < 5, EVENTS := "<5"]
  }
  if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
    flowchart[, MEDICAL_OBSERVATIONS:= as.character(MEDICAL_OBSERVATIONS)][as.numeric(MEDICAL_OBSERVATIONS) > 0 & as.numeric(MEDICAL_OBSERVATIONS) < 5, MEDICAL_OBSERVATIONS := "<5"]
  }
  if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
    flowchart[, SURVEY_OBSERVATIONS:= as.character(SURVEY_OBSERVATIONS)][as.numeric(SURVEY_OBSERVATIONS) > 0 & as.numeric(SURVEY_OBSERVATIONS) < 5, SURVEY_OBSERVATIONS := "<5"]
  }
    write.csv(flowchart, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_flowchart_masked.csv"), row.names = F)
  
  if(length(actual_tables$EVENTS)>0){
  if(description[5, 2]<5 & description[5, 2]>0) {description[5, 2]<-"<5"}
  }
    if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  if(description[5, 3]<5 & description[5, 3]>0) {description[5, 3]<-"<5"} 
    }
    if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
      if(description[5, 4]<5 & description[5, 4]>0) {description[5, 4]<-"<5"} 
    }
      write.csv(description, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_description_masked.csv"), row.names = F)
  
  if(!is.null(tab20)){
    tab20[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
    tab20[, no_empty_code:= as.character(no_empty_code)][as.numeric(no_empty_code) > 0 & as.numeric(no_empty_code) < 5, no_empty_code := "<5"]
    tab20[, percentage_empty_code:= as.character(percentage_empty_code)][no_empty_code == "<5", percentage_empty_code := "N/A"]
    write.csv(tab20, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_tab20_masked.csv"), row.names = F)
    
  }
  
  if(!is.null(tab21)){
    tab21[, count:= as.character(count)][as.numeric(count) > 0 & as.numeric(count) < 5, count := "<5"]
    tab21[, total:= as.character(total)][as.numeric(total) > 0 & as.numeric(total) < 5, total := "<5"]
    write.csv(tab21, paste0(diag_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_tab21_masked.csv"), row.names = F)
  }
  

  do.call(file.remove, list(list.files(events_tmp, full.names = T)))
  do.call(file.remove, list(list.files(mo_tmp, full.names = T)))
  do.call(file.remove, list(list.files(so_tmp, full.names = T)))

  rm(study_population, nr_std, flowchart, description, tab20, tab21)
  }
} else {
  study_population_dir<-study_population_dir[grepl(study_population_dir, pattern="ALL_study_population", fixed=T)]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))[,c("person_id","sex_at_instance_creation","birth_date","op_end_date","start_follow_up","age_start_follow_up")]
  nr_std<-study_population[,.N]
  nr_std_preg<-study_population[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg,.N]
  
  #MEANINGS TO BE EXCLUDED
  meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS",values], pattern = " "))
  meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS", values], pattern = " "))
  meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS", values], pattern = " "))
  
  source(paste0(pre_dir, "DIAGNOSES_L3_pre_script.R"))
  if(!is.null(tab20)){
  tab20<-data.table(tab20, data_access_provider= data_access_provider_name, data_source=data_source_name)
  }
  if(!is.null(tab21)){
  tab21<-data.table(tab21, data_access_provider= data_access_provider_name, data_source=data_source_name)
  }
  #######################################################
  write.csv(flowchart, paste0(diag_dir, "flowchart.csv"), row.names = F)
  write.csv(description, paste0(diag_dir,"description.csv"), row.names = F)
  if(!is.null(tab20)){
    write.csv(tab20, paste0(diag_dir,"tab20.csv"), row.names = F)
  }
  if(!is.null(tab21)){
    write.csv(tab21, paste0(diag_dir,"tab21.csv"), row.names = F)
  }
  ########################################################
  #create masked files
  #######################################################
  if(length(actual_tables$EVENTS)>0){
  flowchart[, EVENTS:= as.character(EVENTS)][as.numeric(EVENTS) > 0 & as.numeric(EVENTS) < 5, EVENTS := "<5"]
  }
  if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
    flowchart[, MEDICAL_OBSERVATIONS:= as.character(MEDICAL_OBSERVATIONS)][as.numeric(MEDICAL_OBSERVATIONS) > 0 & as.numeric(MEDICAL_OBSERVATIONS) < 5, MEDICAL_OBSERVATIONS := "<5"]
  }
  if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
    flowchart[, SURVEY_OBSERVATIONS:= as.character(SURVEY_OBSERVATIONS)][as.numeric(SURVEY_OBSERVATIONS) > 0 & as.numeric(SURVEY_OBSERVATIONS) < 5, SURVEY_OBSERVATIONS := "<5"]
  }
    write.csv(flowchart, paste0(diag_dir,"Masked/", "flowchart_masked.csv"), row.names = F)
    if(length(actual_tables$EVENTS)>0){
  if(description[5, 2]<5 & description[5, 2]>0) {description[5, 2]<-"<5"}
    }
    if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  if(description[5, 3]<5 & description[5, 3]>0) {description[5, 3]<-"<5"} 
    }
    if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
  if(description[5, 4]<5 & description[5, 4]>0) {description[5, 4]<-"<5"} 
    }
  write.csv(description, paste0(diag_dir,"Masked/", "description_masked.csv"), row.names = F)
  
  if(!is.null(tab20)){
  tab20[, no_records:= as.character(no_records)][as.numeric(no_records) > 0 & as.numeric(no_records) < 5, no_records := "<5"]
  tab20[, no_empty_code:= as.character(no_empty_code)][as.numeric(no_empty_code) > 0 & as.numeric(no_empty_code) < 5, no_empty_code := "<5"]
  tab20[, percentage_empty_code:= as.character(percentage_empty_code)][no_empty_code == "<5", percentage_empty_code := "N/A"]
  write.csv(tab20, paste0(diag_dir,"Masked/", "tab20_masked.csv"), row.names = F)
  }
  
  if(!is.null(tab21)){
  tab21[, count:= as.character(count)][as.numeric(count) > 0 & as.numeric(count) < 5, count := "<5"]
  tab21[, total:= as.character(total)][as.numeric(total) > 0 & as.numeric(total) < 5, total := "<5"]
  write.csv(tab21, paste0(diag_dir,"Masked/", "tab21_masked.csv"), row.names = F)
  }
  
  do.call(file.remove, list(list.files(events_tmp, full.names = T)))
  do.call(file.remove, list(list.files(mo_tmp, full.names = T)))
  do.call(file.remove, list(list.files(so_tmp, full.names = T)))
  
  rm(study_population, nr_std, flowchart, description, tab20, tab21)
  
}
}

#Delete folders events, so, mo from tmp
unlink(paste0(tmp,"EVENTS"), recursive = T)
unlink(paste0(tmp,"MO"), recursive = T)
unlink(paste0(tmp,"SO"), recursive = T)


keep_environment<-c("StudyName", "data_access_provider_name", "data_source_name", "subpopulations_names", "subpopulations_present", "subpopulations","SUBP",
                    "Age_max","Age_min", "min_age_preg","max_age_preg", 
                    "start_study_date","end_study_date", "lookback_period", "intv","recommended_end_date", "date_creation",
                    "METADATA_subp", "actual_tables", "tmp", "s", "meanings_birth_registry",
                    "diagnoses","pregnancies","diagnoses_pregnancy_med_vacc","diagnoses_pregnancy_med","diagnoses_pregnancy_vacc","pregnancy_only_med_vacc","pregnancy_only_med","pregnancy_only_vacc",
                     "dir_base", "populations_dir", "output_dir", "pre_dir", "study_population_dir", "g_intermediate", "path_dir","projectFolder",
                    "med_dir","medicines_tmp","medicines_pop", "Rmd_MEDICINES",
                    "vacc_dir", "vaccines_tmp", "vaccines_pop","Rmd_VACCINES",
                    "diag_dir", "events_tmp","mo_tmp","so_tmp", "diag_pop","Rmd_DIAGNOSES", "conditions")


list_rm<-ls()[ls() %!in% keep_environment]
rm(list = list_rm)
rm(list_rm)
`%!in%` = Negate(`%in%`)


