#Author: Vjola Hoxhaj Drs.
#email: v.hoxhaj@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 06/07/2021

###############################################################################################
source(paste0(pre_dir, "create_conceptsets.R")) #for diagnoses
###############################################################################################
source(paste0(pre_dir,"info.R"))
########################################################
#Create output folders
########################################################
if (subpopulations_present=="No"){
  #output folder for PREGNANCY report in g_output
  if ("PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    dir.create(paste(preg_dir,"Masked", sep=""))
    preg_less<-paste(preg_dir, "Masked/", sep="")
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    dir.create(paste(preg_dir,"Masked", sep=""))
    preg_less<-paste(preg_dir, "Masked/", sep="")
  }
  
  #output diagnoses_study_population in g_intermediate/populations/pregnancy
  if ("PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
  }
  
  #PREGNANCY_tmp/PREGNANCY_EV folder where all intermediary files are saved
  if ("PREGNANCY_EV" %in% list.files(tmp)){
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_EVENTS folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_EV", sep=""))
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
  }
  
  #PREGNANCY_M_tmp/PREGNANCY_M folder where all intermediary files are saved
  if ("PREGNANCY_M" %in% list.files(tmp)){
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the MO folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_M", sep=""))
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
  }
  
  #SO_tmp/SO folder where all intermediary files are saved
  if ("PREGNANCY_S" %in% list.files(tmp)){
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SO folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_S", sep=""))
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
  }
  
  #SI_tmp/SI folder where all intermediary files are saved
  if ("PREGNANCY_SI" %in% list.files(tmp)){
    preg_si_tmp<-paste(tmp, "PREGNANCY_SI/", sep="")
    do.call(file.remove, list(list.files(preg_si_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SI folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_SI", sep=""))
    preg_si_tmp<-paste(tmp, "PREGNANCY_SI/", sep="")
  }
  
} else {
  #output folder for PREGNANCY report in g_output
  if ("PREGNANCY" %in% list.files(output_dir)){
    unlink(paste0(output_dir,"PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i],"/Masked"))
    }
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(output_dir, "PREGNANCY", sep=""))
    preg_dir<-paste(output_dir, "PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i]))
    }
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_dir, subpopulations_names[i],"/Masked"))
    }
  }
  
  #output diagnoses_study_population in g_intermediate/populations/diagnoses
  if ("PREGNANCY" %in% list.files(populations_dir)){
    unlink(paste0(populations_dir,"PREGNANCY"), recursive = T)#delete folder
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/",sep="")
    
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_pop, subpopulations_names[i]))
    }
  } else {
    #Create the PREGNANCY folder in the output dir
    dir.create(paste(populations_dir, "PREGNANCY", sep=""))
    preg_pop<-paste(populations_dir, "PREGNANCY/", sep="")
    for (i in 1:length(subpopulations_names)){
      dir.create(paste0(preg_pop, subpopulations_names[i]))
    }
  }
  
  #PREGNANCY_EV_tmp/PREGNANCY_EV folder where all intermediary files are saved
  if ("PREGNANCY_EV" %in% list.files(tmp)){
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_EV folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_EV", sep=""))
    preg_ev_tmp<-paste(tmp, "PREGNANCY_EV/", sep="")
  }
  
  #PREGNANCY_M_tmp/PREGNANCY_M folder where all intermediary files are saved
  if ("PREGNANCY_M" %in% list.files(tmp)){
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_M folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_M", sep=""))
    preg_m_tmp<-paste(tmp, "PREGNANCY_M/", sep="")
  }
  
  #PREGNANCY_S_tmp/PREGNANCY_S folder where all intermediary files are saved
  if ("PREGNANCY_S" %in% list.files(tmp)){
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the PREGNANCY_S folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_S", sep=""))
    preg_s_tmp<-paste(tmp, "PREGNANCY_S/", sep="")
  }
  
  
  #SI_tmp/SI folder where all intermediary files are saved
  if ("PREGNANCY_SI" %in% list.files(tmp)){
    preg_si_tmp<-paste(tmp, "PREGNANCY_SI/", sep="")
    do.call(file.remove, list(list.files(preg_si_tmp, full.names = T)))#clean folder completely
  }else{
    #Create the SI folder in the output dir
    dir.create(paste(tmp, "PREGNANCY_SI", sep=""))
    preg_si_tmp<-paste(tmp, "PREGNANCY_SI/", sep="")
  }
}

####################################################################################################

if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS), length(actual_tables$SURVEY_ID))>0){
####################################################################################################
#Main script
####################################################################################################
if (subpopulations_present=="Yes"){
  for (s in 1: length(subpopulations_names)){
    study_sub_population<-study_population_dir[grepl(study_population_dir, pattern=paste0(subpopulations_names[s],"_study_population"))]
    study_sub_population<-study_sub_population[grepl(study_sub_population, pattern=paste0("^", subpopulations_names[s]))]
    study_population<-readRDS(paste0(g_intermediate, "populations/", study_sub_population))[,c("person_id","sex_at_instance_creation","birth_date","op_end_date","start_follow_up","age_start_follow_up")]
    study_population<-study_population[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg]
    nr_std<-study_population[,.N]
    #MEANINGS TO BE EXCLUDED
    meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS" & other==subpopulations_names[s],values], pattern = " "))
    meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
    meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS" & other==subpopulations_names[s],values], pattern = " "))
    meanings_exclude_si<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_ID" & other==subpopulations_names[s],values], pattern = " "))
    
    source(paste0(pre_dir, "PREGNANCY_L3_pre_script.R"))
    
    write.csv(flowchart_preg, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_flowchart_pregnancy.csv"), row.names = F)
    write.csv(description_preg, paste0(preg_dir,subpopulations_names[s], "/", subpopulations_names[s],"_description_pregnancy.csv"), row.names = F)

    if(length(actual_tables$EVENTS)>0){
      suppressWarnings(flowchart_preg[, EVENTS:= as.character(EVENTS)][as.numeric(EVENTS) > 0 & as.numeric(EVENTS) < 5, EVENTS := "<5"])
    }
    if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
      suppressWarnings(flowchart_preg[, MEDICAL_OBSERVATIONS:= as.character(MEDICAL_OBSERVATIONS)][as.numeric(MEDICAL_OBSERVATIONS) > 0 & as.numeric(MEDICAL_OBSERVATIONS) < 5, MEDICAL_OBSERVATIONS := "<5"])
    }
    if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
      suppressWarnings(flowchart_preg[, SURVEY_OBSERVATIONS:= as.character(SURVEY_OBSERVATIONS)][as.numeric(SURVEY_OBSERVATIONS) > 0 & as.numeric(SURVEY_OBSERVATIONS) < 5, SURVEY_OBSERVATIONS := "<5"])
    }
    if(length(actual_tables$SURVEY_ID)>0){
      suppressWarnings(flowchart_preg[, SURVEY_ID:= as.character(SURVEY_ID)][as.numeric(SURVEY_ID) > 0 & as.numeric(SURVEY_ID) < 5, SURVEY_ID := "<5"])
    }
    
    write.csv(flowchart_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_flowchart_pregnancy_masked.csv"), row.names = F)
    
    if(length(actual_tables$EVENTS)>0){
    if(description_preg[4, 2]<5 & description_preg[4, 2]>0) {description_preg[4, 2]<-"<5"}
    }
    if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
    if(description_preg[4, 3]<5 & description_preg[4, 3]>0) {description_preg[4, 3]<-"<5"} 
    }
    if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
    if(description_preg[4, 4]<5 & description_preg[4, 4]>0) {description_preg[4, 4]<-"<5"} 
    }
    if(length(actual_tables$SURVEY_ID)>0){
      if(description_preg[4, 5]<5 & description_preg[4, 5]>0) {description_preg[4, 5]<-"<5"} 
    }
    write.csv(description_preg, paste0(preg_dir,subpopulations_names[s], "/","Masked/", subpopulations_names[s],"_description_pregnancy_masked.csv"), row.names = F)
    
    
    do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))
    do.call(file.remove, list(list.files(preg_si_tmp, full.names = T)))
    
  }
} else {
  study_population_dir<-study_population_dir[grepl(study_population_dir, pattern="ALL_study_population", fixed=T)]
  study_population<-readRDS(paste0(g_intermediate, "populations/", study_population_dir))[,c("person_id","sex_at_instance_creation","birth_date","op_end_date","start_follow_up","age_start_follow_up")]
  study_population<-study_population[sex_at_instance_creation=="F" & age_start_follow_up>=min_age_preg & age_start_follow_up<=max_age_preg]
   nr_std<-study_population[,.N]
  #MEANINGS TO BE EXCLUDED
  meanings_exclude_events<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="EVENTS",values], pattern = " "))
  meanings_exclude_mo<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="MEDICAL_OBSERVATIONS", values], pattern = " "))
  meanings_exclude_so<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_OBSERVATIONS", values], pattern = " "))
  meanings_exclude_si<-unlist(str_split(METADATA_subp[type_of_metadata=="exclude_meaning" & tablename=="SURVEY_ID", values], pattern = " "))
  
  source(paste0(pre_dir, "PREGNANCY_L3_pre_script.R"))
  
   #######################################################
  write.csv(flowchart_preg, paste0(preg_dir, "flowchart_pregnancy.csv"), row.names = F)
  write.csv(description_preg, paste0(preg_dir,"description_pregnancy.csv"), row.names = F)
  ########################################################
  #create masked files
  #######################################################
  if(length(actual_tables$EVENTS)>0){
    suppressWarnings(flowchart_preg[, EVENTS:= as.character(EVENTS)][as.numeric(EVENTS) > 0 & as.numeric(EVENTS) < 5, EVENTS := "<5"])
  }
  if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
    suppressWarnings(flowchart_preg[, MEDICAL_OBSERVATIONS:= as.character(MEDICAL_OBSERVATIONS)][as.numeric(MEDICAL_OBSERVATIONS) > 0 & as.numeric(MEDICAL_OBSERVATIONS) < 5, MEDICAL_OBSERVATIONS := "<5"])
  }
  if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
    suppressWarnings(flowchart_preg[, SURVEY_OBSERVATIONS:= as.character(SURVEY_OBSERVATIONS)][as.numeric(SURVEY_OBSERVATIONS) > 0 & as.numeric(SURVEY_OBSERVATIONS) < 5, SURVEY_OBSERVATIONS := "<5"])
  }
  if(length(actual_tables$SURVEY_ID)>0){
    suppressWarnings(flowchart_preg[, SURVEY_ID:= as.character(SURVEY_ID)][as.numeric(SURVEY_ID) > 0 & as.numeric(SURVEY_ID) < 5, SURVEY_ID := "<5"])
      }
  
    write.csv(flowchart_preg, paste0(preg_dir,"Masked/", "flowchart_pregnancy_masked.csv"), row.names = F)
  
    if(length(actual_tables$EVENTS)>0){
  if(description_preg[4, 2]<5 & description_preg[4, 2]>0) {description_preg[4, 2]<-"<5"}
    }
    if(length(actual_tables$MEDICAL_OBSERVATIONS)>0){
  if(description_preg[4, 3]<5 & description_preg[4, 3]>0) {description_preg[4, 3]<-"<5"} 
    }
    if(length(actual_tables$SURVEY_OBSERVATIONS)>0){
  if(description_preg[4, 4]<5 & description_preg[4, 4]>0) {description_preg[4, 4]<-"<5"} 
    }
    if(length(actual_tables$SURVEY_ID)>0){
  if(description_preg[4, 5]<5 & description_preg[4, 5]>0) {description_preg[4, 5]<-"<5"}
    }
  write.csv(description_preg, paste0(preg_dir,"Masked/", "description_pregnancy_masked.csv"), row.names = F)
  

  do.call(file.remove, list(list.files(preg_ev_tmp, full.names = T)))
  do.call(file.remove, list(list.files(preg_m_tmp, full.names = T)))
  do.call(file.remove, list(list.files(preg_s_tmp, full.names = T)))
  do.call(file.remove, list(list.files(preg_si_tmp, full.names = T)))
  
}
}

#Delete folders events, so, mo from tmp
unlink(paste0(tmp,"PREGNANCY_EV"), recursive = T)
unlink(paste0(tmp,"PREGNANCY_M"), recursive = T)
unlink(paste0(tmp,"PREGNANCY_S"), recursive = T)
unlink(paste0(tmp,"PREGNANCY_SI"), recursive = T)

keep_environment<-c("StudyName", "data_access_provider_name", "data_source_name", "subpopulations_names", "subpopulations_present", "subpopulations","SUBP",
                    "Age_max","Age_min", "min_age_preg","max_age_preg", 
                    "start_study_date","end_study_date", "lookback_period", "intv","recommended_end_date", "date_creation",
                    "METADATA_subp", "actual_tables", "tmp", "s", "meanings_birth_registry",
                    "diagnoses","pregnancies","diagnoses_pregnancy_med_vacc","diagnoses_pregnancy_med","diagnoses_pregnancy_vacc","pregnancy_only_med_vacc","pregnancy_only_med","pregnancy_only_vacc",
                    "dir_base", "populations_dir", "output_dir", "pre_dir", "study_population_dir", "g_intermediate", "path_dir","projectFolder",
                    "med_dir","medicines_tmp","medicines_pop", "Rmd_MEDICINES",
                    "vacc_dir", "vaccines_tmp", "vaccines_pop","Rmd_VACCINES",
                    "diag_dir", "events_tmp","mo_tmp","so_tmp", "diag_pop","Rmd_DIAGNOSES",
                    "preg_dir","preg_ev_tmp","preg_m_tmp","preg_s_tmp", "preg_pop","Rmd_PREGNANCY")








list_rm<-ls()[ls() %!in% keep_environment]
rm(list = list_rm)
rm(list_rm)
`%!in%` = Negate(`%in%`)


