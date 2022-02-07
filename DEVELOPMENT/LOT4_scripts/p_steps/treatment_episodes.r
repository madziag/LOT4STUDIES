# Create treatment episodes script
# R.Pajouheshnia; 17 DEC 2021
# This script does two things:
# 1. it loads valproate or retinoid concept set data sets created by running "to_run_source_pop_counts.R", for separate subpopulations and regions if necessary.
# It then applys createDOT or a fixed duration value to create an estimated end date of treatment for every record
# 2. It creates a new data frame where each row is not a record, but instead a treatment episode.

#INPUTS 
#study_type (Retinoids, Valproates, Both)
#Retinoid.rds or Valproate.rds or both
#p_param\DOT

# what does the data need to look like?
#fetch medicines data from CDMInstances

#g_int/medications/valproate/rds
# Creates treatment episodes directory
invisible(ifelse(!dir.exists(paste0(g_intermediate,"treatment_episodes")), dir.create(paste0(g_intermediate,"treatment_episodes")), FALSE))
#four treatment episode datasets for retinoids (each separate, and one for any retinoid)
# my_retinoid <-list.files(paste0(tmp,"medications/"), pattern="Retinoid")
# my_valproate<-list.files(paste0(tmp,"medications/"), pattern="Valproate")
# 
# if(study_type=="Retinoid") {my_data<-readRDS(paste0(tmp, "medications/",my_retinoid))}
# if(study_type=="Valproate"){my_data<-readRDS(paste0(tmp, "medications/",my_valproate))}
# if(study_type=="Both")     {
#   my_data<-list()
#   my_data[[1]]<-readRDS(paste0(tmp, "medications/",my_retinoid))
#   my_data[[2]]<-readRDS(paste0(tmp, "medications/",my_valproate))
#   my_data<-rbind(my_data[[1]],my_data[[2]])
#   }
# Load medication data (all the above code is done in run_counts_final_each_pop)
## med_files in wrapper script contains the list of Retinoid and/or Valproates that need to be read 
print(med_files)

for (med in 1:length(med_files)){
  my_data <- do.call(rbind,lapply(paste0(medications_pop,"/",med_files[med]), readRDS))
  my_data$assumed_duration<-rep(30, nrow(my_data)) 
  # colnames(my_data)
  # Date should be used instead of dispensing date as some DAP's do not use dispensing date but prescribing date. The variable date is created to account for this
  # my_data$date_dispensing<-as.Date(my_data$date_dispensing, format="%Y%m%d")
  my_data$Date <-as.Date(my_data$Date, format="%Y%m%d")
  # Creates treatment episodes
  my_treat_episode<-compute.treatment.episodes(data= my_data,
                                               ID.colname = "person_id",
                                               event.date.colname = "Date",
                                               event.duration.colname = "assumed_duration",
                                               event.daily.dose.colname = NA,
                                               medication.class.colname = "Code",
                                               carryover.within.obs.window = TRUE,
                                               carry.only.for.same.medication = TRUE,
                                               consider.dosage.change = TRUE,
                                               medication.change.means.new.treatment.episode = TRUE,
                                               dosage.change.means.new.treatment.episode = FALSE,
                                               maximum.permissible.gap = 0,
                                               maximum.permissible.gap.unit = c("days", "weeks", "months", "years", "percent")[1],
                                               maximum.permissible.gap.append.to.episode = FALSE,
                                               followup.window.start = 0,
                                               followup.window.start.unit = c("days", "weeks", "months", "years")[1],
                                               followup.window.duration = 365 * 11,
                                               followup.window.duration.unit = c("days", "weeks", "months", "years")[1],
                                               event.interval.colname = "event.interval",
                                               gap.days.colname = "gap.days",
                                               date.format = "%Y-%m-%d",
                                               parallel.backend = c("none", "multicore", "snow", "snow(SOCK)", "snow(MPI)",
                                                                    "snow(NWS)")[1],
                                               parallel.threads = "auto",
                                               suppress.warnings = FALSE,
                                               return.data.table = FALSE
  ) 
  summary(my_treat_episode)
  hist(my_treat_episode$episode.duration, breaks=100)
  #LOGICAL CHECKS
  #duration is positive
  if(all((my_treat_episode$episode.end-my_treat_episode$episode.start)>0)==FALSE){print("WARNING negative durations detected")}else{print("durations all positive")}
  #person id merged, but no one lost
  original_ids<-unique(my_data$person_id)
  treat_epi_ids<-unique(my_treat_episode$person_id)
  if(all(original_ids%in%treat_epi_ids==T)){print("all person ids from contraception data present in treatment episodes")}else{print("WARNING person id in treatment episodes are not the same as contraception dataset")}
  #HOW IS THERE A DURATION LESS THAN THE SHORTEST ASSUMED DURATION?
  if(all(my_treat_episode$episode.duration>=30)==T){print("OK: minimum treatment episode equal or greater than assumed duration")}else(print("WARNING treatment episodes shorter than assumed duration"))
  #write data
  med_prefix <- gsub(".rds", "", med_files[med])
  saveRDS(my_treat_episode, (paste0(g_intermediate, "treatment_episodes/", med_prefix, "_CMA_treatment_episodes.rds")))
}



### Run separate depending on study

# if (study_type == "Retinoids"){
#   study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
#   study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
#   # Retinoids - subgroups
#   study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
#   study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
#   study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
#   
#   all_dfs_meds <- list(study_pop_all, study_pop_ret_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
#   names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
#   
# } else if (study_type == "Valproates"){
#   study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
#   study_pop_val_unique <- unique(study_pop_val, by = "person_id")
#   
#   all_dfs_meds <- list(study_pop_all, study_pop_val_unique)
#   names(all_dfs_meds) <- c("All Users", "Valproates Only")
#   
# } else if (study_type == "Both"){
#   study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
#   study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
#   study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
#   study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
#   study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
#   
#   study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
#   study_pop_val_unique <- unique(study_pop_val, by = "person_id")
#   
#   all_dfs_meds <- list(study_pop_all, study_pop_ret_unique, study_pop_val_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
#   names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Valproates Only","Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
#   
# }
# 
# 

rm(my_data, my_treat_episode)
