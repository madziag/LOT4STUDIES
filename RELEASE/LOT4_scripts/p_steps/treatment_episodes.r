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



install.packages("AdhereR")
library(AdhereR)

if (multiple_regions == T ){study_pop_all <- study_pop_reg} else {study_pop_all <- study_population}


# what does the data need to look like?
#fetch medicines data from CDMInstances
all_MED<-list.files(path_dir, pattern="^MEDICINES")

MED_tables<-lapply((paste0(path_dir, all_MED)), read.csv)

for (i in 1:length(MED_tables)){
MED_tables[[i]]$disp_date<-as.Date(as.character(MED_tables[[i]]$date_dispensing), format="%Y%m%d")
MED_tables[[i]]$duration<-30
  # sample(c(10, 30, 60), nrow(MED_tables[[i]]))
MED_tables[[i]]$end_date<-((MED_tables[[i]]$disp_date)+(MED_tables[[i]]$duration))
}

dir.create(paste0(output_dir,"treatment_episodes"))



for (i in 1:length(MED_tables)){

  my_treat_episode<-compute.treatment.episodes(data= MED_tables[[i]],
  ID.colname = "person_id",
  event.date.colname = "disp_date",
  event.duration.colname = "duration",
  event.daily.dose.colname = NA,
  medication.class.colname = "medicinal_product_atc_code",
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
  followup.window.duration = 365 * 2,
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
  table_name<-substr(all_MED[[i]], 1,nchar(all_MED[[i]])-8)
  myname<-paste0("treatment_episode_",table_name,".rds")
  saveRDS(my_treat_episode, (paste0(output_dir,"treatment_episodes/")), myname)
}

summary(my_treat_episode)
#does write work for you?


### Run separate depending on study

if (study_type == "Retinoids"){
  study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
  study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
  # Retinoids - subgroups
  study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
  study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
  study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
  
  all_dfs_meds <- list(study_pop_all, study_pop_ret_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
  names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
  
} else if (study_type == "Valproates"){
  study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
  study_pop_val_unique <- unique(study_pop_val, by = "person_id")
  
  all_dfs_meds <- list(study_pop_all, study_pop_val_unique)
  names(all_dfs_meds) <- c("All Users", "Valproates Only")
  
} else if (study_type == "Both"){
  study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
  study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
  study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
  study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
  study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
  
  study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
  study_pop_val_unique <- unique(study_pop_val, by = "person_id")
  
  all_dfs_meds <- list(study_pop_all, study_pop_ret_unique, study_pop_val_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
  names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Valproates Only","Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
  
}


