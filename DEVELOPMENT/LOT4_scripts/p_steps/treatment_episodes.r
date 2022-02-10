#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 18/12/2021

# This script does two things:
# 1. it loads valproate or retinoid concept set data sets created by running "to_run_source_pop_counts.R", for separate subpopulations and regions if necessary.
# It then applys createDOT or a fixed duration value to create an estimated end date of treatment for every record
# 2. It creates a new data frame where each row is not a record, but instead a treatment episode.

#INPUTS 
#study_type (Retinoids, Valproates, Both)
#Retinoid.rds or Valproate.rds or both

# Creates treatment episodes directory
invisible(ifelse(!dir.exists(paste0(g_intermediate,"treatment_episodes")), dir.create(paste0(g_intermediate,"treatment_episodes")), FALSE))
#four treatment episode datasets for retinoids (each separate, and one for any retinoid)
my_retinoid <-list.files(paste0(tmp,"medications/"), pattern="ALL_Retinoid")
my_valproate<-list.files(paste0(tmp,"medications/"), pattern="ALL_Valproate")

if(study_type=="Retinoid") {my_data<-readRDS(paste0(tmp, "medications/",my_retinoid))
my_name<-"Retinoid"}
if(study_type=="Valproate"){my_data<-readRDS(paste0(tmp, "medications/",my_valproate))
my_name<-"Valproate"}
if(study_type=="Both")     {
  my_data<-list()
  my_data[[1]]<-readRDS(paste0(tmp, "medications/",my_retinoid))
  my_data[[2]]<-readRDS(paste0(tmp, "medications/",my_valproate))
  all_data<-bind_rows(my_data, .id = "column_label")
  table(all_data$Code)
  split_data<-split(all_data, all_data$Code)
  my_name<-levels(factor(all_data$Code))
  }


for (i in 1:length(split_data)){
  cma_data <- split_data[[i]]
  cma_data$assumed_duration<-rep(30, nrow(cma_data)) 
  cma_data$Date <-as.Date(cma_data$Date, format="%Y%m%d")
  # Creates treatment episodes
  my_treat_episode<-compute.treatment.episodes(data= cma_data,
                                               ID.colname = "person_id",
                                               event.date.colname = "Date",
                                               event.duration.colname = "assumed_duration",
                                               event.daily.dose.colname = NA,
                                               medication.class.colname = "Code",
                                               carryover.within.obs.window = TRUE,
                                               carry.only.for.same.medication = TRUE,
                                               consider.dosage.change =FALSE,
                                               #change between retinoids counts as a new treatment episode
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
  
  
  ###############
  #LOGICAL CHECKS
  #duration is positive
  if(all((my_treat_episode$episode.end-my_treat_episode$episode.start)>0)==FALSE){print("WARNING negative durations detected")}else{print("durations all positive")}
  #person id merged, but no one lost
  original_ids<-unique(cma_data$person_id)
  treat_epi_ids<-unique(my_treat_episode$person_id)
  if(all(original_ids%in%treat_epi_ids==T)){print("all person ids from contraception data present in treatment episodes")}else{print("WARNING person id in treatment episodes are not the same as contraception dataset")}
  #HOW IS THERE A DURATION LESS THAN THE SHORTEST ASSUMED DURATION?
  if(all(my_treat_episode$episode.duration>=30)==T){print("OK: minimum treatment episode equal or greater than assumed duration")}else(print("WARNING treatment episodes shorter than assumed duration"))
  #write data
 
  saveRDS(my_treat_episode, (paste0(g_intermediate, "treatment_episodes/", my_name[i],"_CMA_treatment_episodes.rds")))
  saveRDS(summary(my_treat_episode), (paste0(g_intermediate, "treatment_episodes/", my_name[i],"_summary_treatment_episodes.rds")))
}



rm(my_data, my_treat_episode)
