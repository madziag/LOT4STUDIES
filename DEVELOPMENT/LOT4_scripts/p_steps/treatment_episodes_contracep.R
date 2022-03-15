#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 26/1/2022

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

contra_data <- readRDS(paste0(contra_folder, pop_prefix, "_all_contra.rds" ))
names(contra_data)
str(contra_data$assumed_duration)
contra_data$assumed_duration<-as.numeric(paste(contra_data$assumed_duration))
str(contra_data$assumed_duration)

  my_treat_episode<-compute.treatment.episodes(data= contra_data,
  ID.colname = "person_id",
  event.date.colname = "contraception_record_date",
  event.duration.colname = "assumed_duration",
  event.daily.dose.colname = NA,
  medication.class.colname = "Code",
  carryover.within.obs.window = TRUE,
  carry.only.for.same.medication = FALSE,
  consider.dosage.change = FALSE,
  medication.change.means.new.treatment.episode = FALSE,
  dosage.change.means.new.treatment.episode = FALSE,
  maximum.permissible.gap = 30,
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
  return.data.table = FALSE)

  # Remove episodes that end before the start of the study period 
  my_treat_episode <- as.data.table(my_treat_episode)
  my_treat_episode[,episode.start:= as.IDate(episode.start,"%Y%m%d")][,episode.end:= as.IDate(episode.end,"%Y%m%d")]
  
  # Merges with study population to get entry and exit dates (study population has been loaded in the wrapper script)
  my_treat_episode1 <- as.data.table(merge(my_treat_episode, study_population[,c("person_id", "entry_date","exit_date")], by = "person_id"))
  # Exclude rows where episode.end is before entry.date-90
  # Therefore, keep records that have a episode.start < entry.date, unless the above exclusion criterion is met  
  my_treat_episode1 <- my_treat_episode1[episode.end > entry_date - 90,]
  #  IF (episode.end > exit.date) {episode.end <- exit.date}
  my_treat_episode1 <- my_treat_episode1[episode.end>exit_date, episode.end:=exit_date]
  #  IF (episode.start >= exit.date) EXCLUDE row
  my_treat_episode1 <- my_treat_episode1[episode.start < exit_date,]
  # Episode end must be > than episode.start
  my_treat_episode1 <- my_treat_episode1[episode.end>episode.start,]
  # Drops columns 
  my_treat_episode1[,entry_date:=NULL][,exit_date:=NULL]
  my_treat_episode <- my_treat_episode1
  
  saveRDS(my_treat_episode, (paste0(g_intermediate, "treatment_episodes/", pop_prefix ,"_contraceptives_treatment_episodes.rds")))
  
summary(my_treat_episode)
hist(my_treat_episode$episode.duration, breaks=200)
hist(my_treat_episode$episode.ID)

#LOGICAL CHECKS
#duration is positive
if(all((my_treat_episode$episode.end-my_treat_episode$episode.start)>0)==FALSE){print("WARNING negative durations detected")}else{print("durations all positive")}

original_ids<-unique(contra_data$person_id)
treat_epi_ids<-unique(my_treat_episode$person_id)
if(all(original_ids%in%treat_epi_ids==T)){print("all person ids from contraception data present in treatment episodes")}else{print("WARNING person id in treatment episodes are not the same as contraception dataset")}

all(my_treat_episode$episode.duration>=28)
table(contra_data$assumed_duration)
table(my_treat_episode$episode.duration)

rm(my_treat_episode, contra_data)