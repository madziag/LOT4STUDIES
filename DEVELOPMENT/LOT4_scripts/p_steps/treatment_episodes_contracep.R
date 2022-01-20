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



# install.packages("AdhereR")
library(AdhereR)

if (multiple_regions == T ){study_pop_all <- study_pop_reg} else {study_pop_all <- study_population}


dir.create(paste0(output_dir,"treatment_episodes"))

contra_data<-readRDS(paste0(tmp, "all_contraception/all_contra.rds"))
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
  return.data.table = FALSE)

  saveRDS(my_treat_episode, (paste0(output_dir,"treatment_episodes/contra_treat_episode.rds")))


summary(my_treat_episode)
