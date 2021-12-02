install.packages("AdhereR")
library(AdhereR)

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
