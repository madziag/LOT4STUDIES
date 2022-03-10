
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
# my_retinoid <-list.files(paste0(tmp,"medications/"), pattern="ALL_Retinoid")
# my_valproate<-list.files(paste0(tmp,"medications/"), pattern="ALL_Valproate")
my_retinoid <-list.files(paste0(tmp,"medications/"), pattern=paste0(pop_prefix, "_Retinoid"))
my_valproate<-list.files(paste0(tmp,"medications/"), pattern=paste0(pop_prefix, "_Valproate"))

if(study_type=="Retinoid") {my_data<-readRDS(paste0(tmp, "medications/",my_retinoid))
all_data<-my_data
my_name<-levels(factor(my_data$Code))
split_data<-split(all_data, all_data$Code)
}

if(study_type=="Valproate"){my_data<-readRDS(paste0(tmp, "medications/",my_valproate))
my_name<-levels(factor(my_data$Code))
all_data<-my_data
split_data<-split(all_data, all_data$Code)}

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
  
  ###############
  my_treat_episode <- as.data.table(my_treat_episode)
  my_treat_episode[,episode.start:= as.IDate(episode.start,"%Y%m%d")][,episode.end:= as.IDate(episode.end,"%Y%m%d")]
  
  # Merges with study population to get birth_date (study population has been loaded in the wrapper script)
  my_treat_episode1 <- as.data.table(merge(my_treat_episode, study_population[,c("person_id", "entry_date","exit_date")], by = "person_id"))
  # Exclude rows where episode.end is before entry.date-90
  # Therefore, keep records that have a episode.start < entry.date, unless the above exclusion criterion is met  
  my_treat_episode1 <- my_treat_episode1[episode.end > entry_date - 90,]
  #  IF (episode.end > exit.date) {episode.end <- exit.date}
  my_treat_episode1 <- my_treat_episode1[episode.end>exit_date, episode.end:=exit_date]
  #  ??? IF (episode.start >= exit.date) EXCLUDE row
  my_treat_episode1 <- my_treat_episode1[episode.start < exit_date,]
  my_treat_episode1[,entry_date:=NULL][,exit_date:=NULL]
  my_treat_episode <- my_treat_episode1
  #LOGICAL CHECKS
  #duration is positive
  if(all((my_treat_episode$episode.end-my_treat_episode$episode.start)>0)==FALSE){print("WARNING negative durations detected")}else{print("durations all positive")}
  #person id merged, but no one lost
  original_ids<-unique(cma_data$person_id)
  treat_epi_ids<-unique(my_treat_episode$person_id)
  if(all(original_ids%in%treat_epi_ids==T)){print("all person ids from contraception data present in treatment episodes")}else{print("WARNING person id in treatment episodes are not the same as contraception dataset")}
  #HOW IS THERE A DURATION LESS THAN THE SHORTEST ASSUMED DURATION?
  if(all(my_treat_episode$episode.duration>=30)==T){print("OK: minimum treatment episode equal or greater than assumed duration")}else(print("WARNING treatment episodes shorter than assumed duration"))
  
  if (nrow(my_treat_episode)>0){
  #write data
  saveRDS(my_treat_episode, (paste0(g_intermediate, "treatment_episodes/", pop_prefix, "_", my_name[i],"_CMA_treatment_episodes.rds")))
  saveRDS(summary(my_treat_episode), (paste0(g_intermediate, "treatment_episodes/", pop_prefix, "_", my_name[i],"_summary_treatment_episodes.rds")))
  }
}

# rm(my_data, my_treat_episode, all_data)

#make "all_retinoid" and "all_valproate"

my_files<-list.files(paste0(g_intermediate, "treatment_episodes/"), pattern="CMA")
# Load files 
retinoid_files <- my_files[grepl(c("D05BB02|D11AH04|D10BA01"), my_files)]

if(pop_prefix == "PC"){retinoid_files <- retinoid_files[!grepl("PC_HOSP",retinoid_files)]}
if(pop_prefix == "PC_HOSP"){retinoid_files <- retinoid_files[grepl("PC_HOSP",retinoid_files)]}
all_ret_list <- lapply(paste0(g_intermediate, "treatment_episodes/", retinoid_files), readRDS)
all_ret <- rbindlist(all_ret_list)

valproate_files <- my_files[grepl(c("N03AG01|N03AG02"), my_files)]
if(pop_prefix == "PC"){valproate_files <- valproate_files[!grepl("PC_HOSP",valproate_files)]}
if(pop_prefix == "PC_HOSP"){valproate_files <- valproate_files[grepl("PC_HOSP",valproate_files)]}
all_valp_list <- lapply(paste0(g_intermediate, "treatment_episodes/", valproate_files), readRDS)

all_valp <- rbindlist(all_valp_list)


if(nrow(all_ret>0)){
  saveRDS(all_ret, (paste0(g_intermediate, "treatment_episodes/", pop_prefix, "_Retinoid_CMA_treatment_episodes.rds")))
}


if(nrow(all_valp>0)){
  saveRDS(all_valp,(paste0(g_intermediate, "treatment_episodes/", pop_prefix, "_Valproate_CMA_treatment_episodes.rds")))
}

# rm(my_data, my_treat_episode)



# my_names<-as.data.frame(matrix(ncol = 2, nrow=5))
# colnames(my_names)<-c("Code", "Drug")
# my_names$Code<-c("D05BB02", "D11AH04","D10BA01", "N03AG01", "N03AG02")
# my_names$Drug<-c("Retinoid", "Retinoid", "Retinoid", "Valproate", "Valproate")
# 
# for(i in 1:length(my_data)){
#   my_label<-my_names[(str_detect(my_files[i], my_names$Code)),]
# 
#   my_data[[i]]$ATC<-rep(my_label[1], nrow(my_data[[i]]))
#   my_data[[i]]$type<-rep(my_label[2], nrow(my_data[[i]]))}
# 
# 
# all_data<-bind_rows(my_data, .id = "column_label")
# 
# all_ret<- as.data.table(all_data[all_data$type=="Retinoid",])
# all_valp<- as.data.table(all_data[all_data$type=="Valproate",])
# 
# # Suppresses warnings
# options(warn=-1)
# 
