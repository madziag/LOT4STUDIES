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
# Looks for retinoid/valproate files in medications folder 
my_retinoid <-list.files(paste0(tmp,"medications/"), pattern=paste0(pop_prefix, "_Retinoid"))
my_valproate<-list.files(paste0(tmp,"medications/"), pattern=paste0(pop_prefix, "_Valproate"))
# Loads files based on study type
### Study type == Retinoids
if(study_type=="Retinoid") {
  my_data<-readRDS(paste0(tmp, "medications/",my_retinoid))
  all_data<-my_data
  my_name<-levels(factor(my_data$Code))
  split_data<-split(all_data, all_data$Code)
}
### Study type == Valproates
if(study_type=="Valproate"){
  my_data<-readRDS(paste0(tmp, "medications/",my_valproate))
  my_name<-levels(factor(my_data$Code))
  all_data<-my_data
  split_data<-split(all_data, all_data$Code)
}
### Study type == Both
if(study_type=="Both"){
  my_data<-list()
  my_data[[1]]<-readRDS(paste0(tmp, "medications/",my_retinoid))
  my_data[[2]]<-readRDS(paste0(tmp, "medications/",my_valproate))
  all_data<-bind_rows(my_data, .id = "column_label")
  table(all_data$Code)
  split_data<-split(all_data, all_data$Code)
  my_name<-levels(factor(all_data$Code))
}

# Loops over each retinoid/valproate ATC codes -> creates treatment episodes for each unique code 
for (i in 1:length(split_data)){
  
  cma_data <- split_data[[i]]
  
  ##############################################################
  ############### DAP SPECIFIC ASSUMED DURATIONS ###############
  ##############################################################
  ### DEFAULT ###
  # cma_data$assumed_duration<-rep(30, nrow(cma_data))
  cma_data[,assumed_duration:=30]
  ### DAP	Type algorithm	Rules ###
  # 1. ARS: None, No additional analyses, keep 30 days
  # - uses default 
  # 2. DNR	Fixed duration	Valp/Retin: All records 90 days
  if(is_Denmark){cma_data[,assumed_duration:=90]}
  # 3. BIFAP: Estimated using local algorithm	Valp/Retin: Use the value in MEDICINES$presc_duration_days --> COLUMN ADDED IN MONTHLY COUNTS ATC
  ### ARE THERE ANY MISSING VALUES??????
  if(is_BIFAP){
    cma_data[,assumed_duration:=presc_duration_days][,assumed_duration:=as.numeric(assumed_duration)]
    # If any NA's, we take the value of 30 days 
    cma_data[is.na(assumed_duration), assumed_duration:=30]
    if(sum(is.na(cma_data$presc_duration_days))>0){
      print("Missing values for duration of treatment detected.")
      print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
    }
  }
  # 4. FISABIO: Retin: Use the value in MEDICINES$presc_duration_days
  if(is_FISABIO){
    cma_data[,assumed_duration:=presc_duration_days][,assumed_duration:=as.numeric(assumed_duration)]
    # If any NA's, we take the value of 30 days 
    cma_data[is.na(assumed_duration), assumed_duration:=30]
    if(sum(is.na(cma_data$presc_duration_days))>0){
      print("Missing values for duration of treatment detected.")
      print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
    }
  }
  # 5. PHARMO Estimated using local algorithm	Valp/Retin: Use the value in MEDICINES$presc_duration_days
  if(is_PHARMO){
    cma_data[,assumed_duration:=presc_duration_days][,assumed_duration:=as.numeric(assumed_duration)]
    # If any NA's, we take the value of 30 days 
    cma_data[is.na(assumed_duration), assumed_duration:=30]
    if(sum(is.na(cma_data$presc_duration_days))>0){
      print("Missing values for duration of treatment detected.")
      print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
    }
  }
  # 6. CPRD	Estimated using local algorithm	Valp: days_treated = disp_number_medicinal_product  / presc_quantity_per_day
  if(is_CPRD){
    cma_data[,disp_number_medicinal_product:=as.numeric(disp_number_medicinal_product)][,presc_quantity_per_day:=as.numeric(presc_quantity_per_day)]
    cma_data[,assumed_duration:= disp_number_medicinal_product/presc_quantity_per_day][,assumed_duration:=as.numeric(assumed_duration)]
    # If any NA's, we take the value of 30 days 
    cma_data[is.na(assumed_duration), assumed_duration:=30]
    if(sum(is.na(cma_data$presc_duration_days))>0){
      print("Missing values for duration of treatment detected.")
      print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
    }
  }
  # 7.   CASERTA	Calculation	Retin: For every retinoid record, calculate the value
  # - If missing(disp_number_medicinal_product) disp_number_medicinal_product <- 1
  # - days_treated = disp_number_medicinal_product (MEDICINES) *  coverage per box 
  #### PENDING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # if(is_CASERTA){
  #   cma_data[is.na(disp_number_medicinal_product), disp_number_medicinal_product:=1]
  #   cma_data[,assumed_duration:=disp_number_medicinal_product * coverage_per_box] ### where do we get this value from?
  #   cma_data[is.na(assumed_duration), assumed_duration:=30]
  # }
  # 
  
  
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
                                               maximum.permissible.gap = 30,
                                               maximum.permissible.gap.unit = c("days", "weeks", "months", "years", "percent")[1],
                                               maximum.permissible.gap.append.to.episode = FALSE,
                                               followup.window.start = 0,
                                               followup.window.start.unit = c("days", "weeks", "months", "years")[1],
                                               followup.window.duration = 365 * 12,
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
  
  # Converts treatment episode to data table
  my_treat_episode <- as.data.table(my_treat_episode)
  # Converts date values to IDate format
  my_treat_episode[,episode.start:= as.IDate(episode.start,"%Y%m%d")][,episode.end:= as.IDate(episode.end,"%Y%m%d")]
  # Merges with study population to get entry and exit dates (study population has been loaded in the wrapper script)
  my_treat_episode1 <- as.data.table(merge(my_treat_episode, study_population[,c("person_id", "entry_date","exit_date")], by = "person_id"))
  # Exclude rows where episode.end is before entry.date-90
  # Therefore, keep records that have a episode.start < entry.date, unless the above exclusion criterion is met  
  my_treat_episode1 <- my_treat_episode1[episode.end > entry_date - 90,]
  #  IF (episode.end > exit.date) {episode.end <- exit.date}
  my_treat_episode1 <- my_treat_episode1[episode.end > exit_date, episode.end:= exit_date]
  #  IF (episode.start >= exit.date) EXCLUDE row
  my_treat_episode1 <- my_treat_episode1[episode.start < exit_date,]
  # Episode end must be > than episode.start
  my_treat_episode1 <- my_treat_episode1[episode.end > episode.start,]
  # Remove unnecessary rows
  my_treat_episode1[,entry_date:=NULL][,exit_date:=NULL]
  # Saves files (only if df is not empty)
  if (nrow(my_treat_episode1)>0){
    saveRDS(my_treat_episode1, (paste0(g_intermediate, "treatment_episodes/", pop_prefix, "_", my_name[i],"_CMA_treatment_episodes.rds")))
    saveRDS(summary(my_treat_episode1), (paste0(g_intermediate, "treatment_episodes/", pop_prefix, "_", my_name[i],"_summary_treatment_episodes.rds")))
  }
}

# Binds all retinoid/valproate treatment episodes to one
# Gets a list of all files in treatment episode folder 
my_files <- list.files(paste0(g_intermediate, "treatment_episodes/"), pattern="CMA")
if(pop_prefix == "PC"){my_files <- my_files[!grepl("PC_HOSP", my_files)]}
if(pop_prefix == "PC_HOSP"){my_files <- my_files[grepl("PC_HOSP", my_files)]}

## Retinoid files 
retinoid_files <- my_files[grepl(c("D05BB02|D11AH04|D10BA01"), my_files)]
# Checks if any Retinoid treatment episodes in list
if(length(retinoid_files)>0){
  # Loads files
  all_ret_list <- lapply(paste0(g_intermediate, "treatment_episodes/", retinoid_files), readRDS)
  # Binds files 
  all_ret <- rbindlist(all_ret_list)
  # Saves files: result is Retinoid treatment episodes (for all retinoid ATC codes - D05BB02|D11AH04|D10BA01)
  if(nrow(all_ret>0)){saveRDS(all_ret, (paste0(g_intermediate, "treatment_episodes/", pop_prefix, "_Retinoid_CMA_treatment_episodes.rds")))}
}

## Valproate files 
valproate_files <- my_files[grepl(c("N03AG01|N03AG02"), my_files)]
# Checks if any Valproate treatment episodes in list
if(length(valproate_files)>0){
  # Loads files
  all_valp_list <- lapply(paste0(g_intermediate, "treatment_episodes/", valproate_files), readRDS)
  # Binds files
  all_valp <- rbindlist(all_valp_list)
  # Saves files: result is Valproate treatment episodes (for all Valproate ATC codes - N03AG01|N03AG02)
  if(nrow(all_valp>0)){saveRDS(all_valp,(paste0(g_intermediate, "treatment_episodes/", pop_prefix, "_Valproate_CMA_treatment_episodes.rds")))}
}


