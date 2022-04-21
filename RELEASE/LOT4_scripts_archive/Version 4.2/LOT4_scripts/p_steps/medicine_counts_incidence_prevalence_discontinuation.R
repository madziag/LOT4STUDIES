#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/01/2022

##################################################################################################################################################
##################################################### OBJECTIVES: 1.1, 1.2, 1.5 ##################################################################
##################################################################################################################################################

## Objective 1.1: Prevalent (current) Retinoid/Valproate use (monthly)
# Numerator -> Number of female subjects in cohort with Retinoid/Valproate episode overlapping the month by at least 1 day
# Denominator -> Total number of female subjects in cohort for at least 1 day in the month (denominator file)
# Records needed -> 1. Retinoid/Valproate treatment episode files 2. Denominator files

## Objective 1.2: Incident (starters) Retinoid/Valproate use (monthly) 
# Numerator -> Number of female subjects in cohort with a Retinoid/Valproate episode start in the month
# Denominator -> Total number of female subjects in cohort for at least 1 day in the month (denominator file)
# Records needed -> 1. Retinoid/Valproate treatment episode files 2. Denominator files

## Objective 1.5: Discontinuation of Retinoid/Valproate use (monthly)
# Numerator -> Number of female subjects in cohort who discontinue Retinoid/Valproate use in the month
# *** Definition of discontinuation: 
# *** Subjects with only 1 treatment episode -> 
# ****** if exit date from study <= 90 days from treatment episode end date -> DOES NOT COUNT AS A DISCONTINUED USER 
# ****** if exit date from study > 90 days from treatment episode end date -> COUNTS AS A DISCONTINUED USER 
# *** Subjects with more than 1 treatment episode -> 
# ****** If not the last of the treatment episode series e.g. if user has 3 treatment episodes, this refers to episodes 1 & 2 -> 
# ********* if next treatment episode start <= 90 days from the previous treatment episode end -> DOES NOT COUNT AS A DISCONTINUED USER 
# ********* if next treatment episode start > 90 days from the previous treatment episode end -> COUNTS AS A DISCONTINUED USER 
# ****** If it is the last of the treatment episode series e.g. if user has 3 treatment episodes, this refers to episode 3 -> 
# ********* if exit date from study <= 90 days from treatment episode end date -> DOES NOT COUNT AS A DISCONTINUED USER 
# ********* if exit date from study > 90 days from treatment episode end date -> COUNTS AS A DISCONTINUED USER 
# Denominator ->  Number of prevalent (current) users that month 
# Records needed -> 1. Retinoid/Valproate treatment episode files 2. Prevalent counts (calculated in 1.1) 3. Indication records for valproates only 

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
### Loads records needed: 
# 1. Treatment episode files 
# Looks for treatment_episode files in treatment_episodes folder (actual files will be loaded in the for loop)
tx_episodes_files <- list.files(paste0(g_intermediate, "treatment_episodes/"), pattern = "Retinoid_CMA|Valproate_CMA", ignore.case = T)
# Filters by current subpopulation 
tx_episodes_files <- tx_episodes_files[grepl(pop_prefix, tx_episodes_files)]
if(populations[pop] == "PC_study_population.rds"){tx_episodes_files <- tx_episodes_files[!grepl("PC_HOSP", tx_episodes_files)]}

# 2. Denominator 
# Looks for denominator file in output directory 
denominator_file <- list.files(tmp, pattern = paste0(pop_prefix,"_denominator.rds"))
# Loads denominator file 
denominator <- readRDS(paste0(tmp, denominator_file))
# Split Y-M variable to year - month columns (for merging later)
denominator[, c("year", "month") := tstrsplit(YM, "-", fixed=TRUE)]
denominator[,year:=as.integer(year)][,month:=as.integer(month)]
min_data_available <- min(denominator$year)
max_data_available <- max(denominator$year)
### Creates empty df for expanding counts files (when not all month-year combinations have counts)
empty_df<-as.data.table(expand.grid(seq(min(denominator$year), max(denominator$year)), seq(1, 12)))
names(empty_df) <- c("year", "month")

# 3. Indication records for valproates only
# Creates folder for all indications
invisible(ifelse(!dir.exists(paste0(tmp,"/","all_indications")), dir.create(paste0(tmp,"/","all_indications")), FALSE))
all_indications_dir <- paste0(tmp,"all_indications")
# Gets list of folders potentially with indication files 
all_temps <- list.files(tmp, pattern="diagnoses|procedures|procedures_dxcodes")
# Checks if there is at least one folder that could have indication files
if(length(all_temps)>0){
  # Creates a list of indications files to be looked for 
  indications <- c("ind_bipolar", "ind_epilepsy", "ind_migraine")
  # Loops through indications
  for(ind in 1:length(indications)){
    # Indications found in diagnosis folder
    if(exists("diagnoses_pop")){
      # Gets list of current indication in diagnosis folder (filtered by pop_prefix)
      indication_file_dx<-list.files(diagnoses_pop,pattern =indications[ind],ignore.case=T,full.names=F)
      indication_file_dx<-indication_file_dx[grepl(pop_prefix,indication_file_dx)]
      if(populations[pop]=="PC_study_population.rds"){indication_file_dx<-indication_file_dx[!grepl("PC_HOSP",indication_file_dx)]}
      # If indication files exist:
      if(length(indication_file_dx)>0){
        # Reads in indication file & creates column with indication type name
        df_dx<-readRDS(paste0(diagnoses_pop, indication_file_dx))[,indication:=indications[ind]]
        # Keeps only  necessary  columns
        df_dx<-df_dx[,c("person_id", "Date", "Code", "Vocabulary", "Meaning", "entry_date", "exit_date", "indication")]
        # Saves modified file in all_indications folder
        saveRDS(df_dx, paste0(all_indications_dir, "/from_events_", indication_file_dx))
      }
    }
    # Indications found in procedures folder
    if(exists("procedures_pop")){
      # Gets list of current indication in diagnosis folder (filtered by pop_prefix)
      indication_file_proc<-list.files(procedures_pop,pattern =indications[ind],ignore.case=T,full.names=F)
      indication_file_proc<-indication_file_proc[grepl(pop_prefix,indication_file_proc)]
      if(populations[pop]=="PC_study_population.rds"){indication_file_proc<-indication_file_proc[!grepl("PC_HOSP",indication_file_proc)]}
      # If indication files exist:
      if(length(indication_file_proc)>0){
        # Reads in indication file & creates column with indication type name
        df_proc<-readRDS(paste0(procedures_pop,indication_file_proc))[,indication:=indications[ind]]
        # Keeps only  necessary  columns
        df_proc<-df_proc[,c("person_id", "Date", "Code", "Vocabulary", "Meaning", "entry_date", "exit_date", "indication")]
        # Saves modified file in all_indications folder
        saveRDS(df_proc, paste0(all_indications_dir, "/from_procedures_", indication_file_proc))
      }
    }
    # Indications found in proc-dx folder 
    if(exists("procedures_dxcodes_pop")){
      # Gets list of current indication in diagnosis folder (filtered by pop_prefix)
      indication_file_proc_dx<-list.files(procedures_dxcodes_pop, pattern =indications[ind],ignore.case=T,full.names=F)
      indication_file_proc_dx<-indication_file_proc_dx[grepl(pop_prefix,indication_file_proc_dx)]
      if(populations[pop]=="PC_study_population.rds"){indication_file_proc_dx<-indication_file_proc_dx[!grepl("PC_HOSP",indication_file_proc_dx)]}
      # If indication files exist:
      if(length(indication_file_proc_dx)>0){
        # Reads in indication file & creates column with indication type name
        df_proc_dx<-readRDS(paste0(procedures_dxcodes_pop,indication_file_proc_dx))[,indication:=indications[ind]]
        # Keeps only  necessary  columns
        df_proc_dx<-df_proc_dx[,c("person_id", "Date", "Code", "Vocabulary", "Meaning", "entry_date", "exit_date", "indication")]
        # Saves modified file in all_indications folder
        saveRDS(df_proc_dx, paste0(all_indications_dir, "/from_procedures_dx_", indication_file_proc_dx))
      }
    }
  }
  
  if(length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))){
    # Gets a list of indication files in all_indication folder (filtered by pop_prefix)
    indications_list <- list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine", full.names = T)
    if(pop_prefix == "PC"){indications_list<-indications_list[!grepl("PC_HOSP", indications_list)]}
    if(pop_prefix == "PC_HOSP"){indications_list<-indications_list[grepl("PC_HOSP", indications_list)]}
    
    # Binds all indication records
    all_indications<- do.call(rbind,lapply(indications_list, readRDS))
    # Keeps only necessary columns 
    all_indications<-all_indications[,c("person_id", "Date", "Code", "indication")]
    # Orders data by person_id, indication and date
    all_indications<-all_indications[order(person_id,indication, Date)]
    # Deduplicates data - you remain with one indication type per person (the one that occurs the earliest)
    all_indications<-all_indications[!duplicated(all_indications[,c("person_id", "indication")]),]
  }
}

# Performs counts using each of the tx_episode files 
for (i in 1:length(tx_episodes_files)){
  # Reads in the treatment episodes file 
  df_episodes <- as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/",tx_episodes_files[i])))
  # Merges with study population to get birth_date (study population has been loaded in the wrapper script)
  df_episodes <- merge(df_episodes, study_population[,c("person_id", "birth_date", "entry_date","exit_date")], by = "person_id")
  # Changes columns to correct data type/add column that indicates rownumber
  df_episodes[,episode.start:=as.IDate(episode.start)][,episode.end:=as.IDate(episode.end)][,entry_date:=as.IDate(entry_date)][,exit_date:=as.IDate(exit_date)]
  # Removes unnecessary columns
  df_episodes <- df_episodes[,-c("end.episode.gap.days", "episode.duration")]
  # Add row numbers to each row 
  df_episodes[,rowID:=.I]
  # Creates a version of df_episodes for incidence counts (we do not need an expanded df for incidence counts)
  df_episodes_for_incidence <- df_episodes
  # Creates a version of df_episodes for discontinued counts (we do not need an expanded df for discontinued counts)
  df_episodes_for_discontinued <- df_episodes
  ### Adds column with indication information ###
  # Checks if there are valproate tx_episode files and if so, that there are indication files for bipolar, epilepsy and migraine 
  if(str_detect(tx_episodes_files[i],"Valproate") & length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
    # Merge tx_episodes with all_indication file
    df_episodes <- all_indications[df_episodes,on=.(person_id), allow.cartesian = T]
    # if indication date is after episode.end, then remove the indication
    df_episodes[Date>episode.end, indication:=NA][Date>episode.end, Code:=NA][Date>episode.end, Date:=NA]
    # Create columns for each of the indication-> if there is more than one indication per treatment episode, then we want them to be in 1 row
    df_episodes[indication=="ind_bipolar", ind_bipolar_date:=Date]
    df_episodes[indication=="ind_epilepsy", ind_epilepsy_date:=Date]
    df_episodes[indication=="ind_migraine", ind_migraine_date:=Date]
    df_episodes<-df_episodes[order(person_id,episode.start, episode.end, ind_bipolar_date, ind_epilepsy_date, ind_migraine_date)]
    # Drop columns you do not need
    df_episodes[,Date:=NULL][,Code:=NULL][,indication:=NULL]
    # Collapse rows so that indications for the same person_id and tx episode will be on the same row
    df_episodes <- setDT(df_episodes)[, lapply(.SD, function(x) unique(na.omit(x))), by = c("person_id", "episode.ID", "episode.start", "episode.end", "birth_date", "entry_date","exit_date")]
    # Remove duplicates if any
    df_episodes <- df_episodes[!duplicated(df_episodes)]
  }
  # Expands data to get every day of treatment per patient (will also be used to add age_groups)
  df_episodes_expanded <- setDT(df_episodes)[,list(idnum = person_id, episode.day = seq(episode.start, episode.end, by = "day")), by = "rowID"]
  # Merges back with original data to get all columns 
  df_episodes_expanded <-  merge(df_episodes, df_episodes_expanded, by = "rowID")
  ### Continuation of adding indication column 
  # Checks if there are indication files and performs action only for DAPs with indication files 
  if(str_detect(tx_episodes_files[i],"Valproate") & length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
    # Adds indication column 
    # Creates columns that calculate difference between episode.day and (ind_epilepsy_date/ind_bipolar_date/ind_migraine_date)
    df_episodes_expanded[,bipolar_diff:=episode.day-ind_bipolar_date][,epilepsy_diff:=episode.day-ind_epilepsy_date][,migraine_diff:=episode.day-ind_migraine_date]
    # If indication date > episode day, then it does not count, indication date is reset to NA
    df_episodes_expanded[bipolar_diff<0,bipolar_diff:=NA][epilepsy_diff<0,epilepsy_diff:=NA][migraine_diff<0,migraine_diff:=NA]
    # Within the 3 difference columns (epilepsy_diff, bipolar_diff, migraine_diff), across each row, counts how many of those columns do not have a missing value. Assigns this to rowSums
    df_episodes_expanded <- df_episodes_expanded[,num_obs:=rowSums(!is.na(df_episodes_expanded[,c("epilepsy_diff", "bipolar_diff", "migraine_diff")]))][]
    # rowSums== 0 means that all 3 columns are NA in that row, therefore indication is unknown
    df_episodes_expanded[num_obs==0, indication:="unknown"]
    # rowSums== 1 means that 2 columns are NA in that row, therefore indication is the name of the indication corresponding to that column
    df_episodes_expanded[num_obs==1 & !is.na(epilepsy_diff), indication:="epilepsy"]
    df_episodes_expanded[num_obs==1 & !is.na(bipolar_diff), indication:="bipolar"]
    df_episodes_expanded[num_obs==1 & !is.na(migraine_diff), indication:="migraine"]
    # rowSums >1 means that at least 2 columns have actual values in that row, therefore indication is multiple
    df_episodes_expanded[num_obs>1, indication:="multiple"]
    # Drops columns we no longer need
    drop.cols <- grep("diff|ind_", colnames(df_episodes_expanded))
    df_episodes_expanded[, (drop.cols) := NULL]
  }
  
  ### Add columns age groups ### 
  # Creates a column with patients age on every day of in the treatment episode
  df_episodes_expanded[,current_age:= floor((episode.day - birth_date)*10/365.25)/10]
  # Add column which groups each patient into an age group, for each day of their treatment
  # We change the min of age group 12-20.99 to 11 to assign a group to those with tx days in 2009 (otherwise it would fall in a group that is not defined)
  df_episodes_expanded[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  df_episodes_expanded[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  df_episodes_expanded[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  df_episodes_expanded[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  
  ### Adds columns tx_duration ### 
  # Adds column with the duration of tx episode (tx_end -tx_start)
  df_episodes_expanded[,tx_duration:=episode.day-episode.start]
  
  # Adds column which groups each patient into an tx_length group
  df_episodes_expanded[,year_month:=paste0(year(episode.day),"-",month(episode.day))]
  # Gets the max tx_duration in that month-year
  max_tx_df <- setDT(df_episodes_expanded)[, .SD[which.max(tx_duration)], by=c("person_id","year_month")]
  df_episodes_expanded[,tx_duration:=NULL]
  df_episodes_expanded<-merge(df_episodes_expanded, max_tx_df[,c("person_id", "year_month", "tx_duration")], by = c("person_id", "year_month"))
  # Assigns tx_episode groups
  df_episodes_expanded[tx_duration >= 0  & tx_duration <= 182, tx_dur_group:= "0-182"]
  df_episodes_expanded[tx_duration > 182 & tx_duration <= 365, tx_dur_group:= "182-365"]
  df_episodes_expanded[tx_duration > 365, tx_dur_group:= "over365"]
  # Create year-months columns based on episode.day
  df_episodes_expanded[,year:=year(episode.day)][,month:=month(episode.day)]
  # Removes unnecessary columns
  df_episodes_expanded <- df_episodes_expanded[,-c("birth_date", "idnum", "episode.ID", "current_age", "tx_duration", "year_month")]
  
  ##################################################################################################
  ################################## Calculates Prevalence #########################################
  ##################################################################################################
  ### Numerator = Number of female subjects in cohort with valproate/retinoid episode overlapping the month by at least 1 day 
  # Removes duplicates - keeps only the earliest record of person_id, year, month => we get the first record of person for every month in every year
  # Creates data where each row represents a month of treatment within the treatment episode (patient is represented once per month)
  df_prevalence <- df_episodes_expanded[!duplicated(df_episodes_expanded[,c("person_id", "episode.start", "year", "month")])]
  # Removes any records where episode.day falls outside of entry & exit dates
  df_prevalence <- df_prevalence[episode.day>=entry_date & episode.day<=exit_date,]
  # Performs prevalence counts 
  prevalence_all <- df_prevalence[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){prevalence_all <- prevalence_all[year < 2020,]} else {prevalence_all <- prevalence_all[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  prevalence_all <- as.data.table(merge(x = empty_df, y = prevalence_all, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  prevalence_all[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  prevalence_all[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masks values less than 5
  # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
  prevalence_all[,masked:=ifelse(N<5 & N>0, 1, 0)]
  # Applies masking 
  if(mask==T){prevalence_all[masked==1,N:=5]} else {prevalence_all[masked==1,N:=N]}
  ### Denominator = Total number of female subjects in the cohort for at least 1 day in the month
  ##### ->  denominator file has already been read in before the for loop, variable name = denominator
  ### Merges numerator file with denominator file
  prevalence_all_counts <- merge(x = prevalence_all, y = denominator, by = c("year", "month"), all.x = TRUE)
  # Calculates rates
  prevalence_all_counts <- prevalence_all_counts[,rates:=round(as.numeric(N)/as.numeric(Freq), 5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  prevalence_all_counts <- prevalence_all_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(prevalence_all_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_prevalence_counts.rds")))
  # Saves df in counts_df folder 
  saveRDS(df_prevalence, (paste0(counts_dfs_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_prevalence_counts.rds")))
  
  ################ STRATIFIED PREVALENCE BY AGE GROUPS ###################
  # Performs prevalence counts - stratified by age group
  prevalence_by_age <- df_prevalence[,.N, by = .(year,month, age_group)]
  # Get unique values of age groups - for the for loop
  age_group_unique <- unique(prevalence_by_age$age_group)
  
  for(group in 1:length(age_group_unique)){
    # Create a subset of age group
    each_group <- prevalence_by_age[age_group==age_group_unique[group]]
    # Adjust for PHARMO
    if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    each_group[is.na(N), N:=0][is.na(age_group), age_group:=age_group_unique[group]]
    # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
    each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
    # Create YM variable 
    each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
    # Masks values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
    # Applies masking 
    if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
    # Prepare denominator (all prevalence counts )
    prevalence_all_counts_min <- prevalence_all_counts[,c("YM", "N")]
    setnames(prevalence_all_counts_min, "N", "Freq")
    # Create counts file
    prevalence_age_counts <- merge(x = each_group, y = prevalence_all_counts_min, by = c("YM"), all.x = TRUE)
    prevalence_age_counts <- prevalence_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    prevalence_age_counts <- prevalence_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    # Saves files in medicine counts folder
    saveRDS(prevalence_age_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_age_group_", age_group_unique[group],"_prevalence_counts.rds")))
  }
  
  ################ STRATIFIED PREVALENCE BY TX DURATION ###################
  # Performs prevalence counts - stratified by tx_duration group
  prevalence_by_tx_dur <- df_prevalence[,.N, by = .(year,month, tx_dur_group)]
  # Get unique values of age groups - for the for loop
  tx_dur_group_unique <- unique(prevalence_by_tx_dur$tx_dur_group)
  
  for(group in 1:length(tx_dur_group_unique)){
    # Create a subset of age group
    each_group <- prevalence_by_tx_dur[tx_dur_group==tx_dur_group_unique[group]]
    # Adjust for PHARMO
    if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    each_group[is.na(N), N:=0][is.na(tx_dur_group), tx_dur_group:=tx_dur_group_unique[group]]
    # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
    each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
    # Create YM variable 
    each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
    # Masks values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
    # Applies masking 
    if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
    # Prepare denominator (all prevalence counts )
    prevalence_all_counts_min <- prevalence_all_counts[,c("YM", "N")]
    setnames(prevalence_all_counts_min, "N", "Freq")
    # Create counts file
    prevalence_tx_dur_counts <- merge(x = each_group, y = prevalence_all_counts_min, by = c("YM"), all.x = TRUE)
    prevalence_tx_dur_counts <- prevalence_tx_dur_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    prevalence_tx_dur_counts <- prevalence_tx_dur_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    # Saves files in medicine counts folder
    saveRDS(prevalence_tx_dur_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_tx_dur_group_", tx_dur_group_unique[group],"_prevalence_counts.rds")))
  }
  ################ STRATIFIED PREVALENCE BY INDICATION ###################
  if(str_detect(tx_episodes_files[i],"Valproate") & length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
    
    # Performs prevalence counts - stratified by indication
    prevalence_by_indication <- df_prevalence[,.N, by = .(year,month, indication)]
    # Get unique values of age groups - for the for loop
    indication_unique <- unique(prevalence_by_indication$indication)
    
    for(group in 1:length(indication_unique)){
      # Create a subset of age group
      each_group <- prevalence_by_indication[indication==indication_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group[is.na(N), N:=0][is.na(indication), indication:=indication_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
      # Prepare denominator (all prevalence counts )
      prevalence_all_counts_min <- prevalence_all_counts[,c("YM", "N")]
      setnames(prevalence_all_counts_min, "N", "Freq")
      # Create counts file
      prevalence_indication_counts <- merge(x = each_group, y = prevalence_all_counts_min, by = c("YM"), all.x = TRUE)
      prevalence_indication_counts <- prevalence_indication_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      prevalence_indication_counts <- prevalence_indication_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(prevalence_indication_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_indication-", indication_unique[group],"_prevalence_counts.rds")))
    }
  }
  ##################################################################################################
  ################################## Calculates Number and rate of incident users ##########################################
  ##################################################################################################
  ### Numerator = Number of female subjects in cohort with a valproate/retinoid episode start in the month 
  # Deduplicate df_episodes_expanded to only include records patients with the first start date 
  df_incidence <- df_episodes_for_incidence[!duplicated(df_episodes_for_incidence)]
  # Removes all episode starts that fall outside entry_date and exit_date
  df_incidence <- df_incidence[episode.start >= entry_date & episode.start<=exit_date,]
  # Creates year and month columns
  df_incidence[,year:=year(episode.start)][,month:=month(episode.start)]
  # Performs incidence counts 
  incidence_all <- df_incidence[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){incidence_all <- incidence_all[year < 2020,]} else {incidence_all <- incidence_all[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  incidence_all <- as.data.table(merge(x = empty_df, y = incidence_all, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  incidence_all[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  incidence_all[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masks values less than 5
  # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
  incidence_all[,masked:=ifelse(N<5 & N>0, 1, 0)]
  # Applies masking 
  if(mask==T){incidence_all[masked==1,N:=5]} else {incidence_all[masked==1,N:=N]}
  ### Denominator = Total number of female subjects in the cohort for at least 1 day in the month
  ##### ->  denominator file has already been read in before the for loop, variable name = denominator
  ### Merges numerator file with denominator file
  incidence_all_counts <- merge(x = incidence_all, y = denominator, by = c("year", "month"), all.x = TRUE)
  # Calculates rates
  incidence_all_counts <- incidence_all_counts[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  incidence_all_counts <- incidence_all_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(incidence_all_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_incidence_counts.rds")))
  # Saves incidence dfs
  saveRDS(df_incidence, (paste0(counts_dfs_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_incidence_counts.rds")))
  ################ STRATIFIED INCIDENCE BY AGE GROUPS ###################
  # Performs incidence counts - stratified by age group
  # Creates a column with patients age on episode.start date
  df_incidence[,current_age:= floor((episode.start - birth_date)*10/365.25)/10]
  # Add column which groups each patient into an age group, for each day of their treatment
  df_incidence[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  df_incidence[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  df_incidence[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  df_incidence[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  # Perform counts by age group
  incidence_by_age <- df_incidence[,.N, by = .(year,month, age_group)]
  # Get unique values of age groups - for the for loop
  age_group_unique <- unique(incidence_by_age$age_group)
  
  for(group in 1:length(age_group_unique)){
    # Create a subset of age group
    each_group <- incidence_by_age[age_group==age_group_unique[group]]
    # Adjust for PHARMO
    if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    each_group[is.na(N), N:=0][is.na(age_group), age_group:=age_group_unique[group]]
    # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
    each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
    # Create YM variable 
    each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
    # Masks values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
    # Applies masking 
    if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
    # Prepare denominator (all prevalence counts )
    incidence_all_counts_min <- incidence_all_counts[,c("YM", "N")]
    setnames(incidence_all_counts_min, "N", "Freq")
    # Create counts file
    incidence_age_counts <- merge(x = each_group, y = incidence_all_counts_min, by = c("YM"), all.x = TRUE)
    incidence_age_counts <- incidence_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    incidence_age_counts <- incidence_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    # Saves files in medicine counts folder
    saveRDS(incidence_age_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_age_group_", age_group_unique[group],"_incidence_counts.rds")))
  }
  
  
  if(str_detect(tx_episodes_files[i],"Valproate") & length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
    ################ STRATIFIED INCIDENCE BY INDICATION ###################
    # Merge tx_episodes with all_indication file
    df_incidence <- all_indications[df_incidence,on=.(person_id), allow.cartesian = T]
    # if indication date is after episode.end, then remove the indication
    df_incidence[Date>episode.end, indication:=NA][Date>episode.end, Code:=NA][Date>episode.end, Date:=NA]
    # Create columns for each of the indication-> if there is more than one indication per treatment episode, then we want them to be in 1 row
    df_incidence[indication=="ind_bipolar", ind_bipolar_date:=Date]
    df_incidence[indication=="ind_epilepsy", ind_epilepsy_date:=Date]
    df_incidence[indication=="ind_migraine", ind_migraine_date:=Date]
    df_incidence<-df_incidence[order(person_id,episode.start, episode.end, ind_bipolar_date, ind_epilepsy_date, ind_migraine_date)]
    # Drop columns you do not need
    df_incidence[,Date:=NULL][,Code:=NULL][,indication:=NULL]
    # Collapse rows so that indications for the same person_id and tx episode will be on the same row
    df_incidence <- setDT(df_incidence)[, lapply(.SD, function(x) unique(na.omit(x))), by = c("person_id", "episode.ID", "episode.start", "episode.end", "birth_date", "entry_date","exit_date")]
    # Remove duplicates if any
    df_incidence <- df_incidence[!duplicated(df_incidence)]
    # Adds indication column 
    # Creates columns that calculate difference between episode.day and (ind_epilepsy_date/ind_bipolar_date/ind_migraine_date)
    df_incidence[,bipolar_diff:=episode.start-ind_bipolar_date][,epilepsy_diff:=episode.start-ind_epilepsy_date][,migraine_diff:=episode.start-ind_migraine_date]
    # If indication date > episode day, then it does not count, indication date is reset to NA
    df_incidence[bipolar_diff<0,bipolar_diff:=NA][epilepsy_diff<0,epilepsy_diff:=NA][migraine_diff<0,migraine_diff:=NA]
    # Within the 3 difference columns (epilepsy_diff, bipolar_diff, migraine_diff), across each row, counts how many of those columns do not have a missing value. Assigns this to rowSums
    df_incidence <- df_incidence[,num_obs:=rowSums(!is.na(df_incidence[,c("epilepsy_diff", "bipolar_diff", "migraine_diff")]))][]
    # rowSums== 0 means that all 3 columns are NA in that row, therefore indication is unknown
    df_incidence[num_obs==0, indication:="unknown"]
    # rowSums== 1 means that 2 columns are NA in that row, therefore indication is the name of the indication corresponding to that column
    df_incidence[num_obs==1 & !is.na(epilepsy_diff), indication:="epilepsy"]
    df_incidence[num_obs==1 & !is.na(bipolar_diff), indication:="bipolar"]
    df_incidence[num_obs==1 & !is.na(migraine_diff), indication:="migraine"]
    # rowSums >1 means that at least 2 columns have actual values in that row, therefore indication is multiple
    df_incidence[num_obs>1, indication:="multiple"]
    # Drops columns we no longer need
    drop.cols <- grep("diff|ind_", colnames(df_incidence))
    df_incidence[, (drop.cols) := NULL]
    
    # Performs incidence counts - stratified by indication
    incidence_by_indication <- df_incidence[,.N, by = .(year,month, indication)]
    # Get unique values of age groups - for the for loop
    indication_unique <- unique(incidence_by_indication$indication)
    
    for(group in 1:length(indication_unique)){
      # Create a subset of age group
      each_group <- incidence_by_indication[indication==indication_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group[is.na(N), N:=0][is.na(indication), indication:=indication_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
      # Prepare denominator (all prevalence counts )
      incidence_all_counts_min <- incidence_all_counts[,c("YM", "N")]
      setnames(incidence_all_counts_min, "N", "Freq")
      # Create counts file
      incidence_indication_counts <- merge(x = each_group, y = incidence_all_counts_min, by = c("YM"), all.x = TRUE)
      incidence_indication_counts <- incidence_indication_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      incidence_indication_counts <- incidence_indication_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(incidence_indication_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_indication-", indication_unique[group],"_incidence_counts.rds")))
    }
  }
  
  # ##################################################################################################
  # ############################# Calculates Discontinuation Rates ###################################
  # ##################################################################################################
  ### Numerator = Number of female subjects in cohort who discontinue valproate/retinoid in the month
  df_discontinued <- df_episodes_for_discontinued[!duplicated(df_episodes_for_discontinued)]
  # Removes all episode ends that fall outside entry_date and exit_date
  df_discontinued <- df_discontinued[episode.end>entry_date & episode.end<=exit_date,]
  # Create new column with the date of the start of the next episode (per patient) if any. If none exists, this becomes an NA
  df_discontinued <- df_discontinued[order(person_id, episode.start)]
  df_discontinued[, next.episode.start:= shift(episode.start, type = "lead" ), by = person_id]
  # Exclude records where exit_date (from study) falls within 90 days after episode.end -> this is not considered a discontinuation
  df_discontinued <- df_discontinued[exit_date - episode.end > 90,]
  # Any records left with NA in the next.episode.start column are now true discontinue-rs and col discontinued is assigned the value 1
  df_discontinued <- df_discontinued[is.na(next.episode.start), discontinued := 1]
  # Exclude records where next.episode.start (if any) falls within 90 days after episode.end
  # If difference between next.episode.start and episode end > 90 then this is a true discontinuer and col discontinued is assigned the value 1
  # if the difference is less than 90 then it is assigned the value 0
  ### discontinuation window is set in 3_to_run_final_counts
  df_discontinued <- df_discontinued[!is.na(next.episode.start), discontinued := ifelse(next.episode.start-episode.end > discontinuation_window, 1, 0)]
  # Get only rows where the value of discontinued == 1 (true discontinue-rs)
  df_discontinued <- df_discontinued[discontinued == 1,]
  # Creates year and month columns
  df_discontinued[,year:=year(episode.end)][,month:=month(episode.end)]
  # Performs discontinued counts 
  discontinued_all <- df_discontinued[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){discontinued_all <- discontinued_all[year < 2020,]} else {discontinued_all <- discontinued_all[year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  discontinued_all <- as.data.table(merge(x = empty_df, y = discontinued_all, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  discontinued_all[is.na(N), N:=0]
  # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
  discontinued_all[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
  # Masks values less than 5
  # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
  discontinued_all[,masked:=ifelse(N<5 & N>0, 1, 0)]
  # Applies masking 
  if(mask==T){discontinued_all[masked==1,N:=5]} else {discontinued_all[masked==1,N:=N]}
  # Create YM variable 
  discontinued_all <- within(discontinued_all, YM<- sprintf("%d-%02d", year, month))
  ### Denominator = Number of prevalent valproate/retinoid users that month
  ##### ->  denominator file has already been created in for loop variable name = prevalence_all_counts
  # Prepare denominator 
  prevalence_all_counts_min <- prevalence_all_counts[,c("YM", "N")]
  setnames(prevalence_all_counts_min, "N", "Freq")
  ### Merges numerator file with denominator file
  discontinued_all_counts <- merge(x = discontinued_all, y = prevalence_all_counts_min, by = c("YM"), all.x = TRUE)
  # Calculates rates
  discontinued_all_counts <- discontinued_all_counts[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
  # Keeps necessary columns 
  discontinued_all_counts <- discontinued_all_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  # Saves files in medicine counts folder
  saveRDS(discontinued_all_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_discontinued_counts.rds")))
  # Saves discontinued dfs
  saveRDS(df_discontinued, (paste0(counts_dfs_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_discontinued_counts.rds")))
  
  ################ STRATIFIED DISCONTINUED BY AGE GROUPS ###################
  # Performs discontinued counts - stratified by age group
  # Creates a column with patients age on episode.end date
  df_discontinued[,current_age:= floor((episode.end - birth_date)*10/365.25)/10]
  
  # Add column which groups each patient into an age group, for each day of their treatment
  df_discontinued[current_age >= 11 & current_age < 21, age_group:= "12-20.99"]
  df_discontinued[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
  df_discontinued[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
  df_discontinued[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
  # Perform counts by age group
  discontinued_by_age <- df_discontinued[,.N, by = .(year,month, age_group)]
  # Get unique values of age groups - for the for loop
  age_group_unique <- unique(discontinued_by_age$age_group)
  
  for(group in 1:length(age_group_unique)){
    # Create a subset of age group
    each_group <- discontinued_by_age[age_group==age_group_unique[group]]
    # Adjust for PHARMO
    if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    each_group[is.na(N), N:=0][is.na(age_group), age_group:=age_group_unique[group]]
    # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
    each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
    # Create YM variable 
    each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
    # Masks values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
    # Applies masking 
    if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
    # Prepare denominator (all prevalence counts )
    discontinued_all_counts_min <- discontinued_all_counts[,c("YM", "N")]
    setnames(discontinued_all_counts_min, "N", "Freq")
    # Create counts file
    discontinued_age_counts <- merge(x = each_group, y = discontinued_all_counts_min, by = c("YM"), all.x = TRUE)
    discontinued_age_counts <- discontinued_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    discontinued_age_counts <- discontinued_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    # Saves files in medicine counts folder
    saveRDS(discontinued_age_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_age_group_", age_group_unique[group],"_discontinued_counts.rds")))
  }
  
  ################ STRATIFIED DISCONTINUED BY TX_DURATION ###################
  # Performs discontinued counts - stratified by tx_duration
  ### Adds columns tx_duration ### 
  # Adds column with the duration of tx episode (tx_end -tx_start)
  df_discontinued[,tx_duration:=episode.end-episode.start]
  # Assigns tx_episode groups
  df_discontinued[tx_duration >= 0  & tx_duration <= 182, tx_dur_group:= "0-182"]
  df_discontinued[tx_duration > 182 & tx_duration <= 365, tx_dur_group:= "182-365"]
  df_discontinued[tx_duration > 365, tx_dur_group:= "over365"]
  
  # Performs counts 
  discontinued_by_tx_dur <- df_discontinued[,.N, by = .(year,month, tx_dur_group)]
  # Get unique values of age groups - for the for loop
  tx_dur_group_unique <- unique(discontinued_by_tx_dur$tx_dur_group)
  
  for(group in 1:length(tx_dur_group_unique)){
    # Create a subset of age group
    each_group <- discontinued_by_tx_dur[tx_dur_group==tx_dur_group_unique[group]]
    # Adjust for PHARMO
    if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    each_group[is.na(N), N:=0][is.na(tx_dur_group), tx_dur_group:=tx_dur_group_unique[group]]
    # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
    each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
    # Create YM variable 
    each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
    # Masks values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
    # Applies masking 
    if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
    # Prepare denominator (all prevalence counts )
    discontinued_all_counts_min <- discontinued_all_counts[,c("YM", "N")]
    setnames(discontinued_all_counts_min, "N", "Freq")
    # Create counts file
    discontinued_tx_dur_counts <- merge(x = each_group, y = discontinued_all_counts_min, by = c("YM"), all.x = TRUE)
    discontinued_tx_dur_counts <- discontinued_tx_dur_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    discontinued_tx_dur_counts <- discontinued_tx_dur_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    # Saves files in medicine counts folder
    saveRDS(discontinued_tx_dur_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_tx_dur_group_", tx_dur_group_unique[group],"_discontinued_counts.rds")))
  }
  
  ################ STRATIFIED DISCONTINUED BY INDICATION ###################
  if(str_detect(tx_episodes_files[i],"Valproate") & length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
    # Merge tx_episodes with all_indication file
    df_discontinued <- all_indications[df_discontinued,on=.(person_id), allow.cartesian = T]
    # if indication date is after episode.end, then remove the indication
    df_discontinued[Date>episode.end, indication:=NA][Date>episode.end, Code:=NA][Date>episode.end, Date:=NA]
    # Create columns for each of the indication-> if there is more than one indication per treatment episode, then we want them to be in 1 row
    df_discontinued[indication=="ind_bipolar", ind_bipolar_date:=Date]
    df_discontinued[indication=="ind_epilepsy", ind_epilepsy_date:=Date]
    df_discontinued[indication=="ind_migraine", ind_migraine_date:=Date]
    df_discontinued<-df_discontinued[order(person_id,episode.start, episode.end, ind_bipolar_date, ind_epilepsy_date, ind_migraine_date)]
    # Drop columns you do not need
    df_discontinued[,Date:=NULL][,Code:=NULL][,indication:=NULL]
    # Collapse rows so that indications for the same person_id and tx episode will be on the same row
    df_discontinued <- setDT(df_discontinued)[, lapply(.SD, function(x) unique(na.omit(x))), by = c("person_id", "episode.ID", "episode.start", "episode.end", "birth_date", "entry_date","exit_date")]
    # Remove duplicates if any
    df_discontinued <- df_discontinued[!duplicated(df_discontinued)]
    # Adds indication column 
    # Creates columns that calculate difference between episode.day and (ind_epilepsy_date/ind_bipolar_date/ind_migraine_date)
    df_discontinued[,bipolar_diff:=episode.start-ind_bipolar_date][,epilepsy_diff:=episode.start-ind_epilepsy_date][,migraine_diff:=episode.start-ind_migraine_date]
    # If indication date > episode day, then it does not count, indication date is reset to NA
    df_discontinued[bipolar_diff<0,bipolar_diff:=NA][epilepsy_diff<0,epilepsy_diff:=NA][migraine_diff<0,migraine_diff:=NA]
    # Within the 3 difference columns (epilepsy_diff, bipolar_diff, migraine_diff), across each row, counts how many of those columns do not have a missing value. Assigns this to rowSums
    df_discontinued <- df_discontinued[,num_obs:=rowSums(!is.na(df_discontinued[,c("epilepsy_diff", "bipolar_diff", "migraine_diff")]))][]
    # rowSums== 0 means that all 3 columns are NA in that row, therefore indication is unknown
    df_discontinued[num_obs==0, indication:="unknown"]
    # rowSums== 1 means that 2 columns are NA in that row, therefore indication is the name of the indication corresponding to that column
    df_discontinued[num_obs==1 & !is.na(epilepsy_diff), indication:="epilepsy"]
    df_discontinued[num_obs==1 & !is.na(bipolar_diff), indication:="bipolar"]
    df_discontinued[num_obs==1 & !is.na(migraine_diff), indication:="migraine"]
    # rowSums >1 means that at least 2 columns have actual values in that row, therefore indication is multiple
    df_discontinued[num_obs>1, indication:="multiple"]
    # Drops columns we no longer need
    drop.cols <- grep("diff|ind_", colnames(df_discontinued))
    df_discontinued[, (drop.cols) := NULL]
    # Performs incidence counts - stratified by indication
    discontinued_by_indication <- df_discontinued[,.N, by = .(year,month, indication)]
    # Get unique values of age groups - for the for loop
    indication_unique <- unique(discontinued_by_indication$indication)
    
    for(group in 1:length(indication_unique)){
      # Create a subset of age group
      each_group <- discontinued_by_indication[indication==indication_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group[is.na(N), N:=0][is.na(indication), indication:=indication_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
      # Prepare denominator (all prevalence counts )
      discontinued_all_counts_min <- discontinued_all_counts[,c("YM", "N")]
      setnames(discontinued_all_counts_min, "N", "Freq")
      # Create counts file
      discontinued_indication_counts <- merge(x = each_group, y = discontinued_all_counts_min, by = c("YM"), all.x = TRUE)
      discontinued_indication_counts <- discontinued_indication_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      discontinued_indication_counts <- discontinued_indication_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(discontinued_indication_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_indication-", indication_unique[group],"_discontinued_counts.rds")))
    }
  }
}



# Move files 
for (file in list.files(path=medicines_counts_dir, pattern="age_group", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_age_groups, "/",file))}
for (file in list.files(path=medicines_counts_dir, pattern="tx_dur", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_tx_dur, "/",file))}
for (file in list.files(path=medicines_counts_dir, pattern="indication", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_indication, "/",file))}


# Clean up 
rm(list = grep("^df_|^discontinued_|^incidence_|^prevalence_|each_group|max_tx_df", ls(), value = TRUE))
