#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 07/02/2022

##################################################################################################################################################
###################################################### OBJECTIVE: 3.1 ############################################################################
##################################################################################################################################################

## Objective 3.1: Rates of pregnancies starting during a Retinoid/Valproate treatment episode
# Numerator -> Number of pregnancies that started during a Retinoid/Valproate episode
# Denominator -> Number of prevalent (current) users that month 
# Records needed -> 1. Pregnancy records (created by ARS Toscana script) 2. Treatment episodes 3. Prevalent user counts (saved in medicine_counts folder)

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

### Loads records needed 
# 1. Pregnancy records 
D3_pregnancy_reconciled <- as.data.table(get(load(paste0(preg_dir, "g_intermediate/D3_pregnancy_reconciled.RData"))))
# Data Cleaning Pregnancy file
D3_pregnancy_reconciled[,person_id:=as.character(person_id)]
D3_pregnancy_reconciled[,pregnancy_start_date:=as.IDate(pregnancy_start_date, "%Y%m%d" )]
D3_pregnancy_reconciled[,pregnancy_end_date:=as.IDate(pregnancy_end_date, "%Y%m%d" )]

D3_pregnancy_reconciled[pregnancy_start_date=="2003-08-21", pregnancy_start_date:=as.IDate(as.character(20171212), "%Y%m%d")]
D3_pregnancy_reconciled[pregnancy_start_date=="2009-11-03", pregnancy_start_date:=as.IDate(as.character(20160120), "%Y%m%d")]
D3_pregnancy_reconciled[pregnancy_start_date=="2012-07-26", pregnancy_start_date:=as.IDate(as.character(20180831), "%Y%m%d")]
D3_pregnancy_reconciled[pregnancy_start_date=="2019-02-23", pregnancy_start_date:=as.IDate(as.character(20190630), "%Y%m%d")]
# 2. Treatment episode files 
# Looks for treatment_episode files in treatment_episodes folder (actual files will be loaded in the for loop)
tx_episodes_files <- list.files(paste0(g_intermediate, "treatment_episodes/"), pattern = "Retinoid_CMA|Valproate_CMA", ignore.case = T)
# Filters by current subpopulation 
tx_episodes_files <- tx_episodes_files[grepl(pop_prefix, tx_episodes_files)]

if(populations[pop] == "PC_study_population.rds"){
  tx_episodes_files <- list.files(paste0(g_intermediate, "treatment_episodes/"), pattern = "Retinoid_CMA|Valproate_CMA", ignore.case = T)
  tx_episodes_files <- tx_episodes_files[!grepl("PC_HOSP", tx_episodes_files)]
}

# 3. Prevalent user counts 
prevalent_counts_files <- list.files(medicines_counts_dir, pattern = "prevalence_counts", ignore.case = T, full.names = T)
# Filters by current subpopulation 
prevalent_counts_files <- prevalent_counts_files[grepl(pop_prefix, prevalent_counts_files)]

if(populations[pop] == "PC_study_population.rds"){
  prevalent_counts_files <- list.files(medicines_counts_dir, pattern = "prevalence_counts", ignore.case = T, full.names = T)
  prevalent_counts_files <- prevalent_counts_files[!grepl("PC_HOSP", prevalent_counts_files)]
}


### Creates empty df for expanding counts files (when not all month-year combinations have counts) - uses denominator file min and max year values 
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
if(is_BIFAP){empty_df<-as.data.table(expand.grid(seq(2010, 2020), seq(1,12)))}else{empty_df<-as.data.table(expand.grid(seq(min(denominator$year), max(denominator$year)), seq(1, 12)))}
names(empty_df) <- c("year", "month")

if (nrow(D3_pregnancy_reconciled)>0){
  # For each treatment episode file 
  for (i in 1:length(tx_episodes_files)){ 
    # Reads in the treatment episodes file 
    tx_episodes <- as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/",tx_episodes_files[i])))
    tx_episodes <- tx_episodes[,-c("ATC", "type")]
    # Merge tx episodes with pregnancy records 
    tx_episodes_preg <- D3_pregnancy_reconciled[tx_episodes, on = .(person_id), allow.cartesian = T] # Left join
    # Delete records without pregnancy records
    tx_episodes_preg <-  tx_episodes_preg[!is.na(pregnancy_start_date),]
    # Remove duplicates
    tx_episodes_preg <- tx_episodes_preg[!duplicated(tx_episodes_preg[,c("person_id", "pregnancy_start_date", "highest_quality")])]
    # Converts dates to be in the same format
    tx_episodes_preg[,episode.start:=as.IDate(episode.start,"%Y%m%d")][,episode.end:=as.IDate(episode.end,"%Y%m%d")]
    # Creates column that indicates if medicine record date is between episode.start and episode.end dates
    tx_episodes_preg[,preg_in_episode:= fifelse(pregnancy_start_date>=episode.start & pregnancy_start_date<=episode.end, 1, 0)] 
    # Creates df of patients who have a pregnancy start date between tx episode.start and episode.end dates
    tx_episodes_preg <- tx_episodes_preg[preg_in_episode == 1,] 
    # Checks if there are any records that meet the criteria. If so it does the calculations
    if (nrow(tx_episodes_preg) > 0){
      # Performs counts 
      preg_start_during_tx_counts <- tx_episodes_preg[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date))] # Performs counts grouped by year, month of medicine prescription date
      preg_start_during_tx_counts <- as.data.table(merge(x = empty_df, y = preg_start_during_tx_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with preg_start_during_tx_counts
      preg_start_during_tx_counts[is.na(preg_start_during_tx_counts[,N]), N:=0] # Fills in missing values with 0
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      # for BIFAP
      preg_start_during_tx_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking
      preg_start_during_tx_counts$masked_num <- ifelse(preg_start_during_tx_counts$N < 5 & preg_start_during_tx_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
      if(mask == T){preg_start_during_tx_counts[preg_start_during_tx_counts$masked_num == 1,]$N <- 5} else {preg_start_during_tx_counts[preg_start_during_tx_counts$masked_num == 1,]$N <- preg_start_during_tx_counts[preg_start_during_tx_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
      ############################
      ##### Calculates Rates #####
      ############################
      preg_start_during_tx_counts <- within(preg_start_during_tx_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      # Load prevalence counts
      prevalent_counts <- readRDS(prevalent_counts_files[grepl(gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), prevalent_counts_files)])
      prevalent_counts <- prevalent_counts[,-c("Freq", "rates", "masked", "true_value")]
      setnames(prevalent_counts, "N", "Freq")
      preg_start_during_tx_counts <- merge(x = preg_start_during_tx_counts, y = prevalent_counts, by = c("YM"), all.x = TRUE) # Merge with med counts
      preg_start_during_tx_counts <- preg_start_during_tx_counts[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
      preg_start_during_tx_counts <- preg_start_during_tx_counts[,c("YM", "N", "Freq", "rates", "masked_num", "true_value")]
      setnames(preg_start_during_tx_counts, "masked_num", "masked")
      # Save files 
      saveRDS(tx_episodes_preg, paste0(counts_dfs_dir, gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_all_preg_starts_during_tx_episodes.rds"))
      saveRDS(preg_start_during_tx_counts, paste0(preg_med_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_all_preg_starts_during_tx_episodes_counts.rds"))
      
      ##### STRATIFICATION BY AGE GROUPS ###
      # Merge data with study population to get date of birth
      preg_starts_df_age_groups <- merge(tx_episodes_preg, study_population[,c("person_id", "birth_date")], by = "person_id")
      # Creates a column with patients age on every day of in the treatment episode
      preg_starts_df_age_groups[,current_age:= floor((pregnancy_start_date - birth_date)*10/365.25)/10]
      # Removes anyone with age over 56
      preg_starts_df_age_groups<-preg_starts_df_age_groups[current_age<56,]
      # Add column which groups each patient into an age group, for each day of their treatment
      preg_starts_df_age_groups[current_age >= 12 & current_age < 21, age_group:= "12-20.99"]
      preg_starts_df_age_groups[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
      preg_starts_df_age_groups[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
      preg_starts_df_age_groups[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
      
      # Performs pgtests counts - stratified by age group
      preg_starts_by_age <- preg_starts_df_age_groups[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date), age_group)]
      # Get unique values of age groups - for the for loop
      age_group_unique <- unique(preg_starts_by_age$age_group)
      
      for(group in 1:length(age_group_unique)){
        # Create a subset of age group
        each_group <- preg_starts_by_age[age_group==age_group_unique[group]]
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
        # Prepare denominator (all pgtests counts )
        preg_starts_all_counts_min <- preg_start_during_tx_counts[,c("YM", "N")]
        setnames(preg_starts_all_counts_min, "N", "Freq")
        # Create counts file
        preg_starts_age_counts <- merge(x = each_group, y = preg_starts_all_counts_min, by = c("YM"), all.x = TRUE)
        preg_starts_age_counts <- preg_starts_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
        preg_starts_age_counts <- preg_starts_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
        # Saves files in medicine counts folder
        saveRDS(preg_starts_age_counts, paste0(preg_med_counts_dir, "/", gsub(".rds", "", med_files[i]), "_age_group_", age_group_unique[group], "_preg_starts_during_tx_episodes_counts.rds")) # Monthly counts file
      }
      
      #### Taking into account highest_quality column in pregnancy df - Counts ####
      # Get the unique value of the highest quality column
      hq_unique<-unique(preg_starts_df_age_groups$highest_quality)
      
      for (j in 1:length(hq_unique)){
        # Create a subset of the unique value
        tx_episodes_preg_unique <- preg_starts_df_age_groups[which(highest_quality==hq_unique[j]),]
        # Performs counts 
        preg_start_during_tx_unique_counts <- tx_episodes_preg_unique[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date))] # Performs counts grouped by year, month of medicine prescription date
        preg_start_during_tx_unique_counts <- as.data.table(merge(x = empty_df, y = preg_start_during_tx_unique_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with preg_start_during_tx_unique_counts
        preg_start_during_tx_unique_counts[is.na(preg_start_during_tx_unique_counts[,N]), N:=0] # Fills in missing values with 0
        # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
        preg_start_during_tx_unique_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
        # Masking
        preg_start_during_tx_unique_counts$masked_num <- ifelse(preg_start_during_tx_unique_counts$N < 5 & preg_start_during_tx_unique_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
        if(mask == T){preg_start_during_tx_unique_counts[preg_start_during_tx_unique_counts$masked_num == 1,]$N <- 5} else {preg_start_during_tx_unique_counts[preg_start_during_tx_unique_counts$masked_num == 1,]$N <- preg_start_during_tx_unique_counts[preg_start_during_tx_unique_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
        ############################
        ##### Calculates Rates #####
        ############################
        preg_start_during_tx_unique_counts <- within(preg_start_during_tx_unique_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
        # Load prevalence counts
        prevalent_counts <- readRDS(prevalent_counts_files[grepl(gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), prevalent_counts_files)])
        prevalent_counts <- prevalent_counts[,-c("Freq", "rates", "masked", "true_value")]
        setnames(prevalent_counts, "N", "Freq")
        preg_start_during_tx_unique_counts <- merge(x = preg_start_during_tx_unique_counts, y = prevalent_counts, by = c("YM"), all.x = TRUE) # Merge with med counts
        preg_start_during_tx_unique_counts <- preg_start_during_tx_unique_counts[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
        preg_start_during_tx_unique_counts <- preg_start_during_tx_unique_counts[,c("YM", "N", "Freq", "rates", "masked_num", "true_value")]
        setnames(preg_start_during_tx_unique_counts, "masked_num", "masked")
        # Save files 
        saveRDS(tx_episodes_preg_unique, paste0(counts_dfs_dir, gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_hq_", hq_unique[j], "_preg_start_during_tx_episodes.rds"))
        saveRDS(preg_start_during_tx_unique_counts, paste0(preg_med_counts_dir,"/",gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_hq_", hq_unique[j], "_preg_starts_during_tx_episodes_counts.rds"))
        
        # STRATIFIED BY QUALITY AND AGE GROUP
        # Performs pgtests counts - stratified by age group
        preg_starts_by_age <- tx_episodes_preg_unique[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date), age_group)]
        # Get unique values of age groups - for the for loop
        age_group_unique <- unique(preg_starts_by_age$age_group)
        
        for(group in 1:length(age_group_unique)){
          # Create a subset of age group
          each_group <- preg_starts_by_age[age_group==age_group_unique[group]]
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
          # Prepare denominator (all pgtests counts )
          preg_starts_all_counts_min <- preg_start_during_tx_counts[,c("YM", "N")]
          setnames(preg_starts_all_counts_min, "N", "Freq")
          # Create counts file
          preg_starts_age_counts <- merge(x = each_group, y = preg_starts_all_counts_min, by = c("YM"), all.x = TRUE)
          preg_starts_age_counts <- preg_starts_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
          preg_starts_age_counts <- preg_starts_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
          # Saves files in medicine counts folder
          saveRDS(preg_starts_age_counts, paste0(preg_med_counts_dir, "/", gsub(".rds", "", med_files[i]), "_hq_", hq_unique[j], "_age_group_", age_group_unique[group], "_preg_starts_during_tx_episodes_counts.rds")) # Monthly counts file
        }
        
        }
    } else {
      print(paste0(gsub("_CMA_treatment_episodes.rds", "",tx_episodes_files[i]), " study: There are no patients with pregnancy start dates that fall between episode start and end dates!"))
    }
  }
} else {
  print("No pregancy records have been found")
}


# Create stratified folders and move files into stratified folders
if(nrow(preg_starts_df_age_groups)>0){
  # Move stratified records into stratified folders
  # Create stratified folder
  invisible(ifelse(!dir.exists(paste0(preg_med_counts_dir,"/","stratified")), dir.create(paste0(preg_med_counts_dir,"/","stratified")), FALSE))
  pregnancies_stratified_dir <- paste0(preg_med_counts_dir,"/","stratified")
  # Create stratified by age groups folder
  invisible(ifelse(!dir.exists(paste0(pregnancies_stratified_dir,"/","age_group")), dir.create(paste0(pregnancies_stratified_dir,"/","age_group")), FALSE))
  pregnancies_stratified_age_groups <- paste0(pregnancies_stratified_dir ,"/","age_group")
  # # Create stratified by tx_duration folder 
  # invisible(ifelse(!dir.exists(paste0(pregnancies_stratified_dir,"/","tx_duration")), dir.create(paste0(pregnancies_stratified_dir,"/","tx_duration")), FALSE))
  # pregnancies_stratified_tx_dur <- paste0(pregnancies_stratified_dir ,"/","tx_duration")
  # # Create stratified by indication folder 
  # invisible(ifelse(!dir.exists(paste0(pregnancies_stratified_dir,"/","indication")), dir.create(paste0(pregnancies_stratified_dir,"/","indication")), FALSE))
  # pregnancies_stratified_indication <- paste0(pregnancies_stratified_dir ,"/","indication")
  
  # Move files 
  for (file in list.files(path=preg_med_counts_dir, pattern="age_group", ignore.case = T)){file.move(paste0(preg_med_counts_dir,"/", file),paste0(pregnancies_stratified_age_groups, "/",file))}
  # for (file in list.files(path=pregnancy_test_counts_dir, pattern="tx_dur", ignore.case = T)){file.move(paste0(pregnancy_test_counts_dir,"/", file),paste0(pregnancy_test_stratified_tx_dur, "/",file))}
  # for (file in list.files(path=pregnancy_test_counts_dir, pattern="indication", ignore.case = T)){file.move(paste0(pregnancy_test_counts_dir,"/", file),paste0(pregnancy_test_stratified_indication, "/",file))}
}

