# Load study population
# study_population <- readRDS(paste0(populations_dir, "ALL_study_pop_all.rds"))

# Load both retinoid/valproate population users 
pattern1 = c("Retinoid","Valproate")
files <- list.files(path=medications_pop, pattern=paste0(pattern1, collapse="|"))
study_pop_all <- do.call(rbind,lapply(paste0(medications_pop, files), readRDS))
# Drop columns you are not using
# colsToDelete <- c("day_of_birth", "month_of_birth", "year_of_birth", "day_of_death", "month_of_death", "year_of_death", "start_follow_up_month", "start_follow_up_year", "start_follow_up_day", "end_follow_up_month", "end_follow_up_year", "end_follow_up_day", "birth_date_month", "birth_date_year", "birth_date_day", "diff_T1_T0_W", "diff_T2_T1_M", "PY", "Year_op" , "Year", "year")
# study_pop_all[, (colsToDelete) := NULL]
# We need to filter on Entry and Exit date (SFU AND EFU NEED TO BE CHANGED TO THE NEW VARIABLES!!!)
# Both Retinoids and Valproates
# study_pop_all[event_date >= start_follow_up & event_date <= end_follow_up]
# Create fu_dur_days col: exit_date - entry_date (WHICH START AND STOP DATE VARIABLES NEED TO BE USED )
# entry_date <- start_follow_up
# exit_date  <- end_follow_up
#### THIS NEEDS TO BE CHANGES TO NEW START AND STOP VARIABLES!!
study_pop_all[,start_follow_up:=as.IDate(start_follow_up,"%Y%m%d")] # Transform to date variables
study_pop_all[,end_follow_up:=as.IDate(end_follow_up, "%Y%m%d")] # Transform to date variables

study_pop_all[, fu_dur_days := end_follow_up - start_follow_up]
# Create age variable = entry_date - birth date  (# Round down)
study_pop_all[, age_at_entry_date := floor((start_follow_up - birth_date)/365)]
study_pop_all[,age_groups:= ifelse(study_pop_all[,age_at_entry_date >= 12 & age_at_entry_date < 19], "12-18.9", 
                                      ifelse(study_pop_all[,age_at_entry_date < 26], "19-25.9",
                                             ifelse(study_pop_all[,age_at_entry_date < 36],  "26-35.9",
                                                    ifelse(study_pop_all[,age_at_entry_date < 46], "36-45.9",
                                                           ifelse(study_pop_all[,age_at_entry_date < 56], "46-55.9", "Not in range" )))))]

study_pop_all[,med_type := ifelse(study_pop_all[,Code %chin% c("D05BB02", "D11AH04", "D10BA01")], "Retinoid",
                                                ifelse(study_pop_all[,Code %chin% c("N03AG01","N03AG02")], "Valproate", "Unknown"))]

# Count Unique med_type values
med_type_per_pt <- study_pop_all[ , .(count = length(unique(med_type))), by = person_id]

# Merge back with study pop -> ALL RECORDS
study_pop_all <- merge(study_pop_all, med_type_per_pt, by.x = "person_id", by.y = "person_id")
# Choose first use of medication per ATC code & patid -> ALL unique person_id/ATC codes records 
study_pop_first_occurrence  <- setDT(study_pop_all)[order(event_date), head(.SD, 1L), by = c("person_id", "Code")]
# Create Subsets 
# Both Retinoid and Valproate users 
# study_pop_both <- setDT(study_pop_first_occurrence)[count == 2]
# study_pop_both_unique <- unique(study_pop_both, by = "person_id")
# Retinoid Users only - ALL CODES 
study_pop_ret <- setDT(study_pop_first_occurrence)[count == 1 & med_type == "Retinoid"]
study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
# Valproate Users only - ALL CODES 
study_pop_val <- setDT(study_pop_first_occurrence)[count == 1 & med_type == "Valproate"]
study_pop_val_unique <- unique(study_pop_val, by = "person_id")
# Retinoids - individuals
study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]



# ##### INDICATIONS ####### ###WORKING ON THIS
# # study_pop_bipolar <- do.call(rbind,lapply(paste0(diagnoses_pop, list.files(path = diagnoses_pop, pattern = "bipolar")), readRDS))
# study_pop_bipolar <- readRDS(paste0(diagnoses_pop, list.files(path = diagnoses_pop, pattern = "bipolar")))
# study_pop_bipolar[,indication := "bipolar"]
# # study_pop_epilepsy <- do.call(rbind,lapply(paste0(diagnoses_pop, list.files(path = diagnoses_pop, pattern = "epilepsy")), readRDS))
# study_pop_epilepsy <- readRDS(paste0(diagnoses_pop, list.files(path = diagnoses_pop, pattern = "epilepsy")))
# study_pop_epilepsy[,indication := "epilepsy"]
# # study_pop_migraine <- do.call(rbind,lapply(paste0(diagnoses_pop, list.files(path = diagnoses_pop, pattern = "migraine")), readRDS))
# study_pop_migraine <- readRDS(paste0(diagnoses_pop, list.files(path = diagnoses_pop, pattern = "migraine")))
# study_pop_migraine[,indication := "migraine"]
# 
# # Join all tables 
# study_pop_ind_ALL <- rbind(study_pop_bipolar, study_pop_epilepsy, study_pop_migraine)
# # Count Unique indication values per id
# ind_type_per_pt <- study_pop_ind_ALL[ , .(count = length(unique(indication))), by = person_id]
# # Merge back with study pop -> ALL RECORDS 
# study_pop_ind_ALL  <- merge(study_pop_ind_ALL , ind_type_per_pt, by.x = "person_id", by.y = "person_id")





# create a vector of data frames to create baseline tables for 
# all_dfs_meds <- list()
all_dfs_meds <- list(study_pop_first_occurrence, study_pop_ret_unique, study_pop_val_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Valproates Only","Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")

for (i in 1:length(all_dfs_meds)){
      df <- all_dfs_meds[[i]]
      # Check if within correct range (PENDING CORRECT DATES)
      df[event_date >= start_follow_up & event_date <= end_follow_up]
      if(nrow(df > 0)){
      ################## BASELINE ALL POPULATION ########################
      # Calculate median of followup in years 
      fu_median <-  median(df$fu_dur_days)/365
      fu_IQR <- IQR(df$fu_dur_days)/365
      # fu_SD <- 
      age_at_ID_mean <-mean(df$age_at_entry_date)
      age_at_ID_SD   <-sd(df$age_at_entry_date)
      # age_at_ID_SD <- do we calculate SD if we are calculating mean
      if(sum(df$age_groups == "12-18.9")>5){
      age_at_ID_12_18.9_count <-  sum(df$age_groups == "12-18.9")}else{age_at_ID_12_18.9_count<-"count=<5"}
        
      if(sum(df$age_groups == "19-25.9")>5){
      age_at_ID_19_25.9_count <- sum(df$age_groups == "19-25.9")}  else{age_at_ID_19_25.9_coun<-"count=<5"}
      
      if(sum(df$age_groups == "26-35.9")){
      age_at_ID_26_35.9_count <- sum(df$age_groups == "26-35.9")} else {age_at_ID_26_35.9_count<-"count=<5"}
      
      if(sum(df$age_groups == "36-45.9")>5)
      age_at_ID_36_45.9_count <- sum(df$age_groups == "36-45.9")else{age_at_ID_36_45.9_count<-"count=<5"}    
      
      if(sum(df$age_groups == "46-55.9")>5){
      age_at_ID_46_55.9_count <-  sum(df$age_groups == "46-55.9")}  else{age_at_ID_46_55.9_count<-"count=<5"}
      
      print("NAs will be generated for masked counts- ignore error")
      
      age_at_ID_12_18.9_prec  <-   (age_at_ID_12_18.9_count/nrow(df)) * 100
      age_at_ID_19_25.9_prec  <-  (age_at_ID_19_25.9_count/nrow(df)) * 100
      age_at_ID_26_35.9_prec  <- (age_at_ID_26_35.9_count/nrow(df)) * 100
      age_at_ID_36_45.9_prec  <- (age_at_ID_36_45.9_count/nrow(df)) * 100
      age_at_ID_46_55.9_prec  <- (age_at_ID_46_55.9_count/nrow(df)) * 100
      
      # Create dataframe
      names <- c("Follow-up, years - median",
                 "Follow-up, years - IQR",
                 "Age at index date (study entry) - mean", 
                 "Age at index date (study entry) - sd",
                 "12.0-18.9 years_count",
                 "12.0-18.9 years_prec",
                 "19.0-25.9 years_count",  
                 "19.0-25.9 years_prec", 
                 "26.0-35.9 years_count",     
                 "26.0-35.9 years_prec", 
                 "36-45.9 years_count",      
                 "36-45.9 years_prec", 
                 "46.0-55.9 years_count",     
                "46.0-55.9 years_prec")
      
      values <- c(as.character(round(fu_median, 1)), 
                  as.character(round(fu_IQR, 1)), 
                  as.character(round(age_at_ID_mean, 1)), 
                  as.character(round(age_at_ID_SD, 1)), 
                  as.character(age_at_ID_12_18.9_count), 
                  as.character(round(age_at_ID_12_18.9_prec, 1)), 
                  as.character(age_at_ID_19_25.9_count),  
                  as.character(round(age_at_ID_19_25.9_prec,)), 
                  as.character(age_at_ID_26_35.9_count),     
                  as.character(round(age_at_ID_26_35.9_prec, 1)), 
                  as.character(age_at_ID_36_45.9_count),      
                  as.character(round(age_at_ID_36_45.9_prec, 1)), 
                  as.character(age_at_ID_46_55.9_count),     
                  as.character(round(age_at_ID_46_55.9_prec), 1))
      
      baseline <- data.table(names, values)
      
      # Save files 
      print(paste("Saving baseline table: ", names(all_dfs_meds[i])))
      saveRDS(baseline, paste0(baseline_tables_dir,"/", names(all_dfs_meds[i]),"_baseline.rds"))
      write.csv(baseline, paste0(baseline_tables_dir ,"/",  names(all_dfs_meds[i]),"baseline.csv"))
      } else {
        print(paste("There are no records for: ", names(all_dfs_meds[i])))
      }
}





