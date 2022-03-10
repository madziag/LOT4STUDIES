#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022

# Creates baseline table for retinoid and valproate counts

# Transforms date variables to date type
# Overall study population
study_population[,entry_date:=as.IDate(entry_date,"%Y%m%d")] 
study_population[,exit_date:=as.IDate(exit_date, "%Y%m%d")]
# Population with Retinoid and/or Valproate use
study_pop_meds[,entry_date:=as.IDate(entry_date,"%Y%m%d")] 
study_pop_meds[,exit_date:=as.IDate(exit_date, "%Y%m%d")] 
# Creates new column in study population: fu_dur_days 
study_population[, fu_dur_days := exit_date - entry_date]
# Creates age variable in study population = entry_date - birth date  (# Rounds down)
study_population[, age_at_entry_date := floor((entry_date - birth_date)/365.25)]
study_population[,age_groups:= ifelse(study_population[,age_at_entry_date >= 12 & age_at_entry_date < 21], "12-20.99", 
                                      ifelse(study_population[,age_at_entry_date >= 21 & age_at_entry_date < 31], "21-30.99",
                                             ifelse(study_population[,age_at_entry_date >= 31 & age_at_entry_date < 41],  "31-40.99",
                                                    ifelse(study_population[,age_at_entry_date >= 41 & age_at_entry_date < 56], "41-55.99", "Not in range" ))))]

# Medicine records need to be between a persons entry and exit dates 
study_pop_meds <- study_pop_meds[Date>=entry_date & Date<exit_date,]
# Creates new column in Population with Retinoid and/or Valproate use: fu_dur_days 
study_pop_meds[,entry_date:=as.IDate(entry_date,"%Y%m%d")] # Transform to date variables
study_pop_meds[,exit_date:=as.IDate(exit_date, "%Y%m%d")] # Transform to date variables
# follow up duration 
study_pop_meds[, fu_dur_days := exit_date - entry_date]
# Creates age variable in Population with Retinoid and/or Valproate use = entry_date - birth date  (# Rounds down)
study_pop_meds[, age_at_entry_date := floor((entry_date - birth_date)/365.25)]
study_pop_meds[,age_groups:= ifelse(study_pop_meds[,age_at_entry_date >= 12 & age_at_entry_date < 21], "12-20.99", 
                                    ifelse(study_pop_meds[,age_at_entry_date >= 21 & age_at_entry_date < 31], "21-30.99",
                                           ifelse(study_pop_meds[,age_at_entry_date >= 31 & age_at_entry_date < 41],  "31-40.99",
                                                  ifelse(study_pop_meds[,age_at_entry_date >= 41 & age_at_entry_date < 56], "41-55.99", "Not in range" ))))]

# Creates column that describes if ATC is Retinoid, Valproate or Unknowns
study_pop_meds[,med_type := ifelse(study_pop_meds[,Code %chin% c("D05BB02", "D11AH04", "D10BA01")], "Retinoid",
                                   ifelse(study_pop_meds[,Code %chin% c("N03AG01","N03AG02")], "Valproate", "Unknown"))]
# Chooses first use of medication per ATC code & patid -> ALL unique person_id/ATC codes records 
study_pop_first_occurrence  <- setDT(study_pop_meds)[order(Date), head(.SD, 1L), by = c("person_id", "Code")]

if (multiple_regions == T){
  # Saves study_population file (with age_group categories)
  saveRDS(study_population, paste0(baseline_pooling_dir,"/", pop_prefix, "_for_pooling_baseline_tables.rds"))
  # Saves Retinoid/Valproate file (with age group categories)
  saveRDS(study_pop_first_occurrence, paste0(baseline_pooling_dir,"/", pop_prefix, "_med_use_for_pooling_baseline_tables.rds"))
}
# Creates Subsets 
if (study_type == "Retinoid"){
  study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
  study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
  # Retinoids - subgroups
  study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
  study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
  study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
  
  all_dfs_meds <- list(study_population, study_pop_ret_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
  names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
  
} else if (study_type == "Valproate"){
  study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
  study_pop_val_unique <- unique(study_pop_val, by = "person_id")
  
  all_dfs_meds <- list(study_population, study_pop_val_unique)
  names(all_dfs_meds) <- c("All Users", "Valproates Only")
  
} else if (study_type == "Both"){
  study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
  study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
  study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
  study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
  study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
  
  study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
  study_pop_val_unique <- unique(study_pop_val, by = "person_id")
  
  all_dfs_meds <- list(study_population, study_pop_ret_unique, study_pop_val_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
  names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Valproates Only","Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
  
}

# Loops through all the subsets depending on the study_type and creates baseline tables 
for (i in 1:length(all_dfs_meds)){
  df <- all_dfs_meds[[i]]
  if(nrow(df > 0)){
    ################## BASELINE ALL POPULATION ########################
    # Calculates median of followup in years 
    fu_median     <- median(df$fu_dur_days)/365.25
    fu_IQR        <- IQR(df$fu_dur_days)/365.25
    fu_min        <- min(df$fu_dur_days)/365.25
    fu_max        <- max(df$fu_dur_days)/365.25
    max_exit_date <- max(df$exit_date)
    # fu_SD
    age_at_ID_mean <-mean(df$age_at_entry_date)
    age_at_ID_SD   <-sd(df$age_at_entry_date)
    # age_at_ID_SD <- do we calculate SD if we are calculating mean
    
    # If count < 5 and mask = T, then instead of actual count, write "count=<5
    if(mask == TRUE){
      
      if(sum(df$age_groups == "12-20.99")<=5) {age_at_ID_12_20.99_count<-"count=<5"} else {age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99")} 
      if(sum(df$age_groups == "21-30.99")<=5) {age_at_ID_21_30.99_count<-"count=<5"} else {age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99")} 
      if(sum(df$age_groups == "31-40.99")<=5) {age_at_ID_31_40.99_count<-"count=<5"} else {age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99")}  
      if(sum(df$age_groups == "41-55.99")<=5) {age_at_ID_41_55.99_count<-"count=<5"} else {age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99")} 
      # Masking
      if (age_at_ID_12_20.99_count == "count=<5" | age_at_ID_21_30.99_count== "count=<5" |  age_at_ID_31_40.99_count == "count=<5" | age_at_ID_41_55.99_count == "count=<5") {
        print("Masked values. Percentages cannot be calculated!")
        # Creates dataframe
        names <- c("Follow-up, years - median",
                   "Follow-up, years - IQR",
                   "Follow-up, years - min",
                   "Follow-up, years - max",
                   "Max exit date",
                   "Age at index date (study entry) - mean",
                   "Age at index date (study entry) - sd",
                   "12.0-20.99 years_count",
                   "21.0-30.99 years_count",
                   "31.0-40.99 years_count",
                   "41.0-55.99 years_count")
        
        values <- c(as.character(round(fu_median,1)),
                    as.character(round(fu_IQR,1)),
                    as.character(round(fu_min,2)),
                    as.character(round(fu_max,2)),
                    as.character(max_exit_date),
                    as.character(round(age_at_ID_mean,1)),
                    as.character(round(age_at_ID_SD,1)),
                    as.character(age_at_ID_12_20.99_count),
                    as.character(age_at_ID_21_30.99_count),
                    as.character(age_at_ID_31_40.99_count),
                    as.character(age_at_ID_41_55.99_count))
      } else { # Performs counts
        age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99")
        age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99")
        age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99")
        age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99")
        # Calculates percentages
        age_at_ID_12_20.99_perc  <- (age_at_ID_12_20.99_count/nrow(df)) * 100
        age_at_ID_21_30.99_perc  <- (age_at_ID_21_30.99_count/nrow(df)) * 100
        age_at_ID_31_40.99_perc  <- (age_at_ID_31_40.99_count/nrow(df)) * 100
        age_at_ID_41_55.99_perc  <- (age_at_ID_41_55.99_count/nrow(df)) * 100
        
        # Create dataframe
        names <- c("Follow-up, years - median",
                   "Follow-up, years - IQR",
                   "Follow-up, years - min",
                   "Follow-up, years - max",
                   "Max exit date",
                   "Age at index date (study entry) - mean",
                   "Age at index date (study entry) - sd",
                   "12.0-20.99 years_count",
                   "12.0-20.99 years_perc",
                   "21.0-30.99 years_count",
                   "21.0-30.99 years_perc",
                   "31.0-40.99 years_count",
                   "31.0-40.99 years_perc",
                   "41.0-55.99 years_count",
                   "41.0-55.99 years_perc")
        
        values <- c(as.character(round(fu_median,1)),
                    as.character(round(fu_IQR,1)),
                    as.character(round(fu_min,2)),
                    as.character(round(fu_max,2)),
                    as.character(max_exit_date),
                    as.character(round(age_at_ID_mean,1)),
                    as.character(round(age_at_ID_SD,1)),
                    as.character(age_at_ID_12_20.99_count),
                    as.character(round(age_at_ID_12_20.99_perc,1)),
                    as.character(age_at_ID_21_30.99_count),
                    as.character(round(age_at_ID_21_30.99_perc,1)),
                    as.character(age_at_ID_31_40.99_count),
                    as.character(round(age_at_ID_31_40.99_perc,1)),
                    as.character(age_at_ID_41_55.99_count),
                    as.character(round(age_at_ID_41_55.99_perc),1))}
      
    } else {
      age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99") 
      age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99")
      age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99") 
      age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99")
      # Calculates percentages
      age_at_ID_12_20.99_perc  <- (age_at_ID_12_20.99_count/nrow(df)) * 100
      age_at_ID_21_30.99_perc  <- (age_at_ID_21_30.99_count/nrow(df)) * 100
      age_at_ID_31_40.99_perc  <- (age_at_ID_31_40.99_count/nrow(df)) * 100
      age_at_ID_41_55.99_perc  <- (age_at_ID_41_55.99_count/nrow(df)) * 100
      
      # Create dataframe
      names <- c("Follow-up, years - median",
                 "Follow-up, years - IQR",
                 "Follow-up, years - min",
                 "Follow-up, years - max",
                 "Max exit date",
                 "Age at index date (study entry) - mean",
                 "Age at index date (study entry) - sd",
                 "12.0-20.99 years_count",
                 "12.0-20.99 years_perc",
                 "21.0-30.99 years_count",
                 "21.0-30.99 years_perc",
                 "31.0-40.99 years_count",
                 "31.0-40.99 years_perc",
                 "41.0-55.99 years_count",
                 "41.0-55.99 years_perc")
      
      values <- c(as.character(round(fu_median,1)),
                  as.character(round(fu_IQR,1)),
                  as.character(round(fu_min,2)),
                  as.character(round(fu_max,2)),
                  as.character(max_exit_date),
                  as.character(round(age_at_ID_mean,1)),
                  as.character(round(age_at_ID_SD,1)),
                  as.character(age_at_ID_12_20.99_count),
                  as.character(round(age_at_ID_12_20.99_perc,1)),
                  as.character(age_at_ID_21_30.99_count),
                  as.character(round(age_at_ID_21_30.99_perc,1)),
                  as.character(age_at_ID_31_40.99_count),
                  as.character(round(age_at_ID_31_40.99_perc,1)),
                  as.character(age_at_ID_41_55.99_count),
                  as.character(round(age_at_ID_41_55.99_perc),1))
    }
    # Creates baseline table
    baseline <- data.table(names, values)
    # Saves files
    print(paste("Saving baseline table: ", pop_prefix, "_", names(all_dfs_meds[i])))
    saveRDS(baseline, paste0(baseline_tables_dir,"/", pop_prefix, "_", names(all_dfs_meds[i]),"_baseline.rds"))
  } else {
    print(paste("There are no records for: ", names(all_dfs_meds[i])))
  }
}


# rm(all_dfs_meds, df, baseline, study_pop_first_occurrence, study_pop_ret, study_pop_ret_D05BB02, study_pop_ret_D10BA01, study_pop_ret_D11AH04, study_pop_ret_unique, study_pop_val, study_pop_val_unique)
