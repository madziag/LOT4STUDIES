# Assign study population (for subpops/regions)
if (multiple_regions == T ){study_pop_all <- study_pop_reg} else {study_pop_all <- study_population}

# Create Columns for ALL STUDY POPULATION 
# Create fu_dur_days col: exit_date 
study_pop_all[,entry_date:=as.IDate(entry_date,"%Y%m%d")] # Transform to date variables
study_pop_all[,exit_date:=as.IDate(exit_date, "%Y%m%d")] # Transform to date variables

# fu duration 
study_pop_all[, fu_dur_days := exit_date - entry_date]
# Create age variable = entry_date - birth date  (# Round down)
study_pop_all[, age_at_entry_date := floor((entry_date - birth_date)/365)]
study_pop_all[,age_groups:= ifelse(study_pop_all[,age_at_entry_date >= 12 & age_at_entry_date < 19], "12-18.9", 
                                   ifelse(study_pop_all[,age_at_entry_date < 26], "19-25.9",
                                          ifelse(study_pop_all[,age_at_entry_date < 36],  "26-35.9",
                                                 ifelse(study_pop_all[,age_at_entry_date < 46], "36-45.9",
                                                        ifelse(study_pop_all[,age_at_entry_date < 56], "46-55.9", "Not in range" )))))]


# Create Columns for RETINOID/VALPROATE/BOTH populations
study_pop_meds[,entry_date:=as.IDate(entry_date,"%Y%m%d")] # Transform to date variables
study_pop_meds[,exit_date:=as.IDate(exit_date, "%Y%m%d")] # Transform to date variables
# fu duration 
study_pop_meds[, fu_dur_days := exit_date - entry_date]
# Create age variable = entry_date - birth date  (# Round down)
study_pop_meds[, age_at_entry_date := floor((entry_date - birth_date)/365)]
study_pop_meds[,age_groups:= ifelse(study_pop_meds[,age_at_entry_date >= 12 & age_at_entry_date < 19], "12-18.9", 
                                    ifelse(study_pop_meds[,age_at_entry_date < 26], "19-25.9",
                                           ifelse(study_pop_meds[,age_at_entry_date < 36],  "26-35.9",
                                                  ifelse(study_pop_meds[,age_at_entry_date < 46], "36-45.9",
                                                         ifelse(study_pop_meds[,age_at_entry_date < 56], "46-55.9", "Not in range" )))))]

# Create column that describes if ATC is Retinoid, Valproate or Unknowns
study_pop_meds[,med_type := ifelse(study_pop_meds[,Code %chin% c("D05BB02", "D11AH04", "D10BA01")], "Retinoid",
                                   ifelse(study_pop_meds[,Code %chin% c("N03AG01","N03AG02")], "Valproate", "Unknown"))]

# Choose first use of medication per ATC code & patid -> ALL unique person_id/ATC codes records 
study_pop_first_occurrence  <- setDT(study_pop_meds)[order(event_date), head(.SD, 1L), by = c("person_id", "Code")]
# Create Subsets 
if (study_type == "Retinoids"){
  study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
  study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
  # Retinoids - subgroups
  study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
  study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
  study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
  
  all_dfs_meds <- list(study_pop_all, study_pop_ret_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
  names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
  
} else if (study_type == "Valproates"){
  study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
  study_pop_val_unique <- unique(study_pop_val, by = "person_id")
  
  all_dfs_meds <- list(study_pop_all, study_pop_val_unique)
  names(all_dfs_meds) <- c("All Users", "Valproates Only")
  
} else if (study_type == "Both"){
  study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
  study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
  study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
  study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
  study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
  
  study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
  study_pop_val_unique <- unique(study_pop_val, by = "person_id")
  
  all_dfs_meds <- list(study_pop_all, study_pop_ret_unique, study_pop_val_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
  names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Valproates Only","Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
  
}

# Loop throoufh all the subsets depending on the study_type and create baseline tables 
for (i in 1:length(all_dfs_meds)){
  df <- all_dfs_meds[[i]]
  if(nrow(df > 0)){
    ################## BASELINE ALL POPULATION ########################
    # Calculate median of followup in years 
    fu_median <-  median(df$fu_dur_days)/365
    fu_IQR <- IQR(df$fu_dur_days)/365
    # fu_SD <- 
    age_at_ID_mean <-mean(df$age_at_entry_date)
    age_at_ID_SD   <-sd(df$age_at_entry_date)
    # age_at_ID_SD <- do we calculate SD if we are calculating mean
    if(sum(df$age_groups == "12-18.9")>5) {age_at_ID_12_18.9_count <- sum(df$age_groups == "12-18.9")} else {age_at_ID_12_18.9_count<-"count=<5"}
    if(sum(df$age_groups == "19-25.9")>5) {age_at_ID_19_25.9_count <- sum(df$age_groups == "19-25.9")} else {age_at_ID_19_25.9_count<-"count=<5"}
    if(sum(df$age_groups == "26-35.9")>5) {age_at_ID_26_35.9_count <- sum(df$age_groups == "26-35.9")} else {age_at_ID_26_35.9_count<-"count=<5"}
    if(sum(df$age_groups == "36-45.9")>5) {age_at_ID_36_45.9_count <- sum(df$age_groups == "36-45.9")} else {age_at_ID_36_45.9_count<-"count=<5"}    
    if(sum(df$age_groups == "46-55.9")>5) {age_at_ID_46_55.9_count <- sum(df$age_groups == "46-55.9")} else {age_at_ID_46_55.9_count<-"count=<5"}
    
    # print("NAs will be generated for masked counts- ignore error")
    
    if (age_at_ID_12_18.9_count == "count=<5" | age_at_ID_19_25.9_count == "count=<5" |  age_at_ID_26_35.9_count == "count=<5" | age_at_ID_36_45.9_count == "count=<5" | age_at_ID_46_55.9_count == "count=<5") {
      print("Masked values. Percentages cannot be calculated!")
      
      # Create dataframe
      names <- c("Follow-up, years - median",
                 "Follow-up, years - IQR",
                 "Age at index date (study entry) - mean", 
                 "Age at index date (study entry) - sd",
                 "12.0-18.9 years_count",
                 "19.0-25.9 years_count",  
                 "26.0-35.9 years_count",     
                 "36.0-45.9 years_count",      
                 "46.0-55.9 years_count")
      
      values <- c(as.character(round(fu_median,1)), 
                  as.character(round(fu_IQR,1)), 
                  as.character(round(age_at_ID_mean,1)), 
                  as.character(round(age_at_ID_SD,1)), 
                  as.character(age_at_ID_12_18.9_count), 
                  as.character(age_at_ID_19_25.9_count),  
                  as.character(age_at_ID_26_35.9_count),     
                  as.character(age_at_ID_36_45.9_count),      
                  as.character(age_at_ID_46_55.9_count))
    }else {
      age_at_ID_12_18.9_prec  <- (age_at_ID_12_18.9_count/nrow(df)) * 100
      age_at_ID_19_25.9_prec  <- (age_at_ID_19_25.9_count/nrow(df)) * 100
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
      
      values <- c(as.character(round(fu_median,1)), 
                  as.character(round(fu_IQR,1)), 
                  as.character(round(age_at_ID_mean,1)), 
                  as.character(round(age_at_ID_SD,1)), 
                  as.character(age_at_ID_12_18.9_count), 
                  as.character(round(age_at_ID_12_18.9_prec,1)), 
                  as.character(age_at_ID_19_25.9_count),  
                  as.character(round(age_at_ID_19_25.9_prec,1)), 
                  as.character(age_at_ID_26_35.9_count),     
                  as.character(round(age_at_ID_26_35.9_prec,1)), 
                  as.character(age_at_ID_36_45.9_count),      
                  as.character(round(age_at_ID_36_45.9_prec,1)), 
                  as.character(age_at_ID_46_55.9_count),     
                  as.character(round(age_at_ID_46_55.9_prec),1))
    }
    
    baseline <- data.table(names, values)
    
    if (multiple_regions == T){
      if (SUBP == TRUE){
        pop_names <- gsub(".rds", "", populations[pop])
        print(paste("Saving baseline table: ", pop_names, names(all_dfs_meds[i])))
        saveRDS(baseline, paste0(baseline_dir,"/", pop_names, "_", names(all_dfs_meds[i]),"_baseline.rds"))
        write.csv(baseline, paste0(baseline_dir ,"/", pop_names, "_", names(all_dfs_meds[i]),"_baseline.csv"))
        
      } else {
        print(paste("Saving baseline table: ", names(all_dfs_meds[i])))
        saveRDS(baseline, paste0(baseline_dir,"/", names(all_dfs_meds[i]),"_baseline.rds"))
        write.csv(baseline, paste0(baseline_dir ,"/",  names(all_dfs_meds[i]),"_baseline.csv"))
      }
      
    }else{
      # Save files 
      if (SUBP == TRUE){
        pop_names <- gsub(".rds", "", populations[pop])
        print(paste("Saving baseline table: ", pop_names, names(all_dfs_meds[i])))
        saveRDS(baseline, paste0(baseline_tables_dir,"/", pop_names, "_", names(all_dfs_meds[i]),"_baseline.rds"))
        write.csv(baseline, paste0(baseline_tables_dir ,"/", pop_names, "_", names(all_dfs_meds[i]),"_baseline.csv"))
        
      } else {
        print(paste("Saving baseline table: ", names(all_dfs_meds[i])))
        saveRDS(baseline, paste0(baseline_tables_dir,"/", names(all_dfs_meds[i]),"_baseline.rds"))
        write.csv(baseline, paste0(baseline_tables_dir ,"/",  names(all_dfs_meds[i]),"_baseline.csv"))
      }
    }
    
  } else {
    print(paste("There are no records for: ", names(all_dfs_meds[i])))
  }
}





