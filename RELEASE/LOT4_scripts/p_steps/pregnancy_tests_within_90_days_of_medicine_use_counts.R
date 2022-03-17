#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 07/02/2022

##################################################################################################################################################
##################################################### OBJECTIVES: 2.1, 2.2 #######################################################################
##################################################################################################################################################

## Objective 2.1: Proportion of Retinoid/Valproate users with pregnancy test performed within 90 days before medication record
# Numerator -> Number of Retinoid/Valproate records with a pregnancy test record (from events, procedures and medical observation tables) within 90 days before medication record
# Denominator -> Total number of Retinoid/Valproate records that month (duplicates removed )
# Intermediate data set -> data set of dispensed/prescribed Retinoid/Valproates per month with column indicating whether there was a prior pregnancy test 
# Records needed -> pregnancy test records (combination of pregtest records from events, procedures and medical observations) 2. Retinoid/Valproate records 

## Objective 2.2: Proportion of Retinoid/Valproate users with pregnancy test performed within 90 days after medication record
# Numerator -> Number of Retinoid/Valproate records with a pregnancy test record (from events, procedures and medical observation tables) within 90 days before medication record
# Denominator -> Total number of Retinoid/Valproate records that month (duplicates removed )
# Intermediate data set -> data set of dispensed/prescribed Retinoid/Valproates per month with column indicating whether there was a prior pregnancy test 
# Records needed -> pregnancy test records (combination of pregtest records from events, procedures and medical observations) 2. Retinoid/Valproate records 

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

### Loads records needed 
# 1. Pregnancy test records 
# Looks for pregnancy test files in tmp folder 
pregtest_files <- list.files(tmp, pattern = paste0(c("pregtest.rds", "preg_test.rds"), collapse = "|"), recursive = T, ignore.case = T, full.names = T)
# Filters by current subpopulation 
pregtest_files <- pregtest_files[grepl(pop_prefix, pregtest_files)]

if(populations[pop] == "PC_study_population.rds"){
  pregtest_files <- list.files(tmp, pattern = paste0(c("pregtest.rds", "preg_test.rds"), collapse = "|"), recursive = T, ignore.case = T, full.names = T)
  pregtest_files <- pregtest_files[!grepl("PC_HOSP", pregtest_files)]
}
# 2. Retinoid/Valproate records 
# Looks for Retinoid/Valproate records in medications folder - this is done in wrapper script run_counts_final_each_pop.R
# name of variable with list of medicines available -> med_files

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
empty_df<-as.data.table(expand.grid(seq(min(denominator$year), max(denominator$year)), seq(1, 12)))
names(empty_df) <- c("year", "month")

# Checks first if there are any pregnancy test records found
if(length(pregtest_files)>0) {
  # Loads files + clean up
  pregtest_df <- do.call(rbind,lapply(pregtest_files, readRDS)) # Loads file
  # pregtest_df <- pregtest_df[,c("person_id", "Date", "Code", "Vocabulary")] # Keeps necessary columns 
  setnames(pregtest_df, "Date", "Pregtest_date") # Renames column 
  setnames(pregtest_df, "Code", "Pregtest_code") # Renames column 
  setnames(pregtest_df, "Vocabulary", "Pregtest_vocabulary") # Renames column
  pregtest_df[,month:=month(Pregtest_date)][,year:=year(Pregtest_date)] # Creates month/year cols (for the purposes of deduplicating - only 1 pregnancy test record per person_id per month/year is allowed)
  pregtest_df <- pregtest_df[!duplicated(pregtest_df[,c("person_id", "month", "year")]),] # Removes duplicates
  pregtest_df <- pregtest_df[,-c("month", "year")]
  
  ##### Loops over medication files to create counts per medication type 
  for(i in 1:length(med_files)){
    ## Loads the medication record
    med_df <- readRDS(paste0(medications_pop, med_files[i])) # Loads file
    med_df <- med_df[Date>=entry_date & Date<exit_date] # Get med records only between entry and exit dates 
    med_df <- med_df[ ,c("person_id", "Date", "Code")] # Keeps necessary columns
    setnames(med_df, "Code", "ATC") # Renames column 
    
    ### Creates denominator: Total number of Retinoid/Valproate records per month
    med_counts <- med_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
    med_counts <- as.data.table(merge(x = empty_df, y = med_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with med_counts
    med_counts[is.na(med_counts[,N]), N:=0] # Fills in missing values with 0
    setnames(med_counts, "N", "Freq") # Renames column
    # Masking
    med_counts$masked_den <- ifelse(med_counts$Freq < 5 & med_counts$Freq > 0, 1, 0) # Creates column that indicates if count value will be masked_den if mask = TRUE
    if(mask == T){med_counts[med_counts$masked_den == 1,]$Freq <- 5} else {med_counts[med_counts$masked_den == 1,]$Freq <- med_counts[med_counts$masked_den == 1,]$Freq} # Changes values less than 5 and more than 0 to 5
    ### Creates numerators
    # Merges pregtest df with medication df 
    pregtest_med_df <- pregtest_df[med_df, on = .(person_id), allow.cartesian = T] # Left join
    # Creates pregtest_prior and pregtest_after columns 
    pregtest_med_df[,pregtest_prior:=ifelse(Date - Pregtest_date >= 0 & Date - Pregtest_date <= 90 , 1, 0)]
    pregtest_med_df[,pregtest_after:=ifelse(Pregtest_date - Date >= 0 & Pregtest_date - Date <= 90 , 1, 0)] 
    # Changes NA values in new columns to 0
    pregtest_med_df$pregtest_prior[is.na(pregtest_med_df$pregtest_prior)] <- 0
    pregtest_med_df$pregtest_after[is.na(pregtest_med_df$pregtest_after)] <- 0
    # Creates subsets where pregtest_prior or pregtest_after == 1
    pregtest_prior_df <- pregtest_med_df[pregtest_med_df$pregtest_prior == 1]
    pregtest_prior_df[,pregtest_after := NULL]
    pregtest_after_df <- pregtest_med_df[pregtest_med_df$pregtest_after == 1]
    pregtest_after_df[,pregtest_prior := NULL]
    # Performs counts if df created is not empty 
    # Pregtest prior
    if (nrow(pregtest_prior_df)>0){
      # Counts
      pregtest_prior_counts <- pregtest_prior_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
      pregtest_prior_counts <- as.data.table(merge(x = empty_df, y = pregtest_prior_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with pregtest_prior_counts
      pregtest_prior_counts[is.na(pregtest_prior_counts[,N]), N:=0] # Fills in missing values with 0
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      pregtest_prior_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking
      pregtest_prior_counts$masked_num <- ifelse(pregtest_prior_counts$N < 5 & pregtest_prior_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
      if(mask == T){pregtest_prior_counts[pregtest_prior_counts$masked_num == 1,]$N <- 5} else {pregtest_prior_counts[pregtest_prior_counts$masked_num == 1,]$N <- pregtest_prior_counts[pregtest_prior_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
      # Rate calculation
      pregtest_prior_counts <- within(pregtest_prior_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      pregtest_prior_counts <- merge(x = pregtest_prior_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
      pregtest_prior_counts <- pregtest_prior_counts[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
      pregtest_prior_counts <- pregtest_prior_counts[,c("YM", "N", "Freq", "rates", "masked_num", "true_value")]
      setnames(pregtest_prior_counts, "masked_num", "masked")
      ## Saves intermediate (to counts_df folder) and monthly count files (to pregnancy_counts folder)
      saveRDS(pregtest_prior_df, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_pgtests_prior.rds")) # Intermediate file
      saveRDS(pregtest_prior_counts, paste0(pregnancy_test_counts_dir, "/", gsub(".rds", "", med_files[i]), "_pgtests_prior_counts.rds")) # Monthly counts file 
      
    } else {
      print(paste0("No pregnancy tests were found within 90 days before ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use." ))
    }
    # Pregtest after
    if (nrow(pregtest_after_df)>0){
      # Counts
      pregtest_after_counts <- pregtest_after_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
      pregtest_after_counts <- as.data.table(merge(x = empty_df, y = pregtest_after_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with pregtest_after_counts
      pregtest_after_counts[is.na(pregtest_after_counts[,N]), N:=0] # Fills in missing values with 0
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      pregtest_after_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking
      pregtest_after_counts$masked_num <- ifelse(pregtest_after_counts$N < 5 & pregtest_after_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
      if(mask == T){pregtest_after_counts[pregtest_after_counts$masked_num == 1,]$N <- 5} else {pregtest_after_counts[pregtest_after_counts$masked_num == 1,]$N <- pregtest_after_counts[pregtest_after_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
      # Rate calculation
      pregtest_after_counts <- within(pregtest_after_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      pregtest_after_counts <- merge(x = pregtest_after_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
      pregtest_after_counts <- pregtest_after_counts[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
      pregtest_after_counts <- pregtest_after_counts[,c("YM", "N", "Freq", "rates", "masked_num", "true_value")]
      setnames(pregtest_after_counts, "masked_num", "masked")
      ## Saves intermediate (to counts_df folder ) and monthly count files (to pregnancy_counts folder)
      saveRDS(pregtest_after_df, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_pgtests_after.rds")) # Intermediate file
      saveRDS(pregtest_after_counts, paste0(pregnancy_test_counts_dir, "/", gsub(".rds", "", med_files[i]), "_pgtests_after_counts.rds")) # Monthly counts file 
    } else {
      print(paste0("No pregnancy tests were found within 90 days after ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use." ))
    }
  }
} else {
  print("There are no Pregnancy Test records available!")
}


# Clean up 
rm(list = grep("^pregtest", ls(), value = TRUE))
rm(med_df)
