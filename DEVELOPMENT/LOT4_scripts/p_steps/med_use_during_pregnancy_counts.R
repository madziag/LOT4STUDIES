#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/01/2022

##################################################################################################################################################
###################################################### OBJECTIVE: 3.2 ############################################################################
##################################################################################################################################################

## Objective 3.2: Proportion of Retinoid/Valproate records that occurred during a pregnancy 
# Numerator -> Number of Retinoid/Valproate records that occurred during a pregnancy
# Denominator -> Number of eligible subjects that month (main denominator)
# Records needed -> 1. Pregnancy records (created by ARS Toscana script) 2. Total number of female subjects in cohort for at least 1 day in the month (denominator file)

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

### Creates empty df for expanding counts files (when not all month-year combinations have counts) - uses denominator file min and max year values 
# Looks for denominator file in output directory 
denominator_file <- list.files(tmp, pattern = paste0(pop_prefix,"_denominator.rds"))
# Loads denominator file 
denominator <- readRDS(paste0(tmp, denominator_file))
# Split Y-M variable to year - month columns (for merging later)
denominator[, c("year", "month") := tstrsplit(YM, "-", fixed=TRUE)]
denominator[,year:=as.integer(year)][,month:=as.integer(month)]
### Creates empty df for expanding counts files (when not all month-year combinations have counts)
empty_df <- as.data.table(expand.grid(seq(min(denominator$year), max(denominator$year)), seq(1, 12)))
names(empty_df) <- c("year", "month")

if (nrow(D3_pregnancy_reconciled)>0){
  # For each treatment episode file 
  for (i in 1:length(med_files)){ 
    ## Loads the medication record
    med_df <- as.data.table(readRDS(paste0(medications_pop, med_files[i]))) # Loads file
    med_df <- med_df[ ,c("person_id", "Date", "Code")] # Keeps necessary columns
    setnames(med_df, "Code", "ATC") # Renames column
    # Merge med file with pregnancy records 
    med_preg <- as.data.table(D3_pregnancy_reconciled[med_df, on = .(person_id), allow.cartesian = T]) # Left join
    # Delete records without pregnancy records
    med_preg <-  med_preg[!is.na(pregnancy_start_date),]
    # Remove duplicates
    med_preg <- med_preg[!duplicated(med_preg[,c("person_id", "pregnancy_start_date", "pregnancy_end_date", "highest_quality")])]
    # Creates column that indicates if medicine record date is between episode.start and episode.end dates
    med_preg[,med_use_during_preg:= fifelse(Date>=pregnancy_start_date & Date<=pregnancy_end_date, 1, 0)] 
    # Creates df of patients who have a pregnancy start date between tx episode.start and episode.end dates
    med_use_during_preg <- med_preg[med_use_during_preg == 1,] 
    # Checks if there are any records that meet the criteria. If so it does the calculations
    if (nrow(med_use_during_preg) > 0){
      # Performs counts 
      med_use_during_preg_counts <- med_use_during_preg[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date))] # Performs counts grouped by year, month of medicine prescription date
      med_use_during_preg_counts <- as.data.table(merge(x = empty_df, y = med_use_during_preg_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with med_use_during_preg_counts
      med_use_during_preg_counts[is.na(med_use_during_preg_counts[,N]), N:=0] # Fills in missing values with 0
      # Masking
      med_use_during_preg_counts$masked_num <- ifelse(med_use_during_preg_counts$N < 5 & med_use_during_preg_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
      if(mask == T){med_use_during_preg_counts[med_use_during_preg_counts$masked_num == 1,]$N <- 5} else {med_use_during_preg_counts[med_use_during_preg_counts$masked_num == 1,]$N <- med_use_during_preg_counts[med_use_during_preg_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
      ############################
      ##### Calculates Rates #####
      ############################
      med_use_during_preg_counts <- within(med_use_during_preg_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      med_use_during_preg_counts <- merge(x = med_use_during_preg_counts, y = denominator, by = c("YM"), all.x = TRUE) # Merge with med counts
      med_use_during_preg_counts <- med_use_during_preg_counts[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
      med_use_during_preg_counts <- med_use_during_preg_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
      setnames(med_use_during_preg_counts, "masked_num", "masked")
      # Save files 
      saveRDS(med_use_during_preg, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_med_use_during_pregnancy.rds"))
      saveRDS(med_use_during_preg_counts, paste0(preg_med_counts_dir,"/", gsub(".rds", "", med_files[i]), "_med_use_during_pregnancy_counts.rds"))
      #### Taking into account highest_quality column in pregnancy df - Counts ####
      # Get the unique value of the highest quality column
      hq_unique <- unique(med_use_during_preg$highest_quality)
      for (j in 1:length(hq_unique)){
        # Create a subset of the unique value
        med_use_during_preg_unique <- med_use_during_preg[which(highest_quality==hq_unique[j]),]
        # Performs counts 
        med_use_during_preg_unique_counts <- med_use_during_preg_unique[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date))] # Performs counts grouped by year, month of medicine prescription date
        med_use_during_preg_unique_counts <- as.data.table(merge(x = empty_df, y = med_use_during_preg_unique_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with med_use_during_preg_unique_counts
        med_use_during_preg_unique_counts[is.na(med_use_during_preg_unique_counts[,N]), N:=0] # Fills in missing values with 0
        # Masking
        med_use_during_preg_unique_counts$masked_num <- ifelse(med_use_during_preg_unique_counts$N < 5 & med_use_during_preg_unique_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
        if(mask == T){med_use_during_preg_unique_counts[med_use_during_preg_unique_counts$masked_num == 1,]$N <- 5} else {med_use_during_preg_unique_counts[med_use_during_preg_unique_counts$masked_num == 1,]$N <- med_use_during_preg_unique_counts[med_use_during_preg_unique_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
        ############################
        ##### Calculates Rates #####
        ############################
        med_use_during_preg_unique_counts <- within(med_use_during_preg_unique_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
        med_use_during_preg_unique_counts <- merge(x = med_use_during_preg_unique_counts, y = denominator, by = c("YM"), all.x = TRUE) # Merge with med counts
        med_use_during_preg_unique_counts <- med_use_during_preg_unique_counts[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
        med_use_during_preg_unique_counts <- med_use_during_preg_unique_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
        setnames(med_use_during_preg_unique_counts, "masked_num", "masked")
        # Save files 
        saveRDS(med_use_during_preg_unique, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_hq_", hq_unique[j], "_med_use_during_pregnancy.rds"))
        saveRDS(med_use_during_preg_unique_counts, paste0(preg_med_counts_dir,"/",gsub(".rds", "", med_files[i]), "_hq_", hq_unique[j], "_med_use_during_pregnancy_counts.rds"))
      }
    } else {
      print(paste0(gsub(".rds", "",med_files[i]), " study: No medicine use during a pregnancy found."))
    }
  }
} else {
  print("No pregnancy records have been found")
}


