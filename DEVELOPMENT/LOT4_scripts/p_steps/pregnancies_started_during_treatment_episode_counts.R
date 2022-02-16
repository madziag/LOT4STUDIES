#Author: Magda Gamba M.D.
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
denominator_file <- list.files(output_dir, pattern = paste0(pop_prefix,"_denominator.rds"))
# Loads denominator file 
denominator <- readRDS(paste0(output_dir, denominator_file))
# Split Y-M variable to year - month columns (for merging later)
denominator[, c("year", "month") := tstrsplit(YM, "-", fixed=TRUE)]
denominator[,year:=as.integer(year)][,month:=as.integer(month)]
### Creates empty df for expanding counts files (when not all month-year combinations have counts)
empty_df <- as.data.table(expand.grid(seq(min(denominator$year), max(denominator$year)), seq(1, 12)))
names(empty_df) <- c("year", "month")
# Clean up
rm(denominator)

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
      # Masking
      preg_start_during_tx_counts$masked_num <- ifelse(preg_start_during_tx_counts$N < 5 & preg_start_during_tx_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
      if(mask == T){preg_start_during_tx_counts[preg_start_during_tx_counts$masked_num == 1,]$N <- 5} else {preg_start_during_tx_counts[preg_start_during_tx_counts$masked_num == 1,]$N <- preg_start_during_tx_counts[preg_start_during_tx_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
      ############################
      ##### Calculates Rates #####
      ############################
      preg_start_during_tx_counts <- within(preg_start_during_tx_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      # Load prevalence counts
      prevalent_counts <- readRDS(prevalent_counts_files[grepl(gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), prevalent_counts_files)])
      prevalent_counts <- prevalent_counts[,-c("Freq", "rates", "masked")]
      setnames(prevalent_counts, "N", "Freq")
      preg_start_during_tx_counts <- merge(x = preg_start_during_tx_counts, y = prevalent_counts, by = c("YM"), all.x = TRUE) # Merge with med counts
      preg_start_during_tx_counts <- preg_start_during_tx_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
      preg_start_during_tx_counts$rates[is.nan(preg_start_during_tx_counts$rates)]<-0
      preg_start_during_tx_counts <- preg_start_during_tx_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
      setnames(preg_start_during_tx_counts, "masked_num", "masked")
      # Save files 
      saveRDS(tx_episodes_preg, paste0(counts_dfs_dir, gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_all_preg_starts_during_tx_episodes.rds"))
      saveRDS(preg_start_during_tx_counts, paste0(preg_med_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_all_preg_starts_during_tx_episodes_counts.rds"))
      
      #### Taking into account highest_quality column in pregnancy df - Counts ####
      # Get the unique value of the highest quality column
      hq_unique <- unique(tx_episodes_preg$highest_quality)
      
      for (j in 1:length(hq_unique)){
        # Create a subset of the unique value
        tx_episodes_preg_unique <- tx_episodes_preg[which(highest_quality==hq_unique[j]),]
        # Performs counts 
        preg_start_during_tx_unique_counts <- tx_episodes_preg_unique[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date))] # Performs counts grouped by year, month of medicine prescription date
        preg_start_during_tx_unique_counts <- as.data.table(merge(x = empty_df, y = preg_start_during_tx_unique_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with preg_start_during_tx_unique_counts
        preg_start_during_tx_unique_counts[is.na(preg_start_during_tx_unique_counts[,N]), N:=0] # Fills in missing values with 0
        # Masking
        preg_start_during_tx_unique_counts$masked_num <- ifelse(preg_start_during_tx_unique_counts$N < 5 & preg_start_during_tx_unique_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
        if(mask == T){preg_start_during_tx_unique_counts[preg_start_during_tx_unique_counts$masked_num == 1,]$N <- 5} else {preg_start_during_tx_unique_counts[preg_start_during_tx_unique_counts$masked_num == 1,]$N <- preg_start_during_tx_unique_counts[preg_start_during_tx_unique_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
        ############################
        ##### Calculates Rates #####
        ############################
        preg_start_during_tx_unique_counts <- within(preg_start_during_tx_unique_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
        # Load prevalence counts
        prevalent_counts <- readRDS(prevalent_counts_files[grepl(gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), prevalent_counts_files)])
        prevalent_counts <- prevalent_counts[,-c("Freq", "rates", "masked")]
        setnames(prevalent_counts, "N", "Freq")
        preg_start_during_tx_unique_counts <- merge(x = preg_start_during_tx_unique_counts, y = prevalent_counts, by = c("YM"), all.x = TRUE) # Merge with med counts
        preg_start_during_tx_unique_counts <- preg_start_during_tx_unique_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
        preg_start_during_tx_unique_counts$rates[is.nan(preg_start_during_tx_unique_counts$rates)]<-0
        preg_start_during_tx_unique_counts <- preg_start_during_tx_unique_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
        setnames(preg_start_during_tx_unique_counts, "masked_num", "masked")
        # Save files 
        saveRDS(tx_episodes_preg_unique, paste0(counts_dfs_dir, gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_hq_", hq_unique[j], "_preg_start_during_tx_episodes.rds"))
        saveRDS(preg_start_during_tx_unique_counts, paste0(preg_med_counts_dir,"/",gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_hq_", hq_unique[j], "_preg_starts_during_tx_episodes_counts.rds"))
      }
    } else {
      print(paste0(gsub("_CMA_treatment_episodes.rds", "",tx_episodes_files[i]), " study: There are no patients with pregnancy start dates that fall between episode start and end dates!"))
    }
  }
} else {
  print("No pregancy records have been found")
}


