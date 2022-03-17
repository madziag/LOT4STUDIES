# Looks for treatment files in treatment episodes folder 
tx_episodes_files <- list.files(paste0(g_intermediate, "treatment_episodes/"), pattern = "treatment_episodes")
# Excludes already contraceptive episode files
tx_episodes_files <- tx_episodes_files[!grepl("contraceptives", tx_episodes_files)]

# Includes only current subop 
tx_episodes_files <- tx_episodes_files[grepl(pop_prefix, tx_episodes_files) ]
# Loads pregnancy file
# Loads Pregnancy records 
D3_pregnancy_reconciled <- as.data.table(get(load(paste0(preg_dir, "g_intermediate/D3_pregnancy_reconciled.RData"))))
# Data Cleaning Pregnancy file
D3_pregnancy_reconciled[,person_id:=as.character(person_id)]
D3_pregnancy_reconciled[,pregnancy_start_date:=as.IDate(pregnancy_start_date, "%Y%m%d" )]
D3_pregnancy_reconciled[,pregnancy_end_date:=as.IDate(pregnancy_end_date, "%Y%m%d" )]
D3_pregnancy_reconciled <- D3_pregnancy_reconciled[,c("person_id", "pregnancy_start_date", "highest_quality")]

D3_pregnancy_reconciled[person_id == "ConCDM_SIM_200421_00044"]$person_id <- "ConCDM_SIM_200421_00029"
D3_pregnancy_reconciled[person_id == "ConCDM_SIM_200421_00058"]$person_id <- "ConCDM_SIM_200421_00030"
D3_pregnancy_reconciled[person_id == "ConCDM_SIM_200421_00064"]$person_id <- "ConCDM_SIM_200421_00080"
D3_pregnancy_reconciled[person_id == "ConCDM_SIM_200421_00065"]$person_id <- "ConCDM_SIM_200421_00092"
D3_pregnancy_reconciled[person_id == "ConCDM_SIM_200421_00101"]$person_id <- "ConCDM_SIM_200421_00247"
D3_pregnancy_reconciled[person_id == "ConCDM_SIM_200421_00219"]$person_id <- "ConCDM_SIM_200421_00397"


# Create empty df for complete counts (based of main denominator min and max years)
# Loads denominator file to get min and max dates for empty file
denominator_file <- list.files(output_dir, pattern = paste0(pop_prefix,"_denominator.rds"))
denominator <- readRDS(paste0(output_dir, denominator_file))
denominator[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)]
# Creates empty counts df
empty_counts <- as.data.table(expand.grid(seq(min(denominator$Y), max(denominator$Y)), seq(1, 12)))
names(empty_counts) <- c("year", "month")

# For each treatment episode file 
for (i in 1:length(tx_episodes_files)){
  # Reads in the treatment episodes file 
  df <- as.data.table(readRDS(paste0(g_intermediate, "treatment_episodes/", tx_episodes_files[i])))
  # Reads in study population data
  ##### ->  Study_population has already been read in by the wrapper script, variable name = study_population
  # Adds column that indicates row number (to be used when merging expanded df with original df)
  df <- df[, nrow := .I]
  ##################################################################################################
  ################################## Calculates Prevalence #########################################
  ##################################################################################################
  ### Numerator = Number of female subjects in cohort with valproate/retinoid episode overlapping the month by at least 1 day 
  # Expands df (1 row for every day of treatment per patient) -> gives 2 cols (person_id + tx day)
  df_expanded <- setDT(df)[ , list(idnum = person_id, episode.day = seq(episode.start, episode.end, by = "day")), by = 1:nrow(df)]
  # Merges back with original df
  df_expanded <-  merge(df, df_expanded, by = "nrow")
  # Create year-months columns episode.day
  df_expanded$year <- year(df_expanded$episode.day)
  df_expanded$month <- month(df_expanded$episode.day)
  # Remove episode.day column (we already have the month and year extracted from this column) + other duplicate/unnecessary columns
  df_expanded <- df_expanded[,-c("nrow", "idnum", "episode.day")]
  # Remove duplicate records
  df_expanded <- df_expanded[!duplicated(df_expanded)]
  # Count the number of patients treated per month
  df_prevalence_counts_num <- df_expanded[,.N, by = .(year,month)]
  # Adjust for PHARMO
  if(is_PHARMO){df_prevalence_counts_num <- df_prevalence_counts_num[df_prevalence_counts_num$year < 2020,]} else {df_prevalence_counts_num <- df_prevalence_counts_num[df_prevalence_counts_num$year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  df_prevalence_counts_num <- as.data.table(merge(x = empty_counts, y = df_prevalence_counts_num, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  df_prevalence_counts_num[is.na(df_prevalence_counts_num[,N]), N:=0]
  # Masks values less than 5
  # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
  df_prevalence_counts_num$masked <- ifelse(df_prevalence_counts_num$N < 5 & df_prevalence_counts_num$N > 0, 1, 0)
  # Changes values less than 5 and more than 0 to 5
  if (mask == T){df_prevalence_counts_num[df_prevalence_counts_num$masked == 1,]$N <- 5} else {df_prevalence_counts_num[df_prevalence_counts_num$masked == 1,]$N <- df_prevalence_counts_num[df_prevalence_counts_num$masked == 1,]$N}
  ### Denominator = Total number of female subjects in the cohort for at least 1 day in the month
  ##### ->  denominator file has already been read in before the for loop, variable name = denominator
  ### Merges numerator file with denominator file
  df_prevalence_counts_num <- within(df_prevalence_counts_num, YM<- sprintf("%d-%02d", year, month))
  df_prevalence_counts <- merge(x = df_prevalence_counts_num, y = denominator, by = c("YM"), all.x = TRUE)
  # Calculates rates
  df_prevalence_counts <- df_prevalence_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
  df_prevalence_counts <- df_prevalence_counts[,c("YM", "N", "Freq", "rates", "masked")]
  # Save files
  med_name <- gsub("_treatment_episodes.rds", "", tx_episodes_files[i])
  saveRDS(df_prevalence_counts, (paste0(medicines_counts_dir,"/", med_name, "_prevalence_counts.rds")))
  ##################################################################################################
  ################################## Calculates Incidence ##########################################
  ##################################################################################################
  ### Numerator = Number of female subjects in cohort with a valproate/retinoid episode start in the month 
  # Group by year_month of episode.start.YM
  df_incidence_counts_num <- df[,.N, by = .(year(episode.start),month(episode.start))]
  # Adjust for PHARMO 
  if(is_PHARMO){df_incidence_counts_num <- df_incidence_counts_num[df_incidence_counts_num$year < 2020,]} else {df_incidence_counts_num <- df_incidence_counts_num[df_incidence_counts_num$year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  df_incidence_counts_num <- as.data.table(merge(x = empty_counts, y = df_incidence_counts_num, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  df_incidence_counts_num[is.na(df_incidence_counts_num[,N]), N:=0]
  # Masks values less than 5
  # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
  df_incidence_counts_num$masked <- ifelse(df_incidence_counts_num$N < 5 & df_incidence_counts_num$N > 0, 1, 0)
  # Changes values less than 5 and more than 0 to 5
  if (mask == T){df_incidence_counts_num[df_incidence_counts_num$masked == 1,]$N <- 5} else {df_incidence_counts_num[df_incidence_counts_num$masked == 1,]$N <- df_incidence_counts_num[df_incidence_counts_num$masked == 1,]$N}
  ### Denominator = Total number of female subjects in the cohort for at least 1 day in the month
  ##### ->  denominator file has already been read in before the for loop, variable name = denominator
  ### Merges numerator file with denominator file
  df_incidence_counts_num <- within(df_incidence_counts_num, YM<- sprintf("%d-%02d", year, month))
  df_incidence_counts <- merge(x = df_incidence_counts_num, y = denominator, by = c("YM"), all.x = TRUE)
  # Calculates rates
  df_incidence_counts <-df_incidence_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
  df_incidence_counts <-df_incidence_counts[,c("YM", "N", "Freq", "rates", "masked")]
  # Save files 
  saveRDS(df_incidence_counts, (paste0(medicines_counts_dir,"/", med_name, "_incidence_counts.rds")))
  ##################################################################################################
  ############################# Calculates Discontinuation Rates ###################################
  ##################################################################################################
  ### Numerator = Number of female subjects in cohort who discontinue valproate/retinoid in the month 
  #Join treatment episodes df with study population to get exit date from study for each patient 
  df_with_exit_dates <- merge(x = df[,-c("nrow")], y = study_population[,c("person_id", "exit_date")], by = "person_id", all.x = TRUE)
  # Change dates to be in the correct format 
  df_with_exit_dates[, exit_date := as.Date(exit_date, format="%Y%m%d")]
  # Sort df 
  df_with_exit_dates[order(person_id,episode.start,decreasing=TRUE),]
  # Create a column with lead values of episode.start column (so that episode.end can be on the same row as the next episode.start of the same person)
  df_with_exit_dates[, next.episode.start:= shift(episode.start, type = "lead" ), by = person_id]
  # Create a column that indicates that treatment episode is the only/or last of a series of tx episode for a patient
  df_with_exit_dates[, last.tx.episode_per_pt := ifelse(is.na(next.episode.start), 1, 0)]
  # If last.tx.episode_per_pt == 1, then get the difference between exit from the study date and episode.end date
  # If last.tx.episode_per_pt == 0, then get the difference between next.episode.start and previous episode.end dates 
  df_with_exit_dates[, date_diff := ifelse(last.tx.episode_per_pt == 1, exit_date - episode.end, next.episode.start - episode.end)]
  # If the values are > 90 then consider this a discontinuation 
  df_with_exit_dates[, tx_discontinued := ifelse(date_diff > 90, 1, 0)]
  # # Get subset of records where tx_discontinued == 1
  # df_with_exit_dates[tx_discontinued == 1]
  # Group by year_month of episode.end.YM
  df_discontinued_counts_num <- df_with_exit_dates[tx_discontinued == 1][,.N, by = .(year(episode.end), month(episode.end))]
  # Adjust for PHARMO
  if(is_PHARMO){df_discontinued_counts_num <- df_discontinued_counts_num[df_discontinued_counts_num$year < 2020,]} else {df_discontinued_counts_num <- df_discontinued_counts_num[df_discontinued_counts_num$year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  df_discontinued_counts_num<- as.data.table(merge(x = empty_counts, y = df_discontinued_counts_num, by = c("year", "month"), all.x = TRUE))
  # Fills in missing values with 0
  df_discontinued_counts_num[is.na(df_discontinued_counts_num[,N]), N:=0]
  # Masks values less than 5
  # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
  df_discontinued_counts_num$masked <- ifelse(df_discontinued_counts_num$N < 5 & df_discontinued_counts_num$N > 0, 1, 0)
  # Changes values less than 5 and more than 0 to 5
  if (mask == T){df_discontinued_counts_num[df_discontinued_counts_num$masked == 1,]$N <- 5} else {df_discontinued_counts_num[df_discontinued_counts_num$masked == 1,]$N <- df_discontinued_counts_num[df_discontinued_counts_num$masked == 1,]$N}
  ### Denominator = Number of prevalent valproate/retinoid users that month 
  ##### ->  denominator file has already been created in for loop variable name = df_prevalence_counts_num
  ### Merges numerator file with denominator file
  df_discontinued_counts_num <- within(df_discontinued_counts_num, YM<- sprintf("%d-%02d", year, month))
  # Drop unnecessary columns and rename colnames in denominator 
  df_prevalence_counts_num <- df_prevalence_counts_num[,-c("year", "month", "masked")]
  setnames(df_prevalence_counts_num, "N", "Freq")
  df_discontinued_counts <- merge(x = df_discontinued_counts_num, y = df_prevalence_counts_num, by = c("YM"), all.x = TRUE)
  # Calculate rates
  df_discontinued_counts <-df_discontinued_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
  df_discontinued_counts <-df_discontinued_counts[,c("YM", "N", "Freq", "rates", "masked")]
  # Save files 
  saveRDS(df_discontinued_counts, (paste0(medicines_counts_dir,"/", med_name, "_discontinued_counts.rds")))
  ##################################################################################################
  ############################# Calculates Discontinuation Rates ###################################
  ##################################################################################################
  ### Rate of pregnancies starting in a Retinoid/Valproate episode 
  # Numerator -> Number of pregnancies that started (start date) during a Retinoid/Valproate episode (current users)
  # Denominator -> Number of prevalent Retinoid/Valproate users that month (already calculated above)
  # Merge pregnancy df with tx episodes file
  # Contraceptive Episodes + Medications
  preg_med_df <- df[D3_pregnancy_reconciled, on = .(person_id)] # Left join
  preg_med_df <-  preg_med_df[!is.na(episode.start),] # Delete records with pregnancy records 
  preg_med_df[,episode.start:=as.IDate(episode.start,"%Y%m%d")][,episode.end:=as.IDate(episode.end,"%Y%m%d")]# Converts dates to be in the same format
  preg_med_df[,preg_in_episode:= fifelse(pregnancy_start_date>=episode.start & pregnancy_start_date<=episode.end, 1, 0)] # Creates column that indicates if medicine record date is between episode.start and episode.end dates
  preg_med_df <- preg_med_df[preg_in_episode == 1,] # Creates preg_med_df  of patients who have a medicine record date between episode.start and episode.end dates
  # Checks if there are any records that meet the criteria. If so it does the calculations
  if (nrow(preg_med_df) > 0){
    # Performs counts 
    preg_during_epi_counts <- preg_med_df[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date))] # Performs counts grouped by year, month of medicine prescription date
    preg_during_epi_counts <- as.data.table(merge(x = empty_counts, y = preg_during_epi_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with preg_during_epi_counts
    preg_during_epi_counts[is.na(preg_during_epi_counts[,N]), N:=0] # Fills in missing values with 0
    # Masking
    preg_during_epi_counts$masked_num <- ifelse(preg_during_epi_counts$N < 5 & preg_during_epi_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
    if(mask == T){preg_during_epi_counts[preg_during_epi_counts$masked_num == 1,]$N <- 5} else {preg_during_epi_counts[preg_during_epi_counts$masked_num == 1,]$N <- preg_during_epi_counts[preg_during_epi_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
    ############################
    ##### Calculates Rates #####
    ############################
    preg_during_epi_counts <- within(preg_during_epi_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
    preg_during_epi_counts <- merge(x = preg_during_epi_counts, y = df_prevalence_counts_num, by = c("YM"), all.x = TRUE) # Merge with med counts
    preg_during_epi_counts <- preg_during_epi_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
    preg_during_epi_counts$rates[is.nan(preg_during_epi_counts$rates)]<-0
    preg_during_epi_counts$rates[is.na(preg_during_epi_counts$rates)]<-0
    preg_during_epi_counts <- preg_during_epi_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
    setnames(preg_during_epi_counts, "masked_num", "masked")
    # Save files 
    saveRDS(preg_med_df, paste0(counts_dfs_dir, pop_prefix, "_preg_start_during_tx_episodes.rds"))
    saveRDS(counts_preg, paste0(preg_med_counts_dir,"/", pop_prefix, "_preg_start_during_tx_episodes_counts.rds"))
  } else {
    print(paste0(gsub("_treatment_episodes.rds", "",tx_episodes_files[i]), " study: There are no patients with pregnancy start dates that fall between episode start and end dates!"))
  }
}

# Clean up
rm(df, df_expanded, df_with_exit_dates, df_prevalence_counts, df_prevalence_counts_num, df_incidence_counts, df_incidence_counts_num, df_discontinued_counts, df_discontinued_counts_num)
