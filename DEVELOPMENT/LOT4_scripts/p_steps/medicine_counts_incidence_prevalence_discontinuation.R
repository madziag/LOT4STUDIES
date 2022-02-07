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
# Records needed -> 1. Retinoid/Valproate treatment episode files 2. Prevalent counts (calculated in 1.1)

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

### Loads records needed: 
# 1. Treatment episode files 
# Looks for treatment_episode files in treatment_episodes folder (actual files will be loaded in the for loop)
tx_episodes_files <- list.files(paste0(g_intermediate, "treatment_episodes/"), pattern = "retinoid|valproate", ignore.case = T)
# Filters by current subpopulation 
tx_episodes_files <- tx_episodes_files[grepl(pop_prefix, tx_episodes_files)]
# 2. Denominator 
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

# Performs counts using each of the tx_episode files 
for (i in 1:length(tx_episodes_files)){
  # Reads in the treatment episodes file 
  df_episodes <- as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/",tx_episodes_files[i])))
  # Reads in study population data
  ##### ->  Study_population has already been read in by the wrapper script, variable name = study_population
  # Adds column that indicates row number (to be used when merging expanded df with original df)
  df_episodes <- df_episodes[,nrow:=.I]
  ##################################################################################################
  ################################## Calculates Prevalence #########################################
  ##################################################################################################
  ### Numerator = Number of female subjects in cohort with valproate/retinoid episode overlapping the month by at least 1 day 
  # Expands df (1 row for every day of treatment per patient) -> gives 2 cols (person_id + tx day)
  df_expanded <- setDT(df_episodes)[,list(idnum = person_id, episode.day = seq(episode.start, episode.end, by = "day")), by = 1:nrow(df_episodes)]
  # Merges back with original df
  df_expanded <-  merge(df_episodes, df_expanded, by = "nrow")
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
  df_prevalence_counts_num <- as.data.table(merge(x = empty_df, y = df_prevalence_counts_num, by = c("year", "month"), all.x = TRUE))
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
  # df_prevalence_counts_num <- within(df_prevalence_counts_num, YM<- sprintf("%d-%02d", year, month))
  df_prevalence_counts <- merge(x = df_prevalence_counts_num, y = denominator, by = c("year", "month"), all.x = TRUE)
  # Calculates rates
  df_prevalence_counts <- df_prevalence_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
  df_prevalence_counts <- df_prevalence_counts[,c("YM", "N", "Freq", "rates", "masked")]
  # Saves files in medicine counts folder
  saveRDS(df_prevalence_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_prevalence_counts.rds")))
  ##################################################################################################
  ################################## Calculates Incidence ##########################################
  ##################################################################################################
  ### Numerator = Number of female subjects in cohort with a valproate/retinoid episode start in the month 
  # Group by year_month of episode.start.YM
  df_incidence_counts_num <- df_episodes[,.N, by = .(year(episode.start),month(episode.start))]
  # Adjust for PHARMO 
  if(is_PHARMO){df_incidence_counts_num <- df_incidence_counts_num[df_incidence_counts_num$year < 2020,]} else {df_incidence_counts_num <- df_incidence_counts_num[df_incidence_counts_num$year < 2021,]}
  # Merge with empty df (for counts that do not have counts for all months and years of study)
  df_incidence_counts_num <- as.data.table(merge(x = empty_df, y = df_incidence_counts_num, by = c("year", "month"), all.x = TRUE))
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
  # df_incidence_counts_num <- within(df_incidence_counts_num, YM<- sprintf("%d-%02d", year, month))
  df_incidence_counts <- merge(x = df_incidence_counts_num, y = denominator, by = c("year", "month"), all.x = TRUE)
  # Calculates rates
  df_incidence_counts <-df_incidence_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
  df_incidence_counts <-df_incidence_counts[,c("YM", "N", "Freq", "rates", "masked")]
  # Saves files in medicine counts folder
  saveRDS(df_incidence_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_incidence_counts.rds")))
  ##################################################################################################
  ############################# Calculates Discontinuation Rates ###################################
  ##################################################################################################
  ### Numerator = Number of female subjects in cohort who discontinue valproate/retinoid in the month 
  #Join treatment episodes df with study population to get exit date from study for each patient 
  df_with_exit_dates <- merge(x = df_episodes[,-c("nrow")], y = study_population[,c("person_id", "exit_date")], by = "person_id", all.x = TRUE)
  # Change dates to be in the correct format 
  df_with_exit_dates[, exit_date := as.Date(exit_date, format="%Y%m%d")]
  # Sort df 
  df_with_exit_dates[order(person_id,episode.start),]
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
  df_discontinued_counts_num <- as.data.table(merge(x = empty_df, y = df_discontinued_counts_num, by = c("year", "month"), all.x = TRUE))
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
  # Drops unnecessary columns and rename colnames in the prevalence denominator 
  df_prevalence_counts_num <- df_prevalence_counts_num[,-c("masked")]
  # Creates column that combines year and month 
  df_prevalence_counts_num <- within(df_prevalence_counts_num, YM<- sprintf("%d-%02d", year, month))
  # Changes column "N" to "Freq" so that it is the same as in the original denominator 
  setnames(df_prevalence_counts_num, "N", "Freq")
  # Merges the discontinued counts with the prevalent counts 
  df_discontinued_counts <- merge(x = df_discontinued_counts_num, y = df_prevalence_counts_num, by = c("year", "month"), all.x = TRUE)
  # Calculates rates
  df_discontinued_counts <-df_discontinued_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
  # Changes nan vales to 0 
  df_discontinued_counts[is.nan(df_discontinued_counts$rates)]$rates <- 0
  # Drops unnecessary columns 
  df_discontinued_counts <-df_discontinued_counts[,c("YM", "N", "Freq", "rates", "masked")]
  # Saves files in medicine counts folder
  saveRDS(df_discontinued_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_discontinued_counts.rds")))
}

