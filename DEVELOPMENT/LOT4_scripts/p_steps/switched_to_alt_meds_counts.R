#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 16/02/2022
##################################################################################################################################################
###################################################### OBJECTIVE: 4.2 ############################################################################
##################################################################################################################################################

## Objective 4.2: Proportion of people who switched from Retinoid/Valproate to an alternative medicine (any of the altmed retin or altmed valp)
# Numerator -> Number of people who have switched from a Retinoid/Valproate to an alternative medicine
# Denominator ->  Number of prevalent (current) users
# Records needed -> 1. Treatment episodes 2. Alternative medicines records (combined set of alt meds for Retinoids/combined set of alt meds for Valproates) 3. Prevalent users patient records 
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
### Loads records needed 
# 1. Alternative medicines files 
# Retinoids 
alt_med_retinoid_files <- list.files(medications_pop, pattern="altmed_retin", ignore.case = T, recursive = T, full.names = T)
# Filters by current subpopulation 
alt_med_retinoid_files <- alt_med_retinoid_files[grepl(pop_prefix, alt_med_retinoid_files)]
if(populations[pop] == "PC_study_population.rds"){
  alt_med_retinoid_files <- list.files(medications_pop, pattern="altmed_retin", ignore.case = T, recursive = T, full.names = T)
  alt_med_retinoid_files <- alt_med_retinoid_files[!grepl("PC_HOSP", alt_med_retinoid_files)]
}

if(is_CASERTA){alt_med_retinoid_files <- alt_med_retinoid_files[!grepl("altmed_retin_psoriasis", alt_med_retinoid_files)]}

# Valproates
alt_med_valproate_files <- list.files(medications_pop, pattern="altmed_valp", ignore.case = T, recursive = T, full.names = T)
# Filters by current subpopulation 
alt_med_valproate_files <- alt_med_valproate_files[grepl(pop_prefix, alt_med_valproate_files)]

if(populations[pop] == "PC_study_population.rds"){
  alt_med_valproate_files <- list.files(medications_pop, pattern="altmed_valp", ignore.case = T, recursive = T, full.names = T)
  alt_med_valproate_files <- alt_med_valproate_files[!grepl("PC_HOSP", alt_med_valproate_files)]
}

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
# Removes age specific prevalent count files
prevalent_counts_files <- prevalent_counts_files[!grepl("age_group", prevalent_counts_files)]

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
### Creates empty df for expanding counts files (when not all month-year combinations have counts)
empty_df <- as.data.table(expand.grid(seq(min(denominator$year), max(denominator$year)), seq(1, 12)))
names(empty_df) <- c("year", "month")
# Clean up
rm(denominator)

if (length(alt_med_retinoid_files) > 0 | length(alt_med_valproate_files)){
  # For each treatment episode file 
  for (i in 1:length(tx_episodes_files)){ 
    # Reads in the treatment episodes file 
    tx_episodes <- as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/",tx_episodes_files[i])))
    tx_episodes <- tx_episodes[,-c("ATC", "type")]
    # Reads in the altmed files (corresponding to the medication)
    if(str_detect(tx_episodes_files[i], pattern = "Retinoid_CMA")){alt_med_df <- do.call(rbind,lapply(alt_med_retinoid_files, readRDS))}
    if(str_detect(tx_episodes_files[i], pattern = "Valproate_CMA")){alt_med_df <- do.call(rbind,lapply(alt_med_valproate_files, readRDS))}
    # Clean up altmed_df
    alt_med_df <- alt_med_df[,c("person_id", "Code", "Date")]
    setnames(alt_med_df, "Code", "ATC")
    setnames(alt_med_df, "Date", "record_date")
    # Merge tx episodes with altmed records 
    alt_med_tx_episodes <- tx_episodes[alt_med_df, on = .(person_id), allow.cartesian = T] 
    # Delete records without alt medicine use
    alt_med_tx_episodes <-  alt_med_tx_episodes[!is.na(episode.ID),]
    # Remove duplicates
    alt_med_tx_episodes <- alt_med_tx_episodes[!duplicated(alt_med_tx_episodes)]
    # Converts dates to be in the same format
    alt_med_tx_episodes[,episode.start:=as.IDate(episode.start,"%Y%m%d")][,episode.end:=as.IDate(episode.end,"%Y%m%d")]
    # Creates episode.end.switch column
    # Definition: 
    # if person has record for alt med between episode.start.date and episode.end.date + 90 days -> value = earliest date of alternative medicine record between the 2 dates)
    # else assign the episode.end.date 
    # Is record_date of alt medicine between episode.start.date and episode.end.date + 90?
    alt_med_tx_episodes[, switch:= fifelse(record_date>episode.start & record_date<=episode.end + 90, 1, 0)]
    # Switchers 
    # Get earliest alt_med record date by patient id ONLY for those who have altmed use within treatment episode # Check if any records match before you run this
    if(nrow(alt_med_tx_episodes[switch == 1,])>0){
      # Creates a df where switch == 1
      alt_med_tx_episodes_1a <- alt_med_tx_episodes[switch == 1,]
      # Assigns the minimal altmed record value as end of episode.switch
      alt_med_tx_episodes_1 <- alt_med_tx_episodes_1a[, episode.end.switch := min(record_date), by=c("episode.start", "person_id")] # Persons that had alt medicines within their tx episodes + 90
      # Remove column with alt med dates (the earliest alt med record date has already been copied to episode.end.switch)
      alt_med_tx_episodes_1 <- alt_med_tx_episodes_1[,-c("record_date")]
      # Removes duplicates
      alt_med_tx_episodes_1 <- alt_med_tx_episodes_1[!duplicated(alt_med_tx_episodes_1[,-c("ATC")])]
      # If more than 2 episodes -> check if difference between next episode start and previous episode end is >= 90 
      # If < 90, then this is not a true switcher -> switch column turns to 0
      # If > 90, switch column remains 1
      # Create column that puts next episode.start date on the same row level as episode.end of previous tx.episode per gtoup
      # Sort df 
      alt_med_tx_episodes_1[order(person_id,episode.start),]
      # Create a column with lead values of episode.start column (so that episode.end can be on the same row as the next episode.start of the same person)
      alt_med_tx_episodes_1[, next.episode.start:= shift(episode.start, type = "lead" ), by = person_id]
      # Subtract episode.end from next.episode.start. If difference <= 90 then switch -> 0, else switch remains 1
      alt_med_tx_episodes_1[, switch:=fifelse(next.episode.start - episode.end <= 90, 0, 1)]
      # if switch value is missing, change it to 1 (all non 0 values are true switchers)
      alt_med_tx_episodes_1[is.na(switch), switch := 1]
      # remove next.episode.start columns
      alt_med_tx_episodes_1 <- alt_med_tx_episodes_1[,-c("next.episode.start")]
      # If switch has now been changed back to 0, then episode.end.switch = episode.end
      alt_med_tx_episodes_1 <- alt_med_tx_episodes_1[switch == 0, episode.end.switch := episode.end] 
      # Drop records where switch == 0
      alt_med_tx_episodes_1 <- alt_med_tx_episodes_1[switch==1,]
      # Creates numerator 
      num_1_counts <- alt_med_tx_episodes_1[,.N, by = .(year(episode.end.switch),month(episode.end.switch))]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      num_1_counts  <- as.data.table(merge(x = empty_df, y = num_1_counts, by = c("year", "month"), all.x = TRUE))
      num_1_counts[is.na(num_1_counts[,N]), N:=0]
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      num_1_counts$masked <- ifelse(num_1_counts$N < 5 & num_1_counts$N > 0, 1, 0)
      # Changes values less than 5 and more than 0 to 5
      if (mask == T){num_1_counts[num_1_counts$masked == 1,]$N <- 5} else {num_1_counts[num_1_counts$masked == 1,]$N <- num_1_counts[num_1_counts$masked == 1,]$N}
      # Create YM variable 
      num_1_counts <- within(num_1_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      # Get denominator 
      # Load prevalence counts
      prevalent_counts <- readRDS(prevalent_counts_files[grepl(gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), prevalent_counts_files)])
      prevalent_counts <- prevalent_counts[,-c("Freq", "rates", "masked")]
      setnames(prevalent_counts, "N", "Freq")
      # Calculate rates 
      ### Merges numerator file with denominator file
      alt_med_counts <- merge(x = num_1_counts, y = prevalent_counts, by = c("YM"), all.x = TRUE)
      # Calculates rates
      alt_med_counts <- alt_med_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      # Adjust for PHARMO
      if(is_PHARMO){alt_med_counts <- alt_med_counts[alt_med_counts$year < 2020,]} else {alt_med_counts <- alt_med_counts[alt_med_counts$year < 2021,]}
      # Drop unnecessary columns
      alt_med_counts <- alt_med_counts[,c("YM", "N", "Freq", "rates", "masked")]
      # Saves files 
      saveRDS(alt_med_counts, (paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_switched_to_alt_meds_counts.rds")))
      # Clean up
      rm(tx_episodes, alt_med_df, alt_med_tx_episodes_0, alt_med_tx_episodes_1, alt_med_tx_episodes_expanded, denom_0_1_counts, num_1_counts, alt_med_counts)
    } else {
      print(paste0("No patients that switched from ", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), " treatment to alternative meds have been found"))
    }
  }
} else {
  print("No alternative records medicines have been found")
}







