##################################################################################################################################################
###################################################### OBJECTIVE: 4.2 ############################################################################
##################################################################################################################################################

## Objective 4.2: Proportion of people who switched from Retinoid/Valproate to an alternative medicine (any of the altmed retin or altmed valp)
# Numerator -> Number of people who have switched from a Retinoid/Valproate to an alternative medicine
# Denominator ->  Number of prevalent (current users) who have not switched to an alternative medicine 
# Records needed -> 1. Treatment episodes 2. Alternative medicines records (combined set of alt meds for Retinoids/combined set of alt meds for Valproates) 3. Prevalent users patient records 
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
### Loads records needed 
# 1. Alternative medicines files 
alt_med_retinoid_files <- list.files(medications_pop, pattern="altmed_retin", ignore.case = T, recursive = T, full.names = T)
alt_med_valproate_files <- list.files(medications_pop, pattern="altmed_valp", ignore.case = T, recursive = T, full.names = T)
# 2. Treatment episode files 
# Looks for treatment_episode files in treatment_episodes folder (actual files will be loaded in the for loop)
tx_episodes_files <- list.files(paste0(g_intermediate, "treatment_episodes/"), pattern = "Retinoid_CMA|Valproate_CMA", ignore.case = T)
# Filters by current subpopulation 
tx_episodes_files <- tx_episodes_files[grepl(pop_prefix, tx_episodes_files)]
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
    # Check if record date is equal to or less than episode start date. If it is
    alt_med_tx_episodes[, switch:= fifelse(record_date < episode.start & switch == 1, 0, switch)]
    # Non-switchers
    # Creates a df where switch == 0
    alt_med_tx_episodes_0a <- alt_med_tx_episodes[switch== 0,]
    alt_med_tx_episodes_0 <- alt_med_tx_episodes_0a[, episode.end.switch := episode.end] # Persons that did not have alt medicines within their tx episodes + 90
    # Remove column with alt med dates
    alt_med_tx_episodes_0 <- alt_med_tx_episodes_0[,-c("record_date")]  
    # Removes duplicates
    alt_med_tx_episodes_0 <- alt_med_tx_episodes_0[!duplicated(alt_med_tx_episodes_0)]
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
      alt_med_tx_episodes_1 <- alt_med_tx_episodes_1[!duplicated(alt_med_tx_episodes_1)]
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
      # Binds the 2 df 
      alt_med_tx_episodes <- rbind(alt_med_tx_episodes_1, alt_med_tx_episodes_0) # Binds the 2 dfs
      ### Creates Denominator
      # Adds column that indicates row number (to be used when merging expanded df with original df)

      alt_med_tx_episodes <- alt_med_tx_episodes[,nrow:=.I]
      # Expands df (1 row for every day of treatment per patient) -> gives 2 cols (person_id + tx day)
      alt_med_tx_episodes_switched <- alt_med_tx_episodes[switch == 1,]
      alt_med_tx_episodes_expanded <- setDT(alt_med_tx_episodes_switched)[,list(idnum = person_id, episode.day = seq(episode.start, episode.end.switch, by = "day")), by = 1:nrow(alt_med_tx_episodes_switched)]
      # Merges back with original df
      alt_med_tx_episodes_expanded <-  merge(alt_med_tx_episodes, alt_med_tx_episodes_expanded, by = "nrow")
      # Create year-months columns episode.day
      alt_med_tx_episodes_expanded$year <- year(alt_med_tx_episodes_expanded$episode.day)
      alt_med_tx_episodes_expanded$month <- month(alt_med_tx_episodes_expanded$episode.day)
      # Remove episode.day column (we already have the month and year extracted from this column) + other duplicate/unnecessary columns
      alt_med_tx_episodes_expanded <- alt_med_tx_episodes_expanded[,-c("nrow", "idnum", "episode.day")]
      # Remove duplicate records
      alt_med_tx_episodes_expanded <- alt_med_tx_episodes_expanded[!duplicated(alt_med_tx_episodes_expanded)]
      # Count the number of patients treated per month
      denom_0_1_counts <- alt_med_tx_episodes_expanded[,.N, by = .(year,month)]
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      denom_0_1_counts <- as.data.table(merge(x = empty_df, y = denom_0_1_counts, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      denom_0_1_counts[is.na(denom_0_1_counts[,N]), N:=0]
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      denom_0_1_counts$masked_den <- ifelse(denom_0_1_counts$N < 5 & denom_0_1_counts$N > 0, 1, 0)
      # Changes values less than 5 and more than 0 to 5
      if (mask == T){denom_0_1_counts[denom_0_1_counts$masked_den == 1,]$N <- 5} else {denom_0_1_counts[denom_0_1_counts$masked_den == 1,]$N <- denom_0_1_counts[denom_0_1_counts$masked_den == 1,]$N}
      # Renames columns (to not have the same name as in the numerator)
      setnames(denom_0_1_counts, "N", "Freq")
      ### Creates numerator 
      # Count the number of patients treated per month
      num_1_counts <- alt_med_tx_episodes_expanded[switch == 1,][,.N, by = .(year,month)]
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
      # Calculate rates 
      ### Merges numerator file with denominator file
      alt_med_counts <- merge(x = num_1_counts, y = denom_0_1_counts, by = c("year", "month"), all.x = TRUE)
      # Calculates rates
      alt_med_counts <- alt_med_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
      # Adjust for PHARMO
      if(is_PHARMO){alt_med_counts <- alt_med_counts[alt_med_counts$year < 2020,]} else {alt_med_counts <- alt_med_counts[alt_med_counts$year < 2021,]}
      alt_med_counts$rates[is.nan(alt_med_counts$rates)]<-0
      alt_med_counts$rates[is.na(alt_med_counts$rates)]<-0
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












