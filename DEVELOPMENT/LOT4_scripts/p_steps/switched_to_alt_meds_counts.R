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

prevalent_counts_files <- prevalent_counts_files[!grepl("age_group|indication|tx_dur", prevalent_counts_files)]
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
# Clean up
rm(denominator)

all_temps <- list.files(tmp, pattern="diagnoses|procedures|procedures|procedures_dxcodes")
# 3. Indication records for valproates only
if(length(all_temps)>0){
  # Creates a list of indications 
  # Get a list of indication files (with added new column to indicate indication type)
  indications_list <- list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine", full.names = T)
  if(pop_prefix == "PC"){indications_list <- indications_list[!grepl("PC_HOSP", indications_list)]}
  if(pop_prefix == "PC_HOSP"){indications_list <- indications_list[grepl("PC_HOSP", indications_list)]}
  # Bind all indication records
  all_indications<- do.call(rbind,lapply(indications_list, readRDS))
  all_indications<-all_indications[,c("person_id", "Date", "Code", "indication")]
  all_indications<-all_indications[!duplicated(all_indications),]
  setnames(all_indications,"Date","indication_date")
}


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
    alt_med_df <- alt_med_df[,c("person_id", "Code", "Date", "entry_date", "exit_date")]
    setnames(alt_med_df, "Code", "ATC")
    setnames(alt_med_df, "Date", "record_date")
    # Remove any records that do not fall between entry and exit into study dates 
    alt_med_df<-alt_med_df[record_date>=entry_date&record_date<=exit_date,]
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
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      num_1_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
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
      prevalent_counts <- prevalent_counts[,-c("Freq", "rates", "masked", "true_value")]
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
      # rm(tx_episodes, alt_med_df, alt_med_tx_episodes_0, alt_med_tx_episodes_1, alt_med_tx_episodes_expanded, denom_0_1_counts, num_1_counts, alt_med_counts)
      
      ##### STRATIFICATION BY AGE GROUPS ###
      # Merge data with study population to get date of birth
      switched_df_age_groups <- merge(alt_med_tx_episodes_1, study_population[,c("person_id", "birth_date")], by = "person_id")
      # Creates a column with patients age on every day of in the treatment episode
      switched_df_age_groups[,current_age:= floor((episode.end.switch - birth_date)*10/365.25)/10]
      # Add column which groups each patient into an age group, for each day of their treatment
      switched_df_age_groups[current_age >= 12 & current_age < 21, age_group:= "12-20.99"]
      switched_df_age_groups[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
      switched_df_age_groups[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
      switched_df_age_groups[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
      
      # Performs pgtests counts - stratified by age group
      switched_by_age <- switched_df_age_groups[,.N, by = .(year(episode.end.switch),month(episode.end.switch), age_group)]
      # Get unique values of age groups - for the for loop
      age_group_unique <- unique(switched_by_age$age_group)
      
      for(group in 1:length(age_group_unique)){
        # Create a subset of age group
        each_group <- switched_by_age[age_group==age_group_unique[group]]
        # Adjust for PHARMO
        if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
        # Merge with empty df (for counts that do not have counts for all months and years of study)
        each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
        # Fills in missing values with 0
        each_group[is.na(N), N:=0][is.na(age_group), age_group:=age_group_unique[group]]
        # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
        each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
        # Create YM variable 
        each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
        # Masks values less than 5
        # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
        each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
        # Applies masking 
        if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
        # Prepare denominator (all pgtests counts )
        switched_all_counts_min <- alt_med_counts[,c("YM", "N")]
        setnames(switched_all_counts_min, "N", "Freq")
        # Create counts file
        switched_age_counts <- merge(x = each_group, y = switched_all_counts_min, by = c("YM"), all.x = TRUE)
        switched_age_counts <- switched_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
        switched_age_counts <- switched_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
        # Saves files in medicine counts folder
        saveRDS(switched_age_counts, paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_age_group_", age_group_unique[group], "_switched_to_alt_meds_counts.rds")) # Monthly counts file
      }    
      ##### STRATIFICATION BY INDICATIONS ###
      # Checks if there are indication files and performs action only for DAPs with indication files 
      if(str_detect(tx_episodes_files[i],"Valproate") & length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){

        # Merge data with study population to get date of birth
        switched_df_indications <- all_indications[alt_med_tx_episodes_1,on=.(person_id), allow.cartesian = T]
        # Create columns for each of the indication-> if there is more than one indication per treatment episode, then we want them to be in 1 row
        switched_df_indications[indication=="ind_bipolar", ind_bipolar_date:=indication_date]
        switched_df_indications[indication=="ind_epilepsy", ind_epilepsy_date:=indication_date]
        switched_df_indications[indication=="ind_migraine", ind_migraine_date:=indication_date]
        switched_df_indications[,Date:=NULL][,Code:=NULL]
        setnames(switched_df_indications, "indication", "temp_indication")
        
        switched_df_indications<-switched_df_indications[order(person_id,episode.start, episode.end, ind_bipolar_date, ind_epilepsy_date, ind_migraine_date)]
        
        switched_df_indications <- switched_df_indications[!duplicated(switched_df_indications[,c("person_id", "episode.start", "episode.end", "temp_indication")])]
        
        switched_df_indications <- setDT(switched_df_indications)[, lapply(.SD, na.omit), by = c("person_id", "episode.start","episode.end")]
        
        switched_df_indications[,bipolar_diff:=episode.end.switch-ind_bipolar_date][,epilepsy_diff:=episode.end.switch-ind_epilepsy_date][,migraine_diff:=episode.end.switch-ind_migraine_date]
        switched_df_indications[bipolar_diff<0,bipolar_diff:=NA][epilepsy_diff<0,epilepsy_diff:=NA][migraine_diff<0,migraine_diff:=NA]
        switched_df_indications <- switched_df_indications[,num_obs:=rowSums(!is.na(switched_df_indications[,c("epilepsy_diff", "bipolar_diff", "migraine_diff")]))][]
        switched_df_indications[num_obs==0, indication:="unknown"]
        switched_df_indications[num_obs==1, indication:=temp_indication][,temp_indication:=NULL]
        switched_df_indications[num_obs>1, indication:="multiple"]
        drop.cols <- grep("diff|ind_", colnames(switched_df_indications))
        switched_df_indications[, (drop.cols) := NULL]
        
        # Performs pgtests counts - stratified by indication group
        switched_by_indication <- switched_df_indications[,.N, by = .(year(episode.end.switch),month(episode.end.switch), indication)]
        # Get unique values of indication groups - for the for loop
        indication_group_unique <- unique(switched_by_indication$indication)
        
        for(group in 1:length(indication_group_unique)){
          # Create a subset of indication group
          each_group <- switched_by_indication[indication==indication_group_unique[group]]
          # Adjust for PHARMO
          if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
          # Merge with empty df (for counts that do not have counts for all months and years of study)
          each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
          # Fills in missing values with 0
          each_group[is.na(N), N:=0][is.na(indication), indication:=indication_group_unique[group]]
          # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
          each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
          # Create YM variable 
          each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
          # Masks values less than 5
          # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
          each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
          # Applies masking 
          if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
          # Prepare denominator (all pgtests counts )
          switched_all_counts_min <- alt_med_counts[,c("YM", "N")]
          setnames(switched_all_counts_min, "N", "Freq")
          # Create counts file
          switched_indication_counts <- merge(x = each_group, y = switched_all_counts_min, by = c("YM"), all.x = TRUE)
          switched_indication_counts <- switched_indication_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
          switched_indication_counts <- switched_indication_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
          # Saves files in medicine counts folder
          saveRDS(switched_indication_counts, paste0(medicines_counts_dir,"/", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), "_indication_", indication_group_unique[group], "_switched_to_alt_meds_counts.rds")) # Monthly counts file
        }                              
      }
    } else {
      print(paste0("No patients that switched from ", gsub("_CMA_treatment_episodes.rds", "", tx_episodes_files[i]), " treatment to alternative meds have been found"))
    }
  }
} else {
  print("No alternative records medicines have been found")
}


# Move files 
for (file in list.files(path=medicines_counts_dir, pattern="age_group", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_age_groups, "/",file))}
for (file in list.files(path=medicines_counts_dir, pattern="indication", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_indication, "/",file))}





