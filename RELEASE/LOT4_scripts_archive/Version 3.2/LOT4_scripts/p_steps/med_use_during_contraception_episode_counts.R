#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/01/2022

##################################################################################################################################################
###################################################### OBJECTIVE: 2.6 ############################################################################
##################################################################################################################################################

## Objective 2.6: Proportion of Retinoid/Valproate records that occurred within a contraception episode
# Numerator -> Number of Retinoid/Valproate records that occurred during a period of contraception coverage
# Denominator -> Total number of Retinoid/Valproate records that month (duplicates removed )
# Records needed -> contraception episode records 2. Retinoid/Valproate records 

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

### Loads records needed 
# 1. Contraception episode records 
# Looks for Contraception episode files in treatment episodes folder 
contra_epi_files <- list.files(paste0(g_intermediate,"treatment_episodes/"), pattern = paste0(pop_prefix, "_contraceptive"), recursive = T, full.names = T)
# Filters by current subpopulation 
contra_epi_files <- contra_epi_files[grepl(pop_prefix, contra_epi_files )]

if(populations[pop] == "PC_study_population.rds"){
  contra_epi_files <- list.files(paste0(g_intermediate,"treatment_episodes/"), pattern = paste0(pop_prefix, "_contraceptive"), recursive = T, full.names = T)
  contra_epi_files <- contra_epi_files[!grepl("PC_HOSP", contra_epi_files)]
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
  setnames(all_indications, "Date", "indication_date")
}

# Checks first if there are any contraception episode records found
if(length(contra_epi_files)>0) {
  # Loads file
  contra_epi_df <- as.data.table(readRDS(contra_epi_files))
  ##### Loops over medication files to create counts per medication type 
  for(i in 1:length(med_files)){
    ## Loads the medication record
    med_df <- as.data.table(readRDS(paste0(medications_pop, med_files[i]))) # Loads file
    med_df <- med_df[Date>=entry_date & Date<=exit_date]
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
    # Merges contraception episodes df with medication df 
    contra_epi_med_df <- contra_epi_df[med_df, on = .(person_id), allow.cartesian = T] # Left join
    # Converts all date columns to same date type (Idate vs Date)
    contra_epi_med_df[,episode.start:=as.IDate(episode.start,"%Y%m%d")][,episode.end:=as.IDate(episode.end,"%Y%m%d")]# Converts dates to be in the same format
    # Creates column that indicates if the medicine record date is within the contraception episode 
    contra_epi_med_df[,tx_in_episode:= fifelse(Date>=episode.start & Date <= episode.end, 1, 0)] 
    # Merges with study population to get birth_date (study population has been loaded in the wrapper script)
    contra_epi_med_df <- merge(contra_epi_med_df, study_population[,c("person_id", "birth_date", "entry_date","exit_date")], by = "person_id")
    #  Creates subset where tx_in_episode == 1
    tx_in_episode_df <- contra_epi_med_df[tx_in_episode == 1,] 
    # Performs counts if df created is not empty 
    if (nrow(tx_in_episode_df)>0){
      # Calculates age at the time of taking the medication during the episode (for stratification analysis)
      # Creates a column with patients age on every day of in the treatment episode
      tx_in_episode_df[,current_age:= floor((Date - birth_date)*10/365.25)/10]
      # Add column which groups each patient into an age group, for each day of their treatment
      tx_in_episode_df[current_age >= 12 & current_age < 21, age_group:= "12-20.99"]
      tx_in_episode_df[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
      tx_in_episode_df[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
      tx_in_episode_df[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
      # Counts
      tx_in_episode_counts <- tx_in_episode_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
      tx_in_episode_counts <- as.data.table(merge(x = empty_df, y = tx_in_episode_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with tx_in_episode_counts
      tx_in_episode_counts[is.na(tx_in_episode_counts[,N]), N:=0] # Fills in missing values with 0
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      tx_in_episode_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking
      tx_in_episode_counts$masked_num <- ifelse(tx_in_episode_counts$N < 5 & tx_in_episode_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
      if(mask == T){tx_in_episode_counts[tx_in_episode_counts$masked_num == 1,]$N <- 5} else {tx_in_episode_counts[tx_in_episode_counts$masked_num == 1,]$N <- tx_in_episode_counts[tx_in_episode_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
      # Rate calculation
      tx_in_episode_counts <- within(tx_in_episode_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      tx_in_episode_counts <- merge(x = tx_in_episode_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
      tx_in_episode_counts <- tx_in_episode_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      tx_in_episode_counts <- tx_in_episode_counts[,c("YM", "N", "Freq", "rates", "masked_num", "true_value")]
      setnames(tx_in_episode_counts, "masked_num", "masked")
      ## Saves intermediate (to counts_df folder) and monthly count files (to medicines counts folder)
      saveRDS(tx_in_episode_df, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_med_use_during_contra_episodes.rds")) # Saves intermediate file
      saveRDS(tx_in_episode_counts, paste0(medicines_counts_dir, "/", gsub(".rds", "", med_files[i]), "_med_use_during_contraception_episodes_counts.rds")) # Saves monthly counts 
      
      ################ MEDICINE COUNTS DURING CONTRACEPTIVE EPISODE STRATIFIED BY AGE GROUPS ###################
      # Performs medicine counts during contraceptive episode - stratified by age group
      tx_in_episode_by_age <- tx_in_episode_df[,.N, by = .(year(Date),month(Date),age_group)]
      # Get unique values of age groups - for the for loop
      age_group_unique <- unique(tx_in_episode_df$age_group)
      
      for(group in 1:length(age_group_unique)){
        # Create a subset of age group
        each_group <- tx_in_episode_by_age[age_group==age_group_unique[group]]
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
        # Prepare denominator (all prevalence counts )
        tx_in_episode_counts_min <- tx_in_episode_counts[,c("YM", "N")]
        setnames(tx_in_episode_counts_min, "N", "Freq")
        # Create counts file
        tx_in_episode_age_counts <- merge(x = each_group, y = tx_in_episode_counts_min, by = c("YM"), all.x = TRUE)
        tx_in_episode_age_counts <- tx_in_episode_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
        tx_in_episode_age_counts <- tx_in_episode_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
        # Saves files in medicine counts folder
        saveRDS(tx_in_episode_age_counts, paste0(medicines_counts_dir, "/", gsub(".rds", "", med_files[i]), "_age_group_", age_group_unique[group],"_med_use_during_contraception_episodes_counts.rds")) # Saves monthly counts 
        # Move files 
        for (file in list.files(path=medicines_counts_dir, pattern="age_group", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_age_groups, "/",file))}
      }
      
      ##### STRATIFICATION BY INDICATION ###
      
      # Checks if there are indication files and performs action only for DAPs with indication files 
      if(str_detect(med_files[i],"Valproate") & length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
        # Merge data with study population to get date of birth
        tx_in_episode_df_indications <- all_indications[tx_in_episode_df,on=.(person_id), allow.cartesian = T]
        # tx_in_episode_df_indications <- tx_in_episode_df_indications[Date>indication_date,]
        tx_in_episode_df_indications[is.na(indication)|indication_date>Date, indication:=NA]
        tx_in_episode_df_indications_missing<- tx_in_episode_df_indications[is.na(indication),]
        tx_in_episode_df_indications_missing[,final_indication:="unknown"]
        tx_in_episode_df_indications_notmissing<- tx_in_episode_df_indications[!is.na(indication),]
        tx_in_episode_df_indications_notmissing[,indication_count:=length(unique(indication)), by = .(person_id, Date)]
        tx_in_episode_df_indications_notmissing[indication_count==1, final_indication:=indication][indication_count>1,final_indication:="multiple"]
        tx_in_episode_df_indications_notmissing[,indication_count:=NULL]
        tx_in_episode_df_indications<-rbind(tx_in_episode_df_indications_missing,tx_in_episode_df_indications_notmissing)
        # Performs pgtests counts - stratified by age group
        tx_in_episode_by_indication <- tx_in_episode_df_indications[,.N, by = .(year(Date),month(Date), final_indication)]
        # Get unique values of age groups - for the for loop
        indication_unique <- unique(tx_in_episode_by_indication$final_indication)
        
        for(group in 1:length(indication_unique)){
          # Create a subset of age group
          each_group <- tx_in_episode_by_indication[final_indication==indication_unique[group]]
          # Adjust for PHARMO
          if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
          # Merge with empty df (for counts that do not have counts for all months and years of study)
          each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
          # Fills in missing values with 0
          each_group[is.na(N), N:=0][is.na(final_indication), final_indication:=indication_unique[group]]
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
          tx_in_episode_all_counts_min <- tx_in_episode_counts[,c("YM", "N")]
          setnames(tx_in_episode_all_counts_min, "N", "Freq")
          # Create counts file
          tx_in_episode_indication_counts <- merge(x = each_group, y = tx_in_episode_all_counts_min, by = c("YM"), all.x = TRUE)
          tx_in_episode_indication_counts <- tx_in_episode_indication_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
          tx_in_episode_indication_counts <- tx_in_episode_indication_counts[,c("YM", "N", "Freq", "rates", "masked","true_value")]
          # Saves files in medicine counts folder
          saveRDS(tx_in_episode_age_counts, paste0(medicines_counts_dir, "/", gsub(".rds", "", med_files[i]), "_indication_", indication_unique[group],"_med_use_during_contraception_episodes_counts.rds")) # Saves monthly counts 
          for (file in list.files(path=medicines_counts_dir, pattern="indication", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_indication, "/",file))}
        }   
      }
    } else {
      print(paste0("No record of ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use during a contraceptive episode." ))
    }
  }
} else {
  print("There are no Contraception records available!")
}


