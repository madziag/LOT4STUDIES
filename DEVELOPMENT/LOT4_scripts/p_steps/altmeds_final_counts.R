#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/12/2021
# 4.1 Incidence rates of alternative medication prescription/dispensings in ever-valproate users per month

### DENOMINATOR FILE ###
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

### RETINOIDS ###
# Get list of alternative meds for retinoids
altmeds_retinoids <- list.files(medications_pop, pattern="altmed_retin", ignore.case = T, recursive = T)
# Filter for subpopulation if any (should only do counts of current subpopulation)
if(pop_prefix == "PC"){altmeds_retinoids <- altmeds_retinoids[!grepl("PC_HOSP",altmeds_retinoids)]}
if(pop_prefix == "PC_HOSP"){altmeds_retinoids <- altmeds_retinoids[grepl("PC_HOSP",altmeds_retinoids)]}

# Only if Retinoid altmeds are present
if(length(altmeds_retinoids)>0){
  # For each of the altmeds in altmed list
  for(med in 1:length(altmeds_retinoids)){
    # Gets full path of altmed
    altmeds_retinoids_file <- list.files(medications_pop, pattern=altmeds_retinoids[med], ignore.case = T, recursive = T, full.names = T)
    # Only if altmed df is not empty (should not be but just in case)
    if(length(altmeds_retinoids_file)>0){
      # Loads alternative meds df 
      altmed_ret_df <- readRDS(altmeds_retinoids_file)
      # Checks that you only have records between the patients entry and exit into study dates
      altmed_ret_df <- altmed_ret_df[Date>=entry_date&Date<exit_date,]
      # Removes any duplicates
      altmed_ret_df <- altmed_ret_df[!duplicated(altmed_ret_df),]
      # Performs medicine counts during contraceptive episode - stratified by age group
      altmed_ret_counts <- altmed_ret_df[,.N, by = .(year(Date),month(Date))]
      # Adjust for PHARMO
      if(is_PHARMO){altmed_ret_counts<-altmed_ret_counts[year<2020,]}else{altmed_ret_counts<-altmed_ret_counts[year<2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      altmed_ret_counts <- as.data.table(merge(x = empty_df, y = altmed_ret_counts, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      altmed_ret_counts[is.na(N), N:=0]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      altmed_ret_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      altmed_ret_counts <- within(altmed_ret_counts, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      altmed_ret_counts[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){altmed_ret_counts[masked==1,N:=5]} else {altmed_ret_counts[masked==1,N:=N]}
      # Prepare denominator 
      denominator1 <- denominator[,c("YM", "Freq")]
      # Create counts file
      altmed_ret_counts1 <- merge(x = altmed_ret_counts, y = denominator1, by = c("YM"), all.x = TRUE)
      altmed_ret_counts1 <- altmed_ret_counts1[,rates:=round((as.numeric(N)/as.numeric(Freq))*1000,5)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_ret_counts1 <- altmed_ret_counts1[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Creates a prefix name to add to saved file
      altmed_name <- gsub(".rds","", altmeds_retinoids[med])
      altmed_name <- gsub(paste0(pop_prefix, "_altmed_"), "", altmed_name)
      # Saves monthly counts in medicine counts folder
      # 
      saveRDS(altmed_ret_counts1, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", altmed_name,"_final_counts.rds")) 
    }
  }
}


### VALPROATES ###
# Get list of alternative meds for retinoids
altmeds_valproates <- list.files(medications_pop, pattern="altmed_valp", ignore.case = T, recursive = T)
# Filter for subpopulation if any (should only do counts of current subpopulation)
if(pop_prefix == "PC"){altmeds_valproates <- altmeds_valproates[!grepl("PC_HOSP",altmeds_valproates)]}
if(pop_prefix == "PC_HOSP"){altmeds_valproates <- altmeds_valproates[grepl("PC_HOSP",altmeds_valproates)]}

# Only if Retinoid altmeds are present
if(length(altmeds_valproates )>0){
  # For each of the altmeds in altmed list
  for(med in 1:length(altmeds_valproates )){
    # Gets full path of altmed
    altmeds_valproates_file <- list.files(medications_pop, pattern=altmeds_valproates[med], ignore.case = T, recursive = T, full.names = T)
    # Only if altmed df is not empty (should not be but just in case)
    if(length(altmeds_valproates_file)>0){
      # Loads alternative meds df 
      altmed_valp_df <- readRDS(altmeds_valproates_file)
      # Checks that you only have records between the patients entry and exit into study dates
      altmed_valp_df <- altmed_valp_df[Date>=entry_date&Date<exit_date,]
      # Removes any duplicates
      altmed_valp_df <- altmed_valp_df[!duplicated(altmed_valp_df),]
      # Performs medicine counts during contraceptive episode - stratified by age group
      altmed_valp_counts <- altmed_valp_df[,.N, by = .(year(Date),month(Date))]
      # Adjust for PHARMO
      if(is_PHARMO){altmed_valp_counts<-altmed_valp_counts[year<2020,]}else{altmed_valp_counts<-altmed_valp_counts[year<2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      altmed_valp_counts <- as.data.table(merge(x = empty_df, y = altmed_valp_counts, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      altmed_valp_counts[is.na(N), N:=0]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      altmed_valp_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      altmed_valp_counts <- within(altmed_valp_counts, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      altmed_valp_counts[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){altmed_valp_counts[masked==1,N:=5]} else {altmed_valp_counts[masked==1,N:=N]}
      # Prepare denominator 
      denominator1 <- denominator[,c("YM", "Freq")]
      # Create counts file
      altmed_valp_counts1 <- merge(x = altmed_valp_counts, y = denominator1, by = c("YM"), all.x = TRUE)
      altmed_valp_counts1 <- altmed_valp_counts1[,rates:=round((as.numeric(N)/as.numeric(Freq))*1000,5)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_valp_counts1 <- altmed_valp_counts1[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Creates a prefix name to add to saved file
      altmed_name <- gsub(".rds","", altmeds_valproates[med])
      altmed_name <- gsub(paste0(pop_prefix, "_altmed_"), "", altmed_name)
      # Saves monthly counts in medicine counts folder
      # 
      saveRDS(altmed_valp_counts1, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", altmed_name,"_final_counts.rds")) 
    }
  }
}

# Clean up
rm(list = grep("^altmed_|denominator1", ls(), value = TRUE))













