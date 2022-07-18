#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/12/2021
# 4.1 Incidence rates of alternative medication prescription/dispensings in ever-valproate users per month

# Loads denominator file
source(paste0(pre_dir,"load_denominator.R"))

### RETINOIDS ###
# Get list of alternative meds for retinoids (multiple prescription dates per person per altmed )
altmeds_retinoids<-list.files(altmeds_dir, pattern="altmed_retin", ignore.case = T, recursive = T)
# Filter for subpopulation if any (should only do counts of current subpopulation)
if(pop_prefix == "PC"){altmeds_retinoids<-altmeds_retinoids[!grepl("PC_HOSP",altmeds_retinoids)]}
if(pop_prefix == "PC_HOSP"){altmeds_retinoids<-altmeds_retinoids[grepl("PC_HOSP",altmeds_retinoids)]}

# Only if Retinoid altmeds are present
if(length(altmeds_retinoids)>0){
  # For each of the altmeds in altmed list
  for(med in 1:length(altmeds_retinoids)){
    # Gets full path of altmed
    altmeds_retinoids_file<-list.files(altmeds_dir, pattern=altmeds_retinoids[med], ignore.case = T, recursive = T, full.names = T)
    # Only if altmed df is not empty (should not be but just in case)
    if(length(altmeds_retinoids_file)>0){
      # Loads alternative meds df 
      altmed_ret_df<-readRDS(altmeds_retinoids_file)
      # Checks that you only have records between the patients entry and exit into study dates
      altmed_ret_df<-altmed_ret_df[Date>=entry_date&Date<exit_date,]
      # Removes any duplicates
      altmed_ret_df<-altmed_ret_df[!duplicated(altmed_ret_df),]
      # Performs medicine counts during contraceptive episode - stratified by age group
      altmed_ret_counts<-altmed_ret_df[,.N, by = .(year(Date),month(Date))]
      # Adjust for PHARMO
      if(is_PHARMO){altmed_ret_counts<-altmed_ret_counts[year<2020,]}else{altmed_ret_counts<-altmed_ret_counts[year<2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      altmed_ret_counts<-as.data.table(merge(x = empty_df, y = altmed_ret_counts, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      altmed_ret_counts[is.na(N), N:=0]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      altmed_ret_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking is not applied 
      altmed_ret_counts[,masked:=0]
      # Create YM variable 
      altmed_ret_counts<-within(altmed_ret_counts, YM<- sprintf("%d-%02d", year, month))
      # # Prepare denominator 
      denominator1<-denominator[,c("YM", "Freq")]
      # Create counts file
      altmed_ret_counts1<-merge(x = altmed_ret_counts, y = denominator1, by = c("YM"), all.x = TRUE)
      altmed_ret_counts1<-altmed_ret_counts1[,rates:=round((as.numeric(N)/as.numeric(Freq))*1000,5)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_ret_counts1<-altmed_ret_counts1[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Creates a prefix name to add to saved file
      altmed_name<-gsub(".rds","", altmeds_retinoids[med])
      altmed_name<-gsub(paste0(pop_prefix, "_altmed_"), "", altmed_name)
      # Saves monthly counts in medicine counts folder
      # 
      saveRDS(altmed_ret_counts1, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", altmed_name,"_final_counts.rds")) 
    }
  }
}


### VALPROATES ###
# Get list of alternative meds for retinoids
altmeds_valproates<-list.files(altmeds_dir, pattern="altmed_valp", ignore.case = T, recursive = T)
# Filter for subpopulation if any (should only do counts of current subpopulation)
if(pop_prefix == "PC"){altmeds_valproates<-altmeds_valproates[!grepl("PC_HOSP",altmeds_valproates)]}
if(pop_prefix == "PC_HOSP"){altmeds_valproates<-altmeds_valproates[grepl("PC_HOSP",altmeds_valproates)]}

# Only if Retinoid altmeds are present
if(length(altmeds_valproates )>0){
  # For each of the altmeds in altmed list
  for(med in 1:length(altmeds_valproates )){
    # Gets full path of altmed
    altmeds_valproates_file<-list.files(altmeds_dir, pattern=altmeds_valproates[med], ignore.case = T, recursive = T, full.names = T)
    # Only if altmed df is not empty (should not be but just in case)
    if(length(altmeds_valproates_file)>0){
      # Loads alternative meds df 
      altmed_valp_df<-readRDS(altmeds_valproates_file)
      # Checks that you only have records between the patients entry and exit into study dates
      altmed_valp_df<-altmed_valp_df[Date>=entry_date&Date<exit_date,]
      # Removes any duplicates
      altmed_valp_df<-altmed_valp_df[!duplicated(altmed_valp_df),]
      # Performs medicine counts during contraceptive episode - stratified by age group
      altmed_valp_counts<-altmed_valp_df[,.N, by = .(year(Date),month(Date))]
      # Adjust for PHARMO
      if(is_PHARMO){altmed_valp_counts<-altmed_valp_counts[year<2020,]}else{altmed_valp_counts<-altmed_valp_counts[year<2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      altmed_valp_counts<-as.data.table(merge(x = empty_df, y = altmed_valp_counts, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      altmed_valp_counts[is.na(N), N:=0]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      altmed_valp_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking is not applied before stratification
      altmed_valp_counts[,masked:=0]
      # Create YM variable 
      altmed_valp_counts<-within(altmed_valp_counts, YM<- sprintf("%d-%02d", year, month))
      # Prepare denominator 
      denominator1<-denominator[,c("YM", "Freq")]
      # Create counts file
      altmed_valp_counts1<-merge(x = altmed_valp_counts, y = denominator1, by = c("YM"), all.x = TRUE)
      altmed_valp_counts1<-altmed_valp_counts1[,rates:=round((as.numeric(N)/as.numeric(Freq))*1000,5)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_valp_counts1<-altmed_valp_counts1[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Creates a prefix name to add to saved file
      altmed_name<-gsub(".rds","", altmeds_valproates[med])
      altmed_name<-gsub(paste0(pop_prefix, "_altmed_"), "", altmed_name)
      # Saves monthly counts in medicine counts folder
      # 
      saveRDS(altmed_valp_counts1, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", altmed_name,"_final_counts.rds")) 
    }
  }
}

# Clean up
rm(list = grep("^altmed_|denominator1", ls(), value = TRUE))













