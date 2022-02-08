#Author: Magda Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022

##################################################################################################################################################
###################################################### OBJECTIVE: 2.5 ############################################################################
##################################################################################################################################################

## Objective 2.5: Proportion of Retinoid/Valproate users with contraception record within 90 days before medication record
# Numerator -> Number of Retinoid/Valproate records with any contraception records (created in contraception_duration.R) within 90 days before medication record
# Denominator -> Total number of Retinoid/Valproate records that month (duplicates removed )
# Intermediate data set -> data set of dispensed/prescribed Retinoid/Valproates per month with column indicating whether there was a prior contraception record 
# THE ABOVE NEEDS TO BE CLARIFIED!!!!
# Records needed -> contraception records (created by contraception_duration.R) 2. Retinoid/Valproate records 

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
### Loads records needed 
# 1. Contraception records 
# Looks for Contraception files in tmp folder 
contra_files <- list.files(paste0(tmp, "all_contraception"), pattern = paste0(pop_prefix, "_all_contra"), recursive = T, ignore.case = T, full.names = T)
# 2. Retinoid/Valproate records 
# Looks for Retinoid/Valproate records in medications folder - this is done in wrapper script run_counts_final_each_pop.R
# name of variable with list of medicines available -> med_files

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

# Checks first if there are any contraception records found
if(length(contra_files)>0) {
  # Loads files + clean up
  contra_df <- readRDS(contra_files) # Loads file
  contra_df <- contra_df[,-c("contraception_meaning", "assumed_duration")] # Keeps necessary columns 
  setnames(contra_df, "Code", "Contraception_code") # Renames column 
  contra_df[,month:=month(contraception_record_date)][,year:=year(contraception_record_date)]
  contra_df <- contra_df[!duplicated(contra_df[,c("person_id", "month", "year")]),]
  contra_df <- contra_df[, -c("month", "year")]
  ##### Loops over medication files to create counts per medication type 
  for(i in 1:length(med_files)){
    ## Loads the medication record
    med_df <- readRDS(paste0(medications_pop, med_files[i])) # Loads file
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
    # Merges contraception df with medication df 
    contra_med_df <- contra_df[med_df, on = .(person_id), allow.cartesian = T] # Left join
    # Creates contra_prior column
    contra_med_df[,contra_prior:=ifelse(Date - contraception_record_date >= 0 & Date - contraception_record_date <= 90 , 1, 0)] # Creates new column contra_prior
    contra_med_df$contra_prior[is.na(contra_med_df$contra_prior)] <- 0 # change NA vales (id's with no contraception records) to 0
    # Creates subset where contraception_prior == 1
    contra_prior_df <- contra_med_df[contra_med_df$contra_prior == 1] # Creates df of patients with contraceptive record dates within 90 days before medication use
    # Performs counts if df created is not empty 
    if (nrow(contra_prior_df)>0){
      # Counts
      contra_prior_counts <- contra_prior_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
      contra_prior_counts <- as.data.table(merge(x = empty_df, y = contra_prior_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with contra_prior_counts
      contra_prior_counts[is.na(contra_prior_counts[,N]), N:=0] # Fills in missing values with 0
      # Masking
      contra_prior_counts$masked_num <- ifelse(contra_prior_counts$N < 5 & contra_prior_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
      if(mask == T){contra_prior_counts[contra_prior_counts$masked_num == 1,]$N <- 5} else {contra_prior_counts[contra_prior_counts$masked_num == 1,]$N <- contra_prior_counts[contra_prior_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
      # Rate calculation
      contra_prior_counts <- within(contra_prior_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      contra_prior_counts <- merge(x = contra_prior_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
      contra_prior_counts <- contra_prior_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
      contra_prior_counts[is.nan(contra_prior_counts$rates)]$rates <- 0
      contra_prior_counts <- contra_prior_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
      setnames(contra_prior_counts, "masked_num", "masked")
      ## Saves intermediate (to counts_df folder) and monthly count files (to contraceptives folder)
      saveRDS(contra_prior_df, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_contraception_prior.rds")) # Saves Contraceptive before records
      saveRDS(contra_prior_counts, paste0(contraceptive_counts_dir, "/", gsub(".rds", "", med_files[i]), "_contraception_prior_counts.rds")) # Saves Contraceptive before counts

    } else {
      print(paste0("No contraceptive records were found within 90 days before ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use." ))
    }
    
  }
} else {
  print("There are no Contraception records available!")
}


