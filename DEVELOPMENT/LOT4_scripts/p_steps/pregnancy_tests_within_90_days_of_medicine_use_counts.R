#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 07/02/2022

##################################################################################################################################################
##################################################### OBJECTIVES: 2.1, 2.2 #######################################################################
##################################################################################################################################################

## Objective 2.1: Proportion of Retinoid/Valproate users with pregnancy test performed within 90 days before medication record
# Numerator -> Number of Retinoid/Valproate records with a pregnancy test record (from events, procedures and medical observation tables) within 90 days before medication record
# Denominator -> Total number of Retinoid/Valproate records that month (duplicates removed )
# Intermediate data set -> data set of dispensed/prescribed Retinoid/Valproates per month with column indicating whether there was a prior pregnancy test 
# Records needed -> pregnancy test records (combination of pregtest records from events, procedures and medical observations) 2. Retinoid/Valproate records 

## Objective 2.2: Proportion of Retinoid/Valproate users with pregnancy test performed within 90 days after medication record
# Numerator -> Number of Retinoid/Valproate records with a pregnancy test record (from events, procedures and medical observation tables) within 90 days before medication record
# Denominator -> Total number of Retinoid/Valproate records that month (duplicates removed )
# Intermediate data set -> data set of dispensed/prescribed Retinoid/Valproates per month with column indicating whether there was a prior pregnancy test 
# Records needed -> pregnancy test records (combination of pregtest records from events, procedures and medical observations) 2. Retinoid/Valproate records 

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

### Loads records needed 
# 1. Pregnancy test records 
# Looks for pregnancy test files in tmp folder 
pregtest_files<-list.files(pregtest_dir, recursive = T, ignore.case = T, full.names = T)
if(pop_prefix=="PC"){pregtest_files<-pregtest_files[!grepl("PC_HOSP",pregtest_files)]}
if(pop_prefix=="PC_HOSP"){pregtest_files<-pregtest_files[grepl("PC_HOSP",pregtest_files)]}
# 2. Loads denominator file 
source(paste0(pre_dir,"load_denominator.R"))

# Create folder to store objective 2 individual level records 
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"objective_2")),dir.create(paste0(counts_dfs_dir,"objective_2")),FALSE))
objective2_dir<-paste0(counts_dfs_dir,"objective_2/")

# Checks first if there are any pregnancy test records found
if(length(pregtest_files)>0) {
  # Loads files + clean up
  pregtest_df<-do.call(rbind,lapply(pregtest_files, readRDS)) # Loads file
  # pregtest_df<-pregtest_df[,c("person_id", "Date", "Code", "Vocabulary")] # Keeps necessary columns 
  setnames(pregtest_df, "Date", "Pregtest_date") # Renames column 
  setnames(pregtest_df, "Code", "Pregtest_code") # Renames column 
  setnames(pregtest_df, "Vocabulary", "Pregtest_vocabulary") # Renames column
  pregtest_df[,month:=month(Pregtest_date)][,year:=year(Pregtest_date)] # Creates month/year cols (for the purposes of deduplicating - only 1 pregnancy test record per person_id per month/year is allowed)
  pregtest_df<-pregtest_df[!duplicated(pregtest_df[,c("person_id", "month", "year")]),] # Removes duplicates
  pregtest_df<-pregtest_df[,-c("month", "year")]
  
  ##### Loops over medication files to create counts per medication type 
  for(i in 1:length(med_files)){
    ## Loads the medication record
    med_df<-readRDS(paste0(medications_pop, med_files[i])) # Loads file
    med_df<-med_df[Date>=entry_date & Date<exit_date] # Get med records only between entry and exit dates 
    med_df<-med_df[ ,c("person_id", "Date", "Code")] # Keeps necessary columns
    setnames(med_df, "Code", "ATC") # Renames column 
    ### Creates denominator: Total number of Retinoid/Valproate records per month
    med_counts<-med_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
    med_counts<-as.data.table(merge(x = empty_df, y = med_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with med_counts
    med_counts[is.na(med_counts[,N]), N:=0] # Fills in missing values with 0
    setnames(med_counts, "N", "Freq") # Renames column
    ### Creates numerators
    # Merges pregtest df with medication df 
    pregtest_med_df<-pregtest_df[med_df, on = .(person_id), allow.cartesian = T] # Left join
    # Creates pregtest_prior and pregtest_after columns 
    pregtest_med_df[,pregtest_prior:=ifelse(Date - Pregtest_date >= 0 & Date - Pregtest_date <= 90 , 1, 0)]
    pregtest_med_df[,pregtest_after:=ifelse(Pregtest_date - Date >= 0 & Pregtest_date - Date <= 90 , 1, 0)] 
    # Changes NA values in new columns to 0
    pregtest_med_df[is.na(pregtest_prior),pregtest_prior:=0]
    pregtest_med_df[is.na(pregtest_after),pregtest_after:=0]
    # Creates subsets where pregtest_prior or pregtest_after == 1
    pregtest_prior_df<-pregtest_med_df[pregtest_prior == 1,][,pregtest_after:= NULL]
    pregtest_after_df<-pregtest_med_df[pregtest_after == 1,][,pregtest_prior := NULL]
    # Performs counts if df created is not empty 
    # Pregtest prior
    if (nrow(pregtest_prior_df)>0){
      # Counts
      pregtest_prior_counts<-pregtest_prior_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
      pregtest_prior_counts<-as.data.table(merge(x = empty_df, y = pregtest_prior_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with pregtest_prior_counts
      pregtest_prior_counts[is.na(pregtest_prior_counts[,N]), N:=0] # Fills in missing values with 0
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      pregtest_prior_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking is not applied before stratification
      pregtest_prior_counts[,masked:=0]
      # Rate calculation
      pregtest_prior_counts<-within(pregtest_prior_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      pregtest_prior_counts<-merge(x = pregtest_prior_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
      pregtest_prior_counts<-pregtest_prior_counts[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
      # Keep necessary columns
      pregtest_prior_counts<-pregtest_prior_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      ## Saves intermediate (to counts_df folder) and monthly count files (to pregnancy_counts folder)
      saveRDS(pregtest_prior_df, paste0(objective2_dir, gsub("_MEDS.rds", "", med_files[i]), "_pgtests_prior.rds")) # Intermediate file
      saveRDS(pregtest_prior_counts, paste0(pregnancy_test_counts_dir, "/", gsub("_MEDS.rds", "", med_files[i]), "_pgtests_prior_counts.rds")) # Monthly counts file 
      
    } else {
      print(paste0("No pregnancy tests were found within 90 days before ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use." ))
    }
    # Pregtest after
    if (nrow(pregtest_after_df)>0){
      # Counts
      pregtest_after_counts<-pregtest_after_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
      pregtest_after_counts<-as.data.table(merge(x = empty_df, y = pregtest_after_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with pregtest_after_counts
      pregtest_after_counts[is.na(pregtest_after_counts[,N]), N:=0] # Fills in missing values with 0
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      pregtest_after_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      pregtest_after_counts[,masked:=0]
      # Calculates rates
      pregtest_after_counts<-within(pregtest_after_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      pregtest_after_counts<-merge(x = pregtest_after_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
      pregtest_after_counts<-pregtest_after_counts[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][is.nan(rates)|is.na(rates), rates:=0]
      pregtest_after_counts<-pregtest_after_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      ## Saves intermediate (to counts_df folder ) and monthly count files (to pregnancy_counts folder)
      saveRDS(pregtest_after_df, paste0(objective2_dir, gsub("_MEDS.rds", "", med_files[i]), "_pgtests_after.rds")) # Intermediate file
      saveRDS(pregtest_after_counts, paste0(pregnancy_test_counts_dir, "/", gsub("_MEDS.rds", "", med_files[i]), "_pgtests_after_counts.rds")) # Monthly counts file 
    } else {
      print(paste0("No pregnancy tests were found within 90 days after ", strsplit(gsub("_MEDS.rds", "", med_files[i]), "_")[[1]][2], " use." ))
    }
  }
} else {
  print("There are no Pregnancy Test records available!")
}


# Clean up 
rm(list = grep("^pregtest", ls(), value = TRUE))
rm(med_df)
