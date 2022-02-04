##### 1. Looks for pregnancy tests performed in Retinoid/valproate users:
####### a. within the 90 days before use of the medication
####### b. within the 90 days after use of the medication
##### 2. Looks for contraceptive (all types) use in Retinoid/Valproate users within the 90 days before use of the medication

##### Loads needed records
## Pregnancy test records
pregtest_files <- list.files(tmp, pattern = paste0(c("pregtest.rds", "preg_test.rds"), collapse = "|"), recursive = T, ignore.case = T, full.names = T)

if (length(pregtest_files) > 0){
  pregtest_df <- do.call(rbind,lapply(pregtest_files, readRDS)) # Loads file
  pregtest_df <- pregtest_df[,c("person_id", "Date", "Code", "Vocabulary")] # Keeps necessary columns 
  setnames(pregtest_df, "Date", "Pregtest_date") # Renames column 
  setnames(pregtest_df, "Code", "Pregtest_code") # Renames column 
  setnames(pregtest_df, "Vocabulary", "Pregtest_vocabulary") # Renames column
} else {
  print("There are no Pregnancy Test records to evaluate")
}

## Contraceptive records
contra_files <- list.files(paste0(tmp, "all_contraception"), pattern = paste0(pop_prefix, "_all_contra"), recursive = T, ignore.case = T, full.names = T)

if(length(contra_files) >0){
  contra_df    <- readRDS(contra_files) # Loads file
  contra_df    <- contra_df[,-c("contraception_meaning", "assumed_duration")] # Keeps necessary columns 
  setnames(contra_df, "Code", "Contraception_code") # Renames column 
} else {
  print("There are no Contraceptive records to evaluate")
}

#### Testing purposes ####
pregtest_df[pregtest_df$person_id == "ConCDM_SIM_200421_00123"]$person_id <- "ConCDM_SIM_200421_00029"
pregtest_df[pregtest_df$person_id == "ConCDM_SIM_200421_00079"]$person_id <- "ConCDM_SIM_200421_00331"
pregtest_df[pregtest_df$person_id == "ConCDM_SIM_200421_00225"]$person_id <- "ConCDM_SIM_200421_00925"
pregtest_df[pregtest_df$person_id == "ConCDM_SIM_200421_00629"]$person_id <- "ConCDM_SIM_200421_00247"
pregtest_df[pregtest_df$person_id == "ConCDM_SIM_200421_00945"]$person_id <- "ConCDM_SIM_200421_00627"

  
if(length(pregtest_files)>0 | length(contra_files)>0){
  ## Retinoid/Valproate records
  # list of medication files assigned to med_files in wrapper script
  ##### Creates empty_df for counts using denominator file 
  denominator     <- as.data.table(readRDS(paste0(output_dir, pop_prefix, "_denominator.rds"))) # Loads denominator file
  denominator[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)] # Splits Y-M column 
  empty_df        <- expand.grid(seq(min(denominator$Y), max(denominator$Y)), seq(1, 12)) # Creates df using min and max years of denominator file
  names(empty_df) <- c("year", "month") # Renames newly created columns 
  
  ##### Loops over medication files to create counts per medication type 
  for(i in 1:length(med_files)){
    ## Loads the medication record
    med_df <- readRDS(paste0(medications_pop, med_files[i])) # Loads file
    med_df <- med_df[ ,c("person_id", "Date", "Code")] # Keeps necessary columns
    setnames(med_df, "Code", "ATC") # Renames column
    
    #######################
    ##### Denominator #####
    #######################
    # 1. Total number of Retinoid/Valproate records that month
    # Performs counts
    med_counts <- med_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
    med_counts <- as.data.table(merge(x = empty_df, y = med_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with med_counts
    med_counts[is.na(med_counts[,N]), N:=0] # Fills in missing values with 0
    setnames(med_counts, "N", "Freq") # Renames column
    # Masking
    med_counts$masked_den <- ifelse(med_counts$Freq < 5 & med_counts$Freq > 0, 1, 0) # Creates column that indicates if count value will be masked_den if mask = TRUE
    if(mask == T){med_counts[med_counts$masked_den == 1,]$Freq <- 5} else {med_counts[med_counts$masked_den == 1,]$Freq <- med_counts[med_counts$masked_den == 1,]$Freq} # Changes values less than 5 and more than 0 to 5
    
    ######################
    ##### Numerators #####
    ######################
    ## Check if there are any pregnancy test records 
    if(length(pregtest_files) > 0){
      ## Merge pregtest and contraceptive records with Retinoid/Valproate records
      # Pregtest + Medications
      pregtest_med_df <- pregtest_df[med_df, on = .(person_id), allow.cartesian = T] # Left join
      # 1. Number of Valproate/Retinoid records of female subjects who have a pregnancy test record <= 90 days before the Valproate/Retinoid record
      pregtest_med_df[,pregtest_prior:=ifelse(Date - Pregtest_date >= 0 & Date - Pregtest_date <= 90 , 1, 0)] # Creates new column pregtest_prior
      pregtest_med_df$pregtest_prior[is.na(pregtest_med_df$pregtest_prior)] <- 0 # Changes NA vales (id's with no pregtest done) to 0
      pregtest_prior_df <- pregtest_med_df[pregtest_med_df$pregtest_prior == 1] # Creates df of patients with pregnancy tests performed within 90 days before medication use
      # Checks if any records present
      if (nrow(pregtest_prior_df) > 0){
        # Performs counts 
        pregtest_prior_counts <- pregtest_prior_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
        pregtest_prior_counts <- as.data.table(merge(x = empty_df, y = pregtest_prior_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with pregtest_prior_counts
        pregtest_prior_counts[is.na(pregtest_prior_counts[,N]), N:=0] # Fills in missing values with 0
        # Masking
        pregtest_prior_counts$masked_num <- ifelse(pregtest_prior_counts$N < 5 & pregtest_prior_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
        if(mask == T){pregtest_prior_counts[pregtest_prior_counts$masked_num == 1,]$N <- 5} else {pregtest_prior_counts[pregtest_prior_counts$masked_num == 1,]$N <- pregtest_prior_counts[pregtest_prior_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
        ############################
        ##### Calculates Rates #####
        ############################
        pregtest_prior_counts <- within(pregtest_prior_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
        pregtest_prior_counts <- merge(x = pregtest_prior_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
        pregtest_prior_counts <- pregtest_prior_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
        pregtest_prior_counts$rates[is.nan(pregtest_prior_counts$rates)]<-0
        pregtest_prior_counts <- pregtest_prior_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
        setnames(pregtest_prior_counts, "masked_num", "masked")
        ####################################################
        ##### Saves individual level records +  counts #####
        ####################################################  
        saveRDS(pregtest_prior_df, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_pgtests_prior.rds")) # Saves Pregtest Prior records 
        saveRDS(pregtest_prior_counts, paste0(pregnancy_test_counts_dir, "/", gsub(".rds", "", med_files[i]), "_pgtests_prior_counts.rds")) # Saves Pregtest Prior counts
        
        # Clean up
        rm(pregtest_prior_df, pregtest_prior_counts)
      } else {
        print(paste0("No pregnancy tests were recorded within the 90 days before ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use." ))
      }
      # 2. Number of Valproate/Retinoid records of female subjects who have a pregnancy test record <= 90 days after the Valproate/Retinoid record
      pregtest_med_df[,pregtest_after:=ifelse(Pregtest_date - Date >= 0 & Pregtest_date - Date <= 90 , 1, 0)] # Creates new column pregtest_after
      pregtest_med_df$pregtest_after[is.na(pregtest_med_df$pregtest_after)] <- 0 # change NA vales (id's with no pregtest done) to 0
      pregtest_after_df <- pregtest_med_df[pregtest_med_df$pregtest_after == 1] # Creates df of patients with pregnancy tests performed within 90 days after medication use
      # Checks if any records present
      if (nrow(pregtest_after_df) > 0){
        pregtest_after_df <- pregtest_after_df[,-c("pregtest_prior")] # Removes pregtest_prior column - clean up before saving file 
        # Performs counts
        pregtest_after_counts <- pregtest_after_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
        pregtest_after_counts <- as.data.table(merge(x = empty_df, y = pregtest_after_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with pregtest_after_counts
        pregtest_after_counts[is.na(pregtest_after_counts[,N]), N:=0] # Fills in missing values with 0
        # Masking
        pregtest_after_counts$masked_num <- ifelse(pregtest_after_counts$N < 5 & pregtest_after_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
        if(mask == T){pregtest_after_counts[pregtest_after_counts$masked_num == 1,]$N <- 5} else {pregtest_after_counts[pregtest_after_counts$masked_num == 1,]$N <- pregtest_after_counts[pregtest_after_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
        ############################
        ##### Calculates Rates #####
        ############################
        pregtest_after_counts <- within(pregtest_after_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
        pregtest_after_counts <- merge(x = pregtest_after_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
        pregtest_after_counts <- pregtest_after_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
        pregtest_after_counts$rates[is.nan(pregtest_after_counts$rates)]<-0
        pregtest_after_counts <- pregtest_after_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
        setnames(pregtest_after_counts, "masked_num", "masked")
        
        ####################################################
        ##### Saves individual level records +  counts #####
        ####################################################  
        saveRDS(pregtest_after_df, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_pgtests_after.rds")) # Saves Pregtest After records
        saveRDS(pregtest_after_counts, paste0(pregnancy_test_counts_dir, "/", gsub(".rds", "", med_files[i]), "_pgtests_after_counts.rds")) # Saves Pregtest After counts
        rm(pregtest_after_df, pregtest_after_counts)
      } else {
        print(paste0("No pregnancy tests were recorded within 90 days after ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use." ))
      }
    }
    
    if(length(contra_files > 0)){
      # 3. Number of Valproate/Retinoid records of female subjects with any contraception record <= 90 days before the Valproate/Retinoid record
      ## Merge pregtest and contraceptive records with Retinoid/Valproate records
      # Contraceptive + Medications
      # Merge with pregnancy test records 
      contra_med_df   <- contra_df[med_df, on = .(person_id), allow.cartesian = T] # Left join
      contra_med_df[,contra_prior:=ifelse(Date - contraception_record_date >= 0 & Date - contraception_record_date <= 90 , 1, 0)] # Creates new column contra_prior
      contra_med_df$contra_prior[is.na(contra_med_df$contra_prior)] <- 0 # change NA vales (id's with no pregtest done) to 0
      contra_prior_df <- contra_med_df[contra_med_df$contra_prior == 1] # Creates df of patients with contraceptive record dates within 90 days before medication use
      # Checks if any records present
      if (nrow(contra_prior_df) > 0){
        # Performs counts
        contra_prior_counts <- contra_prior_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
        contra_prior_counts <- as.data.table(merge(x = empty_df, y = contra_prior_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with contra_prior_counts
        contra_prior_counts[is.na(contra_prior_counts[,N]), N:=0] # Fills in missing values with 0
        # Masking
        contra_prior_counts$masked_num <- ifelse(contra_prior_counts$N < 5 & contra_prior_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
        if(mask == T){contra_prior_counts[contra_prior_counts$masked_num == 1,]$N <- 5} else {contra_prior_counts[contra_prior_counts$masked_num == 1,]$N <- contra_prior_counts[contra_prior_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
        ###########################
        ##### Calculate Rates #####
        ###########################
        # 3. Contraceptives Before 
        contra_prior_counts <- within(contra_prior_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
        contra_prior_counts <- merge(x = contra_prior_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
        contra_prior_counts <- contra_prior_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
        contra_prior_counts$rates[is.nan(contra_prior_counts$rates)]<-0
        contra_prior_counts$masked <- ifelse(contra_prior_counts$masked_num == 1 | contra_prior_counts$masked_den == 1, 1, 0)
        contra_prior_counts <- contra_prior_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
        setnames(contra_prior_counts, "masked_num", "masked")
        ###################################################
        ##### Saves individual level records + counts #####
        ###################################################
        saveRDS(contra_prior_df,   paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_contraception_prior.rds")) # Saves Contraceptive before records
        saveRDS(contra_prior_counts,   paste0(contraceptive_counts_dir, "/", gsub(".rds", "", med_files[i]), "_contraception_prior_counts.rds")) # Saves Contraceptive before counts
        # Clean up
        rm(contra_files, contra_med_df,contra_prior_df, contra_prior_counts)
      } else {
        print(paste0("No contraceptives were recorded within the 90 days before ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use." ))
      }
    }
  }
}






