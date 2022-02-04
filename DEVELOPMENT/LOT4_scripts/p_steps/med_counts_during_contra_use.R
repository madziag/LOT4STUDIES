#### Proportion Retinoid/Valproate records that occurred during a contraception episode
## Numerator -> number of Retinoid/Valproate records that occured during a contraception episode
## Denominator -> total number of valp/retin records 

# Loads necessary files 
## Contraceptive episode records
contra_epi_files <- list.files(paste0(g_intermediate,"treatment_episodes/"), pattern = paste0(pop_prefix, "_contraceptive"), recursive = T, full.names = T)
# Checks for presence of contraceptive episode files 
if(length(contra_epi_files)>0) {
  # Loads file 
  contra_epi_df <- as.data.table(readRDS(contra_epi_files)) 
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
    med_df <- as.data.table(readRDS(paste0(medications_pop, med_files[i]))) # Loads file
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
    if(length(contra_epi_files) > 0){
      # Merge med_df with contraceptives episodes file
      # Contraceptive Episodes + Medications
      df <-  contra_epi_df[med_df, on = .(person_id)] # Left join
      # Delete rows with no contraceptive episodes
      df <- df[!is.na(episode.start),]
      # #################################################
      # # Testing ################# TO BE DELETED ###################
      df[person_id == "ConCDM_SIM_200421_00029" & Date == "2019-09-24"]$episode.start <- as.IDate(as.character(20190830), "%Y%m%d")
      df[person_id == "ConCDM_SIM_200421_00029" & Date == "2019-09-24"]$episode.end <- as.IDate(as.character(20191030), "%Y%m%d")
      df[person_id == "ConCDM_SIM_200421_00627" & Date == "2014-09-08"]$episode.start <- as.IDate(as.character(20140821), "%Y%m%d")
      df[person_id == "ConCDM_SIM_200421_00627" & Date == "2014-09-08"]$episode.end <- as.IDate(as.character(20141021), "%Y%m%d")
      df[person_id == "ConCDM_SIM_200421_00428" & Date == "2018-03-12"]$episode.start <- as.IDate(as.character(20180228), "%Y%m%d")
      df[person_id == "ConCDM_SIM_200421_00428" & Date == "2018-03-12"]$episode.end <- as.IDate(as.character(20180430), "%Y%m%d")
      # Testing ################# TO BE DELETED ###################
      df[,episode.start:=as.IDate(episode.start,"%Y%m%d")][,episode.end:=as.IDate(episode.end,"%Y%m%d")]# Converts dates to be in the same format
      df[,tx_in_episode:= fifelse(Date>=episode.start & Date <= episode.end, 1, 0)] # Creates column that indicates if medicine record date is between episode.start and episode.end dates
      tx_during_epi_df <- df[tx_in_episode == 1,] # Creates df of patients who have a medicine record date between episode.start and episode.end dates
      # Checks if there are any records that meet the criteria. If so it does the calculations
      if (nrow(tx_during_epi_df) > 0){
        # Performs counts 
        tx_during_epi_counts <- tx_during_epi_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
        tx_during_epi_counts <- as.data.table(merge(x = empty_df, y = tx_during_epi_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with tx_during_epi_counts
        tx_during_epi_counts[is.na(tx_during_epi_counts[,N]), N:=0] # Fills in missing values with 0
        # Masking
        tx_during_epi_counts$masked_num <- ifelse(tx_during_epi_counts$N < 5 & tx_during_epi_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
        if(mask == T){tx_during_epi_counts[tx_during_epi_counts$masked_num == 1,]$N <- 5} else {tx_during_epi_counts[tx_during_epi_counts$masked_num == 1,]$N <- tx_during_epi_counts[tx_during_epi_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
        ############################
        ##### Calculates Rates #####
        ############################
        tx_during_epi_counts <- within(tx_during_epi_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
        tx_during_epi_counts <- merge(x = tx_during_epi_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
        tx_during_epi_counts <- tx_during_epi_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
        tx_during_epi_counts$rates[is.nan(tx_during_epi_counts$rates)]<-0
        tx_during_epi_counts <- tx_during_epi_counts[,c("YM", "N", "Freq", "rates", "masked_num")]
        setnames(tx_during_epi_counts, "masked_num", "masked")
        ####################################################
        ##### Saves individual level records +  counts #####
        ####################################################  
        saveRDS(tx_during_epi_df, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_med_use_during_contra_episodes.rds")) # Saves Pregtest Prior records 
        saveRDS(tx_during_epi_counts,  paste0(contraceptive_counts_dir, "/", gsub(".rds", "", med_files[i]), "_med_use_during_contraception_episodes_counts.rds")) # Saves Contraceptive before counts
      } else {print(paste0(gsub(".rds", "",med_files[i]), " study: There are no patients with medical record dates that fall between episode start and end dates!"))}
    }
  }
  
} else {
  print ("No contraceptive episodes have been created")
}




