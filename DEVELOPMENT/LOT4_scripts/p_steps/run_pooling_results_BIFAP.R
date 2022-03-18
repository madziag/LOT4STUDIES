### mask = F
mask = T

# run_pooling <-  function(mask= T){
### BIFAP POOLING ###
print("##################################################")
print("##################################################")
print("################# POOLING RESULTS...##############")
print("##################################################")
print("##################################################")


###########################################################################################################################################################################
########### COUNTS ########################################################################################################################################################
###########################################################################################################################################################################
# 1. Creates ALL_regions folder 
invisible(ifelse(!dir.exists(paste0(projectFolder, "/ALL_regions")), dir.create(paste0(projectFolder, "/ALL_regions")), FALSE))
All_regions_dir<-paste0(projectFolder, "/ALL_regions/")
print("Copying files...")

# 2. Copies all counts, baseline tables, denominator csv files from each region, prefixing each file with the name of the region
## Gets a list of region names from the CDMInstances folder 
regions <- list.dirs(path = multiple_regions_dir, full.names = FALSE, recursive = FALSE)
## Loops over each region
for(reg in 1:length(regions)){
  # Find counts, baseline tables and denominator csv files in each regional folder 
  files_to_move <- list.files(paste0(projectFolder, "/", regions[reg]), pattern = paste0(c("counts.csv", "denominator.csv", "pooling_baseline_tables.rds"), collapse="|"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  # Creates df with list of all files to be moved 
  f1 <- as.data.frame(files_to_move, header = FALSE)
  colnames(f1)  <- 'files.old' # Gives a name to the column
  # Creates new paths for the files to be copied in second column 
  f1$files.new <- stri_replace_all_regex(f1$files.old,
                                         pattern=c(paste0(regions[reg], "/g_intermediate/recs_for_baseline_table_pooling/"), 
                                                   paste0(regions[reg], "/g_output/contraceptive_counts/csv_files/"),
                                                   paste0(regions[reg], "/g_output/medicines_counts/csv_files/"),
                                                   paste0(regions[reg], "/g_output/pregnancy_counts/csv_files/"), 
                                                   paste0(regions[reg], "/g_output/pregnancy_test_counts/csv_files/"),
                                                   paste0(regions[reg], "/g_output/preliminary_counts/csv_files/")),
                                         replacement= paste0("ALL_regions/", regions[reg], "_"), vectorize=FALSE)
  # Copies files from regional folders to ALL_regions folder
  file.copy(as.vector(f1$files.old), as.vector(f1$files.new))
}

# 3. Moves files within ALL_regions folder to subfolders named after the specific counts that have been performed e.g. Valproate.csv files from every region will be moved into  Valproate folder 
## Gets a list of files in ALL_regions folder (for subfolder creation, the region prefix is removed)
all_regions_files                  <- list.files(paste0(projectFolder, "/ALL_regions/"), full.names = FALSE)
all_regions_files_no_prefix        <- gsub("^[A-Z]{2}_" ,"",all_regions_files)
all_regions_files_no_prefix_unique <- unique(unlist(all_regions_files_no_prefix))
# all_regions_files_no_prefix_unique <- all_regions_files_no_prefix_unique[!grepl("denominator", all_regions_files_no_prefix_unique)]

## Loops through all the unique counts files
for (i in 1:length(all_regions_files_no_prefix_unique)){
  ## Removes the .csv from the file name so that it is not created in the name of the newly created folder name
  file_name <- gsub(".csv|.rds", "", all_regions_files_no_prefix_unique[i])
  ## Creates folder + path to folder
  invisible(ifelse(!dir.exists(paste0(projectFolder, "/ALL_regions/", file_name)), dir.create(paste0(projectFolder, "/ALL_regions/", file_name)),FALSE))
  record_dir <-paste0(paste0(projectFolder, "/ALL_regions/", file_name, "/"))
  ## Gets list of files with name matching the newly created folder  
  files = list.files(paste0(projectFolder, "/ALL_regions/"), pattern = all_regions_files_no_prefix_unique[i], full.names = TRUE)
  # Moves these files to the newly created folder of the same name  
  f2 <- as.data.frame(files, header = FALSE)
  colnames(f2) <- 'files.old'
  f2$files.new <- gsub("ALL_regions/", paste0("ALL_regions/", file_name, "/"), f2$files.old) 
  file.move(as.vector(f2$files.old), as.vector(f2$files.new))
}

# 4. Pools denominator files per subpops
all_denominator_folders <- list.files(All_regions_dir, pattern = "denominator", full.names = FALSE)
## Loops through all the denominator folders 
for (i in 1:length(all_denominator_folders)){
  record_dir <- paste0(All_regions_dir, all_denominator_folders[i], "/") # Sets path to denominator folder
  tables_denom <- lapply(paste0(record_dir,list.files(record_dir)), read.csv, header = TRUE) # Loads all denominator files 
  comb_tables_denom <- as.data.table(do.call(rbind , tables_denom)) # Binds all denominator files 
  comb_tables_denom[, Sum:=sum(Freq), by = list(YM)] # Sums counts by year, month
  comb_tables_denom <- comb_tables_denom[,-c("Freq")] # Removes unnecessary columns 
  comb_tables_denom <- comb_tables_denom[!duplicated(comb_tables_denom),] # Removes any duplicates
  write.csv(comb_tables_denom, paste0(record_dir, all_denominator_folders[i], "_Pooled.csv")) # Saves pooled file 
}

print("Pooling counts files...")
# 5. Pools counts files 
all_counts_folders <- list.files(All_regions_dir, pattern = "count", full.names = FALSE)
all_counts_folders <- all_counts_folders[!grepl("age_group|indication|tx_dur|reason|contra_type", all_counts_folders)]

## Loops through all the denominator folders 
for (i in 1:length(all_counts_folders)){
  record_dir <- paste0(All_regions_dir, all_counts_folders[i], "/") # Sets path to numerator folder 
  tables_num <- lapply(paste0(record_dir,list.files(record_dir)), read.csv, header = TRUE) # Loads all numerator files 
  comb_tables_num <- as.data.table(do.call(rbind , tables_num)) # Binds all numerator files 
  comb_tables_num[, Sum:=sum(N), by = list(YM)] # Sums counts by year, month
  comb_tables_num <- comb_tables_num[,c("YM", "Sum")] # Removes unnecessary columns 
  comb_tables_num <- comb_tables_num[!duplicated(comb_tables_num),] # Removes any duplicates
  write.csv(comb_tables_num, paste0(record_dir, all_counts_folders[i],"_Pooled.csv")) # Saves pooled file
}

##########################################################################
##########################################################################
############## USES ORIGINAL DENOMINATOR AS DENOMINATOR ##################
##########################################################################
##########################################################################
# 1. All preliminary counts 
# 2. Prevalence counts
# 3. Incidence counts
# 4. med_use_during_pregnancy
### RATES - MULTIPLY BY 1000

# 6. Moves subpop denominator to each file with corresponding subpopulation
denom_files <- list.files(All_regions_dir, pattern = "denominator")
num_files   <- list.files(All_regions_dir, pattern = "EVENTS|MEDS|PROC|prevalence|incidence|med_use_during_pregnancy|alt_med")
num_files <- num_files[!grepl("age_group|indication|tx_dur|reason|contra_type|switched", num_files)]

if(length(num_files)>0){
  for (i in 1:length(num_files)){
    num_subpop <- strsplit(num_files[i], "_")[[1]][1]
    for (j in 1:length(denom_files)){
      denom_subpop <- strsplit(denom_files[j], "_denominator")[[1]][1]
      if (num_subpop == denom_subpop){
        from <- paste0(All_regions_dir, denom_subpop, "_denominator/", denom_subpop,"_denominator_Pooled.csv")
        to   <- paste0(All_regions_dir, num_files[i], "/", denom_subpop,"_denominator_Pooled.csv")
        file.copy(from, to)
      } 
    }
  }
  
  num_files   <- num_files[!grepl(c("all_pregnancies"), num_files)] # Excludes folders that do not use the overall denominator for calculating rates
  
  # 7. Merge denominator files with count files 
  for (i in 1:length(num_files)){
    # Set path
    record_dir <- paste0(All_regions_dir, num_files[i], "/")
    # Load denominator file
    denom_file <- list.files(record_dir, pattern = "denominator", full.names = T)
    denom_df   <- fread(denom_file)
    setnames(denom_df, "Sum", "Freq")
    
    num_file <- list.files(record_dir, pattern = "counts_Pooled", full.names = T)
    num_df   <- fread(num_file)
    setnames(num_df,"Sum", "N")
    # Masking values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    num_df$masked <- ifelse(num_df$N<5 & num_df$N>0, 1, 0)
    # Changes values less than 5 and more than 0 to 5
    if (mask == T){num_df[num_df$masked == 1,]$N <- 5} else {num_df[num_df$masked == 1,]$N <- num_df[num_df$masked == 1,]$N}
    # Calculates rates
    df <- merge(x = num_df, y = denom_df, by = c("YM"), all.x = TRUE)
    df <- df[,rates:=as.numeric(N)/as.numeric(Freq)]
    df <- df[,rates:=rates*1000]
    df <- df[,c("YM", "N", "Freq", "rates", "masked")]
    write.csv(df, paste0(record_dir, num_files[i],"_Pooled.csv"))
    # Remove denominator file from folder 
    unlink(denom_file)
  }
}
##########################################################################
##########################################################################
############## USES PREVALENCE AS DENOMINATOR ############################
##########################################################################
##########################################################################
# 1. Discontinued - PROPORTION
# 2. Preg starts --- rate * 1000 
# 3. switched - PROPORTION
# 
### Discontinued counts & preg_start_during_tx_episodes use prevalence counts as a denominator
# 8. Move subpop denominator to each file with corresponding subpopulation
denom_files <- list.files(All_regions_dir, pattern = "prevalence")
# print(denom_files)
num_files <- list.files(All_regions_dir, pattern = "discontinued|preg_starts|switched")
num_files <- num_files[!grepl("age_group|indication|tx_dur|contra_type|reason", num_files)]
# print(num_files)
if(length(num_files)){
  for (i in 1:length(num_files)){
    if(str_detect(num_files[i], pattern = "PC_HOSP_Retinoid")){num_subpop <- "PC_HOSP_Retinoid"}
    else if(str_detect(num_files[i], pattern = "PC_HOSP_Valproate")){num_subpop <- "PC_HOSP_Valproate"}
    else if(str_detect(num_files[i], pattern = "PC_Retinoid")){num_subpop <- "PC_Retinoid"}
    else if(str_detect(num_files[i], pattern = "PC_Valproate")){num_subpop <- "PC_Valproate"}
    else(print("NUMERATOR NAME NOT FOUND"))
    
    for (j in 1:length(denom_files)){
      if(str_detect(denom_files[j], pattern = "PC_HOSP_Retinoid")){denom_subpop <- "PC_HOSP_Retinoid"}
      else if(str_detect(denom_files[j], pattern = "PC_HOSP_Valproate")){denom_subpop <- "PC_HOSP_Valproate"}
      else if(str_detect(denom_files[j], pattern = "PC_Retinoid")){denom_subpop <- "PC_Retinoid"}
      else if(str_detect(denom_files[j], pattern = "PC_Valproate")){denom_subpop <- "PC_Valproate"}
      else(print("DENOMINATOR NAME NOT FOUND"))
      
      if (num_subpop == denom_subpop){
        from <- paste0(All_regions_dir, denom_files[j], "/", denom_subpop,"_prevalence_counts_Pooled.csv")
        to   <- paste0(All_regions_dir, num_files[i], "/", denom_subpop,"_prevalence_counts_Pooled.csv")
        file.copy(from, to)
      } 
    }
  }
  
  # 9. Merge denominator files with count files 
  for (i in 1:length(num_files)){
    # Set path
    record_dir <- paste0(All_regions_dir, num_files[i], "/")
    # print(record_dir)
    # Looks for denominator file in numerator folder 
    denom_file <- list.files(record_dir, pattern = "prevalence_counts_Pooled", full.names = T)
    # print(paste0("DENOM:", list.files(record_dir, pattern = "prevalence_counts_Pooled")))
    # Loads denominator file 
    denom_df <- fread(denom_file)
    # File clean up
    denom_df <- denom_df[,-c("V1", "Freq", "masked", "rates")]
    setnames(denom_df, "N", "Freq")
    # Looks for numerator file in numerator folder (depending on numerator type)
    if(str_detect(num_files[i], pattern = "discontinued_counts")){
      num_file <- list.files(record_dir, pattern = "discontinued_counts_Pooled", full.names = T, ignore.case = T)
      # print(paste0("NUM:", list.files(record_dir, pattern = "discontinued_counts_Pooled")))
    }
    if(str_detect(num_files[i], pattern = "all_preg_starts")){
      num_file <- list.files(record_dir, pattern = "during_tx_episodes_counts_Pooled", full.names = T, ignore.case = T)
      # print(paste0("NUM:", list.files(record_dir, pattern = "during_tx_episodes_counts_Pooled")))
    }
    if(str_detect(num_files[i], pattern = "switched")){
      num_file <- list.files(record_dir, pattern = "switched_to_alt_meds_counts_Pooled", full.names = T, ignore.case = T)
      # print(paste0("NUM:", list.files(record_dir, pattern = "switched_to_alt_meds_counts_Pooled")))
    }
    #Loads numerator file  
    num_df <-  fread(num_file)
    # File clean up
    num_df <- num_df[,-c("V1")]
    setnames(num_df,"Sum", "N")
    # Masking values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    num_df[,masked:=ifelse(N>0&N<5,1,0)]
    # Changes values less than 5 and more than 0 to 5
    if (mask == T){num_df[num_df$masked == 1,]$N <- 5} else {num_df[num_df$masked == 1,]$N <- num_df[num_df$masked == 1,]$N}
    # Calculates rates
    df <- merge(x = num_df, y = denom_df, by = c("YM"), all.x = TRUE)
    df <- df[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    if(str_detect(num_files[i], pattern = "all_preg_starts")){df[,rates:=rates*1000]}
    df <- df[,c("YM", "N", "Freq", "rates", "masked")]
    # Save file 
    write.csv(df, paste0(record_dir, num_files[i],"_final_Pooled.csv"))
    # Remove denominator file from folder 
    unlink(denom_file)
  }
}
##########################################################################
##########################################################################
############## USES RETINOID/VALPROATES AS DENOMINATOR ###################
##########################################################################
##########################################################################
# 1. pg test prior - done
# 2. pg test after - done
# 3. contraception prior - done
# 4. med_use_during_contraception - done
### PROPORTIONS

### Pregtest and Contraceptives use retinoid/valproate counts as denominator
# 10. Move subpop denominator to each file with corresponding subpopulation
denom_files <- list.files(All_regions_dir, pattern = paste0(c("Retinoid_MEDS", "Valproate_MEDS"), collapse = "|"))
num_files   <- list.files(All_regions_dir, pattern = "prior|after|med_use_during_contraception")
num_files   <- num_files[!grepl("age_group|indication|tx_dur|contra_type|reason", num_files)]

if(length(num_files)>0){
  for (i in 1:length(num_files)){
    
    if(str_detect(num_files[i], pattern = "pgtests_prior_counts")){num_subpop <- gsub("_pgtests_prior_counts", "", num_files[i])}
    if(str_detect(num_files[i], pattern = "pgtests_after_counts")){num_subpop <- gsub("_pgtests_after_counts", "", num_files[i])}
    if(str_detect(num_files[i], pattern = "contraception_prior_counts")){num_subpop <- gsub("_contraception_prior_counts", "", num_files[i])}
    if(str_detect(num_files[i], pattern = "med_use_during_contraception_episodes")){num_subpop <- gsub("_med_use_during_contraception_episodes_counts", "", num_files[i])}
    
    for (j in 1:length(denom_files)){
      denom_subpop <- gsub("_MEDS_counts", "", denom_files[j])
      if (num_subpop == denom_subpop){
        from <- paste0(All_regions_dir, denom_files[j], "/", denom_subpop,"_MEDS_counts_Pooled.csv")
        to   <- paste0(All_regions_dir, num_files[i], "/", denom_subpop,"_MEDS_counts_Pooled.csv")
        file.copy(from, to)
      }
    }
  }
  
  # 11. Merge denominator files with count files
  for (i in 1:length(num_files)){
    # Set path
    record_dir <- paste0(All_regions_dir, num_files[i], "/")
    # Load denominator file
    denom_file <- list.files(record_dir, pattern = "MEDS_counts_Pooled", full.names = T)
    denom_df <- fread(denom_file)
    denom_df <- denom_df[,-c("V1", "Freq", "masked", "rates")]
    setnames(denom_df, "N", "Freq")
    # Loads numerator file
    if (str_detect(num_files[i], pattern = "pgtests_prior")){num_file <- list.files(record_dir, pattern = "pgtests_prior_counts_Pooled", full.names = T)}
    if (str_detect(num_files[i], pattern = "pgtests_after")){num_file <- list.files(record_dir, pattern = "pgtests_after_counts_Pooled", full.names = T)}
    if (str_detect(num_files[i], pattern = "contraception_prior")){num_file <- list.files(record_dir, pattern = "contraception_prior_counts_Pooled", full.names = T)}
    if (str_detect(num_files[i], pattern = "med_use_during_contraception_episodes")){num_file <- list.files(record_dir, pattern = "med_use_during_contraception_episodes_counts_Pooled", full.names = T)}
    
    num_df <-  fread(num_file)
    num_df <- num_df[,-c("V1")]
    setnames(num_df, "Sum", "N")
    # Masking values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked
    num_df$masked <- ifelse(num_df$N<5 & num_df$N>0, 1, 0)
    # Changes values less than 5 and more than 0 to 5
    if (mask == T){num_df[num_df$masked == 1,]$N <- 5} else {num_df[num_df$masked == 1,]$N <- num_df[num_df$masked == 1,]$N}
    # Calculates rates
    df <- merge(x = num_df, y = denom_df, by = c("YM"), all.x = TRUE)
    df <- df[,rates:=as.numeric(N)/as.numeric(Freq)]
    df$rates[is.nan(df$rates)]<-0
    df <- df[,c("YM", "N", "Freq", "rates", "masked")]
    # Save file
    write.csv(df, paste0(record_dir, num_files[i],"_final_Pooled.csv"))
    # Remove denominator file from folder
    unlink(denom_file)
  }
}
#### STRATIFIED ANALYSIS ####
# Numerator -> Added numerators of all available regions
# Denominator -> Added denominators of all available regions 
all_counts_folders_strat <- list.files(All_regions_dir, pattern = "count", full.names = FALSE)
all_counts_folders_strat <- all_counts_folders_strat[grepl("age_group|indication|tx_dur|reason|contra_type", all_counts_folders_strat)]

## Loops through all the startifed counts folders 
for (i in 1:length(all_counts_folders_strat )){
  record_dir <- paste0(All_regions_dir, all_counts_folders_strat[i], "/") # Sets path to numerator folder 
  tables <- lapply(paste0(record_dir,list.files(record_dir)), read.csv, header = TRUE) # Loads all numerator files 
  comb_tables <- as.data.table(do.call(rbind , tables)) # Binds all numerator files 
  comb_tables <-comb_tables[,c("YM", "N", "Freq", "true_value")]
  comb_tables1 <- comb_tables[, N_all:=sum(N), by = list(YM)][, Freq_all:=sum(Freq), by = list(YM)] # Sums counts by year, month
  comb_tables1[,N:=NULL][,Freq:=NULL]
  setnames(comb_tables1, "N_all", "N")
  setnames(comb_tables1, "Freq_all", "Freq")
  comb_tables1 <- comb_tables1[,c("YM", "N", "Freq", "true_value")] # Removes unnecessary columns 
  comb_tables1 <- comb_tables1[!duplicated(comb_tables1),]
  # Masking 
  comb_tables1[,masked:=ifelse(N<5, 1, 0)]
  if(mask == T){comb_tables1[masked==1,N:=5]}else{comb_tables1[masked==1,N:=N]}
  # Calculate rates 
  comb_tables1[,rates:=as.numeric(N)/as.numeric(Freq)][is.na(rates)|is.nan(rates),rates:=0]
  comb_tables1 <- comb_tables1[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
  short_name<- gsub("_ind_", "_", all_counts_folders_strat[i])
  write.csv(comb_tables1, paste0(record_dir, short_name,"_Pooled.csv")) # Saves pooled file
}


###########################################################################################################################################################################
########### BASELINE TABLES  ##############################################################################################################################################
###########################################################################################################################################################################
print("Pooling baseline tables...")
# 1. Pools study population
study_pop_files  <- list.files(All_regions_dir, pattern = "PC_for_pooling_baseline_tables|PC_HOSP_for_pooling_baseline_tables")
print(paste0("Available for_pooling_baseline_tables: ", study_pop_files))
## Loops through all the baseline folders 
for (i in 1:length(study_pop_files)){
  record_dir <- paste0(All_regions_dir, study_pop_files[i], "/")
  tables_study_pop <- lapply(paste0(record_dir,list.files(record_dir)), readRDS)
  comb_tables_study_pop <- as.data.table(do.call(rbind , tables_study_pop))
  if(str_detect(study_pop_files[i], "PC_HOSP")){
    saveRDS(comb_tables_study_pop, paste0(record_dir, "PC_HOSP_Study_Population_Pooled.rds"))
  } else {
    saveRDS(comb_tables_study_pop, paste0(record_dir, "PC_Study_Population_Pooled.rds"))
  }
  for(file in list.files(path = record_dir, pattern ="for_pooling")){unlink(paste0(record_dir, file), recursive = FALSE)}
}

rm(tables_study_pop)

# 2. Pools retinoid/valproate populations
med_pop_files  <- list.files(All_regions_dir, pattern = "PC_med_use_for_pooling_baseline_tables|PC_HOSP_med_use_for_pooling_baseline_tables")
print(paste0("Meds available for_pooling_baseline_tables: ", med_pop_files))
## Loops through all the denominator folders 
for (i in 1:length(med_pop_files)){
  record_dir <- paste0(All_regions_dir, med_pop_files[i], "/")
  tables_med_pop <- lapply(paste0(record_dir,list.files(record_dir)), readRDS)
  comb_tables_med_pop <- as.data.table(do.call(rbind , tables_med_pop))
  if(str_detect(med_pop_files[i], "PC_HOSP")){
    saveRDS(comb_tables_med_pop, paste0(record_dir, "PC_HOSP_Meds_Population_Pooled.rds"))
  } else {
    saveRDS(comb_tables_med_pop, paste0(record_dir, "PC_Meds_Population_Pooled.rds"))
  }
  for(file in list.files(path = record_dir, pattern ="for_pooling")){unlink(paste0(record_dir, file), recursive = FALSE)}
}

rm(tables_med_pop)
# 3. Move study population to corresponding med_use_for_pooling_baseline_tables folder 
for (i in 1:length(med_pop_files)){
  # if(str_detect(med_pop_files[i], "PC_HOSP")){med_pop_subpop <- "PC_HOSP"} else {med_pop_subpop <- "PC"}
  if(str_detect(med_pop_files[i], "PC_HOSP")){med_pop_subpop <- "PC_HOSP"} else {med_pop_subpop <- "PC"}
  for (j in 1:length(study_pop_files)){
    # if(str_detect(study_pop_files[j], "PC_HOSP")){study_pop_subpop <- "PC_HOSP"} else {study_pop_subpop <- "PC"}
    if(str_detect(study_pop_files[j], "PC_HOSP")){study_pop_subpop <- "PC_HOSP"} else {study_pop_subpop <- "PC"}
    if (med_pop_subpop == study_pop_subpop){
      from <- paste0(All_regions_dir, study_pop_subpop, "_for_pooling_baseline_tables/", study_pop_subpop, "_Study_Population_Pooled.rds")
      to <- paste0(All_regions_dir,study_pop_subpop, "_med_use_for_pooling_baseline_tables/", study_pop_subpop, "_Study_Population_Pooled.rds")
      file.move(from, to)
      
    } 
  }
}

# Remove empty folders 
for(file in list.files(path = All_regions_dir, pattern ="PC_for_pooling_baseline_tables|PC_HOSP_for_pooling_baseline_tables")){unlink(paste0(All_regions_dir, file), recursive = TRUE)}

# 4. Create baseline tables for each subpop
baseline_subpop <- list.files(All_regions_dir, pattern = "med_use_for_pooling_baseline_tables")

for (i in 1:length(baseline_subpop)){
  record_dir <- paste0(All_regions_dir, baseline_subpop[i])
  study_pop_first_occurrence <- readRDS(paste0(record_dir, "/", list.files(record_dir,pattern = "Meds_Population_Pooled")))
  study_population <- readRDS(paste0(record_dir, "/", list.files(record_dir,pattern = "Study_Population_Pooled")))
  # Creates Subsets 
  if (study_type == "Retinoid"){
    study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
    study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
    # Retinoids - subgroups
    study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
    study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
    study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
    
    all_dfs_meds <- list(study_population, study_pop_ret_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
    names(all_dfs_meds) <- c("All_Users", "Retinoids_Only", "Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
    
  } else if (study_type == "Valproate"){
    study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
    study_pop_val_unique <- unique(study_pop_val, by = "person_id")
    
    all_dfs_meds <- list(study_population, study_pop_val_unique)
    names(all_dfs_meds) <- c("All_Users", "Valproates_Only")
    
  } else if (study_type == "Both"){
    study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
    study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
    study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
    study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
    study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
    
    study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
    study_pop_val_unique <- unique(study_pop_val, by = c("person_id"))
    
    all_dfs_meds <- list(study_population, study_pop_ret_unique, study_pop_val_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
    names(all_dfs_meds) <- c("All_Users", "Retinoids_Only", "Valproates_Only","Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
    
  }
  
  # Loops through all the subsets depending on the study_type and creates baseline tables 
  for (j in 1:length(all_dfs_meds)){
    df <- all_dfs_meds[[j]]
    if(nrow(df > 0)){
      ################## BASELINE ALL POPULATION ########################
      # Calculates median of followup in years 
      fu_median <-  median(df$fu_dur_days)/365.25
      fu_IQR <- IQR(df$fu_dur_days)/365.25
      fu_min        <- min(df$fu_dur_days)/365.25
      fu_max        <- max(df$fu_dur_days)/365.25
      max_exit_date <- max(df$exit_date)
      # fu_SD
      age_at_ID_mean <-mean(df$age_at_entry_date)
      age_at_ID_SD   <-sd(df$age_at_entry_date)
      # age_at_ID_SD <- do we calculate SD if we are calculating mean
      
      # If count < 5 and mask = T, then instead of actual count, write "count=<5
      if(mask == TRUE){
        
        if(sum(df$age_groups == "12-20.99")<=5) {age_at_ID_12_20.99_count<-"count=<5"} else {age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99")} 
        if(sum(df$age_groups == "21-30.99")<=5) {age_at_ID_21_30.99_count<-"count=<5"} else {age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99")} 
        if(sum(df$age_groups == "31-40.99")<=5) {age_at_ID_31_40.99_count<-"count=<5"} else {age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99")}  
        if(sum(df$age_groups == "41-55.99")<=5) {age_at_ID_41_55.99_count<-"count=<5"} else {age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99")} 
        # Masking
        if (age_at_ID_12_20.99_count == "count=<5" | age_at_ID_21_30.99_count== "count=<5" |  age_at_ID_31_40.99_count == "count=<5" | age_at_ID_41_55.99_count == "count=<5") {
          print("Masked values. Percentages cannot be calculated!")
          # Creates dataframe
          names <- c("Follow-up, years - median",
                     "Follow-up, years - IQR",
                     "Follow-up, years - min",
                     "Follow-up, years - max",
                     "Max exit date",
                     "Age at index date (study entry) - mean",
                     "Age at index date (study entry) - sd",
                     "12.0-20.99 years_count",
                     "21.0-30.99 years_count",
                     "31.0-40.99 years_count",
                     "41.0-55.99 years_count")
          
          values <- c(as.character(round(fu_median,1)),
                      as.character(round(fu_IQR,1)),
                      as.character(round(fu_min,1)),
                      as.character(round(fu_max,1)),
                      as.character(max_exit_date),
                      as.character(round(age_at_ID_mean,1)),
                      as.character(round(age_at_ID_SD,1)),
                      as.character(age_at_ID_12_20.99_count),
                      as.character(age_at_ID_21_30.99_count),
                      as.character(age_at_ID_31_40.99_count),
                      as.character(age_at_ID_41_55.99_count))
        } else { # Performs counts
          age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99")
          age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99")
          age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99")
          age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99")
          # Calculates percentages
          age_at_ID_12_20.99_perc  <- (age_at_ID_12_20.99_count/nrow(df)) * 100
          age_at_ID_21_30.99_perc  <- (age_at_ID_21_30.99_count/nrow(df)) * 100
          age_at_ID_31_40.99_perc  <- (age_at_ID_31_40.99_count/nrow(df)) * 100
          age_at_ID_41_55.99_perc  <- (age_at_ID_41_55.99_count/nrow(df)) * 100
          
          # Create dataframe
          names <- c("Follow-up, years - median",
                     "Follow-up, years - IQR",
                     "Follow-up, years - min",
                     "Follow-up, years - max",
                     "Max exit date",
                     "Age at index date (study entry) - mean",
                     "Age at index date (study entry) - sd",
                     "12.0-20.99 years_count",
                     "12.0-20.99 years_perc",
                     "21.0-30.99 years_count",
                     "21.0-30.99 years_perc",
                     "31.0-40.99 years_count",
                     "31.0-40.99 years_perc",
                     "41.0-55.99 years_count",
                     "41.0-55.99 years_perc")
          
          values <- c(as.character(round(fu_median,1)),
                      as.character(round(fu_IQR,1)),
                      as.character(round(fu_min,1)),
                      as.character(round(fu_max,1)),
                      as.character(max_exit_date),
                      as.character(round(age_at_ID_mean,1)),
                      as.character(round(age_at_ID_SD,1)),
                      as.character(age_at_ID_12_20.99_count),
                      as.character(round(age_at_ID_12_20.99_perc,1)),
                      as.character(age_at_ID_21_30.99_count),
                      as.character(round(age_at_ID_21_30.99_perc,1)),
                      as.character(age_at_ID_31_40.99_count),
                      as.character(round(age_at_ID_31_40.99_perc,1)),
                      as.character(age_at_ID_41_55.99_count),
                      as.character(round(age_at_ID_41_55.99_perc),1))}
        
      } else {
        age_at_ID_12_20.99_count <- sum(df$age_groups == "12-20.99") 
        age_at_ID_21_30.99_count <- sum(df$age_groups == "21-30.99")
        age_at_ID_31_40.99_count <- sum(df$age_groups == "31-40.99") 
        age_at_ID_41_55.99_count <- sum(df$age_groups == "41-55.99")
        # Calculates percentages
        age_at_ID_12_20.99_perc  <- (age_at_ID_12_20.99_count/nrow(df)) * 100
        age_at_ID_21_30.99_perc  <- (age_at_ID_21_30.99_count/nrow(df)) * 100
        age_at_ID_31_40.99_perc  <- (age_at_ID_31_40.99_count/nrow(df)) * 100
        age_at_ID_41_55.99_perc  <- (age_at_ID_41_55.99_count/nrow(df)) * 100
        
        # Create dataframe
        names <- c("Follow-up, years - median",
                   "Follow-up, years - IQR",
                   "Follow-up, years - min",
                   "Follow-up, years - max",
                   "Max exit date",
                   "Age at index date (study entry) - mean",
                   "Age at index date (study entry) - sd",
                   "12.0-20.99 years_count",
                   "12.0-20.99 years_perc",
                   "21.0-30.99 years_count",
                   "21.0-30.99 years_perc",
                   "31.0-40.99 years_count",
                   "31.0-40.99 years_perc",
                   "41.0-55.99 years_count",
                   "41.0-55.99 years_perc")
        
        values <- c(as.character(round(fu_median,1)),
                    as.character(round(fu_IQR,1)),
                    as.character(round(fu_min,1)),
                    as.character(round(fu_max,1)),
                    as.character(max_exit_date),
                    as.character(round(age_at_ID_mean,1)),
                    as.character(round(age_at_ID_SD,1)),
                    as.character(age_at_ID_12_20.99_count),
                    as.character(round(age_at_ID_12_20.99_perc,1)),
                    as.character(age_at_ID_21_30.99_count),
                    as.character(round(age_at_ID_21_30.99_perc,1)),
                    as.character(age_at_ID_31_40.99_count),
                    as.character(round(age_at_ID_31_40.99_perc,1)),
                    as.character(age_at_ID_41_55.99_count),
                    as.character(round(age_at_ID_41_55.99_perc),1))
      }
      # Creates baseline table
      baseline <- data.table(names, values)
      if(str_detect(baseline_subpop[i], "PC_HOSP")){subpop<-"PC_HOSP"}else{subpop<-"PC"}
      # Saves baseline table
      write.csv(baseline, paste0(record_dir, "/", subpop,"_", names(all_dfs_meds[j]), "_baseline.csv"))
    } else {
      print(paste("There are no records for: ", subpop, "_", names(all_dfs_meds[j])))
    }
  }
}
for(file in list.files(path = record_dir, pattern =".rds")){unlink(paste0(record_dir, "/", file), recursive = FALSE)}


# Sources script that plots aggregated plots 
print("Creating plots...")
# source(paste0(pre_dir,"bifap_aggr_plots.R"))









