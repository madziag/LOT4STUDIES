print("POOLING RESULTS...")
### BIFAP POOLING ###
###########################################################################################################################################################################
########### COUNTS ########################################################################################################################################################
###########################################################################################################################################################################
# 1. Creates ALL_regions folder 
invisible(ifelse(!dir.exists(paste0(projectFolder, "/ALL_regions")), dir.create(paste0(projectFolder, "/ALL_regions")), FALSE))
All_regions_dir<-paste0(projectFolder, "/ALL_regions/")
# 2. Copies all counts, baseline tables, denominator csv files from each region, prefixing each file with the name of the region
## Gets a list of region names from the CDMInstances folder 
regions <- list.dirs(path = multiple_regions_dir, full.names = FALSE, recursive = FALSE)
## Loops over each region
for(reg in 1:length(regions)){
  ## Find counts, baseline tables and denominator csv files in each regional folder 
  pattern1 = c("counts.csv", "denominator.csv", "pooling_baseline_tables.rds")
  files_to_move <- list.files(paste0(projectFolder, "/", regions[reg]), pattern = paste0(pattern1, collapse="|"), recursive = TRUE, full.names =  TRUE, ignore.case =  TRUE)
  ## Copy these files to ALL_regions folder 
  f1 <- as.data.frame(files_to_move, header = FALSE)
  colnames(f1)  <- 'files.old'
  f1$files.new <- gsub(paste0(regions[reg],  "/g_intermediate/recs_for_baseline_table_pooling/"), paste0("ALL_regions/", regions[reg], "_"), f1$files.old)
  f1$files.new1 <- gsub(paste0(regions[reg], "/g_output/pregnancy_counts/csv_files/")  ,          paste0("ALL_regions/", regions[reg], "_"), f1$files.new)
  f1$files.new2 <- gsub(paste0(regions[reg], "/g_output/preliminary_counts/csv_files/"),          paste0("ALL_regions/", regions[reg], "_"), f1$files.new1)
  f1$files.new3 <- gsub(paste0(regions[reg], "/g_output/medicines_counts/csv_files/"),            paste0("ALL_regions/", regions[reg], "_"), f1$files.new2)
  file.copy(as.vector(f1$files.old), as.vector(f1$files.new3))
}


# 3. Moves files within ALL_regions folder to subfolders named after the specific counts that have been performed e.g. Valproate.csv files from every region will be moved into  Valproate folder 
## Gets a list of files in ALL_regions folder (for subfolder creation, the region prefix is removed)
all_regions_files                  <- list.files(paste0(projectFolder, "/ALL_regions/"), full.names = FALSE)
all_regions_files_no_prefix        <- gsub("^[A-Z]{2}_" ,"",all_regions_files)
all_regions_files_no_prefix_unique <- unique(unlist(all_regions_files_no_prefix))

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
  record_dir <- paste0(All_regions_dir, all_denominator_folders[i], "/")
  tables_denom <- lapply(paste0(record_dir,list.files(record_dir)), read.csv, header = TRUE)
  comb_tables_denom <- as.data.table(do.call(rbind , tables_denom))
  comb_tables_denom[, Sum:=sum(Freq), by = list(YM)]
  comb_tables_denom[, Freq:=NULL]
  comb_tables_denom <- comb_tables_denom[!duplicated(comb_tables_denom),]
  write.csv(comb_tables_denom, paste0(record_dir, all_denominator_folders[i], "_Pooled.csv"))
}

# 5. Pools counts files 
all_counts_folders <- list.files(All_regions_dir, pattern = "count", full.names = FALSE)
## Loops through all the denominator folders 
for (i in 1:length(all_counts_folders)){
  record_dir <- paste0(All_regions_dir, all_counts_folders[i], "/")
  tables <- lapply(paste0(record_dir,list.files(record_dir)), read.csv, header = TRUE)
  comb_tables <- as.data.table(do.call(rbind , tables))
  comb_tables[, Sum:=sum(N), by = list(YM)]
  comb_tables1 <- select(comb_tables, YM, Sum)
  comb_tables1 <- comb_tables1[!duplicated(comb_tables1),]
  write.csv(comb_tables1, paste0(record_dir, all_counts_folders[i],"_Pooled.csv"))
}


# 6. Move subpop denominator to each file with corresponding subpopulation
denom_files <- list.files(All_regions_dir, pattern = "denominator")
count_files <- list.files(All_regions_dir, pattern = "counts")

for (i in 1:length(count_files)){
  count_subpop <- strsplit(count_files[i], "_study_population")[[1]][1]
  for (j in 1:length(denom_files)){
    denom_subpop <- strsplit(denom_files[j], "_denominator")[[1]][1]
    if (count_subpop == denom_subpop){
      from <- paste0(All_regions_dir, denom_subpop, "_denominator/", denom_subpop,"_denominator_Pooled.csv")
      to <- paste0(All_regions_dir, count_files[i], "/", denom_subpop,"_denominator_Pooled.csv")
      file.copy(from, to)
    } 
  }
}

# 7. Merge denominator files with count files 
for (i in 1:length(count_files)){
  # Set path
  record_dir <- paste0(All_regions_dir, count_files[i], "/")
  # Load denominator file
  denom_file <- list.files(paste0(All_regions_dir, count_files[i]), pattern = "denominator")
  denom_df <- fread(paste0(All_regions_dir, count_files[i], "/",denom_file))
  setnames(denom_df, "Sum", "Freq")
  
  pooled_file <- list.files(paste0(All_regions_dir, count_files[i]), pattern = "counts_Pooled")
  pooled_df <-  fread(paste0(All_regions_dir, count_files[i], "/",pooled_file))
  setnames(pooled_df,"Sum", "N")
  # Masking values less than 5
  # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
  pooled_df$masked <- ifelse(pooled_df$N<5 & pooled_df$N>0, 1, 0)
  # Changes values less than 5 and more than 0 to 5
  if (mask == T){pooled_df[pooled_df$masked == 1,]$N <- 5} else {pooled_df[pooled_df$masked == 1,]$N <- pooled_df[pooled_df$masked == 1,]$N}
  # Calculates rates
  pooled_df <- merge(x = pooled_df, y = denom_df, by = c("YM"), all.x = TRUE)
  pooled_df <- pooled_df[,rates:=as.numeric(N)/as.numeric(Freq)]
  pooled_df <- pooled_df[,c("YM", "N", "Freq", "rates", "masked")]
  write.csv(pooled_df, paste0(record_dir, count_files[i],"_Pooled.csv"))
}


###########################################################################################################################################################################
########### BASELINE TABLES  ##############################################################################################################################################
###########################################################################################################################################################################

# 1. Pools study population
study_pop_files  <- list.files(All_regions_dir, pattern = "study_population_for_pooling_baseline_tables")
## Loops through all the denominator folders 
for (i in 1:length(study_pop_files)){
  record_dir <- paste0(All_regions_dir, study_pop_files[i], "/")
  tables_study_pop <- lapply(paste0(record_dir,list.files(record_dir)), readRDS)
  comb_tables_study_pop <- as.data.table(do.call(rbind , tables_study_pop))
  saveRDS(comb_tables_study_pop, paste0(record_dir, strsplit(study_pop_files[i], "_study_population")[[1]][1], "_Study_Population_Pooled.rds"))
  for(file in list.files(path = record_dir, pattern ="for_pooling")){unlink(paste0(record_dir, file), recursive = FALSE)}
}

# 2. Pools retinoid/valproate populations
med_pop_files  <- list.files(All_regions_dir, pattern = "med_use_for_pooling_baseline_tables")
## Loops through all the denominator folders 
for (i in 1:length(med_pop_files)){
  record_dir <- paste0(All_regions_dir, med_pop_files[i], "/")
  tables_med_pop <- lapply(paste0(record_dir,list.files(record_dir)), readRDS)
  comb_tables_med_pop <- as.data.table(do.call(rbind , tables_med_pop))
  saveRDS(comb_tables_med_pop, paste0(record_dir, strsplit(med_pop_files[i], "_study_population")[[1]][1], "_Meds_Population_Pooled.rds"))
  for(file in list.files(path = record_dir, pattern ="for_pooling")){unlink(paste0(record_dir, file), recursive = FALSE)}
}

# 3. Move study population to corresponding med_use_for_pooling_baseline_tables folder 
for (i in 1:length(med_pop_files)){
  med_pop_subpop <- strsplit(med_pop_files[i], "_study_population")[[1]][1]
  for (j in 1:length(study_pop_files)){
    study_pop_subpop <- strsplit(study_pop_files[j], "_study_population")[[1]][1]
    if (med_pop_subpop == study_pop_subpop){
      from <- paste0(All_regions_dir, study_pop_subpop, "_study_population_for_pooling_baseline_tables/", study_pop_subpop, "_Study_Population_Pooled.rds")
      to <- paste0(All_regions_dir,study_pop_subpop, "_study_population_med_use_for_pooling_baseline_tables/", study_pop_subpop, "_Study_Population_Pooled.rds")
      file.move(from, to)
      
    } 
  }
}

# Remove empty folders 
for(file in list.files(path = All_regions_dir, pattern ="study_population_for_pooling_baseline_tables")){unlink(paste0(All_regions_dir, file), recursive = TRUE)}

# 4. Create baseline tables for each subpop
baseline_subpop <- list.files(All_regions_dir, pattern = "baseline_tables")

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
    names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
    
  } else if (study_type == "Valproate"){
    study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
    study_pop_val_unique <- unique(study_pop_val, by = "person_id")
    
    all_dfs_meds <- list(study_population, study_pop_val_unique)
    names(all_dfs_meds) <- c("All Users", "Valproates Only")
    
  } else if (study_type == "Both"){
    study_pop_ret <- setDT(study_pop_first_occurrence)[med_type == "Retinoid"]
    study_pop_ret_unique <- unique(study_pop_ret, by = "person_id")
    study_pop_ret_D05BB02 <- setDT(study_pop_ret)[Code == "D05BB02"]
    study_pop_ret_D11AH04 <- setDT(study_pop_ret)[Code == "D11AH04"]
    study_pop_ret_D10BA01 <- setDT(study_pop_ret)[Code == "D10BA01"]
    
    study_pop_val <- setDT(study_pop_first_occurrence)[med_type == "Valproate"]
    study_pop_val_unique <- unique(study_pop_val, by = c("person_id"))
    
    all_dfs_meds <- list(study_population, study_pop_ret_unique, study_pop_val_unique, study_pop_ret_D05BB02, study_pop_ret_D11AH04, study_pop_ret_D10BA01)
    names(all_dfs_meds) <- c("All Users", "Retinoids Only", "Valproates Only","Retinoids_D05BB02", "Retinoids_D11AH04", "Retinoids_D10BA01")
    
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
      # Saves baseline table
      saveRDS(baseline, paste0(record_dir,"/", baseline_subpop[i], "_", names(all_dfs_meds[j]),"_baseline.rds"))
    } else {
      print(paste("There are no records for: ", baseline_subpop[i], "_", names(all_dfs_meds[j])))
    }
  }
}


# Sources script that plots aggregated plots 
print("Creating plots...")
source(paste0(pre_dir,"bifap_aggr_plots.R"))























