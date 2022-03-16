#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/12/2021

# 1. Looks for dx codes from the dx codelist (concept set created via CreateConceptSets_DxCodes.R) in EVENTS TABLES 
# 2. Results in list of records with matching diagnosis codes
# 3. Counts records saved in the previous step by month/year for each code group
# Loads Create Concept Sets file
matches <- c()
source(paste0(pre_dir,"CreateConceptSets_DxCodes.R"))
source(paste0(pre_dir,"excluded_ICD.R"))
# Creates empty table for counts 
# Gets min and max year from denominator file
FUmonths_df <- as.data.table(FUmonths_df)
# Get minimum and maximum year values of denominator
min_data_available <- min(FUmonths_df$Y)
max_data_available <- max(FUmonths_df$Y)
FUmonths_df[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)]

empty_df<-expand.grid(seq(min(FUmonths_df$Y), max(FUmonths_df$Y)), seq(1, 12))
names(empty_df) <- c("year", "month")

# Loads events files
events_files <- list.files(path=path_dir, pattern = "EVENTS", ignore.case = TRUE)
# Checks for EVENTS Tables present
if(length(events_files)>0){
  # Creates a new folder for each code group type (to store records with matching codes) 
  for (z in 1:length(codelist_all)){ifelse(!dir.exists(file.path(events_tmp_DX, names(codelist_all[z]))), dir.create(paste(events_tmp_DX, names(codelist_all[z]), sep="")), FALSE)}
  # Processes each EVENTS table
  for (y in 1:length(events_files)){
    # Gets prefix for events tables 
    events_prefix <- gsub(".csv", "", events_files[y])
    # Loads table 
    df<-fread(paste(path_dir, events_files[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event", "event_free_text")] # Keeps necessary columns
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] # Makes sure missing data is read appropriately
    setnames(df,"meaning_of_event","Meaning") # Renames column names
    setnames(df,"start_date_record","Date") # Renames column names
    setnames(df,"event_record_vocabulary","Vocabulary") # Renames column names
    setnames(df,"event_code","Code") # Renames column names
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    # Merges Events table with study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)] # Left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)] # Transforms to numeric variables 
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    df[,Date:=as.IDate(Date,"%Y%m%d")] # Transforms to date variables
    df[,entry_date:=as.IDate(entry_date,"%Y%m%d")] # Transforms to date variables
    df[,exit_date:=as.IDate(exit_date,"%Y%m%d")] # Transforms to date variables
    # Creates year variable - year of the event
    df[,year:=year(Date)]
    df<-df[!is.na(year)] # Removes records with both dates missing
    # Years used in study
    if(is_PHARMO){df<-df[year>2008 & year<2020]} else {df<-df[year>2008 & year<2021]} # Years used in study
    df<-df[!(is.na(Code) | is.na(Vocabulary))]# Removes records with both event code and event record vocabulary missing
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Removes unspecified sex
    # Exclusion of meanings ### for BIFAP
    # PC: Meanings to be limited/restricted to: "primary_care_events_BIFAP" (or "procedure_primary_care", where applicable); 
    # excludes "primary_care_conditionants_BIFAP", "primary_care_antecedents_BIFAP", "hospitalisation_primary" and "hospitalisation_secundary"
    if(pop_prefix == "PC"){df<-df[Meaning=="primary_care_events_BIFAP",]}
    # PC_HOSP: Meanings to be limited/restricted to:  "primary_care_events_BIFAP" and "hospitalisation_primary" (or "procedure_primary_care" and  "procedure_during_hospitalisation" where applicable); 
    # excludes "primary_care_conditionants_BIFAP", "primary_care_antecedents_BIFAP" and "hospitalisation_secundary".
    if(pop_prefix == "PC_HOSP"){df<-df[Meaning=="primary_care_events_BIFAP" | Meaning=="hospitalisation_primary",]}
    
    # PHARMO free text
    if(is_PHARMO == T){
      df_free_text <- df[Vocabulary == "free_text_dutch"]
      df <- df[!Vocabulary == "free_text_dutch"]
      
      if(nrow(df_free_text)>0){source(paste0(pre_dir, "find_PHARMO_free_text.R"))}
    }
    df <- df[,-c("event_free_text")]
    
    # Adds column with Vocabulary main type i.e. start, READ, SNOMED
    if(nrow(df)>0){
      df[,vocab:= ifelse(df[,Vocabulary] %chin% c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES" , "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP", "ICD9_free_italian_text"), "start",
                         ifelse(df[,Vocabulary] %chin% c("RCD","RCD2", "READ", "CPRD_Read"), "READ", 
                                ifelse(df[,Vocabulary] %chin% c("SNOMEDCT_US", "SCTSPA", "SNOMED"), "SNOMED", "UNKNOWN")))]
    }
    # Check if records have dots or not 
    df[, flag:= ifelse(str_detect(Code, "\\."), 1, 0)]
    # if all flags are 0 then use undotted codes else use dotted codes 
    # If unique value of flag is both 0 and 1, then we assume that the DAP uses dots in their ICD codes
    # If there is only 1 unique value of flag: If equal to 1 -> then DAP uses dots in their ICD code. If flag == 0, then we assume that there are no dots used in the data 
    dotted <- ifelse(length(unique(df$flag)) == 2, "Yes", ifelse((length(unique(df$flag)) == 1 & unique(df$flag)== 0), "No", "Yes"))
    # Prints Message
    print(paste0("Finding matching records in ", events_files[y]))
    
    
    # If more than 1 unique value in vocab column 
    if (length(unique(df$vocab)) > 1){
      # creates a subset for each of the unique values and run the filter on each subset 
      for (voc in 1:length(unique(df$vocab))){
        # Creates subsets for each vocab type
        df_subset_vocab <- setDT(df)[vocab == unique(df$vocab)[voc]]
        # Checks if df is NOT empty
        if(nrow(df_subset_vocab)>0){
          # Looks for matches in df using event vocabulary type specific code list
          # Covers: ICD9, ICD9CM, ICD9PROC, MTHICD9, ICD10, ICD-10, ICD10CM, ICD10/CM, ICD10ES, ICPC, ICPC2, ICPC2P, ICPC-2, CIAP Codes 
          if(length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "start"){
            for (i in 1:length(codelist_start_all)){
              if (dotted == "Yes"){df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]}
              if (dotted == "No"){df_subset <- setDT(df_subset_vocab)[Code %chin% gsub("\\.", "", codelist_start_all[[i]][,Code])]}
              df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[!duplicated(df_subset),]
              
              if(nrow(df_subset)>0){
                saveRDS(data.table(df_subset), paste0(events_tmp_DX, pop_prefix, "_", names(codelist_start_all[i]), "_", events_prefix, "_start.rds"))
                new_file <-c(list.files(events_tmp_DX, "\\_start.rds$"))
                lapply(new_file, function(x){file.rename(from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_start_all[i])), x))})
              }
            }
            # Covers RCD, RCD2, READ, CPRD_Read Codes 
          } else if (length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "READ") {
            for (i in 1:length(codelist_read_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_read_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[!duplicated(df_subset),]
              if(nrow(df_subset)>0){
                saveRDS(data.table(df_subset), paste0(events_tmp_DX, pop_prefix,"_", names(codelist_read_all[i]), "_",events_prefix, "_READ.rds"))
                new_file <-c(list.files(events_tmp_DX, "\\_READ.rds$"))
                lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_read_all[i])), x))})
              }
            }
            # Covers SNOMEDCT_US, SCTSPA, SNOMED Codes 
          } else if (length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "SNOMED") {
            for (i in 1:length(codelist_snomed_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_snomed_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[!duplicated(df_subset),]
              if(nrow(df_subset)>0){
                saveRDS(data.table(df_subset), paste0(events_tmp_DX, pop_prefix, "_", names(codelist_snomed_all[i]), "_",events_prefix, "_SNOMED.rds"))
                new_file <-c(list.files(events_tmp_DX, "\\_SNOMED.rds$"))
                lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_snomed_all[i])), x))})
              }
            }
          } else { 
            print(paste0(unique(df_subset_vocab$Vocabulary), " is not part of code list vocabulary"))
          }
        } else {
          print(paste0("There are no matching records in ", events_files[y]))
        }
      }
    } else {
      # Checks if df is NOT empty
      if(nrow(df)>0){
        # Looks for matches in df using event vocabulary type specific code list
        # Covers: ICD9, ICD9CM, ICD9PROC, MTHICD9, ICD10, ICD-10, ICD10CM, ICD10/CM, ICD10ES, ICPC, ICPC2, ICPC2P, ICPC-2, CIAP Codes 
        if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "start"){
          for (i in 1:length(codelist_start_all)){
            df_subset_vocab <- df
            if (dotted == "Yes"){df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]}
            if (dotted == "No"){df_subset <- setDT(df_subset_vocab)[Code %chin% gsub("\\.", "", codelist_start_all[[i]][,Code])]}
            df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[!duplicated(df_subset),]
            if(nrow(df_subset)>0){
              saveRDS(data.table(df_subset), paste0(events_tmp_DX, pop_prefix, "_", names(codelist_start_all[i]), "_", events_prefix, "_start.rds"))
              new_file <-c(list.files(events_tmp_DX, "\\_start.rds$"))
              lapply(new_file, function(x){file.rename(from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_start_all[i])), x))})
            }
          }
          # Covers RCD, RCD2, READ, CPRD_Read Codes 
        } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "READ") {
          for (i in 1:length(codelist_read_all)){
            df_subset <- setDT(df)[Code %chin% codelist_read_all[[i]][,Code]]
            df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[!duplicated(df_subset),]
            if(nrow(df_subset)>0){
              saveRDS(data.table(df_subset), paste0(events_tmp_DX, pop_prefix,"_", names(codelist_read_all[i]), "_",events_prefix, "_READ.rds"))
              new_file <-c(list.files(events_tmp_DX, "\\_READ.rds$"))
              lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_read_all[i])), x))})
            }
          }
          # Covers SNOMEDCT_US, SCTSPA, SNOMED Codes 
        } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "SNOMED") {
          for (i in 1:length(codelist_snomed_all)){
            df_subset <- setDT(df)[Code %chin% codelist_snomed_all[[i]][,Code]]
            df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[!duplicated(df_subset),]
            if(nrow(df_subset)>0){
              saveRDS(data.table(df_subset), paste0(events_tmp_DX, pop_prefix, "_", names(codelist_snomed_all[i]), "_",events_prefix, "_SNOMED.rds"))
              new_file <-c(list.files(events_tmp_DX, "\\_SNOMED.rds$"))
              lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_snomed_all[i])), x))})
            }
          }
        } else { 
          print(paste0(unique(df$Vocabulary), " is not part of code list vocabulary"))
        }
      } else {
        print(paste0("There are no matching records in ", events_files[y]))
      }
    }
  }
  # Prints Message
  print("Counting records")
  # Monthly Counts
  for(i in 1:length(codelist_all)){
    # Gets list of files in each code group folder
    files <- list.files(path=paste0(events_tmp_DX, names(codelist_all[i])))
    # Performs counts per month/year
    if (length(files)>0){
      # Loads files
      files <- list.files(path=paste0(events_tmp_DX, names(codelist_all[i])), pattern = "\\.rds$", full.names = TRUE)
      comb_meds <- do.call("rbind", lapply(files, readRDS))
      comb_meds <- comb_meds[!duplicated(comb_meds),]
      # Delete all temporary events files - to avoid merging with 2nd subpop
      for(file in list.files(path=paste0(events_tmp_DX, names(codelist_all[i])), pattern = "\\.rds$", full.names = TRUE)){unlink(file)}
      # Only count records that fall between a patients entry and exit to study date 
      comb_meds1 <- comb_meds[Date>=entry_date & Date<=exit_date]
      # Counts by month-year
      counts <- comb_meds1[,.N, by = .(year,month(Date))]
      # Merges with empty_df
      counts <- as.data.table(merge(x = empty_df, y = counts, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      counts[is.na(counts[,N]), N:=0]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      counts[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Changes values less than 5 and more than 0 to 5
      if(mask==T){counts[masked==1,N:=5]} else {counts[masked==1,N:=N]}
      # Calculates rates
      counts <- within(counts, YM<- sprintf("%d-%02d", year, month))
      counts <- merge(x = counts, y = FUmonths_df, by = c("YM"), all.x = TRUE)
      counts <- counts[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
      counts <- counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in g_output monthly counts
      if(comb_meds[,.N]>0){
        saveRDS(comb_meds, paste0(diagnoses_pop, pop_prefix, "_", names(codelist_all[i]),".rds"))
        saveRDS(counts, paste0(monthly_counts_dx,"/",pop_prefix, "_", names(codelist_all[i]),"_EVENTS_counts.rds"))
      } else {
        print(paste("There are no matching records for", names(codelist_all[i])))
      }
    } else {
      print(paste("There are no matching records for", names(codelist_all[i])))
    }
  }
} else {
  print("There are no EVENTS tables to analyse!")
}

# Cleanup
rm(list = grep("^codelist", ls(), value = TRUE))
