#Load study population
study_population <- readRDS(paste0(populations_dir, "ALL_study_population.rds"))
# Load concept sets
matches <- c()
#############################################################################################################################################
#### EVENTS TABLES ##########################################################################################################################
#############################################################################################################################################
# Load Create Concept Sets file
source(paste0(pre_dir,"CreateConceptSets_DxCodes.R"))
#### EVENTS TABLES ####
if(length(actual_tables$EVENTS)>0){
  # Create a new folder for each code group type (to store records with matching codes) 
  for (z in 1:length(codelist_all)){ifelse(!dir.exists(file.path(events_tmp_DX, names(codelist_all[z]))), dir.create(paste(events_tmp_DX, names(codelist_all[z]), sep="")), FALSE)}
  # Process each EVENTS table
  for (y in 1:length(actual_tables$EVENTS)){
    # Load table 
    df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event")] # Keep necessary columns
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] # Make sure missing data is read appropriately
    setnames(df,"meaning_of_event","meaning") # Rename column names
    setnames(df,"start_date_record","event_date") # Rename column names
    setnames(df,"event_record_vocabulary","event_vocabulary") # Rename column names
    setnames(df,"event_code","Code") # Rename column names
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    # Merge Events table with study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)] # Left join, keep all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)] # Transform to numeric variables 
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    df[,event_date:=as.IDate(event_date,"%Y%m%d")] # Transform to date variables
    df[,entry_date:=as.IDate(entry_date,"%Y%m%d")] # Transform to date variables
    # Creates year variable
    df[,year:=year(event_date)]
    df<-df[!is.na(year)] # Remove records with both dates missing
    # df<-df[year>2008 & year<2021] # Years used in study
    df[,date_dif:=entry_date-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identify persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(event_date<entry_date | event_date>exit_date), obs_out:=1] # Remove records that are outside the obs_period for all subjects
    df<-df[is.na(obs_out)] # Remove records outside study period
    df[,obs_out:=NULL]
    df<-df[!is.na(Code) | !is.na(event_vocabulary)]# Remove records with both event code and event record vocabulary missing
    df<-df[!is.na(event_vocabulary)] # Remove empty vocabularies
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Remove unspecified sex
    # Add column with event_vocabulary main type i.e. start, READ, SNOMED
    df[,vocab:= ifelse(df[,event_vocabulary] %chin% c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES" , "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP"), "start",
                       ifelse(df[,event_vocabulary] %chin% c("RCD","RCD2", "READ", "CPRD_Read"), "READ", 
                              ifelse(df[,event_vocabulary] %chin% c("SNOMEDCT_US", "SCTSPA", "SNOMED"), "SNOMED", "UNKNOWN")))]
    # Print Message
    print(paste0("Finding matching records in ", actual_tables$EVENTS[y]))
    # Check if df is NOT empty
    if(nrow(df)>0){
      # Look for matches in df using event vocabulary type specific code list
      # Covers: ICD9, ICD9CM, ICD9PROC, MTHICD9, ICD10, ICD-10, ICD10CM, ICD10/CM, ICD10ES, ICPC, ICPC2, ICPC2P, ICPC-2, CIAP Codes 
      if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "start"){
        for (i in 1:length(codelist_start_all)){
          df_subset <- setDT(df)[Code %chin% codelist_start_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          
          if (names(codelist_start_all[i]) == "ind_bipolar" | names(codelist_start_all[i]) == "ind_epilepsy" |names(codelist_start_all[i]) == "ind_migraine"){
            df_subset <- df_subset
          } else {
            df_subset <-df_subset[year>2008 & year<2021] # Years used in study
          }
          if(nrow(df_subset)>0){
            saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_start_all[i]), "_",actual_tables$EVENTS[y], "_start.rds"))
            new_file <-c(list.files(events_tmp_DX, "\\_start.rds$"))
            lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_start_all[i])), x))})
          }
        }
        # Cover RCD, RCD2, READ, CPRD_Read Codes 
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "READ") {
        for (i in 1:length(codelist_read_all)){
          df_subset <- setDT(df)[Code %chin% codelist_read_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          
          if (names(codelist_read_all[i]) == "ind_bipolar" | names(codelist_read_all[i]) == "ind_epilepsy" |names(codelist_read_all[i]) == "ind_migraine"){
            df_subset <- df_subset
          } else {
            df_subset <-df_subset[year>2008 & year<2021] # Years used in study
          }
          if(nrow(df_subset)>0){
            saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_read_all[i]), "_",actual_tables$EVENTS[y], "_READ.rds"))
            new_file <-c(list.files(events_tmp_DX, "\\_READ.rds$"))
            lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_read_all[i])), x))})
          }
        }
        # Covers SNOMEDCT_US, SCTSPA, SNOMED Codes 
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "SNOMED") {
        for (i in 1:length(codelist_snomed_all)){
          df_subset <- setDT(df)[Code %chin% codelist_snomed_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          if (names(codelist_snomed_all[i]) == "ind_bipolar" | names(codelist_snomed_all[i]) == "ind_epilepsy" |names(codelist_snomed_all[i]) == "ind_migraine"){
            df_subset <- df_subset
          } else {
            df_subset <-df_subset[year>2008 & year<2021] # Years used in study
          }
          if(nrow(df_subset)>0){
            saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_snomed_all[i]), "_",actual_tables$EVENTS[y], "_SNOMED.rds"))
            new_file <-c(list.files(events_tmp_DX, "\\_SNOMED.rds$"))
            lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_snomed_all[i])), x))})
          }
        }
      } else { 
        print(paste0(unique(df$event_vocabulary), " is not part of code list vocabulary"))
      }
      
    } else {
      print(paste0("There are no matching records in ", actual_tables$EVENTS[y]))
    }
  }
  # Concatenate records from different tables 
  for(i in 1:length(codelist_all)){
    # Get list of files in each code group folder 
    files <- list.files(path=paste0(events_tmp_DX, names(codelist_all[i])), pattern = "\\.rds$", full.names = TRUE)
    # Perform counts per month/year
    if (length(files)>0){
      # Loads files 
      
      comb_meds <- do.call("rbind", lapply(files, readRDS))
      saveRDS(comb_meds, paste0(diagnoses_pop,names(codelist_all[i]),".rds"))
    } else {
      print(paste("There are no matching records for", names(codelist_all[i])))
    }
  }
  
   # Remove folder with table level records - these have been concatenated and stored in diagnosis folder 
  # unlink(paste0(tmp, "/events_dx"), recursive = TRUE)
  
}


#############################################################################################################################################
#### MEDICINES TABLES #######################################################################################################################
#############################################################################################################################################
# Load Create Concept Sets file
matches <- c()
source(paste0(pre_dir,"CreateConceptSets_ATC.R"))
# Create other lists
comb_meds <- list()
# Check for MEDICINES Tables present
if(length(actual_tables$MEDICINES)>0){
  # Process each EVENTS table
  for (y in 1:length(actual_tables$MEDICINES)){
    # Load table
    df<-fread(paste(path_dir, actual_tables$MEDICINES[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-df[,c("person_id", "medicinal_product_atc_code", "date_dispensing", "date_prescription", "meaning_of_drug_record")] # Keep necessary columns
    df<-df[,lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] # Make sure missing data is read appropriately
    setnames(df,"medicinal_product_atc_code", "Code") # Rename column names
    # Create new column event_date. It takes the date from date_dispensing. If value from date_dispensing is missing, it takes the date value from date_prescription
    df<-df[,event_date:= ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)]
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    # Merge medicine table with study population table (there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)] # Left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)] # Transform to numeric variables  
    # Remove records with missing values in the medicine table 
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    df[,event_date:=as.IDate(event_date,"%Y%m%d")] # Transform to date variables
    df[,entry_date:=as.IDate(entry_date,"%Y%m%d")] # Transform to date variables
    # Create year variable 
    df[,year:=year(event_date)] 
    df<-df[!is.na(year)] # Remove records with both dates missing
    df<-df[year>2008 & year<2021] # Years used in study
    df[,date_dif:=entry_date-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identify persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(event_date<entry_date | event_date>exit_date), obs_out:=1] # Remove records that are outside the obs_period for all subjects
    df<-df[is.na(obs_out)] # Remove records outside study period
    df[,obs_out:=NULL]
    # remove records with ATC code missing 
    df<-df[!is.na(Code)]
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Remove unspecified sex
    # Print Message
    print(paste0("Finding matching records in ", actual_tables$MEDICINES[y]))
    # Check if df is NOT empty
    if(nrow(df)>0){
      # Look for matches in df using event vocabulary type specific code list
      for (i in 1:length(codelist_all)){
        df_subset <- setDT(df)[Code %chin% codelist_all[[i]][,Code]]
        saveRDS(data.table(df_subset), paste0(events_tmp_ATC, names(codelist_all[i]), "_",actual_tables$MEDICINES[y], ".rds"))
      }
    }
  }
  # Concatenate records from different tables 
  for(i in 1:length(codelist_all)){
    # Get list of files in each code group folder
    files <- list.files(path=events_tmp_ATC, pattern=names(codelist_all[i]))
    # Perform counts per month/year
    if (length(files)>0){
      # Loads files 
      comb_meds[[i]]<-do.call(rbind,lapply(paste0(events_tmp_ATC, files), readRDS))
      saveRDS(comb_meds[[i]], paste0(medications_pop,names(codelist_all[i]),".rds"))
    } else {
      print(paste("There are no matching records for", names(codelist_all[i])))
    }
    
  }

  # Remove folder with table level records - these have been concatenated and stored in medications folder 
  unlink(paste0(tmp, "/events_atc"), recursive = TRUE)
}

#############################################################################################################################################
#### PROCEDURES TABLES #######################################################################################################################
#############################################################################################################################################
matches <- c()
source(paste0(pre_dir,"CreateConceptSets_ProcedureCodes.R"))
# Load Procedure files 
proc_files <- list.files(path=path_dir, pattern = "PROCEDURES", ignore.case = TRUE)
# Check for PROCEDURE Tables present
if(length(proc_files)>0){
  # Process each PROCEDURES table
  for (y in 1:length(proc_files)){
    # Create a new folder for each code group type (to store records with matching codes)
    for (z in 1:length(codelist_all)){ifelse(!dir.exists(file.path(events_tmp_PROC, names(codelist_all[z]))), dir.create(paste(events_tmp_PROC, names(codelist_all[z]), sep="")), FALSE)}
    # Load table 
    df<-fread(paste(path_dir, proc_files[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-df[,c("person_id", "procedure_date", "procedure_code", "procedure_code_vocabulary", "meaning_of_procedure")] # Keep necessary columns
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] # Make sure missing data is read appropriately
    setnames(df,"meaning_of_procedure","meaning") # Rename column names
    setnames(df,"procedure_code_vocabulary","vocabulary") # Rename column names
    setnames(df,"procedure_code","Code") # Rename column names
    colnames_procedures<-names(df)
    colnames_procedures<-colnames_procedures[!colnames_procedures %in% "person_id"]
    # Merge Procedures table with study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)] # Left join, keep all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)] # Transform to numeric variables 
    df<-df[!rowSums(is.na(df[,..colnames_procedures]))==length(colnames_procedures)]
    df[,procedure_date :=as.IDate(procedure_date ,"%Y%m%d")] # Transform to date variables
    df[,entry_date:=as.IDate(entry_date,"%Y%m%d")] # Transform to date variables
    # Creates year variable
    df[,year:=year(procedure_date )]
    df<-df[!is.na(year)] # Remove records with both dates missing
    df<-df[year>2008 & year<2021] # Years used in study
    df[,date_dif:=entry_date-procedure_date ][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identify persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(procedure_date <entry_date | procedure_date >exit_date), obs_out:=1] # Remove records that are outside the obs_period for all subjects
    df<-df[is.na(obs_out)] # Remove records outside study period
    df[,obs_out:=NULL]
    df<-df[!is.na(Code) | !is.na(vocabulary)]# Remove records with both event code and event record vocabulary missing
    df<-df[!is.na(vocabulary)] # Remove empty vocabularies
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Remove unspecified sex
    # Print Message
    print(paste0("Finding matching records in ", proc_files[y]))
    # Add column for origin of code i.e. CPRD, PHARMO
    df[,vocab:= ifelse(df[,vocabulary] %chin% c("OPCS4"), "CPRD",
                       ifelse(df[,vocabulary] %chin% c("CVV", "CBV", "ZA"), "PHARMO", "UNKNOWN"))]
    
    # Check if df is NOT empty
    if(nrow(df)>0){
      # Look for matches in df using event vocabulary type specific code list
      # Covers: CPRD codes
      if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "CPRD"){
        for (i in 1:length(codelist_CPRD_all)){
          df_subset <- setDT(df)[Code %chin% codelist_CPRD_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          if(nrow(df_subset)>0){
            saveRDS(data.table(df_subset), paste0(events_tmp_PROC, names(codelist_CPRD_all[i]), "_",proc_files[y], "_.rds"))
            new_file <-c(list.files(events_tmp_PROC, "\\_.rds$"))
            lapply(new_file, function(x){file.rename( from = file.path(events_tmp_PROC, x), to = file.path(paste0(events_tmp_PROC, names(codelist_CPRD_all[i])), x))})
          }
        }
        # Covers PHARMO Codes
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "PHARMO") {
        for (i in 1:length(codelist_PHARM0_all)){
          df_subset <- setDT(df)[Code %chin% codelist_PHARM0_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          if(nrow(df_subset)>0){
            saveRDS(data.table(df_subset), paste0(events_tmp_PROC, names(codelist_PHARM0_all[i]), "_",proc_files[y], "_.rds"))
            new_file <-c(list.files(events_tmp_PROC, "\\_.rds$"))
            lapply(new_file, function(x){file.rename( from = file.path(events_tmp_PROC, x), to = file.path(paste0(events_tmp_PROC, names(codelist_PHARM0_all[i])), x))})
          }
        }
      } else {print(paste0(unique(df$vocabulary), " is not part of code list vocabulary"))}
      
    } else {
      print(paste0("There are no matching records in ", proc_files[y]))
    }
  }
  
  # Concatenate records from different tables 
  for(i in 1:length(codelist_all)){
    # Get list of files in each code group folder 
    files <- list.files(path=paste0(events_tmp_PROC, names(codelist_all[i])), pattern = "\\.rds$", full.names = TRUE)
    # Perform counts per month/year
    if (length(files)>0){
      # Loads files 
      
      comb_meds <- do.call("rbind", lapply(files, readRDS))
      saveRDS(comb_meds, paste0(procedures_pop,names(codelist_all[i]),".rds"))
    } else {
      print(paste("There are no matching records for", names(codelist_all[i])))
    }
  }
  
  # Remove folder with table level records - these have been concatenated and stored in diagnosis folder 
  unlink(paste0(tmp, "/events_proc"), recursive = TRUE)
  
}


