#Load study population
# study_population <- readRDS(paste0(populations_dir, "ALL_study_population.rds"))
# Load concept sets
matches <- c("sterility")
source(paste0(pre_dir,"CreateConceptSets_DxCodes.R"))

events_files <- list.files(path=path_dir, pattern = "EVENTS", ignore.case = TRUE)
# EVENTS TABLES
if(length(events_files)>0){
  # Process each EVENTS table
  for (y in 1:length(events_files)){
    # Load table
    df<-fread(paste(path_dir, events_files[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-as.data.table(df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event")]) # Keep necessary columns
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
    df[,start_follow_up:=as.IDate(start_follow_up,"%Y%m%d")] # Transform to date variables
    # Creates year variable
    df[,year:=year(event_date)]
    df<-df[!is.na(year)] # Remove records with both dates missing
    df<-df[year>2008 & year<2021] # Years used in study
    df[,date_dif:=start_follow_up-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identify persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1] # Remove records that are outside the obs_period for all subjects
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
    print(paste0("Finding matching records in ", events_files[y]))
    if (length(unique(df$vocab)) > 1){
      # create a subset for each of the unique values and run the filter on each subset 
      for (voc in 1:length(unique(df$vocab))){
        # Create subsets for each vocab type
        df_subset_vocab <- setDT(df)[vocab == unique(df$vocab)[voc]]
        
        # Check if df is NOT empty
        if(nrow(df_subset_vocab)>0){
          # Look for matches in df using event vocabulary type specific code list
          # Covers: ICD9, ICD9CM, ICD9PROC, MTHICD9, ICD10, ICD-10, ICD10CM, ICD10/CM, ICD10ES, ICPC, ICPC2, ICPC2P, ICPC-2, CIAP Codes 
          if(length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "start"){
            
            for (i in 1:length(codelist_start_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              
              if(nrow(df_subset)>0){
                
                if(SUBP == TRUE){
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_",populations[pop], "_",events_files[y], "_start.rds"))
                  new_file <-c(list.files(events_tmp_sterility, "\\_start.rds$"))
                  
                } else { 
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_", events_files[y], "_start.rds"))
                  new_file <-c(list.files(events_tmp_sterility, "\\_start.rds$"))
                  
                }
              }
            }
            # Cover RCD, RCD2, READ, CPRD_Read Codes 
          } else if (length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "READ") {
            
            for (i in 1:length(codelist_read_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_read_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              
              if(nrow(df_subset)>0){
                if(SUBP == TRUE){
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_", populations[pop], "_",events_files[y], "_READ.rds"))
                  new_file <-c(list.files(events_tmp_sterility, "\\_READ.rds$"))
                } else {                  
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_",events_files[y], "_READ.rds"))
                  new_file <-c(list.files(events_tmp_sterility, "\\_READ.rds$"))
       
                }
                
              }
            }
            # Covers SNOMEDCT_US, SCTSPA, SNOMED Codes 
          } else if (length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "SNOMED") {
            
            for (i in 1:length(codelist_snomed_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_snomed_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              
              if(nrow(df_subset)>0){
                if(SUBP == TRUE){
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_", populations[pop],"_",events_files[y], "_SNOMED.rds"))
                  new_file <-c(list.files(events_tmp_sterility, "\\_SNOMED.rds$"))
                  
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_",events_files[y], "_SNOMED.rds"))
                  new_file <-c(list.files(events_tmp_sterility, "\\_SNOMED.rds$"))
                  
                }
                
              }
            }
          } else { 
            print(paste0(unique(df_subset_vocab$event_vocabulary), " is not part of code list vocabulary"))
          }
          
        } else {
          print(paste0("There are no matching records in ", events_files[y]))
        }
        
        
      }
    
      } else {
    
    # Check if df is NOT empty
    if(nrow(df)>0){
      # Look for matches in df using event vocabulary type specific code list
      # Covers: ICD9, ICD9CM, ICD9PROC, MTHICD9, ICD10, ICD-10, ICD10CM, ICD10/CM, ICD10ES, ICPC, ICPC2, ICPC2P, ICPC-2, CIAP Codes
      if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "start"){
        for (i in 1:length(codelist_start_all)){
          df_subset <- setDT(df)[Code %chin% codelist_start_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          if(nrow(df_subset)>0){
            if(SUBP == TRUE){            
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_", populations[pop], "_",events_files[y], "_start.rds"))
              new_file <-c(list.files(events_tmp_sterility, "\\_start.rds$"))
              }else{            
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_",events_files[y], "_start.rds"))
                new_file <-c(list.files(events_tmp_sterility, "\\_start.rds$"))}
          }
        }
        # Cover RCD, RCD2, READ, CPRD_Read Codes
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "READ") {
        for (i in 1:length(codelist_read_all)){
          df_subset <- setDT(df)[Code %chin% codelist_read_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          if(nrow(df_subset)>0){
            if(SUBP == TRUE){        
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_", populations[pop], "_",events_files[y], "_READ.rds"))
              new_file <-c(list.files(events_tmp_sterility, "\\_READ.rds$"))
              }else{        
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_",events_files[y], "_READ.rds"))
                new_file <-c(list.files(events_tmp_sterility, "\\_READ.rds$"))}
    
           }
        }
        # Covers SNOMEDCT_US, SCTSPA, SNOMED Codes
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "SNOMED") {
        for (i in 1:length(codelist_snomed_all)){
          df_subset <- setDT(df)[Code %chin% codelist_snomed_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          if(nrow(df_subset)>0){
            if(SUBP == TRUE){            
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_", populations[pop], "_",events_files[y], "_SNOMED.rds"))
              new_file <-c(list.files(events_tmp_sterility, "\\_SNOMED.rds$"))
              }else{            
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_",events_files[y], "_SNOMED.rds"))
                new_file <-c(list.files(events_tmp_sterility, "\\_SNOMED.rds$"))
                }
          }
        }
      } else {
        print(paste0(unique(df$event_vocabulary), " is not part of code list vocabulary"))
      }

    } else {
      print(paste0("There are no matching records in ", events_files[y]))
    }
      }
  }

  # Get a list of files in sterility folder
    files <- list.files(path= events_tmp_sterility, pattern = "sterility")
    if (length(files)>0){
      sterility_events <-do.call(rbind,lapply(paste0(events_tmp_sterility, files), readRDS))
    } else {
      print(paste("There are no matching records for", names(codelist_all[i])))
    }
} else {
  print("There are no EVENTS tables available")
}


#### PROCEDURES TABLES
# Load Create Concept Sets file
filename <- "CodeLists/Procedure_codes.xlsx"
matches <- c("sterilisation")
source(paste0(pre_dir,"CreateConceptSets_ProcedureCodes.R"))
# Load Procedure files
proc_files <- list.files(path=path_dir, pattern = "PROCEDURES", ignore.case = TRUE)
# Check for PROCEDURE Tables present
if(length(proc_files)>0){
  # Process each PROCEDURES table
  for (y in 1:length(proc_files)){
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
    df[,start_follow_up:=as.IDate(start_follow_up,"%Y%m%d")] # Transform to date variables
    # Creates year variable
    df[,year:=year(procedure_date )]
    df<-df[!is.na(year)] # Remove records with both dates missing
    df<-df[year>2008 & year<2021] # Years used in study
    df[,date_dif:=start_follow_up-procedure_date ][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identify persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(procedure_date <start_follow_up | procedure_date >end_follow_up), obs_out:=1] # Remove records that are outside the obs_period for all subjects
    df<-df[is.na(obs_out)] # Remove records outside study period
    df[,obs_out:=NULL]
    df<-df[!is.na(Code) | !is.na(vocabulary)]# Remove records with both event code and event record vocabulary missing
    df<-df[!is.na(vocabulary)] # Remove empty vocabularies
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Remove unspecified sex
    # Print Message
    print(paste0("Finding matching records in ", proc_files[y]))
    # Add column for origin of code i.e. CPRD, PHARMO
    df[,vocab:= ifelse(df[,vocabulary] %chin% c("OPCS"), "CPRD",
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
            if(SUBP == TRUE){            
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_CPRD_all[i]), "_", populations[pop], "_",proc_files[y], "_.rds"))
              new_file <-c(list.files(events_tmp_sterility, "\\_.rds$"))
              }else{            
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_CPRD_all[i]), "_",proc_files[y], "_.rds"))
                new_file <-c(list.files(events_tmp_sterility, "\\_.rds$"))
                }

            }
        }
        # Covers PHARMO Codes
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "PHARMO") {
        for (i in 1:length(codelist_PHARM0_all)){
          df_subset <- setDT(df)[Code %chin% codelist_PHARM0_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          if(nrow(df_subset)>0){
            if(SUBP == TRUE){            
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_PHARM0_all[i]), "_", populations[pop], "_",proc_files[y], "_.rds"))
              new_file <-c(list.files(events_tmp_sterility, "\\_.rds$"))
              }else{            
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_PHARM0_all[i]), "_",proc_files[y], "_.rds"))
                new_file <-c(list.files(events_tmp_sterility, "\\_.rds$"))
                }

           }
        }
      } else {print(paste0(unique(df$vocabulary), " is not part of code list vocabulary"))}

    } else {
      print(paste0("There are no matching records in ", proc_files[y]))
    }
  }

  # Get a list of files in sterility folder
  files <- list.files(path= events_tmp_sterility, pattern = "sterilisation")
  if (length(files)>0){
    sterility_procedures <-do.call(rbind,lapply(paste0(events_tmp_sterility, files), readRDS))
  } else {
    print(paste("There are no matching records for", names(codelist_all[i])))
  }
  # unlink(paste0(tmp, "/events_sterility"), recursive = TRUE)

} else {
  print("There are no PROCEDURES  table available")
}



# Create Sterility list 
if ((length(list.files(path = events_tmp_sterility, pattern = "sterility")) > 0 )& (length(list.files(path = events_tmp_sterility, pattern = "sterilization" )) > 0 )) 
    {
  # Create column that indicates if record comes from events or procedures
  sterility_events[,table_origin:='EVENTS']
  sterility_procedures[,table_origin:='PROCEDURES']
  # Rename columns
  setnames(sterility_events,"event_date","sterility_date")
  setnames(sterility_procedures,"procedure_date","sterility_date")
  setnames(sterility_events,"event_vocabulary","vocabulary")
  # Join sterility records from events and procedures
  sterility_all <- dplyr::bind_rows(sterility_events, sterility_procedures)
  # Choose record with earliest date of sterility
  sterility_all_first_occurrence  <- setDT(sterility_all)[order(sterility_date), head(.SD, 1L), by = person_id]
  # Save records 
  if (SUBP == TRUE){
    saveRDS(sterility_all, paste0(sterility_pop, populations[pop], "_sterility_all.rds"))
    saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, populations[pop], "_sterility_all_first_occurrence.rds"))
  }else {
    saveRDS(sterility_all, paste0(sterility_pop, "sterility_all.rds"))
    saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, "sterility_all_first_occurrence.rds"))
  }
} else if (length(list.files(path = events_tmp_sterility, pattern = "sterility")) == 0 & length(list.files(path = events_tmp_sterility, pattern = "sterilization" )) > 0 ) {
  # Create column that indicates if record comes from events or procedures
  sterility_events[,table_origin:='EVENTS']
  # Rename columns
  setnames(sterility_events,"event_date","sterility_date")
  setnames(sterility_events,"event_vocabulary","vocabulary")
  # Choose record with earliest date of sterility
  sterility_all_first_occurrence  <- setDT(sterility_events)[order(sterility_date), head(.SD, 1L), by = person_id]
  # Save records 
  if (SUBP == TRUE){
    saveRDS(sterility_events, paste0(sterility_pop, populations[pop], "_sterility_all.rds"))
    saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, populations[pop], "_sterility_all_first_occurrence.rds"))
  }else {
    saveRDS(sterility_events, paste0(sterility_pop, "sterility_all.rds"))
    saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, "sterility_all_first_occurrence.rds"))
  }
} else if ((length(list.files(path = events_tmp_sterility, pattern = "sterility")) > 0) & (length(list.files(path = events_tmp_sterility, pattern = "sterilization")) == 0 )) {
  # Create column that indicates if record comes from events or procedures
  sterility_procedures[,table_origin:='PROCEDURES']
  # Rename columns
  setnames(sterility_procedures,"procedure_date","sterility_date")
  # Choose record with earliest date of sterility
  sterility_all_first_occurrence  <- setDT(sterility_all)[order(sterility_date), head(.SD, 1L), by = person_id]
  # Save records 
  if (SUBP == TRUE){
    saveRDS(sterility_procedures, paste0(sterility_pop, populations[pop], "_sterility_all.rds"))
    saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, populations[pop], "_sterility_all_first_occurrence.rds"))
  }else {
    saveRDS(sterility_procedures, paste0(sterility_pop, "sterility_all.rds"))
    saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, "sterility_all_first_occurrence.rds"))
  }
  
} else {
  print("There are no EVENTS or PROCEDURES tables")
}




# Clean Up 
# rm(list=ls(pattern="codelist"))
# rm(df, df_subset)
