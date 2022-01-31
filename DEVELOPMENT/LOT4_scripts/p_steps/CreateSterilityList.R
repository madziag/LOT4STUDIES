# Finds sterility records in Events and Procedures tables using provides diagnosis and procedure codes. 
## Looks for sterility diagnosis codes in Events and Procedures tables
## Looks for sterility procedure codes in Procedures tables (ONLY for CPRD and PHARMO)
# Sterility records to be used in CreateEntryExit.R

##################################################################################################################
################################# 1. DIAGNOSIS CODES IN EVENTS TABLES ############################################
##################################################################################################################
# Loads sterility codes from Lot4_completediagnosis_codelist_20211110
matches <- c("sterility")
source(paste0(pre_dir,"CreateConceptSets_DxCodes.R"))
# Gets list of events tables from CDM/LOT4 folders
events_files <- list.files(path=path_dir, pattern = "EVENTS", ignore.case = TRUE)
# Finds sterility codes in events tables 
if(length(events_files)>0){
  # Processes each EVENTS table
  for (y in 1:length(events_files)){
    # Loads events table
    df<-fread(paste(path_dir, events_files[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-as.data.table(df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event")]) # Keeps necessary columns
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
    df[,start_follow_up:=as.IDate(start_follow_up,"%Y%m%d")] # Transforms to date variables
    # Creates year variable
    df[,year:=year(Date)]
    df<-df[!is.na(year)] # Removes records with both dates missing
    if(is_PHARMO){df<-df[year>2008 & year<2020]} else {df<-df[year>2008 & year<2021]} # Years used in study
    df[,date_dif:=start_follow_up-Date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identifies persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(Date<start_follow_up | Date>end_follow_up), obs_out:=1] # Removes records that are outside the obs_period for all subjects
    df<-df[is.na(obs_out)] # Removes records outside study period
    df[,obs_out:=NULL]
    df<-df[!is.na(Code) | !is.na(Vocabulary)]# Removes records with both event code and event record vocabulary missing
    df<-df[!is.na(Vocabulary)] # Removes empty vocabularies
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Removes unspecified sex
    # Adds column with vocabulary main type i.e. start, READ, SNOMED
    df[,vocab:= ifelse(df[,Vocabulary] %chin% c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES" , "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP"), "start",
                       ifelse(df[,Vocabulary] %chin% c("RCD","RCD2", "READ", "CPRD_Read"), "READ",
                              ifelse(df[,Vocabulary] %chin% c("SNOMEDCT_US", "SCTSPA", "SNOMED"), "SNOMED", "UNKNOWN")))]
    # Prints Message
    print(paste0("Finding matching records in ", events_files[y]))
    
    if (length(unique(df$vocab)) > 1){
      
      # Creates a subset for each of the unique values and run the filter on each subset 
      for (voc in 1:length(unique(df$vocab))){
        # Creates subsets for each vocab type
        df_subset_vocab <- setDT(df)[vocab == unique(df$vocab)[voc]]
        
        if(nrow(df_subset_vocab)>0){ 
          
          if(length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "start"){
            
            for (i in 1:length(codelist_start_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              df_subset[,table_origin:='EVENTS']
              
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                if(SUBP == TRUE){ 
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_",populations[pop], "_",events_files[y], "_EVENTS_start.rds"))
                } else {
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_", events_files[y], "_EVENTS_start.rds"))
                  
                } 
              } 
            } 
            
          } else if(length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "READ"){ 
            
            for (i in 1:length(codelist_read_all)){ 
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_read_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              df_subset[,table_origin:='EVENTS']
              
              # Saves table only if it is not empty
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                if(SUBP == TRUE){
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_", populations[pop], "_",events_files[y], "_EVENTS_READ.rds"))
                } else {                  
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_",events_files[y], "_EVENTS_READ.rds"))
                  
                }
              } 
            }
            
          } else if (length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "SNOMED") {
            
            for (i in 1:length(codelist_snomed_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_snomed_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              df_subset[,table_origin:='EVENTS']
              
              # Saves table only if it is not empty
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                if(SUBP == TRUE){
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_", populations[pop],"_",events_files[y], "_EVENTS_SNOMED.rds"))
                } else {   
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_",events_files[y], "_EVENTS_SNOMED.rds"))
                } 
              } 
            }
            
          } else { 
            print(paste0(unique(df_subset_vocab$Vocabulary), " is not part of code list Vocabulary"))
          }
        } else {print(paste0("There are no matching records in ", events_files[y]))
        }
      }
      
    } else {
      
      # Checks if df is NOT empty
      if(nrow(df)>0){
        
        if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "start"){
          
          for (i in 1:length(codelist_start_all)){
            df_subset <- setDT(df)[Code %chin% codelist_start_all[[i]][,Code]]
            df_subset <- df_subset[,-c("vocab")]
            df_subset[,table_origin:='EVENTS']
            
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              if(SUBP == TRUE){ 
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_",populations[pop], "_",events_files[y], "_EVENTS_start.rds"))
              } else {
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_", events_files[y], "_EVENTS_start.rds"))
                
              } 
            } 
          } 
          
        } else if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "READ"){ 
          
          for (i in 1:length(codelist_read_all)){ 
            df_subset <- setDT(df)[Code %chin% codelist_read_all[[i]][,Code]]
            df_subset <- df_subset[,-c("vocab")]
            df_subset[,table_origin:='EVENTS']
            
            # Saves table only if it is not empty
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              if(SUBP == TRUE){
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_", populations[pop], "_",events_files[y], "_EVENTS_READ.rds"))
              } else {                  
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_",events_files[y], "_EVENTS_READ.rds"))
                
                
              }
            } 
          } 
          
        } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "SNOMED") {
          
          for (i in 1:length(codelist_snomed_all)){
            df_subset <- setDT(df)[Code %chin% codelist_snomed_all[[i]][,Code]]
            df_subset <- df_subset[,-c("vocab")]
            df_subset[,table_origin:='EVENTS']
            
            # Saves table only if it is not empty
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              if(SUBP == TRUE){
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_", populations[pop],"_",events_files[y], "_EVENTS_SNOMED.rds"))
              } else {   
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_",events_files[y], "_EVENTS_SNOMED.rds"))
              } 
            } 
          } 
          
        } else { 
          print(paste0(unique(df$Vocabulary), " is not part of code list vocabulary"))
        }
      } else {print(paste0("There are no matching records in ", events_files[y]))}
    }
  }
} else {
  print("There are no EVENTS tables available")
}

#################################################################################################################
################################# 2. DIAGNOSIS CODES IN PROCEDURES TABLES ########################################
##################################################################################################################
# Loads sterility codes from Lot4_completediagnosis_codelist_20211110
matches <- c("sterility")
source(paste0(pre_dir,"CreateConceptSets_DxCodes.R"))
# Gets list of procedure tables from CDM/LOT4 folders
proc_files <- list.files(path=path_dir, pattern = "PROCEDURES", ignore.case = TRUE)
# Finds sterility codes in procedures tables 
if(length(proc_files)>0){
  # Processes each EVENTS table
  for (y in 1:length(proc_files)){
    # Loads events table
    df<-fread(paste(path_dir, proc_files[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-as.data.table(df[,c("person_id", "procedure_date", "procedure_code", "procedure_code_vocabulary", "meaning_of_procedure")]) # Keeps necessary columns
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] # Makes sure missing data is read appropriately
    setnames(df,"meaning_of_procedure","Meaning") # Renames column names
    setnames(df,"procedure_date","Date") # Renames column names
    setnames(df,"procedure_code_vocabulary","Vocabulary") # Renames column names
    setnames(df,"procedure_code","Code") # Renames column names
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
    df[,start_follow_up:=as.IDate(start_follow_up,"%Y%m%d")] # Transforms to date variables
    # Creates year variable
    df[,year:=year(Date)]
    df<-df[!is.na(year)] # Removes records with both dates missing
    if(is_PHARMO){df<-df[year>2008 & year<2020]} else {df<-df[year>2008 & year<2021]} # Years used in study
    df[,date_dif:=start_follow_up-Date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identifies persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(Date<start_follow_up | Date>end_follow_up), obs_out:=1] # Removes records that are outside the obs_period for all subjects
    df<-df[is.na(obs_out)] # Removes records outside study period
    df[,obs_out:=NULL]
    df<-df[!is.na(Code) | !is.na(Vocabulary)]# Removes records with both event code and event record vocabulary missing
    df<-df[!is.na(Vocabulary)] # Removes empty vocabularies
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Removes unspecified sex
    # Adds column with vocabulary main type i.e. start, READ, SNOMED
    df[,vocab:= ifelse(df[,Vocabulary] %chin% c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES" , "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP"), "start",
                       ifelse(df[,Vocabulary] %chin% c("RCD","RCD2", "READ", "CPRD_Read"), "READ",
                              ifelse(df[,Vocabulary] %chin% c("SNOMEDCT_US", "SCTSPA", "SNOMED"), "SNOMED", "UNKNOWN")))]
    # Prints Message
    print(paste0("Finding matching records in ", proc_files[y]))
    
    if (length(unique(df$vocab)) > 1){
      
      # creates a subset for each of the unique values and run the filter on each subset 
      for (voc in 1:length(unique(df$vocab))){
        # Creates subsets for each vocab type
        df_subset_vocab <- setDT(df)[vocab == unique(df$vocab)[voc]]
        
        if(nrow(df_subset_vocab)>0){ 
          
          if(length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "start"){
            
            for (i in 1:length(codelist_start_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              df_subset[,table_origin:='PROCEDURES']
              
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                if(SUBP == TRUE){ 
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_",populations[pop], "_",proc_files[y], "_PROCEDURES_start.rds"))
                } else {
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_", proc_files[y], "_PROCEDURES_start.rds"))
                  
                } 
              } 
            } 
            
          } else if(length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "READ"){ 
            
            for (i in 1:length(codelist_read_all)){ 
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_read_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              df_subset[,table_origin:='PROCEDURES']
              
              # Saves table only if it is not empty
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                if(SUBP == TRUE){
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_", populations[pop], "_",proc_files[y], "_PROCEDURES_READ.rds"))
                } else {                  
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_",proc_files[y], "_PROCEDURES_READ.rds"))
                  
                }
              } 
            }
            
          } else if (length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "SNOMED") {
            
            for (i in 1:length(codelist_snomed_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_snomed_all[[i]][,Code]]
              df_subset <- df_subset[,-c("vocab")]
              df_subset[,table_origin:='PROCEDURES']
              
              # Saves table only if it is not empty
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                if(SUBP == TRUE){
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_", populations[pop],"_",proc_files[y], "_PROCEDURES_SNOMED.rds"))
                } else {   
                  saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_",proc_files[y], "_PROCEDURES_SNOMED.rds"))
                  
                  
                } 
              } 
            }
            
          } else { 
            print(paste0(unique(df_subset_vocab$Vocabulary), " is not part of code list vocabulary"))
          }
        } else {print(paste0("There are no matching records in ", proc_files[y]))
        }
      }
      
    } else {
      
      # Checks if df is NOT empty
      if(nrow(df)>0){
        
        if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "start"){
          
          for (i in 1:length(codelist_start_all)){
            df_subset <- setDT(df)[Code %chin% codelist_start_all[[i]][,Code]]
            df_subset <- df_subset[,-c("vocab")]
            df_subset[,table_origin:='PROCEDURES']
            
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              if(SUBP == TRUE){ 
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_",populations[pop], "_",proc_files[y], "_PROCEDURES_start.rds"))
              } else {
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_start_all[i]), "_", proc_files[y], "_PROCEDURES_start.rds"))
                
              } 
            } 
          } 
          
        } else if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "READ"){ 
          
          for (i in 1:length(codelist_read_all)){ 
            df_subset <- setDT(df)[Code %chin% codelist_read_all[[i]][,Code]]
            df_subset <- df_subset[,-c("vocab")]
            df_subset[,table_origin:='PROCEDURES']
            
            # Saves table only if it is not empty
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              if(SUBP == TRUE){
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_", populations[pop], "_",proc_files[y], "_PROCEDURES_READ.rds"))
              } else {                  
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_read_all[i]), "_",proc_files[y], "_PROCEDURES_READ.rds"))
                
                
              }
            } 
          } 
          
        } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "SNOMED") {
          
          for (i in 1:length(codelist_snomed_all)){
            df_subset <- setDT(df)[Code %chin% codelist_snomed_all[[i]][,Code]]
            df_subset <- df_subset[,-c("vocab")]
            df_subset[,table_origin:='PROCEDURES']
            
            # Saves table only if it is not empty
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              if(SUBP == TRUE){
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_", populations[pop],"_",proc_files[y], "_PROCEDURES_SNOMED.rds"))
              } else {   
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_snomed_all[i]), "_",proc_files[y], "_PROCEDURES_SNOMED.rds"))
              } 
            } 
          } 
          
        } else { 
          print(paste0(unique(df$Vocabulary), " is not part of code list vocabulary"))
        }
      } else {print(paste0("There are no matching records in ", proc_files[y]))}
    }
  }
} else {
  print("There are no PROCEDURES tables available")
}

##################################################################################################################
################################# 3. PROCEDURE CODES IN PROCEDURES TABLES ########################################
##################################################################################################################
# Loads sterilisation codes from Procedure_codes.xlsx
matches <- c("sterilisation")
source(paste0(pre_dir,"CreateConceptSets_ProcedureCodes.R"))
# Gets list of procedure tables from CDM/LOT4 folders
proc_files <- list.files(path=path_dir, pattern = "PROCEDURES", ignore.case = TRUE)
# Finds sterility codes in procedures tables 
if(length(proc_files)>0){
  # Processes each PROCEDURES table
  for (y in 1:length(proc_files)){
    # Loads events table
    df<-fread(paste(path_dir, proc_files[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-as.data.table(df[,c("person_id", "procedure_date", "procedure_code", "procedure_code_vocabulary", "meaning_of_procedure")]) # Keeps necessary columns
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] # Makes sure missing data is read appropriately
    setnames(df,"meaning_of_procedure","Meaning") # Renames column names
    setnames(df,"procedure_date","Date") # Renames column names
    setnames(df,"procedure_code_vocabulary","Vocabulary") # Renames column names
    setnames(df,"procedure_code","Code") # Renames column names
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
    df[,start_follow_up:=as.IDate(start_follow_up,"%Y%m%d")] # Transforms to date variables
    # Creates year variable
    df[,year:=year(Date)]
    df<-df[!is.na(year)] # Removes records with both dates missing
    if(is_PHARMO){df<-df[year>2008 & year<2020]} else {df<-df[year>2008 & year<2021]} # Years used in study
    df[,date_dif:=start_follow_up-Date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identifies persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(Date<start_follow_up | Date>end_follow_up), obs_out:=1] # Removes records that are outside the obs_period for all subjects
    df<-df[is.na(obs_out)] # Removes records outside study period
    df[,obs_out:=NULL]
    df<-df[!is.na(Code) | !is.na(Vocabulary)]# Removes records with both event code and event record vocabulary missing
    df<-df[!is.na(Vocabulary)] # Removes empty vocabularies
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Removes unspecified sex
    
    # Adds column for origin of code i.e. CPRD, PHARMO
    df[,vocab:= ifelse(df[,Vocabulary] %chin% c("OPCS4"), "CPRD",
                       ifelse(df[,Vocabulary] %chin% c("cvv_procedure", "cbv_procedure", "za_procedure"), "PHARMO", "UNKNOWN"))]
    
    # Checks if df is NOT empty
    if(nrow(df)>0){
      # Looks for matches in df using event vocabulary type specific code list
      # Covers: CPRD codes
      if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "CPRD"){
        for (i in 1:length(codelist_CPRD_all)){
          df_subset <- setDT(df)[Code %chin% codelist_CPRD_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          df_subset[,table_origin:='PROCEDURES']
          
          if(nrow(df_subset)>0){
            if(SUBP == TRUE){
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_CPRD_all[i]), "_", populations[pop], "_",proc_files[y], "_PROCEDURES.rds"))
            }else{
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_CPRD_all[i]), "_",proc_files[y], "_PROCEDURES.rds"))
            }
            
          }
        }
        # Covers PHARMO Codes
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "PHARMO") {
        for (i in 1:length(codelist_PHARM0_all)){
          df_subset <- setDT(df)[Code %chin% codelist_PHARM0_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          df_subset[,table_origin:='PROCEDURES']
          
          if(nrow(df_subset)>0){
            if(SUBP == TRUE){
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_PHARM0_all[i]), "_", populations[pop], "_",proc_files[y], "_PROCEDURES.rds"))
            }else{
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, names(codelist_PHARM0_all[i]), "_",proc_files[y], "_PROCEDURES.rds"))
            }
            
          }
        }
      } else {print(paste0(unique(df$Vocabulary), " is not part of code list vocabulary"))}
      
    } else {
      print(paste0("There are no matching records in ", proc_files[y]))
    }
  }
} else {
  print("There are no PROCEDUREStable available")
}

#################################################################################################################
################################# 4. BINDS ALL FOUND STERILITY RECORDS###########################################
#################################################################################################################
# Checks if there are sterility records in the file 
if (length(list.files(events_tmp_sterility))> 0){
  # Reads in all files in events_tmp_sterility and bind
  sterility_list <- list.files(events_tmp_sterility)
  sterility_all <-do.call(rbind,lapply(paste0(events_tmp_sterility, sterility_list), readRDS))
  sterility_all_first_occurrence  <- setDT(sterility_all)[order(Date), head(.SD, 1L), by = person_id]
  # Saves records
  if (SUBP == TRUE){
    saveRDS(sterility_all, paste0(sterility_pop, populations[pop], "_sterility_all.rds"))
    saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, populations[pop], "_sterility_all_first_occurrence.rds"))
  }else {
    saveRDS(sterility_all, paste0(sterility_pop, "sterility_all.rds"))
    saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, "sterility_all_first_occurrence.rds"))
  }  
} else {
  print("There are no Sterility records")
  }
