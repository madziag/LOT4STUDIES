#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/01/2022

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
source(paste0(pre_dir,"excluded_ICD.R"))
# Gets list of events tables from CDM/LOT4 folders
events_files <- list.files(path=path_dir, pattern = "EVENTS", ignore.case = TRUE)
# Finds sterility codes in events tables 
if(length(events_files)>0){
  # Processes each EVENTS table
  for (y in 1:length(events_files)){
    # Gets prefix for events tables 
    events_prefix <- gsub(".csv", "", events_files[y])
    # Loads events table
    df<-fread(paste(path_dir, events_files[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-as.data.table(df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event", "event_free_text")]) # Keeps necessary columns
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
    df[,year:=year(Date)] # from the events table (year of event)
    df<-df[!is.na(year)] # Removes records with dates missing
    if(is_PHARMO){df<-df[year>2008 & year<2020]} else {df<-df[year>2008 & year<2021]} # Years used in study
    df<-df[!(is.na(Code) | is.na(Vocabulary))]# Removes records with both event code and event record vocabulary missing
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Removes unspecified sex
    # PHARMO free text
    if(is_PHARMO == T){
      df_free_text <- df[Vocabulary == "free_text_dutch"]
      df <- df[!Vocabulary == "free_text_dutch"]
      
      concept_set_terms <- vector(mode="list")
      concept_set_terms[["sterility"]]=c("menopause", "overgang", "climac.")
      
      for(i in 1:length(concept_set_terms)){
        for(j in 1:length(concept_set_terms[[i]])){
          df_free_text_subset <- df_free_text[grepl(event_free_text, pattern = concept_set_terms[[i]][j], ignore.case = T),]
          df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
          df_free_text_subset <- df_free_text_subset[,c("person_id", "Vocabulary", "Code", "Date")]
          df_free_text_subset[,table_origin:='EVENTS']
          if(nrow(df_free_text_subset)>0){
            saveRDS(df_free_text_subset, paste0(events_tmp_sterility, pop_prefix, "_", names(concept_set_terms[i]), "-", concept_set_terms[[i]][j], "_",events_prefix, "_free_text_dutch.rds"))
          }
        }
      }
    }
    # Drop events free text column
    df <- df[,-c("event_free_text")]
    # Adds column with vocabulary main type i.e. start, READ, SNOMED
    df[,vocab:= ifelse(df[,Vocabulary] %chin% c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES" , "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP", "ICD9_free_italian_text"), "start",
                       ifelse(df[,Vocabulary] %chin% c("RCD","RCD2", "READ", "CPRD_Read"), "READ",
                              ifelse(df[,Vocabulary] %chin% c("SNOMEDCT_US", "SCTSPA", "SNOMED"), "SNOMED", "UNKNOWN")))]
    # Check if records have dots or not Flag = 1 when dotted and 0 when not dotted 
    df[, flag:= ifelse(str_detect(Code, "\\."), 1, 0)]
    # if all flags are 0 then use undotted codes else use dotted codes 
    # If unique value of flag is both 0 and 1, then we assume that the DAP uses dots in their ICD codes
    # If there is only 1 unique value of flag: If equal to 1 -> then DAP uses dots in their ICD code. If flag == 0, then we assume that there are no dots used in the data 
    dotted <- ifelse(length(unique(df$flag)) == 2, "Yes", ifelse((length(unique(df$flag)) == 1 & unique(df$flag)== 0), "No", "Yes"))
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
              if (dotted == "Yes"){df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]}
              if (dotted == "No"){df_subset <- setDT(df_subset_vocab)[Code %chin% gsub("\\.", "", codelist_start_all[[i]][,Code])]}
              # df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
              df_subset[,table_origin:='EVENTS']
              # Remove excluded codes (found as a result of code with no dots)
              df_subset <- setDT(df_subset)[Code %!in% excluded_codes]
              if(nrow(df_subset)>0){
                # Saves with the prefix of subpop name 
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_start_all[i]), "_",events_prefix, "_start.rds"))
              } 
            } 
          } else if(length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "READ"){ 
            for (i in 1:length(codelist_read_all)){ 
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_read_all[[i]][,Code]]
              # df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
              df_subset[,table_origin:='EVENTS']
              # Saves table only if it is not empty
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_read_all[i]), "_",events_prefix, "_READ.rds"))
              } 
            }
          } else if (length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "SNOMED") {
            for (i in 1:length(codelist_snomed_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_snomed_all[[i]][,Code]]
              # df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
              df_subset[,table_origin:='EVENTS']
              # Saves table only if it is not empty
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_snomed_all[i]), "_",events_prefix, "_SNOMED.rds"))
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
            df_subset_vocab <- df
            if (dotted == "Yes"){df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]}
            if (dotted == "No"){df_subset <- setDT(df_subset_vocab)[Code %chin% gsub("\\.", "", codelist_start_all[[i]][,Code])]}
            # df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
            df_subset[,table_origin:='EVENTS']
            # Remove excluded codes (found as a result of code with no dots)
            df_subset <- setDT(df_subset)[Code %!in% excluded_codes]
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_start_all[i]), "_",events_prefix, "_start.rds"))
            } 
          } 
        } else if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "READ"){ 
          for (i in 1:length(codelist_read_all)){ 
            df_subset <- setDT(df)[Code %chin% codelist_read_all[[i]][,Code]]
            # df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
            df_subset[,table_origin:='EVENTS']
            # Saves table only if it is not empty
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_read_all[i]), "_",events_prefix, "_READ.rds"))
            } 
          } 
        } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "SNOMED") {
          for (i in 1:length(codelist_snomed_all)){
            df_subset <- setDT(df)[Code %chin% codelist_snomed_all[[i]][,Code]]
            # df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
            df_subset[,table_origin:='EVENTS']
            # Saves table only if it is not empty
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_snomed_all[i]), "_",events_prefix, "_SNOMED.rds"))
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
    # Gets prefix for procedures tables 
    procedures_prefix <- gsub(".csv", "", proc_files[y])
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
    df<-df[!(is.na(Code) | is.na(Vocabulary))]# Removes records with both event code and event record vocabulary missing
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Removes unspecified sex

    # Adds column with vocabulary main type i.e. start, READ, SNOMED
    df[,vocab:= ifelse(df[,Vocabulary] %chin% c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES" , "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP"), "start",
                       ifelse(df[,Vocabulary] %chin% c("RCD","RCD2", "READ", "CPRD_Read"), "READ",
                              ifelse(df[,Vocabulary] %chin% c("SNOMEDCT_US", "SCTSPA", "SNOMED"), "SNOMED", "UNKNOWN")))]
    
    # Check if records have dots or not 
    df[, flag:= ifelse(str_detect(Code, "\\."), 1, 0)]
    # if all flags are 0 then use undotted codes else use dotted codes 
    # If unique value of flag is both 0 and 1, then we assume that the DAP uses dots in their ICD codes
    # If there is only 1 unique value of flag: If equal to 1 -> then DAP uses dots in their ICD code. If flag == 0, then we assume that there are no dots used in the data 
    dotted <- ifelse(length(unique(df$flag)) == 2, "Yes", ifelse((length(unique(df$flag)) == 1 & unique(df$flag)== 0), "No", "Yes"))
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
              if (dotted == "Yes"){df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]}
              if (dotted == "No"){df_subset <- setDT(df_subset_vocab)[Code %chin% gsub("\\.", "", codelist_start_all[[i]][,Code])]}
              # df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
              df_subset[,table_origin:='PROCEDURES']
              # Remove excluded codes (found as a result of code with no dots)
              df_subset <- setDT(df_subset)[Code %!in% excluded_codes]
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_start_all[i]), "_",procedures_prefix, "_start.rds"))
              } 
            } 
          } else if(length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "READ"){ 
            for (i in 1:length(codelist_read_all)){ 
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_read_all[[i]][,Code]]
              # df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
              df_subset[,table_origin:='PROCEDURES']
              # Saves table only if it is not empty
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_read_all[i]), "_",procedures_prefix, "_READ.rds"))
              } 
            }
          } else if (length(unique(df_subset_vocab$vocab)) == 1 & unique(df_subset_vocab$vocab) == "SNOMED") {
            for (i in 1:length(codelist_snomed_all)){
              df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_snomed_all[[i]][,Code]]
              # df_subset <- df_subset[,-c("vocab", "flag")]
              df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
              df_subset[,table_origin:='PROCEDURES']
              # Saves table only if it is not empty
              if(nrow(df_subset)>0){
                # Checks for subpops - if present, saves with the prefix of subpop name 
                saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_snomed_all[i]), "_",procedures_prefix, "_SNOMED.rds"))
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
            df_subset_vocab <- df
            if (dotted == "Yes"){df_subset <- setDT(df_subset_vocab)[Code %chin% codelist_start_all[[i]][,Code]]}
            if (dotted == "No"){df_subset <- setDT(df_subset_vocab)[Code %chin% gsub("\\.", "", codelist_start_all[[i]][,Code])]}
            # df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
            df_subset[,table_origin:='PROCEDURES']
            # Remove excluded codes (found as a result of code with no dots)
            df_subset <- setDT(df_subset)[Code %!in% excluded_codes]

            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_start_all[i]), "_",procedures_prefix, "_start.rds"))
            } 
          } 
        } else if(length(unique(df$vocab)) == 1 & unique(df$vocab) == "READ"){ 
          for (i in 1:length(codelist_read_all)){ 
            df_subset <- setDT(df)[Code %chin% codelist_read_all[[i]][,Code]]
            # df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
            df_subset[,table_origin:='PROCEDURES']
            # Saves table only if it is not empty
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_read_all[i]), "_",procedures_prefix, "_READ.rds"))
            } 
          } 
        } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "SNOMED") {
          for (i in 1:length(codelist_snomed_all)){
            df_subset <- setDT(df)[Code %chin% codelist_snomed_all[[i]][,Code]]
            # df_subset <- df_subset[,-c("vocab", "flag")]
            df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
            df_subset[,table_origin:='PROCEDURES']
            # Saves table only if it is not empty
            if(nrow(df_subset)>0){
              # Checks for subpops - if present, saves with the prefix of subpop name 
              saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_snomed_all[i]), "_",procedures_prefix, "_SNOMED.rds"))
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
    # Gets prefix for procedures tables 
    procedures_prefix <- gsub(".csv", "", proc_files[y])
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
    df<-df[!(is.na(Code) | is.na(Vocabulary))]# Removes records with both event code and event record vocabulary missing
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
          # df_subset <- df_subset[,-c("vocab")]
          df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
          df_subset[,table_origin:='PROCEDURES']
          if(nrow(df_subset)>0){
            saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_CPRD_all[i]), "_", procedures_prefix, "_CPRD.rds"))
          }
        }
        # Covers PHARMO Codes
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "PHARMO") {
        for (i in 1:length(codelist_PHARMO_all)){
          df_subset <- setDT(df)[Code %chin% codelist_PHARMO_all[[i]][,Code]]
          df_subset <- df_subset[,c("person_id", "Vocabulary", "Code", "Date")]
          df_subset[,table_origin:='PROCEDURES']
          if(nrow(df_subset)>0){
            saveRDS(data.table(df_subset), paste0(events_tmp_sterility, pop_prefix, "_", names(codelist_PHARMO_all[i]), "_",procedures_prefix, "_PHARMO.rds"))
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
  saveRDS(sterility_all, paste0(sterility_pop, pop_prefix, "_sterility_all.rds"))
  saveRDS(sterility_all_first_occurrence, paste0(sterility_pop, pop_prefix, "_sterility_all_first_occurrence.rds"))
  rm(codelist_all, codelist_CPRD_all, codelist_PHARMO_all, codelist_read_all, codelist_snomed_all, codelist_start_all, df, df_subset, df_subset_vocab, sterility_all, sterility_all_first_occurrence)
} else {
  print("There are no Sterility records")
}

