#Load study population
# study_population <- readRDS(paste0(populations_dir, "ALL_study_population.rds"))
#Load study population/populations 
# populations <- list.files(populations_dir, pattern = "study_population")
# Load Create Concept Sets file
matches <- c()
source(paste0(pre_dir,"CreateConceptSets_DxCodes.R"))
# Create empty table for counts 
# Get min and max year from denominator file
FUmonths_df <- as.data.table(FUmonths_df)
FUmonths_df[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)]
empty_counts_df <- expand.grid(seq(min(FUmonths_df$Y), max(FUmonths_df$Y)), seq(1, 12))
names(empty_counts_df) <- c("year", "month")

  # Check for EVENTS Tables present
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
      df[,exit_date:=as.IDate(exit_date,"%Y%m%d")] # Transform to date variables
      # Creates year variable
      df[,year:=year(event_date)]
      df<-df[!is.na(year)] # Remove records with both dates missing
      df<-df[year>2008 & year<2021] # Years used in study
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
            
            if(nrow(df_subset)>0){
              
              if(SUBP == TRUE){
                saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_start_all[i]), "_",populations[pop], "_",actual_tables$EVENTS[y], "_start.rds"))
                new_file <-c(list.files(events_tmp_DX, "\\_start.rds$"))
                lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_start_all[i])), x))})
              } else { 
                saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_start_all[i]), "_", actual_tables$EVENTS[y], "_start.rds"))
                new_file <-c(list.files(events_tmp_DX, "\\_start.rds$"))
                lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_start_all[i])), x))})
                }
            }
          }
            # Cover RCD, RCD2, READ, CPRD_Read Codes 
            } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "READ") {
              
              for (i in 1:length(codelist_read_all)){
                df_subset <- setDT(df)[Code %chin% codelist_read_all[[i]][,Code]]
                df_subset <- df_subset[,-c("vocab")]
          
                if(nrow(df_subset)>0){
                  if(SUBP == TRUE){
                    saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_read_all[i]), "_", populations[pop], "_",actual_tables$EVENTS[y], "_READ.rds"))
                    new_file <-c(list.files(events_tmp_DX, "\\_READ.rds$"))
                    lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_read_all[i])), x))})
                  } else {                  
                    saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_read_all[i]), "_",actual_tables$EVENTS[y], "_READ.rds"))
                    new_file <-c(list.files(events_tmp_DX, "\\_READ.rds$"))
                    lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_read_all[i])), x))})
                    }
       
          }
        }
      # Covers SNOMEDCT_US, SCTSPA, SNOMED Codes 
      } else if (length(unique(df$vocab)) == 1 & unique(df$vocab) == "SNOMED") {
        
        for (i in 1:length(codelist_snomed_all)){
          df_subset <- setDT(df)[Code %chin% codelist_snomed_all[[i]][,Code]]
          df_subset <- df_subset[,-c("vocab")]
          
          if(nrow(df_subset)>0){
            if(SUBP == TRUE){
              saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_snomed_all[i]), "_", populations[pop],"_",actual_tables$EVENTS[y], "_SNOMED.rds"))
              new_file <-c(list.files(events_tmp_DX, "\\_SNOMED.rds$"))
              lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_snomed_all[i])), x))})
            }else{
              saveRDS(data.table(df_subset), paste0(events_tmp_DX, names(codelist_snomed_all[i]), "_",actual_tables$EVENTS[y], "_SNOMED.rds"))
              new_file <-c(list.files(events_tmp_DX, "\\_SNOMED.rds$"))
              lapply(new_file, function(x){file.rename( from = file.path(events_tmp_DX, x), to = file.path(paste0(events_tmp_DX, names(codelist_snomed_all[i])), x))})
            }

          }
        }
      } else { 
        print(paste0(unique(df$event_vocabulary), " is not part of code list vocabulary"))
        }
      
    } else {
      print(paste0("There are no matching records in ", actual_tables$EVENTS[y]))
      }
    }
      
  # Print Message
  print("Counting records") 
  # Monthly Counts 
  for(i in 1:length(codelist_all)){
    # Get list of files in each code group folder 
    files <- list.files(path=paste0(events_tmp_DX, names(codelist_all[i]))) 
    # Perform counts per month/year
    if (length(files)>0){
      # Loads files 
      files <- list.files(path=paste0(events_tmp_DX, names(codelist_all[i])), pattern = "\\.rds$", full.names = TRUE)
      comb_meds <- do.call("rbind", lapply(files, readRDS))
      # Count by month-year
      counts <- comb_meds[,.N, by = .(year,month(event_date))]
      # Merge with empty_counts_df
      counts <- as.data.table(merge(x = empty_counts_df, y = counts, by = c("year", "month"), all.x = TRUE))
      # Fill in missing values with 0
      counts[is.na(counts[,N]), N:=0]
      # Calculate rates
      counts <- within(counts, YM<- sprintf("%d-%02d", year, month))
      counts <- merge(x = counts, y = FUmonths_df, by = c("YM"), all.x = TRUE) 
      counts <-counts[,c("YM", "N", "Freq")]
      counts <-counts[,rates:=as.numeric(N)/as.numeric(Freq)]
      # Save files in g_output monthly counts 
      if(comb_meds[,.N]>0){
        
        if(SUBP == TRUE){      
          pop_names <- gsub(".rds", "", populations[pop])
          saveRDS(comb_meds, paste0(diagnoses_pop,pop_names, "_", names(codelist_all[i]),".rds"))
          saveRDS(counts, paste0(monthly_counts_dx,"/",pop_names, "_", names(codelist_all[i]),"_counts.rds"))
        } else {
          saveRDS(comb_meds, paste0(diagnoses_pop,names(codelist_all[i]),".rds"))
          saveRDS(counts, paste0(monthly_counts_dx,"/",names(codelist_all[i]),"_counts.rds"))
          }
      } else {
        print(paste("There are no matching records for", names(codelist_all[i])))
      }
    } else {
      print(paste("There are no matching records for", names(codelist_all[i])))
    }
  }
  # Delete events folder -> records have now been concatenated and saved in diagnosis folder 
   # unlink(paste0(tmp, "/events_dx"), recursive = TRUE)

  } else {
  print("There are no EVENTS tables to analyse!")
}
  

# Clean up
# rm(list=ls(pattern="codelist"))
# rm(comb_meds, counts, df, df_subset, empty_counts_df)

  


