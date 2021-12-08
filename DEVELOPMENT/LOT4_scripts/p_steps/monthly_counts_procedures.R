#Load study population
study_population <- readRDS(paste0(populations_dir, "ALL_study_population.rds"))
# Load Create Concept Sets file
matches <- c()
source(paste0(pre_dir,"CreateConceptSets_ProcedureCodes.R"))
# Load Procedure files 
proc_files <- list.files(path=path_dir, pattern = "PROCEDURES", ignore.case = TRUE)
# Create empty table for counts 
empty_counts_df <- expand.grid(seq(2009, 2020), seq(1, 12))
names(empty_counts_df) <- c("year", "month")
# Check for PROCEDURE Tables present
if(length(proc_files)>0){
  # Create a new folder for each code group type (to store records with matching codes)
  for (z in 1:length(codelist_all)){ifelse(!dir.exists(file.path(events_tmp_PROC, names(codelist_all[z]))), dir.create(paste(events_tmp_PROC, names(codelist_all[z]), sep="")), FALSE)}
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
  # Print Message
  print("Counting records")
  # Monthly Counts
  for(i in 1:length(codelist_all)){
    # Get list of files in each code group folder
    files <- list.files(path=paste0(events_tmp_PROC, names(codelist_all[i])))
    # Perform counts per month/year
    if (length(files)>0){
      # Loads files
      files <- list.files(path=paste0(events_tmp_PROC, names(codelist_all[i])), pattern = "\\.rds$", full.names = TRUE)
      comb_meds <- do.call("rbind", lapply(files, readRDS))
      # Count by month-year
      counts <- comb_meds[,.N, by = .(year,month(procedure_date ))]
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
      if (subpopulations_present=="Yes"){
        if(comb_meds[,.N]>0){
          saveRDS(comb_meds, paste0(procedures_pop,subpopulations_names[s], "/", names(codelist_all[i]),".rds"))
          saveRDS(counts, paste0(monthly_counts_proc,"/",subpopulations_names[s], "/", names(codelist_all[i]),"_counts.rds"))
        } else {
          print(paste("There are no matching records for", names(codelist_all[i])))}
      } else {
        if(comb_meds[,.N]>0){
          saveRDS(comb_meds, paste0(procedures_pop,names(codelist_all[i]),".rds"))
          saveRDS(counts, paste0(monthly_counts_proc,"/",names(codelist_all[i]),"_counts.rds"))
        } else {print(paste("There are no matching records for", names(codelist_all[i])))
        }
      }
    } else {
      print(paste("There are no matching records for", names(codelist_all[i])))
    }
  }
  # # Delete events folder -> records have now been concatenated and saved in diagnosis folder 
   # unlink(paste0(tmp, "/events_proc"), recursive = TRUE)
  
}
# Clean up
rm(list=ls(pattern="codelist"))





