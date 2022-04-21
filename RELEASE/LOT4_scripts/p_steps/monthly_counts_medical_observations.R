if (is_BIFAP){
  #### Get list of medical observation tables
  mo_files<-list.files(path=path_dir,pattern="^MEDICAL_OB",ignore.case = TRUE)
  # Creates empty table for counts
  # Gets min and max year from denominator file
  FUmonths_df <- as.data.table(FUmonths_df)
  min_data_available <- min(FUmonths_df$Y)
  max_data_available <- max(FUmonths_df$Y)
  FUmonths_df[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)]
  empty_df<-expand.grid(seq(min(FUmonths_df$Y), max(FUmonths_df$Y)), seq(1, 12))
  names(empty_df) <- c("year", "month")
  
  for(y in 1:length(mo_files)){
    # Gets prefix for medicines tables 
    mo_prefix<-gsub(".csv","",mo_files[y])
    # Loads table
    df<-fread(paste(path_dir,mo_files[y],sep=""),stringsAsFactors = FALSE)
    # Data Cleaning
    df<-df[,c("person_id","mo_date","mo_code","mo_record_vocabulary","mo_source_table","mo_source_column","mo_source_value","mo_meaning")] # Keeps necessary columns
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] # Makes sure missing data is read appropriately
    setnames(df,"mo_meaning","Meaning") # Renames column names
    setnames(df,"mo_date","Date") # Renames column names
    setnames(df,"mo_record_vocabulary","Vocabulary") # Renames column names
    setnames(df,"mo_code","Code") # Renames column names
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
    df<-df[!(is.na(Code) | is.na(Vocabulary))]# Removes records with both event code and event record vocabulary missing
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Removes unspecified sex
    print(paste0("Finding pregnancy test records in ", mo_files[y]))
    if(nrow(df)>0){
      df_subset<-df[mo_source_table=="datos_generales_paciente" & mo_source_column =="PT",]
      if(nrow(df_subset)>0){
        saveRDS(df_subset, paste0(events_tmp_mo,pop_prefix, "_pregtests_",mo_prefix, ".rds"))
      }
    }
  }
  
  # Read in all pregnancy test files and bind them 
  preg_test_files<-list.files(path=events_tmp_mo,pattern=paste0(pop_prefix,"_pregtest"),ignore.case=TRUE,full.names=T)
  if(length(preg_test_files)>0){
    preg_test_df<-do.call(rbind,lapply(preg_test_files,readRDS))
    preg_test_df<-preg_test_df[,-c("mo_source_table","mo_source_column","mo_source_value")]
    # Remove duplicates (by person_id and pregnancy_date)
    preg_test_df<-preg_test_df[!duplicated(preg_test_df[,c("person_id", "Date")])]
    # Counts 
    counts<-preg_test_df[,.N,by =.(year,month(Date))]
    # Merges with empty_df
    counts<-as.data.table(merge(x=empty_df,y=counts,by=c("year","month"), all.x=TRUE))
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
    counts <-counts[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
    counts <-counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    # Saves files in g_output monthly counts
    if(preg_test_df[,.N]>0){
      saveRDS(preg_test_df, paste0(mo_pop,pop_prefix, "_pregtests_MEDICAL_OBSERVATIONS.rds"))
      saveRDS(counts, paste0(monthly_counts_mo,"/",pop_prefix, "_pregtests_MO_counts.rds"))
    } else {
      print(paste0("No pregnancy records were found in the Medical Observation Tables"))
    }
  }
}
