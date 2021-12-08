# Load study population
study_population <- readRDS(paste0(populations_dir, "ALL_study_population.rds"))
# Load Create Concept Sets file
source(paste0(pre_dir,"CreateConceptSets_ATC.R"))
# Create empty table for counts 
empty_counts_df <- expand.grid(seq(2009, 2020), seq(1, 12))
names(empty_counts_df) <- c("year", "month")
# Create List of Retinoid and Valproates for individual counts
codelist_ind <- Filter(function(x) names(codelist_all)== "Valproate" | names(codelist_all) == "Retinoid", codelist_all)
codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)
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
    # Create year variable 
    df[,year:=year(event_date)] 
    df<-df[!is.na(year)] # Remove records with both dates missing
    df<-df[year>2008 & year<2021] # Years used in study
    df[,date_dif:=start_follow_up-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)] # Identify persons that have an event before start_of_follow_up
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1] # Remove records that are outside the obs_period for all subjects
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
    print("Counting records")
    # Monthly Counts 
    for(i in 1:length(codelist_all)){
      # Get list of files in each code group folder
      files <- list.files(path=events_tmp_ATC, pattern=names(codelist_all[i]))
      # Perform counts per month/year
      if (length(files)>0){
        # Loads files 
        comb_meds[[i]]<-do.call(rbind,lapply(paste0(events_tmp_ATC, files), readRDS))
        # Count by month-year
        counts<- comb_meds[[i]][,.N, by = .(year,month(event_date))]
        # Merge with empty_counts_df
        counts<- as.data.table(merge(x = empty_counts_df, y = counts, by = c("year", "month"), all.x = TRUE))
        # Fill in missing values with 0
        counts[is.na(counts[,N]), N:=0]
        # Calculate rates
        counts <- within(counts, YM<- sprintf("%d-%02d", year, month))
        counts <- merge(x = counts, y = FUmonths_df, by = c("YM"), all.x = TRUE)
        counts <-counts[,c("YM", "N", "Freq")]
        counts <-counts[,rates:=as.numeric(N)/as.numeric(Freq)]
        # Save files in g_output monthly counts 
        if (subpopulations_present=="Yes"){
          if(comb_meds[[i]][,.N]>0){
            saveRDS(comb_meds[[i]], paste0(medications_pop,subpopulations_names[s], "/", names(codelist_all[i]),".rds"))
            saveRDS(counts, paste0(monthly_counts_atc,"/",subpopulations_names[s], "/", names(codelist_all[i]),"_counts.rds"))
            } else {print(paste("There are no matching records for", names(codelist_all[i])))}
        } else {
          if(comb_meds[[i]][,.N]>0){
            saveRDS(comb_meds[[i]], paste0(medications_pop,names(codelist_all[i]),".rds"))
            saveRDS(counts, paste0(monthly_counts_atc,"/",names(codelist_all[i]),"_counts.rds"))
          } else {print(paste("There are no matching records for", names(codelist_all[i])))}
        }
      } else {
        print(paste("There are no matching records for", names(codelist_all[i])))
      }

    }
  # Individual counts for Valproates and Retinoids
    # Individual counts for Valproates and Retinoids
    for(i in 1:length(codelist_ind)){
      df_ind<- readRDS(paste0(medications_pop, list.files(path=medications_pop, pattern = paste0(names(codelist_ind)[i], ".rds"))))
      for(j in 1:length(unique(codelist_ind[[i]]$Code))){
        sub_ind <- setDT(df_ind)[Code %chin% codelist_ind[[i]][j][,Code]]
        counts_ind<- sub_ind[,.N, by = .(year,month(event_date))]
        counts_ind<- as.data.table(merge(x = empty_counts_df, y = counts_ind, by = c("year", "month"), all.x = TRUE))
        counts_ind[is.na(counts_ind[,N]), N:=0]
        # Calculate rates
        counts_ind <- within(counts_ind, YM<- sprintf("%d-%02d", year, month))
        counts_ind <- merge(x = counts_ind, y = FUmonths_df, by = c("YM"), all.x = TRUE)
        counts_ind <-counts_ind[,c("YM", "N", "Freq")]
        counts_ind <-counts_ind[,rates:=as.numeric(N)/as.numeric(Freq)]
        
        if (subpopulations_present=="Yes"){
          if(comb_meds[[i]][,.N]>0){
            saveRDS(counts_ind, paste0(medications_pop,subpopulations_names[s], "/",tolower(names(codelist_ind[i])),"_",codelist_ind[[i]][j][,Medication],codelist_ind[[i]][j][,Code],"_counts.rds"))
          } else {print(paste("There are no matching records for", names(codelist_all[i])))}
        } else {
          if(comb_meds[[i]][,.N]>0){
            saveRDS(counts_ind, paste0(monthly_counts_atc,"/",tolower(names(codelist_ind[i])),"_",codelist_ind[[i]][j][,Medication],codelist_ind[[i]][j][,Code],"_counts.rds"))
          } else {print(paste("There are no matching records for", names(codelist_all[i])))}
        }
      }
      
    }
    # Delete events folder -> records have now been concatenated and saved in diagnosis folder 
    unlink(paste0(tmp, "/events_atc"), recursive = TRUE)
}

# CLEAN UP
rm(list=ls(pattern="codelist"))
rm(comb_meds, counts)

