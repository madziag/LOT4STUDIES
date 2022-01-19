# Load Create Concept Sets file
matches <- c()
source(paste0(pre_dir,"CreateConceptSets_ATC.R"))
# Create empty table for counts 
# Get min and max year from denominator file
FUmonths_df <- as.data.table(FUmonths_df)
FUmonths_df[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)]
empty_counts_df <- expand.grid(seq(min(FUmonths_df$Y), max(FUmonths_df$Y)), seq(1, 12))
names(empty_counts_df) <- c("year", "month")
# Create List of Retinoid and Valproates for individual counts
codelist_ind <- Filter(function(x) names(codelist_all)== "Valproate" | names(codelist_all) == "Retinoid", codelist_all)
codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)

if(study_type == "Retinoids"){
  # Create List of Retinoid and Valproates for individual counts
  codelist_ind <- Filter(function(x) names(codelist_all) == "Retinoid", codelist_all)
  codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)
} else if (study_type == "Valproates") {
  # Create List of Retinoid and Valproates for individual counts
  codelist_ind <- Filter(function(x) names(codelist_all)== "Valproate", codelist_all)
  codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)
} else if (study_type == "Both") {
  # Create List of Retinoid and Valproates for individual counts
  codelist_ind <- Filter(function(x) names(codelist_all)== "Valproate" | names(codelist_all) == "Retinoid", codelist_all)
  codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)
} else {
  print("Please indicate study type")
  }
# Create other lists
comb_meds <- list()

# Load Medicines files
med_files <- list.files(path=path_dir, pattern = "MEDICINES", ignore.case = TRUE) 

# Check for MEDICINES Tables present
if(length(med_files)>0){
  # Process each EVENTS table
  for (y in 1:length(med_files)){
    # Load table
    df<-fread(paste(path_dir, med_files[y], sep=""), stringsAsFactors = FALSE)
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
    print(paste0("Finding matching records in ", med_files[y]))
    # Check if df is NOT empty
    if(nrow(df)>0){
      # Look for matches in df using event vocabulary type specific code list
      for (i in 1:length(codelist_all)){
        df_subset <- setDT(df)[Code %chin% codelist_all[[i]][,Code]]
        if(SUBP == TRUE){
          saveRDS(data.table(df_subset), paste0(events_tmp_ATC, populations[pop], names(codelist_all[i]), "_",med_files[y], ".rds"))
        }else{
          saveRDS(data.table(df_subset), paste0(events_tmp_ATC, names(codelist_all[i]), "_",med_files[y], ".rds"))
        }
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
      if(comb_meds[[i]][,.N]>0){
        if (SUBP == TRUE){
          pop_names <- gsub(".rds", "", populations[pop])
          saveRDS(comb_meds[[i]], paste0(medications_pop,pop_names, "_", names(codelist_all[i]),".rds"))
          saveRDS(counts, paste0(monthly_counts_atc,"/", pop_names, "_",names(codelist_all[i]),"_MEDS_counts.rds"))
        }else {
          saveRDS(comb_meds[[i]], paste0(medications_pop,names(codelist_all[i]),".rds"))
          saveRDS(counts, paste0(monthly_counts_atc,"/",names(codelist_all[i]),"_MEDS_counts.rds"))
        }
        
      } else {
        print(paste("There are no matching records for", names(codelist_all[i])))
      }
    } else {
      print(paste("There are no matching records for", names(codelist_all[i])))
    }
    
  }
  # Individual counts for Valproates and Retinoids
  for(i in 1:length(codelist_ind)){
    ind_files <- list.files(path=medications_pop, pattern = paste0(names(codelist_ind)[i], ".rds"))
    
    for(x in 1:length(ind_files)){
      df_ind<- readRDS(paste0(medications_pop, ind_files[x]))
      
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
        
        if(comb_meds[[i]][,.N]>0){
          if(SUBP == TRUE){
            pop_names <- gsub(".rds", "", populations[pop])
            saveRDS(counts_ind, paste0(monthly_counts_atc,"/",pop_names,"_", tolower(names(codelist_ind[i])),"_",codelist_ind[[i]][j][,Medication],codelist_ind[[i]][j][,Code],"_MEDS_counts.rds"))
          }else{
            saveRDS(counts_ind, paste0(monthly_counts_atc,"/",names(codelist_ind[i]),"_",codelist_ind[[i]][j][,Medication],codelist_ind[[i]][j][,Code],"_MEDS_counts.rds"))
          }
          
        } else {
          print(paste("There are no matching records for", names(codelist_all[i])))
        }
      }
      
    }
  }
} else {
  print("There are no MEDICATIONS tables to analyse!")
}



