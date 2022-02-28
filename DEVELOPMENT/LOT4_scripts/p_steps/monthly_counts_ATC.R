#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/12/2021

# 1. Looks for ATC codes from the ATC codelist (concept set created via CreateConceptSets_ATC.R) in MEDICINES TABLES 
# 2. Results in list of records with matching ATC
# 3. Counts records saved in the previous step by month/year for each code group 
# 4. Counts records saved in the previous step by month/year for each ATC of Retinoids and/or Valproates 
# Loads Create Concept Sets file
matches <- c()
source(paste0(pre_dir,"CreateConceptSets_ATC.R"))
# Creates empty table for counts 
# Gets min and max year from denominator file
FUmonths_df <- as.data.table(FUmonths_df)
FUmonths_df[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)]
if(is_BIFAP){empty_df<-expand.grid(seq(2010, 2020), seq(1, 12))}else{empty_df<-expand.grid(seq(min(FUmonths_df$Y), max(FUmonths_df$Y)), seq(1, 12))}
names(empty_df) <- c("year", "month")
# Creates List of Retinoid and Valproates for individual counts
codelist_ind <- Filter(function(x) names(codelist_all)== "Valproate" | names(codelist_all) == "Retinoid", codelist_all)
codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)
if(study_type == "Retinoid"){
  # Removes Valproate codes
  codelist_all <- codelist_all[!grepl("valp", names(codelist_all), ignore.case = TRUE)]
  # Removes Valproate codes for individual counts
  codelist_ind <- Filter(function(x) names(codelist_all) == "Retinoid", codelist_all)
  codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)
} else if (study_type == "Valproate") {
  # Removes Retinoid codes
  codelist_all <- codelist_all[!grepl("retin", names(codelist_all), ignore.case = TRUE)]
  # Removes Retinoid codes for individual counts
  codelist_ind <- Filter(function(x) names(codelist_all)== "Valproate", codelist_all)
  codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)
} else if (study_type == "Both") {
  # Creates List of Retinoid and Valproates for individual counts
  codelist_ind <- Filter(function(x) names(codelist_all)== "Valproate" | names(codelist_all) == "Retinoid", codelist_all)
  codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)
} else {
  print("Please indicate study type")
}
# Creates other lists
comb_meds <- list()
comb_meds1 <- list()
# Loads Medicines files
med_files <- list.files(path=path_dir, pattern = "MEDICINES", ignore.case = TRUE) 
# Checks for MEDICINES Tables present
if(length(med_files)>0){
  # Processes each EVENTS table
  for (y in 1:length(med_files)){
    # Gets prefix for medicines tables 
    meds_prefix <- gsub(".csv", "", med_files[y])
    # Loads table
    df<-fread(paste(path_dir, med_files[y], sep=""), stringsAsFactors = FALSE)
    # Data Cleaning
    df<-df[,c("person_id", "medicinal_product_atc_code", "date_dispensing", "date_prescription", "meaning_of_drug_record")] # Keep necessary columns
    df<-df[,lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] # Makes sure missing data is read appropriately
    setnames(df,"meaning_of_drug_record","Meaning") # Renames column names
    setnames(df,"medicinal_product_atc_code", "Code") # Renames column names
    # Creates new column Date. It takes the date from date_dispensing. If value from date_dispensing is missing, it takes the date value from date_prescription
    df<-df[,Date:= ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)]
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    # Merges medicine table with study population table (there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)] # Left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)] # Transforms to numeric variables  
    # Removes records with missing values in the medicine table 
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    df[,Date:=as.IDate(Date,"%Y%m%d")] # Transforms to date variables
    df[,entry_date:=as.IDate(entry_date,"%Y%m%d")] # Transforms to date variables
    # Creates year variable 
    df[,year:=year(Date)] 
    df<-df[!is.na(year)] # Removes records with both dates missing
    if(is_PHARMO){df<-df[year>2008 & year<2020]} else {df<-df[year>2008 & year<2021]} # Years used in study
    # Removes records with ATC code missing 
    df<-df[!(is.na(Code))]
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"] # Removes unspecified sex
    # Prints Message
    print(paste0("Finding matching records in ", med_files[y]))
    # Checks if df is NOT empty
    if(nrow(df)>0){
      # Looks for matches in df using event vocabulary type specific code list
      for (i in 1:length(codelist_all)){
        df_subset <- setDT(df)[Code %chin% codelist_all[[i]][,Code]]
        df_subset <- df_subset[!duplicated(df_subset),]
        saveRDS(data.table(df_subset), paste0(events_tmp_ATC, pop_prefix,"_", names(codelist_all[i]), "_",meds_prefix, ".rds"))
      }
    }
  }
  print("Counting records")
  # Monthly Counts 
  for(i in 1:length(codelist_all)){
    # Gets list of files in each code group folder
    files <- list.files(path=events_tmp_ATC, pattern=names(codelist_all[i]))
    # Performs counts per month/year
    if (length(files)>0){
      # Loads files 
      comb_meds[[i]] <- do.call(rbind,lapply(paste0(events_tmp_ATC, files), readRDS))
      comb_meds[[i]] <- comb_meds[[i]][!duplicated(comb_meds[[i]]),]
      comb_meds1[[i]] <- comb_meds[[i]][Date>=entry_date & Date<=exit_date]
      # Counts by month-yearcode
      counts <- comb_meds1[[i]][,.N, by = .(year,month(Date))]
      # Merges with empty_df
      counts<- as.data.table(merge(x = empty_df, y = counts, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      counts[is.na(counts[,N]), N:=0]
      # Masking values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      counts[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Changes values less than 5 and more than 0 to 5
      if(mask==T){counts[masked==1,N:=5]} else {counts[masked==1,N:=N]}
      # Calculates rates
      counts <- within(counts, YM<- sprintf("%d-%02d", year, month))
      counts <- merge(x = counts, y = FUmonths_df, by = c("YM"), all.x = TRUE)
      counts <-counts[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
      counts <-counts[,c("YM", "N", "Freq", "rates", "masked")]
      # Saves files in g_output monthly counts 
      if(comb_meds[[i]][,.N]>0){
        saveRDS(comb_meds[[i]], paste0(medications_pop, pop_prefix, "_", names(codelist_all[i]),".rds"))
        saveRDS(counts, paste0(monthly_counts_atc,"/", pop_prefix, "_", names(codelist_all[i]),"_MEDS_counts.rds"))
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
      df_ind <- readRDS(paste0(medications_pop, ind_files[x]))
      df_ind <- df_ind[!duplicated(df_ind),]
      df_ind <- df_ind[Date>=entry_date & Date<=exit_date]
      for(j in 1:length(unique(codelist_ind[[i]]$Code))){
        sub_ind <- setDT(df_ind)[Code %chin% codelist_ind[[i]][j][,Code]]
        counts_ind<- sub_ind[,.N, by = .(year,month(Date))]
        counts_ind<- as.data.table(merge(x = empty_df, y = counts_ind, by = c("year", "month"), all.x = TRUE))
        counts_ind[is.na(counts_ind[,N]), N:=0]
        # Masking values less than 5
        # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
        counts_ind[,masked:=ifelse(N<5 & N>0, 1, 0)]
        # Changes values less than 5 and more than 0 to 5
        if(mask==T){counts_ind[masked==1,N:=5]} else {counts_ind[masked==1,N:=N]}
        # Calculates rates
        counts_ind <- within(counts_ind, YM<- sprintf("%d-%02d", year, month))
        counts_ind <- merge(x = counts_ind, y = FUmonths_df, by = c("YM"), all.x = TRUE)
        counts_ind <-counts_ind[,rates:=as.numeric(N)/as.numeric(Freq)][,rates:=rates*1000][is.nan(rates)|is.na(rates), rates:=0]
        counts_ind <-counts_ind[,c("YM", "N", "Freq", "rates", "masked")]
        if(sub_ind[,.N]>0){
          saveRDS(counts_ind, paste0(monthly_counts_atc,"/",pop_prefix,"_", tolower(names(codelist_ind[i])),"_",codelist_ind[[i]][j][,Medication],codelist_ind[[i]][j][,Code],"_MEDS_counts.rds"))
        } else {
          print(paste("There are no matching records for", names(codelist_all[i])))
        }
      }
    }
  }
} else {
  print("There are no MEDICATIONS tables to analyse!")
}



