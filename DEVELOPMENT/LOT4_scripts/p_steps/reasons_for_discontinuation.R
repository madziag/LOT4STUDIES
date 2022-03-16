#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022


### Reasons for discontinuation: 
# For Retinoids: 1. Folic acid 2. Pregnancy Start 3. ADR_retinoids, 4. Multiple 5. Unknown
# For Valproates: 1. Folic acid 2. Pregnancy Start 3. ADR_valproates, 4. Multiple 5. Unknown

# The following files are needed: 
# 1. Discontinued RECORDS created in medicine_counts_incidence_prevalence_discontinuation.R)
discontinued_all_files <- list.files(counts_dfs_dir, pattern="discontinued")
# Filter for subpopulation if any (should only do counts of current subpopulation)
if(pop_prefix == "PC"){discontinued_all_files <- discontinued_all_files[!grepl("PC_HOSP",discontinued_all_files)]}
if(pop_prefix == "PC_HOSP"){discontinued_all_files <- discontinued_all_files[grepl("PC_HOSP",discontinued_all_files)]}
# 2. Discontinued COUNTS (created in medicine_counts_incidence_prevalence_discontinuation.R)
discontinued_counts_files <- list.files(medicines_counts_dir, pattern = "discontinued_counts", ignore.case = T, full.names = T)
if(pop_prefix == "PC"){discontinued_counts_files <- discontinued_counts_files[!grepl("PC_HOSP",discontinued_counts_files)]}
if(pop_prefix == "PC_HOSP"){discontinued_counts_files <- discontinued_counts_files[grepl("PC_HOSP",discontinued_counts_files)]}
# Removes age specific prevalent count files
discontinued_counts_files <- discontinued_counts_files[!grepl("age_group|indication|tx_dur", discontinued_counts_files)]

# 3. Folic Acid records (created in preliminary counts)
folic_acid_files <- list.files(medications_pop, pattern = "folic_acid")
if(pop_prefix == "PC"){folic_acid_files <- folic_acid_files[!grepl("PC_HOSP",folic_acid_files)]}
if(pop_prefix == "PC_HOSP"){folic_acid_files <- folic_acid_files[grepl("PC_HOSP",folic_acid_files)]}

# 4. Pregnancy start df (from ARS toscana - D3_pregnancy_reconciled)
D3_pregnancy_reconciled <- as.data.table(get(load(paste0(preg_dir, "g_intermediate/D3_pregnancy_reconciled.RData"))))
# Drop no longer needed columns 
D3_pregnancy_reconciled <- D3_pregnancy_reconciled[,c("person_id", "pregnancy_start_date")]
D3_pregnancy_reconciled[,pregnancy_start_date:=as.IDate(pregnancy_start_date)]

# 5. ADR records for retinoids/valproates (created in preliminary counts)
# From Diagnosis folder
adr_all_files_events <- list.files(diagnoses_pop, pattern = "adr")
if(pop_prefix == "PC"){adr_all_files_events <- adr_all_files_events[!grepl("PC_HOSP",adr_all_files_events)]}
if(pop_prefix == "PC_HOSP"){adr_all_files_events <- adr_all_files_events[grepl("PC_HOSP",adr_all_files_events)]}
# From Events folder 
adr_all_files_procedures <- list.files(procedures_dxcodes_pop, pattern = "adr")
if(pop_prefix == "PC"){adr_all_files_procedures <- adr_all_files_procedures[!grepl("PC_HOSP",adr_all_files_procedures)]}
if(pop_prefix == "PC_HOSP"){adr_all_files_procedures <- adr_all_files_procedures[grepl("PC_HOSP",adr_all_files_procedures)]}

# Retinoids
adr_retinoids_events <- adr_all_files_events[!grepl("nausea|tremor",adr_all_files_events)]
adr_retinoids_procedures <- adr_all_files_procedures[!grepl("nausea|tremor",adr_all_files_procedures)]
# Read in files from both events and procedures (joining them all)
if(length(adr_retinoids_events)>0){adr_retinoids_events_df <- do.call(rbind,lapply(paste0(diagnoses_pop,"/",adr_retinoids_events), readRDS))}
if(length(adr_retinoids_procedures)>0){adr_retinoids_procedures_df <- do.call(rbind,lapply(paste0(procedures_dxcodes_pop,"/",adr_retinoids_procedures), readRDS))}
# Loads and binds only if files present
if(exists("adr_retinoids_events_df") & exists("adr_retinoids_procedures_df")){adr_retinoids <- rbind(adr_retinoids_events_df, adr_retinoids_procedures_df)}
if(exists("adr_retinoids_events_df")){adr_retinoids <- adr_retinoids_events_df}
if(exists("adr_retinoids_procedures_df")){adr_retinoids <- adr_retinoids_procedures_df}
if(exists("adr_retinoids_events_df") | exists("adr_retinoids_procedures_df")){
  adr_retinoids <- adr_retinoids[,c("person_id", "Date", "Code")]
  setnames(adr_retinoids, "Date", "adr_date")
}

# Valproates
adr_valproates_events <- adr_all_files_events[grepl("nausea|tremor",adr_all_files_events)]
adr_valproates_procedures <- adr_all_files_procedures[grepl("nausea|tremor",adr_all_files_procedures)]
# Read in files from both events and procedures (joining them all)
if(length(adr_valproates_events)>0){adr_valproates_events_df <- do.call(rbind,lapply(paste0(diagnoses_pop,"/",adr_valproates_events), readRDS))}
if(length(adr_valproates_procedures)>0){adr_valproates_procedures_df <- do.call(rbind,lapply(paste0(procedures_dxcodes_pop,"/",adr_valproates_procedures), readRDS))}
# Loads and binds only if files present
if(exists("adr_valproates_events_df") & exists("adr_valproates_procedures_df")){adr_valproates <- rbind(adr_valproates_events_df, adr_valproates_procedures_df)}
if(exists("adr_valproates_events_df")){adr_valproates <- adr_valproates_events_df}
if(exists("adr_valproates_procedures_df")){adr_valproates <- adr_valproates_procedures_df}
if(exists("adr_valproates_events_df") | exists("adr_valproates_procedures_df")){
  adr_valproates <- adr_valproates[,c("person_id", "Date")]
  setnames(adr_valproates, "Date", "adr_date")
}

# 6. Denominator - for empty counts df
# Looks for denominator file in output directory 
denominator_file <- list.files(tmp, pattern = paste0(pop_prefix,"_denominator.rds"))
# Loads denominator file 
denominator <- readRDS(paste0(tmp, denominator_file))
# Split Y-M variable to year - month columns (for merging later)
denominator[, c("year", "month") := tstrsplit(YM, "-", fixed=TRUE)]
denominator[,year:=as.integer(year)][,month:=as.integer(month)]
min_data_available <- min(denominator$year)
max_data_available <- max(denominator$year)
### Creates empty df for expanding counts files (when not all month-year combinations have counts)
empty_df<-as.data.table(expand.grid(seq(min(denominator$year), max(denominator$year)), seq(1, 12)))
names(empty_df) <- c("year", "month")


# Checks if discontinued file exists
if(length(discontinued_all_files)>1){
  for(i in 1:length(discontinued_all_files)){
    # Loads discontinued file
    df_discontinued <- readRDS(paste0(counts_dfs_dir, discontinued_all_files[i])) # 39
    # Removes no longer needed columns 
    df_discontinued <- df_discontinued[,c("person_id", "episode.end")]
    # Renames columns
    setnames(df_discontinued, "episode.end", "discontinued_date")
    ############################
    ######## FOLIC ACID ########
    ############################
    # Load folic acid records
    if(length(folic_acid_files)>0){
      # Loads files
      df_folic_acid <- readRDS(paste0(medications_pop,folic_acid_files))
      # Renames columns
      setnames(df_folic_acid, "Date", "folic_acid_date")
      # Removes no longer needed columns
      df_folic_acid <- df_folic_acid[,c("person_id", "folic_acid_date")]
      # Merge discontinued with folic acid records # LEFT JOIN
      df_discontinued <- df_folic_acid[df_discontinued,on=.(person_id), allow.cartesian = T] 
      # ignore folic acid values if date is 90+ days before discontinuation date or if folic acid date is after disconinuation date
      df_discontinued[discontinued_date-folic_acid_date>=90|discontinued_date<folic_acid_date, folic_acid_date:=NA]
      # Order data 
      df_discontinued <- df_discontinued[order(person_id,discontinued_date, folic_acid_date)]
      # Deduplicate records
      df_discontinued <- df_discontinued[!duplicated(df_discontinued[,c("person_id", "discontinued_date")])]
    }
    
    ###################################
    ######## PREGNANCY RECORDS ########
    ###################################
    # Pregnancy records loaded outside the for loop
    # Merge study population with pregnancy records
    df_discontinued <- D3_pregnancy_reconciled[df_discontinued,on=.(person_id), allow.cartesian = T] 
    # ignore pregnancies if date is 90+ days before discontinuation date or if folic acid date is after disconinuation date
    df_discontinued[discontinued_date-pregnancy_start_date>=90|discontinued_date<pregnancy_start_date, pregnancy_start_date:=NA]
    # Order data 
    df_discontinued <- df_discontinued[order(person_id,discontinued_date, pregnancy_start_date)]
    # Deduplicate records
    df_discontinued <- df_discontinued[!duplicated(df_discontinued[,c("person_id", "discontinued_date")])]
    
    #############################
    ######## ADR RECORDS ########
    #############################
    
    if(length(adr_all_files_events)>0 | length(adr_all_files_procedures)>0){
      # Loads Retinoid/Valproate records depending on discontinuation file type ie retinoid or valproate
      if(str_detect(discontinued_all_files[i], "Retinoid")){
        if(exists("adr_retinoids")){
          adr_record <- adr_retinoids
          # Merge with discontinued file
          df_discontinued <- adr_record[df_discontinued,on=.(person_id), allow.cartesian = T] 
          # ignore adrs if date is 90+ days before discontinuation date or if folic acid date is after disconinuation date
          df_discontinued[discontinued_date-adr_date>=90|discontinued_date<adr_date, adr_date:=NA]
          # Order data 
          df_discontinued <- df_discontinued[order(person_id,discontinued_date, adr_date)]
          # Deduplicate records
          df_discontinued <- df_discontinued[!duplicated(df_discontinued[,c("person_id", "discontinued_date")])]
        } else {
          print("There are no Retinoid ADR's")
        }
      }
      if(str_detect(discontinued_all_files[i], "Valproate")){
        if(exists("adr_valproates")){
          adr_record <- adr_valproates
          # Merge with discontinued file
          df_discontinued <- adr_record[df_discontinued,on=.(person_id), allow.cartesian = T] 
          # ignore adrs if date is 90+ days before discontinuation date or if folic acid date is after disconinuation date
          df_discontinued[discontinued_date-adr_date>=90|discontinued_date<adr_date, adr_date:=NA]
          # Order data 
          df_discontinued <- df_discontinued[order(person_id,discontinued_date, adr_date)]
          # Deduplicate records
          df_discontinued <- df_discontinued[!duplicated(df_discontinued[,c("person_id", "discontinued_date")])]
        } else {
          print(paste0("There are no Valproate ADR's for ", gsub("rds","",populations[pop])))
        }
      }
    }
    
    # Create all reason columns regardless of whether they will be filled or not
    if("folic_acid_date" %!in% colnames(df_discontinued)){df_discontinued[,folic_acid_date:=NA]}
    if("pregnancy_start_date" %!in% colnames(df_discontinued)){df_discontinued[,pregnancy_start_date:=NA]}
    if("adr_date" %!in% colnames(df_discontinued)){df_discontinued[,adr_date:=NA]}
    
    ### Creates column discontinuation reason based on columns
    # Counts the number of  columns with missing values
    df_discontinued <- df_discontinued[,num_obs:=rowSums(!is.na(df_discontinued[,c("folic_acid_date","pregnancy_start_date","adr_date")]))][]
    df_discontinued[num_obs==0, reason:= "unknown"]
    df_discontinued[num_obs==1 & !is.na(adr_date), reason:= "adr"]
    df_discontinued[num_obs==1 & !is.na(pregnancy_start_date), reason:= "pregnancy start"]
    df_discontinued[num_obs==1 & !is.na(folic_acid_date), reason:= "pregnancy wish"]
    df_discontinued[num_obs>1, reason:= "multiple"] 
    
    # Load discontinued counts files to use as denominator
    if(str_detect(discontinued_all_files[i], "Retinoid")){
      discontinued_rets <- discontinued_counts_files[grepl("Retinoid", discontinued_counts_files)]
      discontinued_rets <- discontinued_rets[!grepl("_reason-", discontinued_rets)]
      discontinued_all_counts <- readRDS(discontinued_rets)
    }
    if(str_detect(discontinued_all_files[i], "Valproate")){
      discontinued_valps <- discontinued_counts_files[grepl("Valproate", discontinued_counts_files)]
      discontinued_valps <- discontinued_rets[!grepl("_reason-", discontinued_valps)]
      discontinued_all_counts <- readRDS(discontinued_valps)
    }
    
    ### PERFORMS STRATIFIED COUNTS ### 
    # Performs incidence counts - stratified by reason
    discontinued_by_reason <- df_discontinued[,.N, by = .(year(discontinued_date),month(discontinued_date), reason)]
    # Get unique values of age groups - for the for loop
    reason_unique <- unique(discontinued_by_reason$reason)
    
    for(group in 1:length(reason_unique)){
      # Create a subset of age group
      each_group <- discontinued_by_reason[reason==reason_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group[is.na(N), N:=0][is.na(reason), reason:=reason_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
      # Prepare denominator (all discontinued counts )
      discontinued_all_counts_min <- discontinued_all_counts[,c("YM", "N")]
      setnames(discontinued_all_counts_min, "N", "Freq")
      # Create counts file
      discontinued_reason_counts <- merge(x = each_group, y = discontinued_all_counts_min, by = c("YM"), all.x = TRUE)
      discontinued_reason_counts <- discontinued_reason_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      discontinued_reason_counts <- discontinued_reason_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(discontinued_reason_counts, (paste0(medicines_counts_dir,"/", gsub("_discontinued_counts.rds", "",discontinued_all_files[i]), "_reason-", reason_unique[group],"_discontinued_counts.rds")))
    }
  }
}


for (file in list.files(path=medicines_counts_dir, pattern="reason", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_reasons, "/",file))}


# Clean up 
rm(list = grep("^adr_|^discontinued_|df_discontinued|df_folic_acid|each_group", ls(), value = TRUE))








