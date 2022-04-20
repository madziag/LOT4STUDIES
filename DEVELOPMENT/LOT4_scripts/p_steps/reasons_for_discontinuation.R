#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022


### Reasons for discontinuation: 
# For Retinoids: 1. Folic acid 2. Pregnancy Start 3. ADR_retinoids, 4. Multiple 5. Unknown
# For Valproates: 1. Folic acid 2. Pregnancy Start 3. ADR_valproates, 4. Multiple 5. Unknown

# The following files are needed: 
# 1. Discontinued RECORDS created in medicine_counts_incidence_prevalence_discontinuation.R)
discontinued_df_files <- list.files(objective1_dir, pattern="discontinued")
# Filter for subpopulation if any (should only do counts of current subpopulation)
if(pop_prefix == "PC"){discontinued_df_files <- discontinued_df_files[!grepl("PC_HOSP",discontinued_df_files)]}
if(pop_prefix == "PC_HOSP"){discontinued_df_files <- discontinued_df_files[grepl("PC_HOSP",discontinued_df_files)]}

# 2. Discontinued COUNTS (created in medicine_counts_incidence_prevalence_discontinuation.R)
discontinued_counts_files <- list.files(medicines_counts_dir, pattern = "discontinued_counts", ignore.case = T, full.names = T)
if(pop_prefix == "PC"){discontinued_counts_files <- discontinued_counts_files[!grepl("PC_HOSP",discontinued_counts_files)]}
if(pop_prefix == "PC_HOSP"){discontinued_counts_files <- discontinued_counts_files[grepl("PC_HOSP",discontinued_counts_files)]}

# 3. Folic Acid records (created in preliminary counts) # PREGWISH
folic_acid_files <- list.files(medications_pop, pattern = "folic_acid", full.names = T)
if(pop_prefix == "PC"){folic_acid_files <- folic_acid_files[!grepl("PC_HOSP",folic_acid_files)]}
if(pop_prefix == "PC_HOSP"){folic_acid_files <- folic_acid_files[grepl("PC_HOSP",folic_acid_files)]}

# 4. Pregnancy start df (from ARS toscana - D3_pregnancy_reconciled) # PREGSTART
D3_pregnancy_reconciled <- as.data.table(get(load(paste0(preg_dir, "g_intermediate/D3_pregnancy_reconciled.RData"))))
# Drop no longer needed columns 
D3_pregnancy_reconciled <- D3_pregnancy_reconciled[,c("person_id", "pregnancy_start_date")]
D3_pregnancy_reconciled[,pregnancy_start_date:=as.IDate(pregnancy_start_date)]

# 5. ADR records for retinoids/valproates (created in preliminary counts) # ADR's
if(length(list.files(adr_dir))>0){
  adr_files<-list.files(adr_dir)
  if(pop_prefix == "PC"){adr_files<-adr_files[!grepl("PC_HOSP",adr_files)]}
  if(pop_prefix == "PC_HOSP"){adr_files<-adr_files[grepl("PC_HOSP",adr_files)]}
  # Separates adr's depending on study
  if(study_type=="Retinoid") {adr_files<-adr_files[!grepl("nausea|tremor",adr_files)]}
  if(study_type=="Valproate"){adr_files<-adr_files[grepl("nausea|tremor",adr_files)]}
  # Creates list of adrs 
  all_adrs <- list()
  #Adds adr column to files 
  for(adr in 1:length(adr_files)){
    # Gets name of adr (to be added to column: adr)
    adr_name<-gsub(".rds", "", adr_files[adr])
    adr_name<-gsub(paste0(pop_prefix, "_adr_"), "", adr_name)
    # Reads in adr and adds column with adr name to df
    df <-readRDS(paste0(adr_dir, adr_files[adr]))[,adr:=adr_name]
    df <- df[,c("person_id", "Date", "Code", "adr")]
    setnames(df, "Date", "adr_date")
    all_adrs[[adr]] <- df
  }
  # Binds all adr df's
  all_adrs_df<-rbindlist(all_adrs)
}
# 6. Denominator - for empty counts df
source(paste0(pre_dir,"load_denominator.R"))

# Checks if discontinued file exists
if(length(discontinued_df_files)>0){
  
  for(i in 1:length(discontinued_df_files)){
    # Creates empty df to store all reasons for discontinuation (that meet criteria) 
    all_reasons_df <- data.table(person_id=character(), discontinued_date=as.IDate(character()), reason=character(), stringsAsFactors=FALSE)         
    # Loads discontinued file
    df_discontinued <- readRDS(paste0(objective1_dir, discontinued_df_files[i]))
    # Removes no longer needed columns 
    df_discontinued <- df_discontinued[,c("person_id", "episode.end")]
    # Renames columns
    setnames(df_discontinued, "episode.end", "discontinued_date")
    
    #######################################
    ######## PREG WISH: FOLIC ACID ########
    #######################################
    # Load folic acid records
    if(length(folic_acid_files)>0){
      # Loads files
      df_folic_acid <- readRDS(folic_acid_files)
      # Renames columns
      setnames(df_folic_acid, "Date", "folic_acid_date")
      # Removes no longer needed columns
      df_folic_acid <- df_folic_acid[,c("person_id", "folic_acid_date")]
      # Merge discontinued with folic acid records # LEFT JOIN
      df_discontinued_fa <- df_folic_acid[df_discontinued,on=.(person_id), allow.cartesian = T] 
      # ignore folic acid values if date is 90+ days before discontinuation date or if folic acid date is after disconinuation date
      df_discontinued_fa[,pregwish:=ifelse(discontinued_date-folic_acid_date<90 & discontinued_date-folic_acid_date>0, 1, 0)]
      df_discontinued_fa[is.na(folic_acid_date), pregwish:=0]
      # Get subset of records with pregwish
      df_pregwish<-df_discontinued_fa[pregwish==1,]
      df_pregwish[,reason:="pregwish"]
      df_pregwish<-df_pregwish[,c("person_id", "discontinued_date", "reason")]
      # Bind df to all_reasons_df
      all_reasons_df <- rbind(all_reasons_df, df_pregwish)
      # Clean up
      rm(df_discontinued_fa, df_pregwish)
    }
    
    ###################################
    ######## PREGNANCY RECORDS ########
    ###################################
    # Pregnancy records loaded outside the for loop
    # Merge study population with pregnancy records
    df_discontinued_preg <- D3_pregnancy_reconciled[df_discontinued,on=.(person_id), allow.cartesian = T] 
    # ignore pregnancy start date if date is 90+ days before discontinuation date or if pregnany start date is after disconinuation date
    df_discontinued_preg[,pregstart:=ifelse(discontinued_date-pregnancy_start_date<90 & discontinued_date-pregnancy_start_date>0, 1, 0)]
    df_discontinued_preg[is.na(pregnancy_start_date), pregstart:=0]
    # Get subset of records with pregstart
    df_pregstart<-df_discontinued_preg[pregstart==1,]
    df_pregstart[,reason:="pregstart"]
    df_pregstart<-df_pregstart[,c("person_id", "discontinued_date", "reason")]
    # Bind df to all_reasons_df
    all_reasons_df <- rbind(all_reasons_df, df_pregstart)
    # Clean up
    rm(df_discontinued_preg, df_pregstart)
    
    #############################
    ######## ADR RECORDS ########
    #############################
    if(nrow(all_adrs_df)>0){
      if(str_detect(discontinued_df_files[i], "Retinoid")){adr_record<-all_adrs_df[adr%!in%c("nausea","tremor"),]}
      if(str_detect(discontinued_df_files[i], "Valproate")){adr_record<-all_adrs_df[adr%in%c("nausea","tremor"),]}
      # Merge with discontinued file
      df_discontinued_adr <- adr_record[df_discontinued,on=.(person_id), allow.cartesian = T] 
      # ignore adrs if date is 90+ days before discontinuation date or if adr date is after disconinuation date
      df_discontinued_adr[,adr:=ifelse(discontinued_date-adr_date<90 & discontinued_date-adr_date>0, 1, 0)]
      df_discontinued_adr[is.na(adr_date), adr:=0]
      # Get subset of records with adr
      df_adr<-df_discontinued_adr[adr==1,]
      df_adr[,reason:="adr"]
      df_adr<-df_adr[,c("person_id", "discontinued_date", "reason")]
      # Bind df to all_reasons_df
      all_reasons_df <- rbind(all_reasons_df, df_adr)
      # Clean up
      rm(df_discontinued_adr, df_adr)
    }
    
    # Merges all_reasons_df with original discontinued df (left join so that all discontinued records are included)
    df_discontinued<-all_reasons_df[df_discontinued,on=.(person_id, discontinued_date), allow.cartesian = T] 
    # If reason column is NA, then reason  unknown
    df_discontinued[is.na(reason), reason:='unknown']
    # Count the number of unique values of reason per person id and discontinuation date 
    df_discontinued[!is.na(reason),count:=.N,by=c("person_id", "discontinued_date")]
    # If count is multiple, the final reason = multiple
    # if count is 1, then final reason= reason
    df_discontinued[count==1, final_reason:=reason][count>1,final_reason:="multiple"]
    # Drop original reason column and rename final_reason back to reason
    df_discontinued[,reason:=NULL][,count:=NULL]
    setnames(df_discontinued, "final_reason", "reasons")
    # Remove duplicates
    df_discontinued<-df_discontinued[!duplicated(df_discontinued)]
    
    # Load discontinued counts files to use as denominator
    if(str_detect(discontinued_df_files[i], "Retinoid")){
      discontinued_rets <- discontinued_counts_files[grepl("Retinoid", discontinued_counts_files)]
      discontinued_rets <- discontinued_rets[!grepl("_reason-", discontinued_rets)]
      discontinued_all_counts <- readRDS(discontinued_rets)
    }
    if(str_detect(discontinued_df_files[i], "Valproate")){
      discontinued_valps <- discontinued_counts_files[grepl("Valproate", discontinued_counts_files)]
      discontinued_valps <- discontinued_valps[!grepl("_reason-", discontinued_valps)]
      discontinued_all_counts <- readRDS(discontinued_valps)
    }
    
    ################ STRATIFIED DISCONTINUED BY REASON FOR DISCONTINUATION ###################
    discontinued_by_reason <- df_discontinued[,.N, by = .(year(discontinued_date),month(discontinued_date), reasons)]
    # Get unique values of age groups - for the for loop
    reason_unique <- unique(discontinued_by_reason$reasons)
    
    for(group in 1:length(reason_unique)){
      # Create a subset of age group
      each_group <- discontinued_by_reason[reasons==reason_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group[is.na(N), N:=0][is.na(reasons), reasons:=reason_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
      # Prepare denominator (all discontinued counts )
      discontinued_all_counts_min <- discontinued_all_counts[,c("YM", "N")]
      setnames(discontinued_all_counts_min, "N", "Freq")
      # Create counts file
      discontinued_reason_counts <- merge(x = each_group, y = discontinued_all_counts_min, by = c("YM"), all.x = TRUE)
      # Masking no longer applied
      discontinued_reason_counts[,masked:=0]
      # discontinued_reason_counts[,masked:=ifelse(N<5&N>0,1,0)]
      # # Applies masking 
      # if(mask==T){discontinued_reason_counts[N>0&N<5,N:=5][Freq>0&Freq<5,Freq:=5]}
      # Calculates rates
      discontinued_reason_counts <- discontinued_reason_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      discontinued_reason_counts <- discontinued_reason_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(discontinued_reason_counts, (paste0(medicines_counts_dir,"/", gsub("_discontinued_counts_df.rds", "",discontinued_df_files[i]), "_reason_", reason_unique[group],"_discontinued_counts.rds")))
    }
  }
  for (file in list.files(path=medicines_counts_dir, pattern="reason", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_reasons, "/",file))}
}

# Clean up 
rm(list = grep("^discontinued_|df_discontinued|df_folic_acid|each_group", ls(), value = TRUE))








