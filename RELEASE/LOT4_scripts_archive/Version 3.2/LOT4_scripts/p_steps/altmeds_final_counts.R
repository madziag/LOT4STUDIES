#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/12/2021
# 4.1 Incidence rates of alternative medication prescription/dispensings in ever-valproate users per month

## INDICATIONS
# 1. Indication records for valproates only
all_temps <- list.files(tmp, pattern="diagnoses|procedures|procedures|procedures_dxcodes")
# 3. Indication records for valproates only
if(length(all_temps)>0){
  # Creates a list of indications 
  # Get a list of indication files (with added new column to indicate indication type)
  indications_list <- list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine", full.names = T)
  if(pop_prefix == "PC"){indications_list <- indications_list[!grepl("PC_HOSP", indications_list)]}
  if(pop_prefix == "PC_HOSP"){indications_list <- indications_list[grepl("PC_HOSP", indications_list)]}
  # Bind all indication records
  all_indications<- do.call(rbind,lapply(indications_list, readRDS))
  all_indications<-all_indications[,c("person_id", "Date", "Code", "indication")]
  setnames(all_indications, "Date", "indication_date")
  setnames(all_indications, "Code", "indication_Code")
  all_indications<-all_indications[!duplicated(all_indications),]
  # indications <- list.files(diagnoses_pop, pattern = "ind_bipolar|ind_epilepsy|ind_migraine")
  # 
  # if(pop_prefix == "PC"){indications <- indications[!grepl("PC_HOSP",indications)]}
  # if(pop_prefix == "PC_HOSP"){indications <- indications[!grepl("PC",indications)]}
  # 
  # 
  # for(ind in 1:length(indications)){
  #   indication_file<-list.files(diagnoses_pop,pattern =indications[ind],ignore.case=T,full.names=T)
  #   indication_file<-indication_file[grepl(pop_prefix,indication_file)]
  #   if(populations[pop]=="PC_study_population.rds"){indication_file<-indication_file[!grepl("PC_HOSP",indication_file)]}
  #   if(length(indication_file)>0){
  #     name_of_ind <-gsub(".rds","", indications[ind])
  #     name_of_ind<- gsub(paste0(pop_prefix, "_ind_"),"", name_of_ind)
  #     df<-readRDS(indication_file)[,indication:=name_of_ind]
  #     saveRDS(df, indication_file)
  #   }
  # }
  # # Get a list of indication files (with added new column to indicate indication type)
  # indications_list <- list.files(diagnoses_pop, pattern = "ind_bipolar|ind_epilepsy|ind_migraine", full.names = T)
  # # Bind all indication records
  # all_indications<- do.call(rbind,lapply(indications_list, readRDS))
  # all_indications<-all_indications[,c("person_id", "Date", "Code", "indication")]
  # all_indications<-all_indications[!duplicated(all_indications),]
  
}

### RETINOIDS ###
altmeds_retinoids <- list.files(medications_pop, pattern="altmed_retin", ignore.case = T, recursive = T)
if(pop_prefix == "PC"){altmeds_retinoids <- altmeds_retinoids[!grepl("PC_HOSP",altmeds_retinoids)]}
if(pop_prefix == "PC_HOSP"){altmeds_retinoids <- altmeds_retinoids[!grepl("PC",altmeds_retinoids)]}

if(length(altmeds_retinoids)>0){
  for(med in 1:length(altmeds_retinoids)){
    altmeds_retinoids_file <- list.files(medications_pop, pattern=altmeds_retinoids[med], ignore.case = T, recursive = T, full.names = T)
    altmeds_retinoids_file <- altmeds_retinoids_file[grepl(pop_prefix,altmeds_retinoids_file)]
    if(populations[pop]=="PC_study_population.rds"){altmeds_retinoids_file<-altmeds_retinoids_file[!grepl("PC_HOSP",altmeds_retinoids_file)]}
    if(length(altmeds_retinoids_file)>0){
      name_of_altmed<- gsub(paste0(".rds"),"", altmeds_retinoids[med])
      name_of_altmed<- gsub(paste0(pop_prefix, "_altmed_"),"", name_of_altmed)
      df<-readRDS(altmeds_retinoids_file)[,alt_med_type:=name_of_altmed]
      saveRDS(df, altmeds_retinoids_file)
    }
  }
  
  # Bind all indication records
  all_altmed_retinoids <- do.call(rbind,lapply(paste0(medications_pop,altmeds_retinoids), readRDS))
  alt_med_retinoids_df <- all_altmed_retinoids[,c("person_id", "Code", "Date","birth_date", "entry_date", "exit_date", "alt_med_type")]
  # Check that you only have records between the patients entry and exit into study dates
  alt_med_retinoids_df <- alt_med_retinoids_df[Date>=entry_date&Date<=exit_date,]
  alt_med_retinoids_df <-alt_med_retinoids_df [!duplicated(alt_med_retinoids_df),]
  # Creates a column with patients age on every day of in the treatment episode
  alt_med_retinoids_df[,age_at_dispensing:= floor((Date- birth_date)*10/365.25)/10]
  # Add column which groups each patient into an age group, for each day of their treatment
  alt_med_retinoids_df[age_at_dispensing >= 12 & age_at_dispensing < 21, age_group:= "12-20.99"]
  alt_med_retinoids_df[age_at_dispensing >= 21 & age_at_dispensing < 31, age_group:= "21-30.99"]
  alt_med_retinoids_df[age_at_dispensing >= 31 & age_at_dispensing < 41, age_group:= "31-40.99"]
  alt_med_retinoids_df[age_at_dispensing >= 41 & age_at_dispensing < 56, age_group:= "41-55.99"]
  
  # ## Add indications ###
  # # Merge data with study population to get date of birth
  # alt_med_retinoids_df <- all_indications[alt_med_retinoids_df ,on=.(person_id), allow.cartesian=T]
  # # Create columns for each of the indication-> if there is more than one indication per treatment episode, then we want them to be in 1 row
  # alt_med_retinoids_df[indication=="ind_bipolar", ind_bipolar_date:=Date]
  # alt_med_retinoids_df[indication=="ind_epilepsy", ind_epilepsy_date:=Date]
  # alt_med_retinoids_df[indication=="ind_migraine", ind_migraine_date:=Date]
  # setnames(alt_med_retinoids_df, "indication", "temp_indication")
  # alt_med_retinoids_df <- setDT(alt_med_retinoids_df)[, lapply(.SD, na.omit), by = c("person_id", "Date")]
  # 
  # alt_med_retinoids_df[,bipolar_diff:=Date-ind_bipolar_date][,epilepsy_diff:=Date-ind_epilepsy_date][,migraine_diff:=Date-ind_migraine_date]
  # alt_med_retinoids_df[bipolar_diff<0,bipolar_diff:=NA][epilepsy_diff<0,epilepsy_diff:=NA][migraine_diff<0,migraine_diff:=NA]
  # alt_med_retinoids_df <- alt_med_retinoids_df[,num_obs:=rowSums(!is.na(alt_med_retinoids_df[,c("epilepsy_diff", "bipolar_diff", "migraine_diff")]))][]
  # alt_med_retinoids_df[num_obs==0, indication:="unknown"]
  # alt_med_retinoids_df[num_obs==1, indication:=temp_indication][,temp_indication:=NULL]
  # alt_med_retinoids_df[num_obs>1, indication:="multiple"]
  # drop.cols <- grep("diff|ind_", colnames(alt_med_retinoids_df))
  # alt_med_retinoids_df[, (drop.cols) := NULL]
  
}


### VALPROATES ### 
altmeds_valproates <- list.files(medications_pop, pattern="altmed_valp", ignore.case = T, recursive = T)
if(pop_prefix == "PC"){altmeds_valproates <- altmeds_valproates[!grepl("PC_HOSP",altmeds_valproates)]}
if(pop_prefix == "PC_HOSP"){altmeds_valproates <- altmeds_valproates[!grepl("PC",altmeds_valproates)]}

if(length(altmeds_valproates)>0){
  for(med in 1:length(altmeds_valproates)){
    altmeds_valproates_file <- list.files(medications_pop, pattern=altmeds_valproates[med], ignore.case = T, recursive = T, full.names = T)
    altmeds_valproates_file <- altmeds_valproates_file[grepl(pop_prefix,altmeds_valproates_file)]
    if(populations[pop]=="PC_study_population.rds"){altmeds_valproates_file<-altmeds_valproates_file[!grepl("PC_HOSP",altmeds_valproates_file)]}
    if(length(altmeds_valproates_file)>0){
      name_of_altmed<- gsub(".rds","", altmeds_valproates[med])
      name_of_altmed<- gsub(paste0(pop_prefix, "_altmed_"),"", name_of_altmed)
      df<-readRDS(altmeds_valproates_file)[,alt_med_type:=name_of_altmed]
      saveRDS(df, altmeds_valproates_file)
    }
  }
  # Bind all indication records
  all_altmed_valproates <- do.call(rbind,lapply(paste0(medications_pop,altmeds_valproates), readRDS))
  alt_med_valproates_df <- all_altmed_valproates[,c("person_id", "Code", "Date","birth_date", "entry_date", "exit_date", "alt_med_type")]
  # Check that you only have records between the patients entry and exit into study dates
  alt_med_valproates_df <- alt_med_valproates_df[Date>=entry_date&Date<=exit_date,]
  alt_med_valproates_df <-alt_med_valproates_df [!duplicated(alt_med_valproates_df),]
  
  # Creates a column with patients age on every day of in the treatment episode
  alt_med_valproates_df[,age_at_dispensing:= floor((Date- birth_date)*10/365.25)/10]
  # Add column which groups each patient into an age group, for each day of their treatment
  alt_med_valproates_df[age_at_dispensing >= 12 & age_at_dispensing < 21, age_group:= "12-20.99"]
  alt_med_valproates_df[age_at_dispensing >= 21 & age_at_dispensing < 31, age_group:= "21-30.99"]
  alt_med_valproates_df[age_at_dispensing >= 31 & age_at_dispensing < 41, age_group:= "31-40.99"]
  alt_med_valproates_df[age_at_dispensing >= 41 & age_at_dispensing < 56, age_group:= "41-55.99"]
  
  if(length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
    ## Add indications ###
    # Merge data with study population to get date of birth
    alt_med_valproates_df <- all_indications[alt_med_valproates_df ,on=.(person_id), allow.cartesian=T]
    # Create columns for each of the indication-> if there is more than one indication per treatment episode, then we want them to be in 1 row
    alt_med_valproates_df[indication=="ind_bipolar", ind_bipolar_date:=Date]
    alt_med_valproates_df[indication=="ind_epilepsy", ind_epilepsy_date:=Date]
    alt_med_valproates_df[indication=="ind_migraine", ind_migraine_date:=Date]
    setnames(alt_med_valproates_df, "indication", "temp_indication")
    alt_med_valproates_df <- setDT(alt_med_valproates_df)[, lapply(.SD, na.omit), by = c("person_id", "Date")]
    
    alt_med_valproates_df[,bipolar_diff:=Date-ind_bipolar_date][,epilepsy_diff:=Date-ind_epilepsy_date][,migraine_diff:=Date-ind_migraine_date]
    alt_med_valproates_df[bipolar_diff<0,bipolar_diff:=NA][epilepsy_diff<0,epilepsy_diff:=NA][migraine_diff<0,migraine_diff:=NA]
    alt_med_valproates_df <- alt_med_valproates_df[,num_obs:=rowSums(!is.na(alt_med_valproates_df[,c("epilepsy_diff", "bipolar_diff", "migraine_diff")]))][]
    alt_med_valproates_df[num_obs==0, indication:="unknown"]
    alt_med_valproates_df[num_obs==1, indication:=temp_indication][,temp_indication:=NULL]
    alt_med_valproates_df[num_obs>1, indication:="multiple"]
    drop.cols <- grep("diff|ind_", colnames(alt_med_valproates_df))
    alt_med_valproates_df[, (drop.cols) := NULL]
    
  }
}

# 4. Denominator 
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

# ALL alt_med counts FINAL 
### STRATIFIED BY ALTMED TYPE ###
if(length(altmeds_retinoids)>0){
  # Performs medicine counts during contraceptive episode - stratified by age group
  alt_med_retinoids_by_type <- alt_med_retinoids_df[,.N, by = .(year(Date),month(Date),alt_med_type)]
  # Get unique values of age groups - for the for loop
  alt_med_type_unique <- unique(alt_med_retinoids_by_type$alt_med_type)
  
  for(group in 1:length(alt_med_type_unique)){
    each_alt_type_df <- alt_med_retinoids_df[alt_med_type==alt_med_type_unique[group]]
    # Create a subset of age group
    each_group <- alt_med_retinoids_by_type[alt_med_type==alt_med_type_unique[group]]
    # Adjust for PHARMO
    if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    each_group[is.na(N), N:=0][is.na(alt_med_type), alt_med_type:=alt_med_type_unique[group]]
    # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
    each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
    # Create YM variable 
    each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
    # Masks values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
    # Applies masking 
    if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
    # Prepare denominator (all prevalence counts )
    denominator1 <- denominator[,c("YM", "Freq")]
    # Create counts file
    alt_med_all_counts <- merge(x = each_group, y = denominator1, by = c("YM"), all.x = TRUE)
    alt_med_all_counts <- alt_med_all_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    alt_med_all_counts <- alt_med_all_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    # Saves files in medicine counts folder
    saveRDS(alt_med_all_counts, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_counts.rds")) # Saves monthly counts 
    saveRDS(each_group, paste0(counts_dfs_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_counts.rds")) # Saves monthly counts 
    
    
    ### STRATIFICATION BY AGE GROUP ####
    ################ MEDICINE COUNTS DURING CONTRACEPTIVE EPISODE STRATIFIED BY AGE GROUPS ###################
    # Performs medicine counts during contraceptive episode - stratified by age group
    altmeds_by_age <-  each_alt_type_df[,.N, by = .(year(Date),month(Date),age_group)]
    # Get unique values of age groups - for the for loop
    age_group_unique <- unique(altmeds_by_age$age_group)
    
    for(group_1 in 1:length(age_group_unique)){
      # Create a subset of age group
      each_group_1 <- altmeds_by_age[age_group==age_group_unique[group_1]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group_1 <- each_group_1[year < 2020,]} else {each_group_1 <- each_group_1[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group_1 <- as.data.table(merge(x = empty_df, y = each_group_1, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group_1[is.na(N), N:=0][is.na(age_group), age_group:=age_group_unique[group_1]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group_1[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group_1 <- within(each_group_1, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group_1[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group_1[masked==1,N:=5]} else {each_group_1[masked==1,N:=N]}
      # Prepare denominator (all prevalence counts )
      alt_med_counts_min <- alt_med_all_counts[,c("YM", "N")]
      setnames(alt_med_counts_min, "N", "Freq")
      # Create counts file
      altmed_age_counts <- merge(x = each_group_1, y = alt_med_counts_min, by = c("YM"), all.x = TRUE)
      altmed_age_counts <- altmed_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_age_counts <- altmed_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(altmed_age_counts, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_age_group_", age_group_unique[group_1], "_counts.rds")) # Saves monthly counts 
    }
    
    
    # ### STRATIFICATION BY INDICATION ####
    # altmeds_by_indication <- each_alt_type_df[,.N, by = .(year(Date),month(Date),indication)]
    # # Get unique values of age groups - for the for loop
    # indication_unique <- unique( altmeds_by_indication$indication)
    
    # for(group_1 in 1:length(indication_unique)){
    #   # Create a subset of age group
    #   each_group_1 <- altmeds_by_indication[indication==indication_unique[group_1]]
    #   # Adjust for PHARMO
    #   if(is_PHARMO){each_group_1 <- each_group_1[year < 2020,]} else {each_group_1 <- each_group_1[year < 2021,]}
    #   # Merge with empty df (for counts that do not have counts for all months and years of study)
    #   each_group_1 <- as.data.table(merge(x = empty_df, y = each_group_1, by = c("year", "month"), all.x = TRUE))
    #   # Fills in missing values with 0
    #   each_group_1[is.na(N), N:=0][is.na(indication), indication:=indication_unique[group_1]]
    #   # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
    #   each_group_1[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
    #   # Create YM variable 
    #   each_group_1 <- within(each_group_1, YM<- sprintf("%d-%02d", year, month))
    #   # Masks values less than 5
    #   # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    #   each_group_1[,masked:=ifelse(N<5 & N>0, 1, 0)]
    #   # Applies masking 
    #   if(mask==T){each_group_1[masked==1,N:=5]} else {each_group_1[masked==1,N:=N]}
    #   # Prepare denominator (all prevalence counts )
    #   alt_med_counts_min <- alt_med_all_counts[,c("YM", "N")]
    #   setnames(alt_med_counts_min, "N", "Freq")
    #   # Create counts file
    #   altmed_indication_counts <- merge(x = each_group_1, y = alt_med_counts_min, by = c("YM"), all.x = TRUE)
    #   altmed_indication_counts <- altmed_indication_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    #   altmed_indication_counts <- altmed_indication_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    #   # Saves files in medicine counts folder
    #   saveRDS(altmed_indication_counts, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_indication_", indication_unique[group_1], "_counts.rds")) # Saves monthly counts 
    # }
    # 
    ### STRATIFICATION BY ATC CODE ####
    altmeds_by_ATC <- each_alt_type_df[,.N, by = .(year(Date),month(Date),Code)]
    # Get unique values of age groups - for the for loop
    ATC_unique <- unique(altmeds_by_ATC$Code)
    
    for(group_1 in 1:length(ATC_unique)){
      # Create a subset of age group
      each_group_1 <- altmeds_by_ATC[Code==ATC_unique[group_1]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group_1 <- each_group_1[year < 2020,]} else {each_group_1 <- each_group_1[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group_1 <- as.data.table(merge(x = empty_df, y = each_group_1, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group_1[is.na(N), N:=0][is.na(Code), Code:=ATC_unique[group_1]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group_1[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group_1 <- within(each_group_1, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group_1[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group_1[masked==1,N:=5]} else {each_group_1[masked==1,N:=N]}
      # Prepare denominator (all prevalence counts )
      alt_med_counts_min <- alt_med_all_counts[,c("YM", "N")]
      setnames(alt_med_counts_min, "N", "Freq")
      # Create counts file
      altmed_ATC_counts <- merge(x = each_group_1, y = alt_med_counts_min, by = c("YM"), all.x = TRUE)
      altmed_ATC_counts <- altmed_ATC_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_ATC_counts <- altmed_ATC_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(altmed_ATC_counts, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_ATC_", ATC_unique[group_1], "_counts.rds")) # Saves monthly counts 
    }
    
  }
}





#### VALPROATES ##########
### STRATIFIED BY ALTMED TYPE ###
if(length(altmeds_valproates)>0){
  # Performs medicine counts during contraceptive episode - stratified by age group
  alt_med_valproates_by_type <- alt_med_valproates_df[,.N, by = .(year(Date),month(Date),alt_med_type)]
  # Get unique values of age groups - for the for loop
  alt_med_type_unique <- unique(alt_med_valproates_by_type$alt_med_type)
  
  for(group in 1:length(alt_med_type_unique)){
    each_alt_type_df <- alt_med_valproates_df[alt_med_type==alt_med_type_unique[group]]
    # Create a subset of age group
    each_group <- alt_med_valproates_by_type[alt_med_type==alt_med_type_unique[group]]
    # Adjust for PHARMO
    if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
    # Merge with empty df (for counts that do not have counts for all months and years of study)
    each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
    # Fills in missing values with 0
    each_group[is.na(N), N:=0][is.na(alt_med_type), alt_med_type:=alt_med_type_unique[group]]
    # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
    each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
    # Create YM variable 
    each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
    # Masks values less than 5
    # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
    each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
    # Applies masking 
    if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
    # Prepare denominator (all prevalence counts )
    denominator1 <- denominator[,c("YM", "Freq")]
    # Create counts file
    alt_med_all_counts <- merge(x = each_group, y = denominator1, by = c("YM"), all.x = TRUE)
    alt_med_all_counts <- alt_med_all_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
    alt_med_all_counts <- alt_med_all_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
    # Saves files in medicine counts folder
    saveRDS(alt_med_all_counts, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_counts.rds")) # Saves monthly counts 
    saveRDS(each_group, paste0(counts_dfs_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_counts.rds")) # Saves monthly counts 
    
    
    ### STRATIFICATION BY AGE GROUP ####
    ################ MEDICINE COUNTS DURING CONTRACEPTIVE EPISODE STRATIFIED BY AGE GROUPS ###################
    # Performs medicine counts during contraceptive episode - stratified by age group
    altmeds_by_age <-  each_alt_type_df[,.N, by = .(year(Date),month(Date),age_group)]
    # Get unique values of age groups - for the for loop
    age_group_unique <- unique(altmeds_by_age$age_group)
    
    for(group_1 in 1:length(age_group_unique)){
      # Create a subset of age group
      each_group_1 <- altmeds_by_age[age_group==age_group_unique[group_1]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group_1 <- each_group_1[year < 2020,]} else {each_group_1 <- each_group_1[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group_1 <- as.data.table(merge(x = empty_df, y = each_group_1, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group_1[is.na(N), N:=0][is.na(age_group), age_group:=age_group_unique[group_1]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group_1[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group_1 <- within(each_group_1, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group_1[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group_1[masked==1,N:=5]} else {each_group_1[masked==1,N:=N]}
      # Prepare denominator (all prevalence counts )
      alt_med_counts_min <- alt_med_all_counts[,c("YM", "N")]
      setnames(alt_med_counts_min, "N", "Freq")
      # Create counts file
      altmed_age_counts <- merge(x = each_group_1, y = alt_med_counts_min, by = c("YM"), all.x = TRUE)
      altmed_age_counts <- altmed_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_age_counts <- altmed_age_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(altmed_age_counts, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_age_group_", age_group_unique[group_1], "_counts.rds")) # Saves monthly counts 
    }
    
    
    ### STRATIFICATION BY INDICATION ####
    altmeds_by_indication <- each_alt_type_df[,.N, by = .(year(Date),month(Date),indication)]
    # Get unique values of age groups - for the for loop
    indication_unique <- unique( altmeds_by_indication$indication)
    
    for(group_1 in 1:length(indication_unique)){
      # Create a subset of age group
      each_group_1 <- altmeds_by_indication[indication==indication_unique[group_1]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group_1 <- each_group_1[year < 2020,]} else {each_group_1 <- each_group_1[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group_1 <- as.data.table(merge(x = empty_df, y = each_group_1, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group_1[is.na(N), N:=0][is.na(indication), indication:=indication_unique[group_1]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group_1[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group_1 <- within(each_group_1, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group_1[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group_1[masked==1,N:=5]} else {each_group_1[masked==1,N:=N]}
      # Prepare denominator (all prevalence counts )
      alt_med_counts_min <- alt_med_all_counts[,c("YM", "N")]
      setnames(alt_med_counts_min, "N", "Freq")
      # Create counts file
      altmed_indication_counts <- merge(x = each_group_1, y = alt_med_counts_min, by = c("YM"), all.x = TRUE)
      altmed_indication_counts <- altmed_indication_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_indication_counts <- altmed_indication_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(altmed_indication_counts, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_indication_", indication_unique[group_1], "_counts.rds")) # Saves monthly counts 
    }
    
    ### STRATIFICATION BY ATC CODE ####
    altmeds_by_ATC <- each_alt_type_df[,.N, by = .(year(Date),month(Date),Code)]
    # Get unique values of age groups - for the for loop
    ATC_unique <- unique(altmeds_by_ATC$Code)
    
    for(group_1 in 1:length(ATC_unique)){
      # Create a subset of age group
      each_group_1 <- altmeds_by_ATC[Code==ATC_unique[group_1]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group_1 <- each_group_1[year < 2020,]} else {each_group_1 <- each_group_1[year < 2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group_1 <- as.data.table(merge(x = empty_df, y = each_group_1, by = c("year", "month"), all.x = TRUE))
      # Fills in missing values with 0
      each_group_1[is.na(N), N:=0][is.na(Code), Code:=ATC_unique[group_1]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group_1[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group_1 <- within(each_group_1, YM<- sprintf("%d-%02d", year, month))
      # Masks values less than 5
      # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
      each_group_1[,masked:=ifelse(N<5 & N>0, 1, 0)]
      # Applies masking 
      if(mask==T){each_group_1[masked==1,N:=5]} else {each_group_1[masked==1,N:=N]}
      # Prepare denominator (all prevalence counts )
      alt_med_counts_min <- alt_med_all_counts[,c("YM", "N")]
      setnames(alt_med_counts_min, "N", "Freq")
      # Create counts file
      altmed_ATC_counts <- merge(x = each_group_1, y = alt_med_counts_min, by = c("YM"), all.x = TRUE)
      altmed_ATC_counts <- altmed_ATC_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      altmed_ATC_counts <- altmed_ATC_counts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      # Saves files in medicine counts folder
      saveRDS(altmed_ATC_counts, paste0(medicines_counts_dir, "/", pop_prefix, "_alt_med_", alt_med_type_unique[group],"_ATC_", ATC_unique[group_1], "_counts.rds")) # Saves monthly counts 
    }
  }
}
# Create stratified by ATC folder 
invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","ATC")), dir.create(paste0(medicines_stratified_dir,"/","ATC")), FALSE))
medicines_stratified_ATC <- paste0(medicines_stratified_dir ,"/","ATC")
# Move files 
for (file in list.files(path=medicines_counts_dir, pattern="age_group", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_age_groups, "/",file))}
for (file in list.files(path=medicines_counts_dir, pattern="indication", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_indication, "/",file))}
for (file in list.files(path=medicines_counts_dir, pattern="ATC", ignore.case = T)){file.move(paste0(medicines_counts_dir,"/", file),paste0(medicines_stratified_ATC, "/",file))}

