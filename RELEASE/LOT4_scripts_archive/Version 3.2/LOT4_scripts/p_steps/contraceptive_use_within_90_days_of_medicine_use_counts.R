#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022

##################################################################################################################################################
###################################################### OBJECTIVE: 2.5 ############################################################################
##################################################################################################################################################

## Objective 2.5: Proportion of Retinoid/Valproate users with contraception record within 90 days before medication record
# Numerator -> Number of Retinoid/Valproate records with any contraception records (created in contraception_duration.R) within 90 days before medication record
# Denominator -> Total number of Retinoid/Valproate records that month (duplicates removed )
# Intermediate data set -> data set of dispensed/prescribed Retinoid/Valproates per month with column indicating whether there was a prior contraception record 
# THE ABOVE NEEDS TO BE CLARIFIED!!!!
# Records needed -> contraception records (created by contraception_duration.R) 2. Retinoid/Valproate records 

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
### Loads records needed 
# 1. Contraception records 
# Looks for Contraception files in tmp folder 
contra_files <- list.files(paste0(tmp, "all_contraception"), pattern = paste0(pop_prefix, "_all_contra"), recursive = T, ignore.case = T, full.names = T)
# Filters by current subpopulation 
contra_files <- contra_files[grepl(pop_prefix, contra_files)]

if(populations[pop] == "PC_study_population.rds"){
  contra_files <- list.files(paste0(tmp, "all_contraception"), pattern = paste0(pop_prefix, "_all_contra"), recursive = T, ignore.case = T, full.names = T)
  contra_files <- contra_files[!grepl("PC_HOSP", contra_files)]
}
# 2. Retinoid/Valproate records 
# Looks for Retinoid/Valproate records in medications folder - this is done in wrapper script run_counts_final_each_pop.R
# name of variable with list of medicines available -> med_files

### Creates empty df for expanding counts files (when not all month-year combinations have counts) - uses denominator file min and max year values 
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
# Clean up
rm(denominator)

# # 3. Indication records for valproates only
# if(length(list.files(diagnoses_pop, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
#   # Creates a list of indications 
#   indications <- c("ind_bipolar", "ind_epilepsy", "ind_migraine")
#   
#   for(ind in 1:length(indications)){
#     indication_file<-list.files(diagnoses_pop,pattern =indications[ind],ignore.case=T,full.names=T)
#     indication_file<-indication_file[grepl(pop_prefix,indication_file)]
#     if(populations[pop]=="PC_study_population.rds"){indication_file<-indication_file[!grepl("PC_HOSP",indication_file)]}
#     if(length(indication_file)>0){
#       df<-readRDS(indication_file)[,indication:=indications[ind]]
#       saveRDS(df, indication_file)
#     }
#   }
#   # Get a list of indication files (with added new column to indicate indication type)
#   indications_list <- list.files(diagnoses_pop, pattern = "ind_bipolar|ind_epilepsy|ind_migraine", full.names = T)
#   # Bind all indication records
#   all_indications<- do.call(rbind,lapply(indications_list, readRDS))
#   all_indications<-all_indications[,c("person_id", "Date", "Code", "indication")]
#   all_indications<-all_indications[!duplicated(all_indications),]
#   setnames(all_indications,"Date","indication_date")
# }
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
  all_indications<-all_indications[!duplicated(all_indications),]
  setnames(all_indications,"Date","indication_date")
}
# Checks first if there are any contraception records found
if(length(contra_files)>0) {
  # Loads files + clean up
  contra_df <- readRDS(contra_files) # Loads file
  contra_df <- contra_df[,-c("contraception_meaning", "assumed_duration")] # Keeps necessary columns 
  setnames(contra_df, "Code", "Contraception_code") # Renames column 
  contra_df[,month:=month(contraception_record_date)][,year:=year(contraception_record_date)]
  contra_df <- contra_df[!duplicated(contra_df[,c("person_id", "month", "year")]),]
  contra_df <- contra_df[, -c("month", "year")]
  ##### Loops over medication files to create counts per medication type 
  for(i in 1:length(med_files)){
    ## Loads the medication record
    med_df <- readRDS(paste0(medications_pop, med_files[i])) # Loads file
    med_df <- med_df[Date>=entry_date & Date<=exit_date]
    med_df <- med_df[ ,c("person_id", "Date", "Code")] # Keeps necessary columns
    setnames(med_df, "Code", "ATC") # Renames column 
    ### Creates denominator: Total number of Retinoid/Valproate records per month
    med_counts <- med_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
    med_counts <- as.data.table(merge(x = empty_df, y = med_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with med_counts
    med_counts[is.na(med_counts[,N]), N:=0] # Fills in missing values with 0
    setnames(med_counts, "N", "Freq") # Renames column
    # Masking
    med_counts$masked_den <- ifelse(med_counts$Freq < 5 & med_counts$Freq > 0, 1, 0) # Creates column that indicates if count value will be masked_den if mask = TRUE
    if(mask == T){med_counts[med_counts$masked_den == 1,]$Freq <- 5} else {med_counts[med_counts$masked_den == 1,]$Freq <- med_counts[med_counts$masked_den == 1,]$Freq} # Changes values less than 5 and more than 0 to 5
    ### Creates numerators
    # Merges contraception df with medication df 
    contra_med_df <- contra_df[med_df, on = .(person_id), allow.cartesian = T] # Left join
    # Creates contra_prior column
    contra_med_df[,contra_prior:=ifelse(Date - contraception_record_date >= 0 & Date - contraception_record_date <= contraceptives_window , 1, 0)] # Creates new column contra_prior
    contra_med_df$contra_prior[is.na(contra_med_df$contra_prior)] <- 0 # change NA vales (id's with no contraception records) to 0
    # Creates subset where contraception_prior == 1
    contra_prior_df <- contra_med_df[contra_med_df$contra_prior == 1] # Creates df of patients with contraceptive record dates within 90 days before medication use
    # Performs counts if df created is not empty 
    if (nrow(contra_prior_df)>0){
      # Counts
      contra_prior_counts <- contra_prior_df[,.N, by = .(year(Date),month(Date))] # Performs counts grouped by year, month of medicine prescription date
      contra_prior_counts <- as.data.table(merge(x = empty_df, y = contra_prior_counts, by = c("year", "month"), all.x = TRUE)) # Merges empty_df with contra_prior_counts
      contra_prior_counts[is.na(contra_prior_counts[,N]), N:=0] # Fills in missing values with 0
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      contra_prior_counts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking
      contra_prior_counts$masked_num <- ifelse(contra_prior_counts$N < 5 & contra_prior_counts$N > 0, 1, 0) # Creates column that indicates if count value will be masked_num if mask = TRUE
      if(mask == T){contra_prior_counts[contra_prior_counts$masked_num == 1,]$N <- 5} else {contra_prior_counts[contra_prior_counts$masked_num == 1,]$N <- contra_prior_counts[contra_prior_counts$masked_num == 1,]$N} # Changes values less than 5 and more than 0 to 5
      # Rate calculation
      contra_prior_counts <- within(contra_prior_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
      contra_prior_counts <- merge(x = contra_prior_counts, y = med_counts, by = c("year", "month"), all.x = TRUE) # Merge with med counts
      contra_prior_counts <- contra_prior_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
      contra_prior_counts <- contra_prior_counts[,c("YM", "N", "Freq", "rates", "masked_num", "true_value")]
      setnames(contra_prior_counts, "masked_num", "masked")
      ## Saves intermediate (to counts_df folder) and monthly count files (to contraceptives folder)
      saveRDS(contra_prior_df, paste0(counts_dfs_dir, gsub(".rds", "", med_files[i]), "_contraception_prior.rds")) # Saves Contraceptive before records
      saveRDS(contra_prior_counts, paste0(contraceptive_counts_dir, "/", gsub(".rds", "", med_files[i]), "_contraception_prior_counts.rds")) # Saves Contraceptive before counts
      
      ##### STRATIFICATION BY AGE GROUPS ###
      # Merge data with study population to get date of birth
      contra_prior_df_age_groups <- merge(contra_prior_df[,c("person_id", "contraception_record_date", "Date")], study_population[,c("person_id", "birth_date")], by = "person_id")
      # Creates a column with patients age on every day of in the treatment episode
      contra_prior_df_age_groups[,current_age:= floor((contraception_record_date - birth_date)*10/365.25)/10]
      # Add column which groups each patient into an age group, for each day of their treatment
      contra_prior_df_age_groups[current_age >= 12 & current_age < 21, age_group:= "12-20.99"]
      contra_prior_df_age_groups[current_age >= 21 & current_age < 31, age_group:= "21-30.99"]
      contra_prior_df_age_groups[current_age >= 31 & current_age < 41, age_group:= "31-40.99"]
      contra_prior_df_age_groups[current_age >= 41 & current_age < 56, age_group:= "41-55.99"]
      
      # Performs pgtests counts - stratified by age group
      contra_prior_by_age <- contra_prior_df_age_groups[,.N, by = .(year(Date),month(Date), age_group)]
      # Get unique values of age groups - for the for loop
      age_group_unique <- unique(contra_prior_by_age$age_group)
      
      for(group in 1:length(age_group_unique)){
        # Create a subset of age group
        each_group <- contra_prior_by_age[age_group==age_group_unique[group]]
        # Adjust for PHARMO
        if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
        # Merge with empty df (for counts that do not have counts for all months and years of study)
        each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
        # Fills in missing values with 0
        each_group[is.na(N), N:=0][is.na(age_group), age_group:=age_group_unique[group]]
        # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
        each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
        # Create YM variable 
        each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
        # Masks values less than 5
        # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
        each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
        # Applies masking 
        if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
        # Prepare denominator (all pgtests counts )
        contra_prior_all_counts_min <- contra_prior_counts[,c("YM", "N")]
        setnames(contra_prior_all_counts_min, "N", "Freq")
        # Create counts file
        contra_prior_age_counts <- merge(x = each_group, y = contra_prior_all_counts_min, by = c("YM"), all.x = TRUE)
        contra_prior_age_counts <- contra_prior_age_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
        contra_prior_age_counts <- contra_prior_age_counts[,c("YM", "N", "Freq", "rates", "masked","true_value")]
        # Saves files in medicine counts folder
        saveRDS(contra_prior_age_counts, paste0(contraceptive_counts_dir, "/", gsub(".rds", "", med_files[i]), "_age_group_", age_group_unique[group], "_contraception_prior_counts.rds")) # Monthly counts file
      }   

      ##### STRATIFICATION BY INDICATION ###
      
      # Checks if there are indication files and performs action only for DAPs with indication files 
      if(str_detect(med_files[i],"Valproate") & length(list.files(all_indications_dir, pattern = "ind_bipolar|ind_epilepsy|ind_migraine"))>0){
        # Merge data with study population to get date of birth
        contra_prior_df_indications <- all_indications[contra_prior_df,on=.(person_id), allow.cartesian = T]
        # contra_prior_df_indications <- contra_prior_df_indications[Date>indication_date,]
        contra_prior_df_indications[is.na(indication)|indication_date>Date, indication:=NA]
        contra_prior_df_indications_missing<- contra_prior_df_indications[is.na(indication),]
        contra_prior_df_indications_missing[,final_indication:="unknown"]
        contra_prior_df_indications_notmissing<- contra_prior_df_indications[!is.na(indication),]
        contra_prior_df_indications_notmissing[,indication_count:=length(unique(indication)), by = .(person_id, Date)]
        contra_prior_df_indications_notmissing[indication_count==1, final_indication:=indication][indication_count>1,final_indication:="multiple"]
        contra_prior_df_indications_notmissing[,indication_count:=NULL]
        contra_prior_df_indications<-rbind(contra_prior_df_indications_missing,contra_prior_df_indications_notmissing)
        # Performs pgtests counts - stratified by age group
        contra_prior_by_indication <- contra_prior_df_indications[,.N, by = .(year(Date),month(Date), final_indication)]
        # Get unique values of age groups - for the for loop
        indication_unique <- unique(contra_prior_by_indication$final_indication)
        
        for(group in 1:length(indication_unique)){
          # Create a subset of age group
          each_group <- contra_prior_by_indication[final_indication==indication_unique[group]]
          # Adjust for PHARMO
          if(is_PHARMO){each_group <- each_group[year < 2020,]} else {each_group <- each_group[year < 2021,]}
          # Merge with empty df (for counts that do not have counts for all months and years of study)
          each_group <- as.data.table(merge(x = empty_df, y = each_group, by = c("year", "month"), all.x = TRUE))
          # Fills in missing values with 0
          each_group[is.na(N), N:=0][is.na(final_indication), final_indication:=indication_unique[group]]
          # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
          each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
          # Create YM variable 
          each_group <- within(each_group, YM<- sprintf("%d-%02d", year, month))
          # Masks values less than 5
          # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
          each_group[,masked:=ifelse(N<5 & N>0, 1, 0)]
          # Applies masking 
          if(mask==T){each_group[masked==1,N:=5]} else {each_group[masked==1,N:=N]}
          # Prepare denominator (all pgtests counts )
          contra_prior_all_counts_min <- contra_prior_counts[,c("YM", "N")]
          setnames(contra_prior_all_counts_min, "N", "Freq")
          # Create counts file
          contra_prior_indication_counts <- merge(x = each_group, y = contra_prior_all_counts_min, by = c("YM"), all.x = TRUE)
          contra_prior_indication_counts <- contra_prior_indication_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
          contra_prior_indication_counts <- contra_prior_indication_counts[,c("YM", "N", "Freq", "rates", "masked","true_value")]
          # Saves files in medicine counts folder
          saveRDS(contra_prior_indication_counts, paste0(contraceptive_counts_dir, "/", gsub(".rds", "", med_files[i]), "_indication_", indication_unique[group], "_contraception_prior_counts.rds")) # Monthly counts file
        }   
      }
    } else {
      print(paste0("No contraceptive records were found within", contraceptives_window, "days before ", strsplit(gsub(".rds", "", med_files[i]), "_")[[1]][2], " use." ))
    }
    
  }
} else {
  print("There are no Contraception records available!")
}

# Create stratified folders and move files into stratified folders
if(nrow(contra_prior_df)>0){
  # # Move stratified records into stratified folders
  # # Create stratified folder
  # invisible(ifelse(!dir.exists(paste0(contraceptive_counts_dir,"/","stratified")), dir.create(paste0(contraceptive_counts_dir,"/","stratified")), FALSE))
  # contraceptives_stratified_dir <- paste0(contraceptive_counts_dir,"/","stratified")
  # # Create stratified by age groups folder
  # invisible(ifelse(!dir.exists(paste0(contraceptives_stratified_dir,"/","age_group")), dir.create(paste0(contraceptives_stratified_dir,"/","age_group")), FALSE))
  # contraceptives_stratified_age_groups <- paste0(contraceptives_stratified_dir ,"/","age_group")
  # # Create stratified by indication folder
  # invisible(ifelse(!dir.exists(paste0(contraceptives_stratified_dir,"/","indication")), dir.create(paste0(contraceptives_stratified_dir,"/","indication")), FALSE))
  # contraceptives_stratified_indication <- paste0(contraceptives_stratified_dir ,"/","indication")
  
  # Move files 
  for (file in list.files(path=contraceptive_counts_dir, pattern="age_group", ignore.case = T)){file.move(paste0(contraceptive_counts_dir,"/", file),paste0(contraceptives_stratified_age_groups, "/",file))}
  for (file in list.files(path=contraceptive_counts_dir, pattern="indication", ignore.case = T)){file.move(paste0(contraceptive_counts_dir,"/", file),paste0(contraceptives_stratified_indication, "/",file))}
}




