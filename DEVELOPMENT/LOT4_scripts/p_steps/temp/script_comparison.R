# 1. Pregnancy records
D3_pregnancy_reconciled<-as.data.table(get(load(paste0(preg_dir,"g_intermediate/D3_pregnancy_reconciled.RData"))))
# Data Cleaning Pregnancy file
D3_pregnancy_reconciled[,person_id:=as.character(person_id)][,pregnancy_start_date:=as.IDate(pregnancy_start_date,"%Y%m%d")][,pregnancy_end_date:=as.IDate(pregnancy_end_date,"%Y%m%d")]
# Remove duplicate pregnancies
D3_pregnancy_reconciled<-D3_pregnancy_reconciled[!duplicated(D3_pregnancy_reconciled),]

# 2.  Medicine records 
# Loads files with matching pattern 
med_files <- list.files(path=medications_pop, pattern="Valproate")
# Reads in records of population with indicated study type
med_df<-as.data.table(readRDS(paste0(medications_pop,"ALL_Valproate_MEDS.rds")))
med_df <- med_df[Date>=entry_date & Date<exit_date]
med_df <- med_df[ ,c("person_id", "Date", "Code")] # Keeps necessary columns
setnames(med_df, "Code", "ATC") # Renames column

# 3. Treatment episodes
# Reads in the treatment episodes file
tx_episodes<-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/ALL_Valproate_CMA_treatment_episodes.rds")))

# 4. Study population
study_population <-readRDS(paste0(populations_dir, "ALL_study_population.rds"))

##### MED USE DURING PREGNANCY
#### MARCH SCRIPT####
# Merge med file with pregnancy records 
med_df1<-as.data.table(D3_pregnancy_reconciled[med_df,on=.(person_id),allow.cartesian=T]) # Left join
# Delete records without pregnancy records
med_df1<-med_df1[!is.na(pregnancy_start_date),]
# Remove duplicates
med_df1 <- med_df1[!duplicated(med_df1[,c("person_id", "pregnancy_start_date", "pregnancy_end_date", "highest_quality")])]
# Creates column that indicates if medicine record date is between episode.start and episode.end dates
med_df1[,med_use_during_preg:= fifelse(Date>=pregnancy_start_date & Date<=pregnancy_end_date, 1, 0)] 
# Creates df of patients who have a pregnancy start date between tx episode.start and episode.end dates
print(paste0("Previous Counts (med_use): ", nrow(med_df1[med_use_during_preg == 1,])))

#### APRIL SCRIPT####
# Join pregnancy records with medicine records (we should have all the medicine records)
med_df2<-as.data.table(D3_pregnancy_reconciled[med_df,on =.(person_id),allow.cartesian = T])
# Remove medicine records that are NOT attached to a pregnancy
med_df2<-med_df2[!is.na(pregnancy_start_date),]
# Create subset where medication prescription/dispensing occurs during a pregnancy
med_df2<-med_df2[Date>=pregnancy_start_date&Date<=pregnancy_end_date,]
# Order the records by person_id, pregnancy_start date and Date (medication dispensed or prescribed date)
med_df2<-med_df2[order(person_id,pregnancy_start_date,Date)]
# Keep only the first instance of use of medication during pregnancy
med_df2<-med_df2[,head(.SD,1),by=c("person_id","pregnancy_start_date","pregnancy_end_date")]
# Print statement
print(paste0("Current Counts: (med_use) ", nrow(med_df2)))

##########################################################
##########################################################
##########################################################
# Pregnancy Starts 
# MARCH
# Merge tx episodes with pregnancy records 
preg_df1 <- D3_pregnancy_reconciled[tx_episodes, on = .(person_id), allow.cartesian = T] # Left join
# Merge records with study population to get entry and exit date 
# Merges with study population to get birth_date (study population has been loaded in the wrapper script)
preg_df1 <- merge(preg_df1, study_population[,c("person_id", "entry_date","exit_date")], by = "person_id")
# Remove pregnancies that started outside study period
preg_df1 <- preg_df1[pregnancy_start_date>=entry_date & pregnancy_start_date<exit_date,]
# Delete records without pregnancy records
preg_df1 <- preg_df1[!is.na(pregnancy_start_date),]
# # Remove duplicates
preg_df1 <- preg_df1[!duplicated(preg_df1[,c("person_id", "pregnancy_start_date", "highest_quality")])]
# Converts dates to be in the same format
preg_df1[,episode.start:=as.IDate(episode.start,"%Y%m%d")][,episode.end:=as.IDate(episode.end,"%Y%m%d")]
# Creates column that indicates if medicine record date is between episode.start and episode.end dates
preg_df1[,preg_in_episode:= fifelse(pregnancy_start_date>=episode.start & pregnancy_start_date<=episode.end, 1, 0)] 
# Creates df of patients who have a pregnancy start date between tx episode.start and episode.end dates
print(paste0("Previous Counts (preg_starts): ", nrow(preg_df1[preg_in_episode == 1,])))

### APRIL
# Merge tx episodes with pregnancy records
preg_df2<-D3_pregnancy_reconciled[tx_episodes,on=.(person_id),allow.cartesian=T]
# Merge records with study population to get entry and exit date
preg_df2<-merge(preg_df2,study_population[,c("person_id","entry_date","exit_date")],by="person_id")
# Keep records of pregnancies that started inside the study period (between entry and exit dates)
preg_df2<-preg_df2[pregnancy_start_date>=entry_date&pregnancy_start_date<exit_date,]
# Converts dates to be in the same format
preg_df2[,episode.start:=as.IDate(episode.start,"%Y%m%d")][,episode.end:=as.IDate(episode.end,"%Y%m%d")]
# Keep records of pregnancies that occurred between episode.start and episode.end dates 
preg_df2<-preg_df2[pregnancy_start_date>=episode.start&pregnancy_start_date<=episode.end,]
# Remove duplicates of pregnancies i.e. each pregnancy needs to be counted only once
preg_df2<-preg_df2[order(person_id,pregnancy_start_date,pregnancy_end_date,episode.start)]
preg_df2<-preg_df2[!duplicated(preg_df2[,c("person_id","pregnancy_start_date","pregnancy_end_date")])]
# Print
print(paste0("Current Counts: (med_use) ", nrow(preg_df2)))

