#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 07/02/2022

##################################################################################################################################################
###################################################### OBJECTIVE: 3.1 ############################################################################
##################################################################################################################################################

## Objective 3.1: Rates of pregnancies starting during a Retinoid/Valproate treatment episode
# Numerator -> Number of pregnancies that started during a Retinoid/Valproate episode
# Denominator -> Number of prevalent (current) users that month 
# Records needed -> 1. Pregnancy records (created by ARS Toscana script) 2. Treatment episodes 3. Prevalent user counts (saved in medicine_counts folder)
# *** Each pregnancy that starts during a treatment episode is counted 
##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################

### Loads records needed
# 1. Pregnancy records
D3_pregnancy_reconciled<-as.data.table(get(load(paste0(preg_dir,"g_intermediate/D3_pregnancy_reconciled.RData"))))
# Data Cleaning Pregnancy file
D3_pregnancy_reconciled[,person_id:=as.character(person_id)][,pregnancy_start_date:=as.IDate(pregnancy_start_date,"%Y%m%d")][,pregnancy_end_date:=as.IDate(pregnancy_end_date,"%Y%m%d")]
# Remove duplicate pregnancies
D3_pregnancy_reconciled<-D3_pregnancy_reconciled[!duplicated(D3_pregnancy_reconciled),]

# 2. Treatment episode files
# Looks for treatment_episode files in treatment_episodes folder (actual files will be loaded in the for loop)
tx_episodes_files<-list.files(paste0(g_intermediate,"treatment_episodes/"),pattern="Retinoid_CMA|Valproate_CMA",ignore.case = T)
if(pop_prefix=="PC"){tx_episodes_files<-tx_episodes_files[!grepl("PC_HOSP",tx_episodes_files)]}
if(pop_prefix=="PC_HOSP"){tx_episodes_files<-tx_episodes_files[grepl("PC_HOSP",tx_episodes_files)]}

# 3. Prevalent user counts
prevalent_counts_files<-list.files(medicines_counts_dir,pattern="prevalence_counts",ignore.case=T,full.names=T)
if(pop_prefix=="PC"){prevalent_counts_files<-prevalent_counts_files[!grepl("PC_HOSP",prevalent_counts_files)]}
if(pop_prefix=="PC_HOSP"){prevalent_counts_files<-prevalent_counts_files[grepl("PC_HOSP",prevalent_counts_files)]}

# 4. Denominator file
source(paste0(pre_dir,"load_denominator.R"))

# Create folders to store individual level records
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"objective_3")),dir.create(paste0(counts_dfs_dir,"objective_3")),FALSE))
objective3_dir<-paste0(counts_dfs_dir,"objective_3/")
invisible(ifelse(!dir.exists(paste0(g_intermediate,"DAP_pregnancies")),dir.create(paste0(g_intermediate,"DAP_pregnancies")),FALSE))
DAP_pregnancies_dir<-paste0(g_intermediate,"DAP_pregnancies")

if (nrow(D3_pregnancy_reconciled)>0){
  # For each treatment episode file
  for (i in 1:length(tx_episodes_files)){
    # Reads in the treatment episodes file
    tx_episodes<-as.data.table(readRDS(paste0(g_intermediate,"treatment_episodes/",tx_episodes_files[i])))
    # Merge tx episodes with pregnancy records
    tx_episodes_preg<-D3_pregnancy_reconciled[tx_episodes,on=.(person_id),allow.cartesian=T]
    # Merge records with study population to get entry and exit date
    tx_episodes_preg<-merge(tx_episodes_preg,study_population[,c("person_id","entry_date","exit_date")],by="person_id")
    # Keep records of pregnancies that started inside the study period (between entry and exit dates)
    tx_episodes_preg<-tx_episodes_preg[pregnancy_start_date>=entry_date&pregnancy_start_date<exit_date,]
    # Converts dates to be in the same format
    tx_episodes_preg[,episode.start:=as.IDate(episode.start,"%Y%m%d")][,episode.end:=as.IDate(episode.end,"%Y%m%d")]
    # Keep records of pregnancies that occurred between episode.start and episode.end dates 
    tx_episodes_preg<-tx_episodes_preg[pregnancy_start_date>=episode.start&pregnancy_start_date<=episode.end,]
    # Remove duplicates of pregnancies i.e. each pregnancy needs to be counted only once
    tx_episodes_preg<-tx_episodes_preg[order(person_id,pregnancy_start_date,pregnancy_end_date, episode.start)]
    tx_episodes_preg<-tx_episodes_preg[!duplicated(tx_episodes_preg[,c("person_id","pregnancy_start_date","pregnancy_end_date")])]
    
    # Checks if there are any records that meet the criteria. If so it does the calculations
    if (nrow(tx_episodes_preg)>0){
      # Monthly counts
      preg_starts<-tx_episodes_preg[,.N,by=.(year(pregnancy_start_date),month(pregnancy_start_date))] 
      # Merges with empty_df to expand counts -> to have all month/year combos regardless of counts present or not
      preg_starts<-as.data.table(merge(x=empty_df,y=preg_starts,by=c("year","month"),all.x=TRUE)) 
      # Fill in missing values with 0
      preg_starts[is.na(preg_starts[,N]),N:=0] 
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      preg_starts[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Masking is not applied before stratification
      preg_starts[,masked:=0]
      # Creates YM column 
      preg_starts<-within(preg_starts,YM<-sprintf("%d-%02d",year,month)) 
      # Load prevalence counts
      prevalent_counts<-readRDS(prevalent_counts_files[grepl(gsub("_CMA_treatment_episodes.rds", "",tx_episodes_files[i]), prevalent_counts_files)])
      prevalent_counts<-prevalent_counts[,-c("Freq", "rates", "masked", "true_value")]
      setnames(prevalent_counts,"N","Freq")
      # Merge numerator with denominator 
      preg_starts<-merge(x=preg_starts,y=prevalent_counts,by=c("YM"),all.x=TRUE) # Merge with med counts
      preg_starts<-preg_starts[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
      # Save monthly counts
      preg_starts_monthly<-preg_starts[,c("YM", "N", "Freq", "rates", "masked", "true_value")]
      saveRDS(preg_starts_monthly,paste0(preg_med_counts_dir,"/", gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]), "_all_preg_starts_during_tx_episodes_counts.rds"))
      # Save df of all pregnancies found
      saveRDS(tx_episodes_preg,paste0(objective3_dir,gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]),"_all_preg_starts_during_tx_episodes.rds"))
      # Yearly counts
      preg_starts_yearly<-preg_starts[,sum_N:=sum(N),by=.(year)][,sum_Freq:=sum(Freq),by=.(year)]
      # Drop original N and Freq columns 
      preg_starts_yearly<-preg_starts_yearly[,-c("N","Freq","rates","YM","month")]
      setnames(preg_starts_yearly,"sum_N","N")
      setnames(preg_starts_yearly,"sum_Freq","Freq")
      # Remove duplicates
      preg_starts_yearly<-preg_starts_yearly[!duplicated(preg_starts_yearly)]
      # recalculate rates 
      preg_starts_yearly<-preg_starts_yearly[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
      preg_starts_yearly<-preg_starts_yearly[,c("year","N","Freq","rates","masked","true_value")]
      # Save file
      saveRDS(preg_starts_yearly,paste0(preg_med_counts_dir,"/", gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]), "_preg_starts_during_tx_episodes_PER-YEAR.rds"))
      # Pre-Post Intervention
      # Add intervention column
      preg_starts<-preg_starts[YM<"2018-08",intervention:="pre"][YM>="2018-08", intervention:="post"]
      preg_starts_pre_post<-preg_starts[,sum_N:=sum(N),by=.(intervention)][,sum_Freq:=sum(Freq),by=.(intervention)]
      # Drop original N and Freq columns 
      preg_starts_pre_post<-preg_starts_pre_post[,-c("YM","year","month","N","Freq","rates")]
      setnames(preg_starts_pre_post,"sum_N","N")
      setnames(preg_starts_pre_post,"sum_Freq","Freq")
      # Remove duplicates
      preg_starts_pre_post<-preg_starts_pre_post[!duplicated(preg_starts_pre_post)]
      # recalculate rates 
      preg_starts_pre_post<-preg_starts_pre_post[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
      preg_starts_pre_post<-preg_starts_pre_post[,c("intervention","N", "Freq", "rates", "masked", "true_value")]
      # Save file
      saveRDS(preg_starts_pre_post,paste0(preg_med_counts_dir,"/", gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]), "_preg_starts_during_tx_episodes_PRE-POST.rds"))
      
      # Stratification based on highest quality 
      # Perform counts by age group
      preg_starts_by_hq<-tx_episodes_preg[,.N,by=.(year(pregnancy_start_date),month(pregnancy_start_date),highest_quality)]
      # Get unique values of age groups - for the for loop
      hq_unique <- unique(preg_starts_by_hq$highest_quality)
      
      for(group in 1:length(hq_unique)){
        # Create a subset of highest quality counts
        each_group<-preg_starts_by_hq[highest_quality==hq_unique[group]]
        # Adjust for PHARMO
        if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
        # Merge with empty df (for counts that do not have counts for all months and years of study)
        each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
        # Fills in missing values with 0
        each_group[is.na(N), N:=0][is.na(highest_quality),highest_quality:=hq_unique[group]]
        # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
        each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
        # Create YM variable 
        each_group<-within(each_group,YM<-sprintf("%d-%02d",year, month))
        # Prepare denominator (all prevalence counts )
        preg_starts_min<-preg_starts[,c("YM", "N")]
        setnames(preg_starts_min,"N","Freq")
        # Create counts file
        preg_starts_by_hq_counts<-merge(x=each_group,y=preg_starts_min,by=c("YM"),all.x = TRUE)
        # Masking no longer applied 
        preg_starts_by_hq_counts[,masked:=0]
        # Calculates rates
        preg_starts_by_hq_counts<-preg_starts_by_hq_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
        # Keeps necessary columns
        preg_starts_by_hq_counts<-preg_starts_by_hq_counts[,c("YM","N","Freq","rates","masked","true_value")]
        # Saves files in medicine counts folder
        saveRDS(preg_starts_by_hq_counts,paste0(preg_med_counts_dir,"/", gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]), "_hq_", hq_unique[group],"_preg_starts_during_tx_episodes_counts.rds"))
      }
      # Save records for DAPS to go through
      pregnancies<-tx_episodes_preg[,c("person_id","age_at_start_of_pregnancy","pregnancy_start_date","pregnancy_end_date","highest_quality","episode.start", "ATC")]
      saveRDS(pregnancies,paste0(DAP_pregnancies_dir,"/",gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]),"_preg_starts_during_tx_episodes.rds"))
      
    } else {
      print(paste0(gsub("_CMA_treatment_episodes.rds", "",tx_episodes_files[i]), " study: There are no patients with pregnancy start dates that fall between episode start and end dates!"))
    }
  }
} else {
  print("No pregancy records have been found")
}

# rm(list = grep("^tx_episode", ls(), value = TRUE))
