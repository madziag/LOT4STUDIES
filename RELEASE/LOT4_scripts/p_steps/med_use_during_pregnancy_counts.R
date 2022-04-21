#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 10/01/2022

##################################################################################################################################################
###################################################### OBJECTIVE: 3.2 ############################################################################
##################################################################################################################################################

## Objective 3.2: Proportion of Retinoid/Valproate records that occurred during a pregnancy
# Numerator -> Number of Retinoid/Valproate records that occurred during a pregnancy
# Denominator -> Number of eligible subjects that month (main denominator)
# Records needed -> 1. Pregnancy records (created by ARS Toscana script) 2.Retinoid/Valproate records 3. Total number of female subjects in cohort for at least 1 day in the month (denominator file)

##################################################################################################################################################
##################################################################################################################################################
##################################################################################################################################################
### Loads records needed
### Loads records needed
# 1. Pregnancy records
D3_pregnancy_reconciled<-as.data.table(get(load(paste0(preg_dir,"g_intermediate/D3_pregnancy_reconciled.RData"))))
# Data Cleaning Pregnancy file
D3_pregnancy_reconciled[,person_id:=as.character(person_id)][,pregnancy_start_date:=as.IDate(pregnancy_start_date,"%Y%m%d")][,pregnancy_end_date:=as.IDate(pregnancy_end_date,"%Y%m%d")]
# Remove duplicate pregnancies
D3_pregnancy_reconciled<-D3_pregnancy_reconciled[!duplicated(D3_pregnancy_reconciled),]

# 2. Retinoid/Valproate records 
# Loaded in run_counts_final_each_pop.R # med_files

# 3. Load denominator file 
source(paste0(pre_dir,"load_denominator.R"))

# Runs if pregnancy records exist
if (nrow(D3_pregnancy_reconciled)>0){
  # For each medication file 
  for (i in 1:length(med_files)){
    # Loads the medication record
    med_df<-as.data.table(readRDS(paste0(medications_pop,med_files[i])))
    # Remove records that fall outside patients entry and exit into study dates
    med_df<-med_df[Date>=entry_date&Date<exit_date]
    # Keep only necessary columns (Date = Date of prescribing/dispensing as indicated in the preliminary counts)
    med_df<-med_df[,c("person_id","Date","Code")] 
    # Rename columns 
    setnames(med_df,"Code","ATC") 
    # Join pregnancy records with medicine records (we should have all the medicine records)
    med_df_with_pregs<-as.data.table(D3_pregnancy_reconciled[med_df,on =.(person_id),allow.cartesian = T])
    # Remove medicine records that are NOT attached to a pregnancy
    med_df_with_pregs<-med_df_with_pregs[!is.na(pregnancy_start_date),]
    # Create subset where medication prescription/dispensing occurs during a pregnancy
    med_df_with_pregs<-med_df_with_pregs[Date>=pregnancy_start_date&Date<=pregnancy_end_date,]
    # Order the records by person_id, pregnancy_start date and Date (medication dispensed or prescribed date)
    med_df_with_pregs<-med_df_with_pregs[order(person_id,pregnancy_start_date,Date)]
    # Keep only the first instance of use of medication during pregnancy
    med_df_with_pregs<-med_df_with_pregs[,head(.SD,1),by=c("person_id", "pregnancy_start_date", "pregnancy_end_date")]
    # Perform counts (after checking that there are medicine records that occurred during a pregnancy)
    if (nrow(med_df_with_pregs)>0){
      # Monthly counts
      med_use<-med_df_with_pregs[,.N,by=.(year(Date),month(Date))] 
      # Merge with empty_df to give value for all months/years
      med_use<-as.data.table(merge(x=empty_df,y=med_use,by=c("year","month"),all.x = TRUE))
      # Fill in missing values with 0
      med_use[is.na(med_use[,N]),N:=0] 
      # Create column that detects if data is available a certain year or not: 
      # 3-> data is not available, 0 values because data does not exist; 
      # 16-> data is available, any 0 values are true
      med_use[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # WE NO LONGER MASK BEFORE STRATIFICATION  even if user parameter mask is set to T!
      med_use[,masked:=0]
      # Merge counts file with denominator file on year/month 
      med_use<-merge(x=med_use,y=denominator,by = c("year","month"),all.x=TRUE)
      # Calculate rates (medicine count/denominator count)
      med_use<-med_use[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
      # Rearrange columns 
      med_use_monthly<-med_use[,c("YM","N","Freq","rates","masked","true_value")]
      # Save Files
      saveRDS(med_df_with_pregs,paste0(objective3_dir,gsub("_MEDS.rds","", med_files[i]),"_med_use_during_pregnancy.rds"))
      saveRDS(med_use_monthly,paste0(preg_med_counts_dir,"/",gsub("_MEDS.rds","",med_files[i]),"_med_use_during_pregnancy_counts.rds"))
      # Yearly counts
      med_use_yearly<-med_use[,sum_N:=sum(N),by=.(year)][,sum_Freq:=sum(Freq),by=.(year)]
      # Drop original N and Freq columns 
      med_use_yearly<-med_use_yearly[,-c("N","Freq","rates","YM","month")]
      setnames(med_use_yearly,"sum_N","N")
      setnames(med_use_yearly,"sum_Freq","Freq")
      # Remove duplicates
      med_use_yearly<-med_use_yearly[!duplicated(med_use_yearly)]
      # recalculate rates 
      med_use_yearly<-med_use_yearly[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
      med_use_yearly<-med_use_yearly[,c("year","N","Freq","rates","masked","true_value")]
      # Save file
      saveRDS(med_use_yearly,paste0(preg_med_counts_dir,"/", gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]), "_med_use_during_pregnancy_PER-YEAR.rds"))
      # Pre-Post Intervention
      # Add intervention column
      med_use<-med_use[YM<"2018-08",intervention:="pre"][YM>="2018-08", intervention:="post"]
      med_use_pre_post<-med_use[,sum_N:=sum(N),by=.(intervention)][,sum_Freq:=sum(Freq),by=.(intervention)]
      # Drop original N and Freq columns 
      med_use_pre_post<-med_use_pre_post[,-c("YM","year","month","N","Freq","rates")]
      setnames(med_use_pre_post,"sum_N","N")
      setnames(med_use_pre_post,"sum_Freq","Freq")
      # Remove duplicates
      med_use_pre_post<-med_use_pre_post[!duplicated(med_use_pre_post)]
      # recalculate rates 
      med_use_pre_post<-med_use_pre_post[,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
      med_use_pre_post<-med_use_pre_post[,c("intervention","N", "Freq", "rates", "masked", "true_value")]
      # Save file
      saveRDS(med_use_pre_post,paste0(preg_med_counts_dir,"/", gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]), "_med_use_during_pregnancy_PRE-POST.rds"))
      ##### Stratification by highest quality #####
      # Performs monthly counts grouped by highest quality
      med_df_with_pregs_by_hq<-med_df_with_pregs[,.N,by =.(year(Date),month(Date), highest_quality)]
      # Get list of unique values of highest quality column
      hq_unique<-unique(med_df_with_pregs_by_hq$highest_quality)
      # Start for loop for each unique hq color
      for(group in 1:length(hq_unique)){
        # Create a subset of unique highest quality
        each_group<-med_df_with_pregs_by_hq[highest_quality==hq_unique[group]]
        # Adjust for PHARMO
        if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
        # Merge with empty df (for counts that do not have counts for all months and years of study)
        each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
        # Fills in missing values with 0
        each_group[is.na(N),N:=0][is.na(highest_quality), highest_quality:=hq_unique[group]]
        # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
        each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
        # Create YM variable 
        each_group<-within(each_group, YM<- sprintf("%d-%02d",year,month))
        # Prepare denominator (all treatment during pregnancies counts)
        med_use_min<-med_use[,c("YM", "N")]
        setnames(med_use_min, "N", "Freq")
        # Create counts file
        med_use_by_hq<-merge(x=each_group,y=med_use_min,by=c("YM"),all.x=TRUE)
        # Masking no longer applied
        med_use_by_hq[,masked:=0]
        # Calculates rates
        med_use_by_hq<-med_use_by_hq[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates), rates:=0]
        # Rearrange columns
        med_use_by_hq<-med_use_by_hq[,c("YM","N","Freq","rates","masked","true_value")]
        # Saves individual level files in g_intermediate and counts in g_output
        saveRDS(med_use_by_hq,paste0(preg_med_counts_dir,"/",gsub("_MEDS.rds","",med_files[i]),"_hq_",hq_unique[group], "_med_use_during_pregnancy_counts.rds"))
      }
      # Save records for DAPS to go through
      pregnancies<-med_df_with_pregs[,c("person_id","age_at_start_of_pregnancy","pregnancy_start_date","pregnancy_end_date","highest_quality","Date", "ATC")]
      setnames(pregnancies, "Date", "prescribing/dispensing_date")
      saveRDS(pregnancies,paste0(DAP_pregnancies_dir,"/",gsub("_CMA_treatment_episodes.rds","",tx_episodes_files[i]),"_med_use_during_pregnancy_counts.rds"))
    } else {
      print(paste0(gsub(".rds", "",med_files[i])," study: No medicine use during a pregnancy found!"))
    }
  }
}else{
  print("No pregnancy records have been found")
}










# 
# # Cleanup
# rm(list = grep("^med_use_during_|med_preg|med_df", ls(), value = TRUE))
# 
