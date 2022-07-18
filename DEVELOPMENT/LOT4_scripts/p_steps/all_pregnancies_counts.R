#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 11/02/2022

# Reads in pregnancy file created by pregnancy script
# 1. Pregnancy records 
D3_pregnancy_reconciled<-as.data.table(get(load(paste0(preg_dir, "g_intermediate/D3_pregnancy_reconciled.RData"))))

# 2. Loads denominator file
source(paste0(pre_dir,"load_denominator.R"))

# Performs counts of all pregnancies 
pregnancy_all_counts<-D3_pregnancy_reconciled[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date))]
pregnancy_all_counts<-as.data.table(merge(x = empty_df, y = pregnancy_all_counts , by = c("year", "month"), all.x = TRUE)) # Merges empty_df with pregnancy_all_counts 
pregnancy_all_counts[is.na(pregnancy_all_counts [,N]), N:=0] # Fills in missing values with 0
# Masking is not applied 
pregnancy_all_counts[,masked:=0]
pregnancy_all_counts<-within(pregnancy_all_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
pregnancy_all_counts<-pregnancy_all_counts[,c("YM", "N", "masked")]
saveRDS(pregnancy_all_counts, paste0(preg_med_counts_dir,"/", pop_prefix, "_all_pregnancies_counts.rds"))

# Stratified by highest quality
pregnancy_all_counts_stratified<-D3_pregnancy_reconciled[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date), highest_quality)]

hq_unique<-unique(pregnancy_all_counts_stratified$highest_quality)

for(i in 1:length(hq_unique)){
  counts_subset<-pregnancy_all_counts_stratified[highest_quality == hq_unique[i]]
  counts_subset<-as.data.table(merge(x = empty_df, y = counts_subset , by = c("year", "month"), all.x = TRUE)) # Merges empty_df with counts_subset 
  counts_subset[is.na(counts_subset [,N]),N:=0] # Fills in missing values with 0
  # Masking no longer applied
  counts_subset[,masked:=0]
  counts_subset<-within(counts_subset, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
  counts_subset<-counts_subset[,c("YM", "N", "masked")]
  saveRDS(counts_subset, paste0(preg_med_counts_dir,"/", pop_prefix,"_hq_", hq_unique[i], "_all_pregnancies_counts.rds"))
}

# Clean up 
rm(list = grep("^pregnancy_all_counts|counts_subset", ls(), value = TRUE))