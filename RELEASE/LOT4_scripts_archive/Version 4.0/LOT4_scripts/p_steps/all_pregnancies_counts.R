#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 11/02/2022

# Reads in pregnancy file created by pregnancy script
# 1. Pregnancy records 
D3_pregnancy_reconciled <- as.data.table(get(load(paste0(preg_dir, "g_intermediate/D3_pregnancy_reconciled.RData"))))

### Creates empty df for expanding counts files (when not all month-year combinations have counts) - uses denominator file min and max year values 
# Looks for denominator file in output directory 
denominator_file <- list.files(tmp, pattern = paste0(pop_prefix,"_denominator.rds"))
# Loads denominator file 
denominator <- readRDS(paste0(tmp, denominator_file))
# Split Y-M variable to year - month columns (for merging later)
denominator[, c("year", "month") := tstrsplit(YM, "-", fixed=TRUE)]
denominator[,year:=as.integer(year)][,month:=as.integer(month)]
### Creates empty df for expanding counts files (when not all month-year combinations have counts)
empty_df<-as.data.table(expand.grid(seq(min(denominator$year), max(denominator$year)), seq(1, 12)))
names(empty_df) <- c("year", "month")
# Clean up
rm(denominator)

# Performs counts of all pregnancies 
pregnancy_all_counts <- D3_pregnancy_reconciled[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date))]
pregnancy_all_counts <- as.data.table(merge(x = empty_df, y = pregnancy_all_counts , by = c("year", "month"), all.x = TRUE)) # Merges empty_df with pregnancy_all_counts 
pregnancy_all_counts[is.na(pregnancy_all_counts [,N]), N:=0] # Fills in missing values with 0
pregnancy_all_counts$masked <- ifelse(pregnancy_all_counts $N < 5 & pregnancy_all_counts $N > 0, 1, 0) # Creates column that indicates if count value will be masked if mask = TRUE
if(mask == T){pregnancy_all_counts[pregnancy_all_counts$masked == 1,]$N <- 5} else {pregnancy_all_counts[pregnancy_all_counts$masked == 1,]$N <- pregnancy_all_counts[pregnancy_all_counts$masked == 1,]$N} # Changes values less than 5 and more than 0 to 5
pregnancy_all_counts <- within(pregnancy_all_counts, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
pregnancy_all_counts <- pregnancy_all_counts[,c("YM", "N", "masked")]
saveRDS(pregnancy_all_counts, paste0(preg_med_counts_dir,"/", pop_prefix, "_all_pregnancies_counts.rds"))

# Stratified by highest quality
pregnancy_all_counts_stratified <- D3_pregnancy_reconciled[,.N, by = .(year(pregnancy_start_date),month(pregnancy_start_date), highest_quality)]

hq_unique <- unique(pregnancy_all_counts_stratified$highest_quality)

for(i in 1:length(hq_unique)){
  counts_subset <- pregnancy_all_counts_stratified[highest_quality == hq_unique[i]]
  counts_subset <- as.data.table(merge(x = empty_df, y = counts_subset , by = c("year", "month"), all.x = TRUE)) # Merges empty_df with counts_subset 
  counts_subset[is.na(counts_subset [,N]), N:=0] # Fills in missing values with 0
  counts_subset$masked <- ifelse(counts_subset $N < 5 & counts_subset $N > 0, 1, 0) # Creates column that indicates if count value will be masked if mask = TRUE
  if(mask == T){counts_subset[counts_subset$masked == 1,]$N <- 5} else {counts_subset[counts_subset$masked == 1,]$N <- counts_subset[counts_subset$masked == 1,]$N} # Changes values less than 5 and more than 0 to 5
  counts_subset <- within(counts_subset, YM<- sprintf("%d-%02d", year, month)) # Create a YM column
  counts_subset <- counts_subset[,c("YM", "N", "masked")]
  saveRDS(counts_subset, paste0(preg_med_counts_dir,"/", pop_prefix,"_", hq_unique[i], "_all_pregnancies_counts.rds"))
}

# Clean up 
rm(list = grep("^pregnancy_all_counts|counts_subset", ls(), value = TRUE))