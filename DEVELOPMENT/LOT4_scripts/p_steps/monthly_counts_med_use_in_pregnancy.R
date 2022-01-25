# Counts the number of records where there was Retinoid and/or Valproate use during a pregnancy, stratified by highest quality level

# Loads Pregnancy records 
D3_pregnancy_reconciled <- as.data.table(get(load(paste0(preg_dir, "g_intermediate/D3_pregnancy_reconciled.RData"))))
# Data Cleaning Pregnancy file
D3_pregnancy_reconciled[,person_id:=as.character(person_id)]
D3_pregnancy_reconciled[,pregnancy_start_date:=as.IDate(pregnancy_start_date, "%Y%m%d" )]
D3_pregnancy_reconciled[,pregnancy_end_date:=as.IDate(pregnancy_end_date, "%Y%m%d" )]
# Creates empty data frame for counts 
# Loads denominator file to get min and max dates for empty file 
denominator <- list.files(output_dir, pattern = populations[pop])
prefix_pop <- gsub("study_population.rds", "",populations[pop])
FUmonths_df <- as.data.table(readRDS(paste0(output_dir, prefix_pop, "denominator.rds")))
# Splits Y-M column 
FUmonths_df[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)]
empty_counts <- expand.grid(seq(min(FUmonths_df$Y), max(FUmonths_df$Y)), seq(1, 12))
names(empty_counts) <- c("year", "month")    

study_preg_meds <- study_pop_meds 
# Creates column that describes if ATC is Retinoid, Valproate or Unknowns (file read in run_all_counts_final.R)
study_preg_meds[,med_type := ifelse(study_preg_meds[,Code %chin% c("D05BB02", "D11AH04", "D10BA01")], "Retinoid",
                                   ifelse(study_preg_meds[,Code %chin% c("N03AG01","N03AG02")], "Valproate", "Unknown"))]
# Data cleaning 
study_preg_meds[,person_id:=as.character(person_id)]
study_preg_meds[,event_date:=as.IDate(event_date,"%Y%m%d")]
# Merges with pregnancy list to get list of patients who were pregnant and used Retinoids/Valproates
study_preg_meds <- D3_pregnancy_reconciled[study_preg_meds, on="person_id", nomatch=0]
# Removes records where medication was prescribed/dispensed outside of the pregnancy period
study_preg_meds <- study_preg_meds[study_preg_meds$event_date >= study_preg_meds$pregnancy_start_date & study_preg_meds$event_date <= study_preg_meds$pregnancy_end_date,]
# Save records 
saveRDS(study_preg_meds, paste0(counts_dfs_dir, prefix_pop, "med_use_during_pregnancy.rds"))

if(nrow(study_preg_meds) > 0) {
  # Gets unique values of med_type column 
  med_type_unique <- unique(study_preg_meds$med_type)
  # Monthly Counts
  if (length(med_type_unique > 0)){
    for (mt in 1:length(med_type_unique)){
      med_df1 <- study_preg_meds[study_preg_meds$med_type == med_type_unique[mt],]
      # # Performs counts per month/year for every available highest_quality level
      # # Gets unique values of column "highest_quality"
      hq_unique <- unique(med_df1$highest_quality)
      if (length(hq_unique) > 0){
        for (hq in 1:length(hq_unique)){
          med_df <- med_df1[med_df1$highest_quality == hq_unique[hq],]
          if (nrow(med_df) >0){
            counts <- med_df[,.N, by = .(year(med_df$event_date),month(med_df$event_date))]
            counts <- as.data.table(merge(x = empty_counts, y = counts, by = c("year", "month"), all.x = TRUE))
            counts[is.na(counts[,N]), N:=0]
            # Masking values less than 5
            # Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
            counts$masked <- ifelse(counts$N<5 & counts$N>0, 1, 0)
            # Changes values less than 5 and more than 0 to 5
            if (mask == T){counts[counts$masked == 1,]$N <- 5} else {counts[counts$masked == 1,]$N <- counts[counts$masked == 1,]$N }
            # Calculates rates
            counts <- within(counts, YM<- sprintf("%d-%02d", year, month))
            counts <- merge(x = counts, y = FUmonths_df, by = c("YM"), all.x = TRUE)
            counts <-counts[,rates:=as.numeric(N)/as.numeric(Freq)]
            counts <-counts[,c("YM", "N", "Freq", "rates", "masked")]
            print(paste("Saving monthly counts: Use of", med_type_unique[mt], "during pregnancy, data quality level:", hq_unique[hq]))
            # Saves file
            if(SUBP == TRUE){
              pop_names <- gsub(".rds", "", populations[pop])
              saveRDS(counts, paste0(preg_med_counts,"/",pop_names, "_", med_type_unique[mt], "_preg_", hq_unique[hq], "_counts.rds"))
            }else{
              saveRDS(counts, paste0(preg_med_counts,"/", med_type_unique[mt], "_preg_",hq_unique[hq] , "_counts.rds"))
            }
          }
        }
      }
    }
  }
} else {
  print(paste("There are", nrow(study_preg_meds), "patient(s) who used Retinoids/Valproates during their pregnancy" ))
}



