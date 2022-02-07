# Counts the number of records where there was Retinoid and/or Valproate use during a pregnancy, stratified by highest quality level

# Loads Pregnancy records 
D3_pregnancy_reconciled <- as.data.table(get(load(paste0(preg_dir, "g_intermediate/D3_pregnancy_reconciled.RData"))))
# Data Cleaning Pregnancy file
D3_pregnancy_reconciled[,person_id:=as.character(person_id)]
D3_pregnancy_reconciled[,pregnancy_start_date:=as.IDate(pregnancy_start_date, "%Y%m%d" )]
D3_pregnancy_reconciled[,pregnancy_end_date:=as.IDate(pregnancy_end_date, "%Y%m%d" )]
# Creates empty data frame for counts 
# Loads denominator file to get min and max dates for empty file 
denominator <- list.files(output_dir, pattern = pop_prefix)
FUmonths_df <- as.data.table(readRDS(paste0(output_dir, pop_prefix, "_denominator.rds")))
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
study_preg_meds[,Date:=as.IDate(Date,"%Y%m%d")]
# Merges with pregnancy list to get list of patients who were pregnant and used Retinoids/Valproates
study_preg_meds <- D3_pregnancy_reconciled[study_preg_meds, on="person_id", nomatch=0]
# Removes records where medication was prescribed/dispensed outside of the pregnancy period
study_preg_meds <- study_preg_meds[study_preg_meds$Date >= study_preg_meds$pregnancy_start_date & study_preg_meds$Date <= study_preg_meds$pregnancy_end_date,]
# deduplicate records 
study_preg_meds <- study_preg_meds[!duplicated(study_preg_meds),]
# Save records 
saveRDS(study_preg_meds, paste0(counts_dfs_dir, pop_prefix, "_med_use_during_pregnancy.rds"))

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
            counts <- med_df[,.N, by = .(year(med_df$Date),month(med_df$Date))]
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
            saveRDS(counts, paste0(preg_med_counts_dir,"/",pop_prefix, "_", med_type_unique[mt], "_preg_", hq_unique[hq], "_counts.rds"))
          }
        }
      }
    }
  }
} else {
  print(paste("There are", nrow(study_preg_meds), "patient(s) who used Retinoids/Valproates during their pregnancy" ))
}


#### Monthly Pregnancy counts overall###
### D3_pregnancy_reconciled
D3_pregnancy_reconciled <- D3_pregnancy_reconciled[, nrow := .I]
D3_pregnancy_reconciled_expanded <- setDT(D3_pregnancy_reconciled)[ , list(idnum = person_id, pregnancy.day = seq(pregnancy_start_date, pregnancy_end_date, by = "day")), by = 1:nrow(D3_pregnancy_reconciled)]
# Merge back with original df
D3_pregnancy_reconciled_expanded<-  merge(D3_pregnancy_reconciled, D3_pregnancy_reconciled_expanded, by = "nrow")
# Create year-months columns episode.day
# df_expanded$episode.day.YM <-  format(as.Date(df_expanded$episode.day), "%Y-%m")
D3_pregnancy_reconciled_expanded$year <- year(D3_pregnancy_reconciled_expanded$pregnancy.day)
D3_pregnancy_reconciled_expanded$month <- month(D3_pregnancy_reconciled_expanded$pregnancy.day)
# Remove duplicate records
df_deduplicated <- unique(D3_pregnancy_reconciled_expanded, by=c("person_id", "year", "month"))
counts_preg <- df_deduplicated[,.N, by = .(year(df_deduplicated$pregnancy.day),month(df_deduplicated$pregnancy.day))]
counts_preg <- as.data.table(merge(x = empty_counts, y = counts_preg, by = c("year", "month"), all.x = TRUE))
counts_preg[is.na(counts_preg[,N]), N:=0]
# Masking values less than 5
# Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
counts_preg$masked <- ifelse(counts_preg$N<5 & counts_preg$N>0, 1, 0)
# Changes values less than 5 and more than 0 to 5
if (mask == T){counts_preg[counts_preg$masked == 1,]$N <- 5} else {counts_preg[counts_preg$masked == 1,]$N <- counts_preg[counts_preg$masked == 1,]$N }
counts_preg <- within(counts_preg, YM<- sprintf("%d-%02d", year, month))
counts_preg <-counts_preg[,c("YM", "N", "masked")]

# Saves file
saveRDS(counts_preg, paste0(preg_med_counts_dir,"/", pop_prefix, "_Pregnancy_ALL_counts.rds"))