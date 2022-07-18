# Valproates 
df_med_use<-ALL_Valproate_med_use_during_pregnancy
df_preg_starts<-ALL_Valproate_all_preg_starts_during_tx_episodes
# Retinoids
# df_med_use<-ALL_Retinoid_med_use_during_pregnancy
# df_preg_starts<-ALL_Retinoid_all_preg_starts_during_tx_episodes

# Med use during pregnancies
# Check for duplicates
print(paste0("Med use during pregnancy: ",nrow(df_med_use)-nrow(df_med_use[!duplicated(df_med_use)]), " duplicates found"))
# Check for any records where med_use falls outside pregnancy start and pregnancy end dates
print(paste0("Med use during pregnancy, records outside range: ", nrow(df_med_use[Date<pregnancy_start_date|Date>pregnancy_end_date,])))

# Preg starts during treatment episode 
# Check for duplicates
print(paste0("Preg starts during treatment ",nrow(df_preg_starts)-nrow(df_preg_starts[!duplicated(df_preg_starts)]), " duplicates found"))
# Count number of records 
print(paste0("Preg starts during treatment episode, records outside range: ", nrow(df_preg_starts[pregnancy_start_date<episode.start|pregnancy_start_date>episode.end,])))

