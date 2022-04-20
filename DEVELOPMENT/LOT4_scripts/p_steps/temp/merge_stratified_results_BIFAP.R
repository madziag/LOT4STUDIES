# Load libraries 
library(stringi);library(data.table);library(stringr);library(dplyr);library(tidyr); library(purrr)
# Set path to output folder (All_regions) for BIFAP 
output_dir <- "C:/Users/31653/Desktop/LOT4_Sourcetree/DEVELOPMENT/LOT4_scripts/ALL_regions/"
# Create folder for stratified analysis 
invisible(ifelse(!dir.exists(paste0(output_dir, "stratified")), dir.create(paste0(output_dir, "stratified")), FALSE))
stratified_dir <- paste0(output_dir, "stratified/")
invisible(ifelse(!dir.exists(paste0(output_dir, "combined_with_strats")), dir.create(paste0(output_dir, "combined_with_strats")), FALSE))
combined_dir <- paste0(output_dir, "combined_with_strats/")
# All pooled counts: 
all_pooled_files <- list.files(output_dir, pattern = "pooled.csv", recursive = T, ignore.case = T, full.names = T) 
all_pooled_files <- all_pooled_files[!grepl(c("all_pregnancies|pgtests|preg_starts|med_use_during_pregnancy|denominator"), all_pooled_files, ignore.case = F)]
all_pooled_files <- all_pooled_files[!grepl(c("Retinoid_switched_to_alt_meds_counts_Pooled|Valproate_switched_to_alt_meds_counts_Pooled"), all_pooled_files, ignore.case = F)]
all_pooled_files <- all_pooled_files[!grepl(c("Retinoid_discontinued_counts_Pooled|Valproate_discontinued_counts_Pooled"), all_pooled_files, ignore.case = F)]
all_pooled_files <- all_pooled_files[!grepl(c("Retinoid_contraception_prior_counts_Pooled|Valproate_contraception_prior_counts_Pooled"), all_pooled_files, ignore.case = F)]
all_pooled_files <- all_pooled_files[!grepl(c("PC_alt_med|PC_HOSP_alt_med|Retinoid_MEDS|Valproate_MEDS"), all_pooled_files, ignore.case = F)]
all_pooled_files <- all_pooled_files[!grepl(c("Retinoid_med_use_during_contraception_episodes_counts_Pooled|Valproate_med_use_during_contraception_episodes_counts_Pooled"), all_pooled_files, ignore.case = F)]

#### Copies files #### 
for(i in 1:length(all_pooled_files)){
    file_name <- map(strsplit(all_pooled_files[i], split = "\\/"), 11)
    file.copy(all_pooled_files[i], paste0(stratified_dir, file_name))
}

stratified_files <- list.files(stratified_dir)


for(i in 1:length(stratified_files)){
  # Reads in the file 
  df<-fread(paste0(stratified_dir,"/",stratified_files[i]))
  # Non stratified
  if(str_detect(stratified_files[i], "Valproate_prevalence|Retinoid_prevalence|Valproate_incidence|Retinoid_incidence|Valproate_discontinued|Retinoid_discontinued|Valproate_switched_to_alt_meds|Retinoid_switched_to_alt_meds|Valproate_contraception_prior|Retinoid_contraception_prior|Valproate_med_use_during_contraception_episodes|Retinoid_med_use_during_contraception_episodes")){setnames(df,"N","N_overall");setnames(df,"Freq","Freq_overall");setnames(df,"rates","Rate_overall")}
  # Age groups
  if(str_detect(stratified_files[i],"age_group_12-20.99")){setnames(df,"N","N_age_12_21");setnames(df,"rates","Proportion_age_12_21")}
  if(str_detect(stratified_files[i],"age_group_21-30.99")){setnames(df,"N","N_age_21_31");setnames(df,"rates","Proportion_age_21_31")}
  if(str_detect(stratified_files[i],"age_group_31-40.99")){setnames(df,"N","N_age_31_41");setnames(df,"rates","Proportion_age_31_41")}
  if(str_detect(stratified_files[i],"age_group_41-55.99")){setnames(df,"N","N_age_41_56");setnames(df,"rates","Proportion_age_41_56")}
  # TX duration
  if(str_detect(stratified_files[i],"tx_dur_group_0-182"))  {setnames(df,"N","N_tx_duration_1_182")  ;setnames(df,"rates","Proportion_tx_duration_1_182")}
  if(str_detect(stratified_files[i],"tx_dur_group_182-365")){setnames(df,"N","N_tx_duration_182_365");setnames(df,"rates","Proportion_tx_duration_182_365")}
  if(str_detect(stratified_files[i],"tx_dur_group_over365")){setnames(df,"N","N_tx_duration_over365");setnames(df,"rates","Proportion_tx_duration_over365")}
  # Indications
  if(str_detect(stratified_files[i],"indication-bipolar")) {setnames(df,"N","N_indication_bipolar") ;setnames(df,"rates","Proportion_indication_bipolar")}
  if(str_detect(stratified_files[i],"indication-epilepsy")){setnames(df,"N","N_indication_epilepsy");setnames(df,"rates","Proportion_indication_epilepsy")}
  if(str_detect(stratified_files[i],"indication-migraine")){setnames(df,"N","N_indication_migraine");setnames(df,"rates","Proportion_indication_migraine")}
  if(str_detect(stratified_files[i],"indication-multiple")){setnames(df,"N","N_indication_multiple");setnames(df,"rates","Proportion_indication_multiple")}
  if(str_detect(stratified_files[i],"indication-unknown")) {setnames(df,"N","N_indication_unknown") ;setnames(df,"rates","Proportion_indication_unknown")}
  if(str_detect(stratified_files[i],"indication_bipolar")) {setnames(df,"N","N_indication_bipolar") ;setnames(df,"rates","Proportion_indication_bipolar")}
  if(str_detect(stratified_files[i],"indication_epilepsy")){setnames(df,"N","N_indication_epilepsy");setnames(df,"rates","Proportion_indication_epilepsy")}
  if(str_detect(stratified_files[i],"indication_migraine")){setnames(df,"N","N_indication_migraine");setnames(df,"rates","Proportion_indication_migraine")}
  if(str_detect(stratified_files[i],"indication_multiple")){setnames(df,"N","N_indication_multiple");setnames(df,"rates","Proportion_indication_multiple")}
  if(str_detect(stratified_files[i],"indication_unknown")) {setnames(df,"N","N_indication_unknown") ;setnames(df,"rates","Proportion_indication_unknown")}
  
  # Reasons for discontinuation
  if(str_detect(stratified_files[i],"reason-adr"))            {setnames(df,"N","N_reason_discontinue_ADR")      ;setnames(df,"rates","Proportion_reason_discontinue_ADR")}
  if(str_detect(stratified_files[i],"reason-pregnancy start")){setnames(df,"N","N_reason_discontinue_pregnancy");setnames(df,"rates","Proportion_reason_discontinue_pregnancy")}
  if(str_detect(stratified_files[i],"reason-pregnancy wish")) {setnames(df,"N","N_reason_discontinue_pregwish") ;setnames(df,"rates","Proportion_reason_discontinue_pregwish")}
  if(str_detect(stratified_files[i],"reason-multiple"))       {setnames(df,"N","N_reason_discontinue_multiple") ;setnames(df,"rates","Proportion_reason_discontinue_multiple")}
  if(str_detect(stratified_files[i],"reason-unknown"))        {setnames(df,"N","N_reason_discontinue_unknown")  ;setnames(df,"rates","Proportion_reason_discontinue_unknown")}
  # Contraceptives type
  if(str_detect(stratified_files[i],"contra_type_progest"))    {setnames(df,"N","N_contraceptive_progest")    ;setnames(df,"rates","Proportion_contraceptive_progest")}
  if(str_detect(stratified_files[i],"contra_type_fixedcomb"))  {setnames(df,"N","N_contraceptive_fixedcomb")  ;setnames(df,"rates","Proportion_contraceptive_fixedcomb")}
  if(str_detect(stratified_files[i],"contra_type_sequenprep")) {setnames(df,"N","N_contraceptive_sequenprep") ;setnames(df,"rates","Proportion_contraceptive_sequenprep")}
  if(str_detect(stratified_files[i],"contra_type_vaginalring")){setnames(df,"N","N_contraceptive_vaginalring");setnames(df,"rates","Proportion_contraceptive_vaginalring")}
  if(str_detect(stratified_files[i],"contra_type_patch"))      {setnames(df,"N","N_contraceptive_patch")      ;setnames(df,"rates","Proportion_contraceptive_patch")}
  if(str_detect(stratified_files[i],"contra_type_implant"))    {setnames(df,"N","N_contraceptive_implant")    ;setnames(df,"rates","Proportion_contraceptive_implant")}
  if(str_detect(stratified_files[i],"contra_type_injection"))  {setnames(df,"N","N_contraceptive_injection")  ;setnames(df,"rates","Proportion_contraceptive_injection")}
  # if(str_detect(stratified_files[i],"contra_type_IUD"))        {setnames(df,"N","N_contraceptive_IUD")        ;setnames(df,"rates","Proportion_contraceptive_IUD")}
  # if(str_detect(stratified_files[i],"contra_type_iud"))        {setnames(df,"N","N_contraceptive_iud")        ;setnames(df,"rates","Proportion_contraceptive_iud")}
  if(str_detect(stratified_files[i],"contra_type_iud_diag"))   {setnames(df,"N","N_contraceptive_IUD")   ;setnames(df,"rates","Proportion_contraceptive_IUD")}
  #Drops columns
  if("Freq"%in%colnames(df)){df[,Freq:=NULL]}
  if("masked"%in%colnames(df)){df[,masked:=NULL]}
  if("true_value"%in%colnames(df)){df[,true_value:=NULL]}
  #Saves files
  df <- df[order(YM)]
  saveRDS(df,paste0(stratified_dir,"/",gsub(".csv","",stratified_files[i]),".rds"))
  
}

for(file in list.files(path = stratified_dir, pattern =".csv")){unlink(paste0(stratified_dir, file), recursive = TRUE)}

# 1. PREVALENCE
prev_files<-list.files(stratified_dir,pattern="prevalence",full.names=T)
## A. Retinoids
prev_files_rets<-prev_files[grepl(c("Retinoid"),prev_files)]
### i. PC
prev_files_rets_PC<-prev_files_rets[!grepl(c("PC_HOSP"),prev_files_rets)]
prev_ret_PC_df<-do.call(cbind,lapply(prev_files_rets_PC, readRDS))
prev_ret_PC_df<-prev_ret_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                  "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                  "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365")]
saveRDS(prev_ret_PC_df,paste0(combined_dir,"PC_Retinoid_Prevalence_combined.rds") )
write.csv(prev_ret_PC_df,paste0(combined_dir,"PC_Retinoid_Prevalence_combined.csv"))

rm(prev_files_rets_PC,prev_ret_PC_df)

### ii. PC_HOSP
prev_files_rets_PC_HOSP<-prev_files_rets[grepl(c("PC_HOSP"),prev_files_rets)]
prev_ret_PC_HOSP_df<-do.call(cbind,lapply(prev_files_rets_PC_HOSP,readRDS))
prev_ret_PC_HOSP_df<-prev_ret_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                            "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365")]
saveRDS(prev_ret_PC_HOSP_df,paste0(combined_dir,"PC_HOSP_Retinoid_Prevalence_combined.rds") )
write.csv(prev_ret_PC_HOSP_df,paste0(combined_dir,"PC_HOSP_Retinoid_Prevalence_combined.csv"))

rm(prev_files_rets_PC_HOSP,prev_ret_PC_HOSP_df)

## B. Valproates
prev_files_valps<-prev_files[grepl(c("Valproate"),prev_files)]
### i. PC
prev_files_valps_PC<-prev_files_valps[!grepl(c("PC_HOSP"),prev_files_valps)]
prev_valp_PC_df<-do.call(cbind,lapply(prev_files_valps_PC,readRDS))
prev_valp_PC_df<-prev_valp_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                    "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                    "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown", 
                                    "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365")]
saveRDS(prev_valp_PC_df,paste0(combined_dir,"PC_Valproate_Prevalence_combined.rds") )
write.csv(prev_valp_PC_df,paste0(combined_dir,"PC_Valproate_Prevalence_combined.csv"))

rm(prev_files_valps_PC,prev_valp_PC_df)

### ii. PC_HOSP
prev_files_valps_PC_HOSP<-prev_files_valps[grepl(c("PC_HOSP"),prev_files_valps)]
prev_valp_PC_HOSP_df<-do.call(cbind,lapply(prev_files_valps_PC_HOSP,readRDS))
prev_valp_PC_HOSP_df<-prev_valp_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                              "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown", 
                                              "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365")]
saveRDS(prev_valp_PC_HOSP_df,paste0(combined_dir,"PC_HOSP_Valproate_Prevalence_combined.rds") )
write.csv(prev_valp_PC_HOSP_df,paste0(combined_dir,"PC_HOSP_Valproate_Prevalence_combined.csv"))

rm(prev_files_valps_PC_HOSP,prev_valp_PC_HOSP_df)

rm(prev_files, prev_files_rets,prev_files_valps)

for(file in list.files(path = stratified_dir, pattern ="prevalence_counts")){unlink(paste0(stratified_dir, file), recursive = TRUE)}

# 2. INCIDENCE
incidence_files<-list.files(stratified_dir,pattern="incidence",full.names = T)
## A. Retinoids
incidence_files_rets<-incidence_files[grepl(c("Retinoid"),incidence_files)]
### i. PC
incidence_files_rets_PC<-incidence_files_rets[!grepl(c("PC_HOSP"),incidence_files_rets)]
incidence_ret_PC_df<-do.call(cbind,lapply(incidence_files_rets_PC,readRDS))
incidence_ret_PC_df<-incidence_ret_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(incidence_ret_PC_df,paste0(combined_dir, "PC_Retinoid_Incidence_combined.rds") )
write.csv(incidence_ret_PC_df,paste0(combined_dir, "PC_Retinoid_Incidence_combined.csv"))

rm(incidence_files_rets_PC,incidence_ret_PC_df)

### ii. PC_HOSP
incidence_files_rets_PC_HOSP <- incidence_files_rets[grepl(c("PC_HOSP"), incidence_files_rets)]
incidence_ret_PC_HOSP_df<-do.call(cbind,lapply(incidence_files_rets_PC_HOSP,readRDS))
incidence_ret_PC_HOSP_df<-incidence_ret_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                      "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(incidence_ret_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Retinoid_Incidence_combined.rds") )
write.csv(incidence_ret_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Retinoid_Incidence_combined.csv"))

rm(incidence_files_rets_PC_HOSP,incidence_ret_PC_HOSP_df)

## B. Valproates
incidence_files_valps <- incidence_files[grepl(c("Valproate"), incidence_files)]
### i. PC
incidence_files_valps_PC <- incidence_files_valps[!grepl(c("PC_HOSP"), incidence_files_valps)]
incidence_valp_PC_df<-do.call(cbind,lapply(incidence_files_valps_PC,readRDS))
incidence_valp_PC_df<-incidence_valp_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                            "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(incidence_valp_PC_df,paste0(combined_dir, "PC_Valproate_Incidence_combined.rds") )
write.csv(incidence_valp_PC_df,paste0(combined_dir, "PC_Valproate_Incidence_combined.csv"))

rm(incidence_files_valps_PC,incidence_valp_PC_df)

### ii. PC_HOSP
incidence_files_valps_PC_HOSP <- incidence_files_valps[grepl(c("PC_HOSP"), incidence_files_valps)]
incidence_valp_PC_HOSP_df<-do.call(cbind,lapply(incidence_files_valps_PC_HOSP,readRDS))
incidence_valp_PC_HOSP_df<-incidence_valp_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                              "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(incidence_valp_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Valproate_Incidence_combined.rds") )
write.csv(incidence_valp_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Valproate_Incidence_combined.csv"))

rm(incidence_files_valps_PC_HOSP,incidence_valp_PC_HOSP_df)

rm(incidence_files, incidence_files_rets, incidence_files_valps)

for(file in list.files(path = stratified_dir, pattern ="incidence_counts")){unlink(paste0(stratified_dir, file), recursive = TRUE)}


# 3. DISCONTINUED
disc_files <- list.files(stratified_dir, pattern="discontinued", full.names = T)
## A. Retinoids
disc_files_rets <- disc_files[grepl(c("Retinoid"), disc_files)]
### i. PC
disc_files_rets_PC <- disc_files_rets[!grepl(c("PC_HOSP"), disc_files_rets)]
disc_ret_PC_df<-do.call(cbind,lapply(disc_files_rets_PC, readRDS))
disc_ret_PC_df<-disc_ret_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                  "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                  "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365",
                                  "N_reason_discontinue_pregwish", "Proportion_reason_discontinue_pregwish","N_reason_discontinue_pregnancy","Proportion_reason_discontinue_pregnancy","N_reason_discontinue_ADR","Proportion_reason_discontinue_ADR", "N_reason_discontinue_multiple", "Proportion_reason_discontinue_multiple", "N_reason_discontinue_unknown","Proportion_reason_discontinue_unknown")]
saveRDS(disc_ret_PC_df,paste0(combined_dir,"PC_Retinoid_Discontinued_combined.rds") )
write.csv(disc_ret_PC_df,paste0(combined_dir,"PC_Retinoid_Discontinued_combined.csv"))

rm(disc_files_rets_PC,disc_ret_PC_df)

### ii. PC_HOSP
disc_files_rets_PC_HOSP <- disc_files_rets[grepl(c("PC_HOSP"), disc_files_rets)]
disc_ret_PC_HOSP_df<-do.call(cbind,lapply(disc_files_rets_PC_HOSP, readRDS))
disc_ret_PC_HOSP_df<-disc_ret_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                            "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365",
                                            "N_reason_discontinue_pregwish", "Proportion_reason_discontinue_pregwish","N_reason_discontinue_pregnancy","Proportion_reason_discontinue_pregnancy","N_reason_discontinue_ADR","Proportion_reason_discontinue_ADR", "N_reason_discontinue_multiple", "Proportion_reason_discontinue_multiple", "N_reason_discontinue_unknown","Proportion_reason_discontinue_unknown")]
saveRDS(disc_ret_PC_HOSP_df,paste0(combined_dir,"PC_HOSP_Retinoid_Discontinued_combined.rds") )
write.csv(disc_ret_PC_HOSP_df,paste0(combined_dir,"PC_HOSP_Retinoid_Discontinued_combined.csv"))

rm(disc_files_rets_PC_HOSP,disc_ret_PC_HOSP_df)

## B. Valproates
disc_files_valps <- disc_files[grepl(c("Valproate"), disc_files)]
### i. PC
disc_files_valps_PC <- disc_files_valps[!grepl(c("PC_HOSP"), disc_files_valps)]
disc_valp_PC_df<-do.call(cbind,lapply(disc_files_valps_PC, readRDS))
disc_valp_PC_df<-disc_valp_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                    "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                    "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown", 
                                    "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365",
                                    "N_reason_discontinue_pregwish", "Proportion_reason_discontinue_pregwish","N_reason_discontinue_pregnancy","Proportion_reason_discontinue_pregnancy","N_reason_discontinue_ADR","Proportion_reason_discontinue_ADR", "N_reason_discontinue_multiple", "Proportion_reason_discontinue_multiple", "N_reason_discontinue_unknown","Proportion_reason_discontinue_unknown")]
saveRDS(disc_valp_PC_df,paste0(combined_dir,"PC_Valproate_Discontinued_combined.rds") )
write.csv(disc_valp_PC_df,paste0(combined_dir,"PC_Valproate_Discontinued_combined.csv"))

rm(disc_files_valps_PC,disc_valp_PC_df)

### ii. PC_HOSP
disc_files_valps_PC_HOSP <- disc_files_valps[grepl(c("PC_HOSP"), disc_files_valps)]
disc_valp_PC_HOSP_df<-do.call(cbind,lapply(disc_files_valps_PC_HOSP, readRDS))
disc_valp_PC_HOSP_df<-disc_valp_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                              "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown", 
                                              "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365",
                                              "N_reason_discontinue_pregwish", "Proportion_reason_discontinue_pregwish","N_reason_discontinue_pregnancy","Proportion_reason_discontinue_pregnancy","N_reason_discontinue_ADR","Proportion_reason_discontinue_ADR", "N_reason_discontinue_multiple", "Proportion_reason_discontinue_multiple", "N_reason_discontinue_unknown","Proportion_reason_discontinue_unknown")]
saveRDS(disc_valp_PC_HOSP_df,paste0(combined_dir,"PC_HOSP_Valproate_Discontinued_combined.rds") )
write.csv(disc_valp_PC_HOSP_df,paste0(combined_dir,"PC_HOSP_Valproate_Discontinued_combined.csv"))

rm(disc_files_valps_PC_HOSP,disc_valp_PC_HOSP_df)

rm(disc_files, disc_files_rets, disc_files_valps)

for(file in list.files(path = stratified_dir, pattern ="discontinued_counts")){unlink(paste0(stratified_dir, file), recursive = TRUE)}

# 4. SWITCHED
switched_files <- list.files(stratified_dir, pattern="switched", full.names = T)
## A. Retinoids
switched_files_rets <- switched_files[grepl(c("Retinoid"), switched_files)]
### i. PC
switched_files_rets_PC <- switched_files_rets[!grepl(c("PC_HOSP"), switched_files_rets)]
switched_ret_PC_df<-do.call(cbind,lapply(switched_files_rets_PC,readRDS))
switched_ret_PC_df<-switched_ret_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(switched_ret_PC_df,paste0(combined_dir, "PC_Retinoid_Switched_to_Alt_Meds_combined.rds") )
write.csv(switched_ret_PC_df,paste0(combined_dir, "PC_Retinoid_Switched_to_Alt_Meds_combined.csv"))

rm(switched_files_rets_PC,switched_ret_PC_df)

### ii. PC_HOSP
switched_files_rets_PC_HOSP <- switched_files_rets[grepl(c("PC_HOSP"), switched_files_rets)]
switched_ret_PC_HOSP_df<-do.call(cbind,lapply(switched_files_rets_PC_HOSP,readRDS))
switched_ret_PC_HOSP_df<-switched_ret_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                          "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(switched_ret_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Retinoid_Switched_to_Alt_Meds_combined.rds") )
write.csv(switched_ret_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Retinoid_Switched_to_Alt_Meds_combined.csv"))

rm(switched_files_rets_PC_HOSP,switched_ret_PC_HOSP_df)

## B. Valproates
switched_files_valps <- switched_files[grepl(c("Valproate"), switched_files)]
### i. PC
switched_files_valps_PC <- switched_files_valps[!grepl(c("PC_HOSP"), switched_files_valps)]
switched_valp_PC_df<-do.call(cbind,lapply(switched_files_valps_PC ,readRDS))
switched_valp_PC_df<-switched_valp_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                              "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(switched_valp_PC_df,paste0(combined_dir, "PC_Valproate_Switched_to_Alt_Meds_combined.rds") )
write.csv(switched_valp_PC_df,paste0(combined_dir, "PC_Valproate_Switched_to_Alt_Meds_combined.csv"))

rm(switched_files_valps_PC ,switched_valp_PC_df)

### ii. PC_HOSP
switched_files_valps_PC_HOSP <- switched_files_valps[grepl(c("PC_HOSP"), switched_files_valps)]
switched_valp_PC_HOSP_df<-do.call(cbind,lapply(switched_files_valps_PC_HOSP,readRDS))
switched_valp_PC_HOSP_df<-switched_valp_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                            "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(switched_valp_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Valproate_Switched_to_Alt_Meds_combined.rds") )
write.csv(switched_valp_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Valproate_Switched_to_Alt_Meds_combined.csv"))

rm(switched_files_valps_PC_HOSP,switched_valp_PC_HOSP_df)

for(file in list.files(path = stratified_dir, pattern ="switched_to_alt_meds_counts")){unlink(paste0(stratified_dir, file), recursive = TRUE)}

# 5. CONTRA_PRIOR
contra_prior_files <- list.files(stratified_dir, pattern="contraception_prior", full.names = T)
## A. Retinoids
contra_prior_files_rets <- contra_prior_files[grepl(c("Retinoid"), contra_prior_files)]
### i. PC
contra_prior_files_rets_PC <- contra_prior_files_rets[!grepl(c("PC_HOSP"), contra_prior_files_rets)]
contra_prior_ret_PC_df<-do.call(cbind,lapply(contra_prior_files_rets_PC,readRDS))
contra_prior_ret_PC_df<-contra_prior_ret_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(contra_prior_ret_PC_df,paste0(combined_dir, "PC_Retinoid_Contra_Prior_combined.rds") )
write.csv(contra_prior_ret_PC_df,paste0(combined_dir, "PC_Retinoid_Contra_Prior_combined.csv"))

rm(contra_prior_files_rets_PC,contra_prior_ret_PC_df)

### ii. PC_HOSP
contra_prior_files_rets_PC_HOSP <- contra_prior_files_rets[grepl(c("PC_HOSP"), contra_prior_files_rets)]
contra_prior_ret_PC_HOSP_df<-do.call(cbind,lapply(contra_prior_files_rets_PC_HOSP,readRDS))
contra_prior_ret_PC_HOSP_df<-contra_prior_ret_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                             "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(contra_prior_ret_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Retinoid_Contra_Prior_combined.rds") )
write.csv(contra_prior_ret_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Retinoid_Contra_Prior_combined.csv"))

rm(contra_prior_files_rets_PC_HOSP,contra_prior_ret_PC_HOSP_df)

## B. Valproates
contra_prior_files_valps <- contra_prior_files[grepl(c("Valproate"), contra_prior_files)]
### i. PC
contra_prior_files_valps_PC <- contra_prior_files_valps[!grepl(c("PC_HOSP"), contra_prior_files_valps)]
contra_prior_valp_PC_df<-do.call(cbind,lapply(contra_prior_files_valps_PC,readRDS))

contra_prior_valp_PC_df<-contra_prior_valp_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                              "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(contra_prior_valp_PC_df,paste0(combined_dir, "PC_Valproate_Contra_Prior_combined.rds") )
write.csv(contra_prior_valp_PC_df,paste0(combined_dir, "PC_Valproate_Contra_Prior_combined.csv"))

rm(contra_prior_files_valps_PC,contra_prior_valp_PC_df)

### ii. PC_HOSP
contra_prior_files_valps_PC_HOSP <- contra_prior_files_valps[grepl(c("PC_HOSP"), contra_prior_files_valps)]
contra_prior_valp_PC_HOSP_df<-do.call(cbind,lapply(contra_prior_files_valps_PC_HOSP,readRDS))
contra_prior_valp_PC_HOSP_df<-contra_prior_valp_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                    "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                                    "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(contra_prior_valp_PC_HOSP_df,paste0(combined_dir, "PC__HOSP_Valproate_Contra_Prior_combined.rds") )
write.csv(contra_prior_valp_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Valproate_Contra_Prior_combined.csv"))

rm(contra_prior_files_valps_PC_HOSP,contra_prior_valp_PC_HOSP_df)

rm(contra_prior_files, contra_prior_files_rets, contra_prior_files_valps)

for(file in list.files(path = stratified_dir, pattern ="contraception_prior_counts")){unlink(paste0(stratified_dir, file), recursive = TRUE)}

# 6. MED USE DURING CONTRA EPISODE
med_use_during_contra_files <- list.files(stratified_dir, pattern="med_use_during_contraception_episodes|med_use_during_contra_epis", full.names = T)
## A. Retinoids
med_use_during_contra_files_rets <- med_use_during_contra_files[grepl(c("Retinoid"), med_use_during_contra_files)]
### i. PC
med_use_during_contra_files_rets_PC <- med_use_during_contra_files_rets[!grepl(c("PC_HOSP"), med_use_during_contra_files_rets)]
med_use_during_contra_ret_PC_df<-do.call(cbind,lapply(med_use_during_contra_files_rets_PC,readRDS))
med_use_during_contra_ret_PC_df<-med_use_during_contra_ret_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                  "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(med_use_during_contra_ret_PC_df,paste0(combined_dir, "PC_Retinoid_Med_Use_During_Contra_Episode_combined.rds") )
write.csv(med_use_during_contra_ret_PC_df,paste0(combined_dir, "PC_Retinoid_Med_Use_During_Contra_Episode_combined.csv"))

rm(med_use_during_contra_files_rets_PC,med_use_during_contra_ret_PC_df)

### ii. PC_HOSP
med_use_during_contra_files_rets_PC_HOSP <- med_use_during_contra_files_rets[grepl(c("PC_HOSP"), med_use_during_contra_files_rets)]
med_use_during_contra_ret_PC_HOSP_df<-do.call(cbind,lapply(med_use_during_contra_files_rets_PC_HOSP,readRDS))
med_use_during_contra_ret_PC_HOSP_df<-med_use_during_contra_ret_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                                    "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(med_use_during_contra_ret_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Retinoid_Med_Use_During_Contra_Episode_combined.rds") )
write.csv(med_use_during_contra_ret_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Retinoid_Med_Use_During_Contra_Episode_combined.csv"))

rm(med_use_during_contra_files_rets_PC_HOSP,med_use_during_contra_ret_PC_HOSP_df)

## B. Valproates
med_use_during_contra_files_valps <- med_use_during_contra_files[grepl(c("Valproate"), med_use_during_contra_files)]
### i. PC
med_use_during_contra_files_valps_PC <- med_use_during_contra_files_valps[!grepl(c("PC_HOSP"), med_use_during_contra_files_valps)]
med_use_during_contra_valp_PC_df<-do.call(cbind,lapply(med_use_during_contra_files_valps_PC,readRDS))
med_use_during_contra_valp_PC_df<-med_use_during_contra_valp_PC_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                                    "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                                                    "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown",
                                                                    "N_contraceptive_fixedcomb", "Proportion_contraceptive_fixedcomb","N_contraceptive_patch","Proportion_contraceptive_patch","N_contraceptive_progest","Proportion_contraceptive_progest", "N_contraceptive_sequenprep","Proportion_contraceptive_sequenprep","N_contraceptive_vaginalring","Proportion_contraceptive_vaginalring", "N_contraceptive_IUD","Proportion_contraceptive_IUD",  "N_contraceptive_implant", "Proportion_contraceptive_implant")]
saveRDS(med_use_during_contra_valp_PC_df,paste0(combined_dir, "PC_Valproate_Med_Use_During_Contra_Episode_combined.rds") )
write.csv(med_use_during_contra_valp_PC_df,paste0(combined_dir, "PC_Valproate_Med_Use_During_Contra_Episode_combined.csv"))

rm(med_use_during_contra_files_valps_PC,med_use_during_contra_valp_PC_df)

### ii. PC_HOSP
med_use_during_contra_files_valps_PC_HOSP <- med_use_during_contra_files_valps[grepl(c("PC_HOSP"), med_use_during_contra_files_valps)]
med_use_during_contra_valp_PC_HOSP_df<-do.call(cbind,lapply(med_use_during_contra_files_valps_PC_HOSP,readRDS))
med_use_during_contra_valp_PC_HOSP_df<-med_use_during_contra_valp_PC_HOSP_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                                      "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                                                      "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown",
                                                                      "N_contraceptive_fixedcomb", "Proportion_contraceptive_fixedcomb","N_contraceptive_patch","Proportion_contraceptive_patch","N_contraceptive_progest","Proportion_contraceptive_progest", "N_contraceptive_sequenprep","Proportion_contraceptive_sequenprep","N_contraceptive_vaginalring","Proportion_contraceptive_vaginalring", "N_contraceptive_IUD","Proportion_contraceptive_IUD",  "N_contraceptive_implant", "Proportion_contraceptive_implant")]
saveRDS(med_use_during_contra_valp_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Valproate_Med_Use_During_Contra_Episode_combined.rds") )
write.csv(med_use_during_contra_valp_PC_HOSP_df,paste0(combined_dir, "PC_HOSP_Valproate_Med_Use_During_Contra_Episode_combined.csv"))

rm(med_use_during_contra_files_valps_PC_HOSP,med_use_during_contra_valp_PC_HOSP_df)

rm(med_use_during_contra_files, med_use_during_contra_files_rets, med_use_during_contra_files_valps)

for(file in list.files(path = stratified_dir, pattern ="med_use_during_contraception_episodes_counts|med_use_during_contra_epis_counts")){unlink(paste0(stratified_dir, file), recursive = TRUE)}





