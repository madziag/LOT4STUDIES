# Load libraries
library(stringi);library(data.table);library(stringr);library(dplyr);library(tidyr); library(purrr)
# Set path to output folder (All_regions) for BIFAP
output_dir <- "C:/Users/31653/Desktop/LOT4_Sourcetree/DEVELOPMENT/LOT4_scripts/"

# Create folder to store results
invisible(ifelse(!dir.exists(paste0(output_dir, "combined_with_strats")), dir.create(paste0(output_dir, "combined_with_strats")), FALSE))
combined_dir <- paste0(output_dir, "combined_with_strats/")

stratified_dir <- "C:/Users/31653/Desktop/LOT4_Sourcetree/DEVELOPMENT/LOT4_scripts/PHARMO"
stratified_files <- list.files(stratified_dir, pattern=".csv")

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
  if(str_detect(stratified_files[i],"contra_type_IUD"))        {setnames(df,"N","N_contraceptive_IUD")        ;setnames(df,"rates","Proportion_contraceptive_IUD")}
  # if(str_detect(stratified_files[i],"contra_type_iud"))        {setnames(df,"N","N_contraceptive_IUD")        ;setnames(df,"rates","Proportion_contraceptive_IUD")}
  if(str_detect(stratified_files[i],"contra_type_iud_diag"))   {setnames(df,"N","N_contraceptive_IUD")        ;setnames(df,"rates","Proportion_contraceptive_IUD")}
  #Drops columns
  if("Freq"%in%colnames(df)){df[,Freq:=NULL]}
  if("masked"%in%colnames(df)){df[,masked:=NULL]}
  if("true_value"%in%colnames(df)){df[,true_value:=NULL]}
  if("true_value"%in%colnames(df)){df[,true_value:=NULL]}
  #Saves files
  df <- df[order(YM)]
  saveRDS(df,paste0(stratified_dir,"/",gsub(".csv","",stratified_files[i]),".rds"))
  
}

for(file in list.files(path = stratified_dir, pattern =".csv")){unlink(paste0(stratified_dir, "/", file), recursive = TRUE)}


################################################################################################################################################################
#### RETINOIDS #################################################################################################################################################  
################################################################################################################################################################   
# 1. PREVALENCE
prev_files<-list.files(stratified_dir,pattern="prevalence",full.names=T)
prev_files_rets<-prev_files[grepl(c("Retinoid"),prev_files)]
prev_ret_df<-do.call(cbind,lapply(prev_files_rets, readRDS))
prev_ret_df<-prev_ret_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                            "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365")]
saveRDS(prev_ret_df,paste0(combined_dir,"Retinoid_Prevalence_stratified_combined_age-tx_dur.rds") )
write.csv(prev_ret_df,paste0(combined_dir,"Retinoid_Prevalence_stratified_combined_age-tx_dur.csv"))
rm(prev_files_rets,prev_ret_df)
# 2. INCIDENCE
incidence_files<-list.files(stratified_dir,pattern="incidence",full.names = T)
incidence_files_rets<-incidence_files[grepl(c("Retinoid"),incidence_files)]
incidence_ret_df<-do.call(cbind,lapply(incidence_files_rets,readRDS))
incidence_ret_df<-incidence_ret_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                      "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(incidence_ret_df,paste0(combined_dir, "Retinoid_Incidence_stratified_combined_age.rds") )
write.csv(incidence_ret_df,paste0(combined_dir, "Retinoid_Incidence_stratified_combined_age.csv"))
rm(incidence_files_rets,incidence_ret_df)
# 3. DISCONTINUED
disc_files <- list.files(stratified_dir, pattern="discontinued", full.names = T)
disc_files_rets<-disc_files[grepl(c("Retinoid"), disc_files)]
disc_ret_df<-do.call(cbind,lapply(disc_files_rets, readRDS))
disc_ret_df<-disc_ret_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                            "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365",
                            "N_reason_discontinue_pregwish", "Proportion_reason_discontinue_pregwish","N_reason_discontinue_pregnancy","Proportion_reason_discontinue_pregnancy","N_reason_discontinue_ADR","Proportion_reason_discontinue_ADR", "N_reason_discontinue_multiple", "Proportion_reason_discontinue_multiple", "N_reason_discontinue_unknown","Proportion_reason_discontinue_unknown")]

saveRDS(disc_ret_df,paste0(combined_dir,"Retinoid_Discontinued_stratified_combined_age-tx_dur-reason.rds") )
write.csv(disc_ret_df,paste0(combined_dir,"Retinoid_Discontinued_stratified_combined_age-tx_dur-reason.csv"))
rm(disc_files_rets,disc_ret_df)

# 4. SWITCHED
switched_files <- list.files(stratified_dir, pattern="switched", full.names = T)
switched_files_rets <- switched_files[grepl(c("Retinoid"), switched_files)]
switched_ret_df<-do.call(cbind,lapply(switched_files_rets,readRDS))
switched_ret_df<-switched_ret_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                    "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(switched_ret_df,paste0(combined_dir, "Retinoid_Switched_to_Alt_Meds_stratified_combined_age.rds") )
write.csv(switched_ret_df,paste0(combined_dir, "Retinoid_Switched_to_Alt_Meds_stratified_combined_age.csv"))
rm(switched_files_rets,switched_ret_df)

# 5. CONTRA_PRIOR
contra_prior_files <- list.files(stratified_dir, pattern="contraception_prior", full.names = T)
contra_prior_files_rets <- contra_prior_files[grepl(c("Retinoid"), contra_prior_files)]
contra_prior_ret_df<-do.call(cbind,lapply(contra_prior_files_rets,readRDS))
contra_prior_ret_df<-contra_prior_ret_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                            "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(contra_prior_ret_df,paste0(combined_dir, "Retinoid_Contra_Prior_stratified_combined_age.rds") )
write.csv(contra_prior_ret_df,paste0(combined_dir, "Retinoid_Contra_Prior_stratified_combined_age.csv"))
rm(contra_prior_files_rets,contra_prior_ret_df)

# 6. MED USE DURING CONTRA EPISODE
med_use_during_contra_files <- list.files(stratified_dir, pattern="med_use_during_contraception_episodes|med_use_during_contra_epis", full.names = T)
med_use_during_contra_files_rets <- med_use_during_contra_files[grepl(c("Retinoid"), med_use_during_contra_files)]
med_use_during_contra_ret_df<-do.call(cbind,lapply(med_use_during_contra_files_rets,readRDS))
med_use_during_contra_ret_df<-med_use_during_contra_ret_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56")]
saveRDS(med_use_during_contra_ret_df,paste0(combined_dir, "Retinoid_Med_Use_During_Contra_Episode_stratified_combined_age.rds") )
write.csv(med_use_during_contra_ret_df,paste0(combined_dir, "Retinoid_Med_Use_During_Contra_Episode_stratified_combined_age.csv"))
rm(med_use_during_contra_files_rets,med_use_during_contra_ret_df)

################################################################################################################################################################
#### VALPROATES #################################################################################################################################################
################################################################################################################################################################ 
# 1. PREVALENCE
prev_files<-list.files(stratified_dir,pattern="prevalence",full.names=T)
prev_files_valps<-prev_files[grepl(c("Valproate"),prev_files)]
prev_valp_df<-do.call(cbind,lapply(prev_files_valps,readRDS))
prev_valp_df<-prev_valp_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                              "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown", 
                              "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365")]
saveRDS(prev_valp_df,paste0(combined_dir,"Valproate_Prevalence_stratified_combined_age-indication-tx_dur.rds") )
write.csv(prev_valp_df,paste0(combined_dir,"Valproate_Prevalence_stratified_combined_age-indication-tx_dur.csv"))
rm(prev_files_valps,prev_valp_df)
# 2. INCIDENCE
incidence_files<-list.files(stratified_dir,pattern="incidence",full.names = T)
incidence_files_valps <- incidence_files[grepl(c("Valproate"), incidence_files)]
incidence_valp_df<-do.call(cbind,lapply(incidence_files_valps,readRDS))
incidence_valp_df<-incidence_valp_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                        "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                        "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(incidence_valp_df,paste0(combined_dir, "Valproate_Incidence_stratified_combined_age-indication.rds") )
write.csv(incidence_valp_df,paste0(combined_dir, "Valproate_Incidence_stratified_combined_age-indication.csv"))
rm(incidence_files_valps,incidence_valp_df)
# 3. DISCONTINUED
disc_files<-list.files(stratified_dir, pattern="discontinued", full.names = T)
disc_files_valps<-disc_files[grepl(c("Valproate"), disc_files)]
disc_valp_df<-do.call(cbind,lapply(disc_files_valps, readRDS))
disc_valp_df<-disc_valp_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                              "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown",
                              "N_tx_duration_1_182","Proportion_tx_duration_1_182","N_tx_duration_182_365","Proportion_tx_duration_182_365","N_tx_duration_over365", "Proportion_tx_duration_over365",
                              "N_reason_discontinue_pregwish", "Proportion_reason_discontinue_pregwish","N_reason_discontinue_pregnancy","Proportion_reason_discontinue_pregnancy","N_reason_discontinue_ADR","Proportion_reason_discontinue_ADR", "N_reason_discontinue_multiple", "Proportion_reason_discontinue_multiple", "N_reason_discontinue_unknown","Proportion_reason_discontinue_unknown")]

saveRDS(disc_valp_df,paste0(combined_dir,"Valproate_Discontinued_stratified_combined_age-indication-tx_dur-reason.rds") )
write.csv(disc_valp_df,paste0(combined_dir,"Valproate_Discontinued_stratified_combined_age-indication-tx_dur-reason.csv"))
rm(disc_files_valps,disc_valp_df)
# 4. SWITCHED
switched_files <- list.files(stratified_dir, pattern="switched", full.names = T)
switched_files_valps <- switched_files[grepl(c("Valproate"), switched_files)]
switched_valp_df<-do.call(cbind,lapply(switched_files_valps ,readRDS))
switched_valp_df<-switched_valp_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                      "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                      "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(switched_valp_df,paste0(combined_dir, "Valproate_Switched_to_Alt_Meds_stratified_combined_age-indication.rds") )
write.csv(switched_valp_df,paste0(combined_dir, "Valproate_Switched_to_Alt_Meds_stratified_combined_age-indication.csv"))
rm(switched_files_valps ,switched_valp_df)

# 5. CONTRA PRIOR
contra_prior_files <- list.files(stratified_dir, pattern="contraception_prior", full.names = T)
contra_prior_files_valps <- contra_prior_files[grepl(c("Valproate"), contra_prior_files)]
contra_prior_valp_df<-do.call(cbind,lapply(contra_prior_files_valps,readRDS))
contra_prior_valp_df<-contra_prior_valp_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                              "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                              "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown")]
saveRDS(contra_prior_valp_df,paste0(combined_dir, "Valproate_Contra_Prior_stratified_combined_age-indication.rds") )
write.csv(contra_prior_valp_df,paste0(combined_dir, "Valproate_Contra_Prior_stratified_combined_age-indication.csv"))
rm(contra_prior_files_valps,contra_prior_valp_df)
# 6. MED USE DURING CONTRA EPISODE
med_use_during_contra_files <- list.files(stratified_dir, pattern="med_use_during_contraception_episodes|med_use_during_contra_epis", full.names = T)
med_use_during_contra_files_valps <- med_use_during_contra_files[grepl(c("Valproate"), med_use_during_contra_files)]
med_use_during_contra_valp_df<-do.call(cbind,lapply(med_use_during_contra_files_valps,readRDS))
med_use_during_contra_valp_df<-med_use_during_contra_valp_df[,c("YM","N_overall","Freq_overall","Rate_overall",
                                                                "N_age_12_21","Proportion_age_12_21","N_age_21_31","Proportion_age_21_31","N_age_31_41","Proportion_age_31_41","N_age_41_56", "Proportion_age_41_56",
                                                                "N_indication_bipolar", "Proportion_indication_bipolar","N_indication_epilepsy", "Proportion_indication_epilepsy","N_indication_migraine", "Proportion_indication_migraine","N_indication_multiple", "Proportion_indication_multiple","N_indication_unknown", "Proportion_indication_unknown",
                                                                "N_contraceptive_fixedcomb", "Proportion_contraceptive_fixedcomb","N_contraceptive_patch","Proportion_contraceptive_patch","N_contraceptive_progest","Proportion_contraceptive_progest", "N_contraceptive_sequenprep","Proportion_contraceptive_sequenprep","N_contraceptive_vaginalring","Proportion_contraceptive_vaginalring", "N_contraceptive_IUD","Proportion_contraceptive_IUD",  "N_contraceptive_injection", "Proportion_contraceptive_injection","N_contraceptive_implant", "Proportion_contraceptive_implant")]
saveRDS(med_use_during_contra_valp_df,paste0(combined_dir, "Valproate_Med_Use_During_Contra_Episode_stratified_combined_age_indication_contratype.rds") )
write.csv(med_use_during_contra_valp_df,paste0(combined_dir, "Valproate_Med_Use_During_Contra_Episode_stratified_combined_age_indication_contratype.csv"))
rm(med_use_during_contra_files_valps,med_use_during_contra_valp_df)

