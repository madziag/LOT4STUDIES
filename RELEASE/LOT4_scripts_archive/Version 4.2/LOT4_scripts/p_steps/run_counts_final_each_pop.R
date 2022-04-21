#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 16/02/2022

# VERSION 3.2.1 <- THIS HAS SOME SCRIPTS COMMENTED OUT IN ORDER TO SPEED UP LAST MINUTE RE-RUN OF TO_RUN_FINAL_COUNTS.R. FOR VERSION 3.3, the commented out sourced scripts need to be uncommented so they can be re-run one final time.

# Takes into account subpopulations 
# Loads records with Retinoid/Valproate use (depending on study type)
# Loads study_population 
# Runs individual scripts for each subpopulation
# Result: If SUBP -> TRUE then each folder will contain (if present) results coming from all indicated subpops. Resulting files are prefixed with the name of the subpop

# Loads study population/populations 
populations <- list.files(populations_dir, pattern = "study_population")
# Checks for study type
if(study_type == "Retinoid"){
  pattern_meds = c("Retinoid")
} else if (study_type == "Valproate") {
  pattern_meds = c("Valproate")
} else if (study_type == "Both") {
  pattern_meds = c("Retinoid","Valproate")
} else {
  print ("Please indicate Study Type")
}

# Move denominator file to tmp folder 
for(file in list.files(path=output_dir, pattern="denominator", ignore.case = T)){file.move(paste0(output_dir,file), paste0(paste0(tmp, "/") ,file))}


if (is_Denmark == T){
  
  for(pop in 1:length(populations)){
    # Loads files with matching pattern 
    med_files <- list.files(path=medications_pop, pattern=paste0(pattern_meds, collapse="|"))
    # Reads in records of population with indicated study type
    study_pop_meds <- do.call(rbind,lapply(paste0(medications_pop,"/",med_files), readRDS))
    # Loads study population
    study_population <- readRDS(paste0(populations_dir, populations))
    # Assign study population prefix name
    pop_prefix <- gsub("_study_population.rds", "", populations)
    # Creates baseline tables 
    source(paste0(pre_dir,"CreateBaselineTables.R"))
    # Creates Retinoid/Valproate treatment episodes 
    source(paste0(pre_dir, "treatment_episodes.R"))
    # Creates KM plots 
    source(paste0(pre_dir, "KaplanMeier.R")) # COMMENT THIS OUT WITH "#" if you get an error and rerun
    # Creates contraceptive record with all contraceptives and their respective duration (for use in creating contraception treatment episodes)
    source(paste0(pre_dir, "contraception_duration.R"))
    # Creates contraception treatment episodes 
    source(paste0(pre_dir, "treatment_episodes_contracep.R"))
    # Counts of prevalence, incidence, discontinuation - medicines use 
    source(paste0(pre_dir, "medicine_counts_incidence_prevalence_discontinuation.R"))
    # Counts of discontinuation, stratified by reason for discontinuation
    source(paste0(pre_dir, "reasons_for_discontinuation.R"))
    # Counts of contraception records within 90 days before medication record 
    source(paste0(pre_dir, "contraceptive_use_within_90_days_of_medicine_use_counts.R"))
    # Counts of medicine records during contraception episodes
    source(paste0(pre_dir, "med_use_during_contraception_episode_counts.R"))
    # Alternative medicine counts
    source(paste0(pre_dir, "altmeds_final_counts.R"))
    # Counts of patients who switched from Retinoid/Valproate use to alt med use
    source(paste0(pre_dir, "switched_to_alt_meds_counts.R"))
    # Converts all .rds files into .csv or .xlsx (indicated by user)
    source(paste0(pre_dir, "write_output.R"))
  }
} else {
  # Loops over each subpopulation
  for(pop in 1:length(populations)){
    # Loads study population
    study_population <- readRDS(paste0(populations_dir, populations[pop]))
    # Make sure last exit data is 2019 if DAP == "PHARMO"
    if (is_PHARMO){study_population <- study_population[year(study_population$exit_date) < 2020,]} else {study_population <- study_population}
    # Assign study population prefix name
    pop_prefix <- gsub("_study_population.rds", "", populations[pop])
    # Loads files with matching pattern 
    med_files <- list.files(path=medications_pop, pattern=paste0(pattern_meds, collapse="|"))
    if(pop_prefix == "PC"){med_files <- med_files[!grepl("PC_HOSP",med_files)]}
    if(pop_prefix == "PC_HOSP"){med_files <- med_files[grepl("PC_HOSP",med_files)]}
    
    if(length(med_files)>0){
      # Reads in records of population with indicated study type
      study_pop_meds <- do.call(rbind,lapply(paste0(medications_pop,"/",med_files), readRDS))
      # Creates baseline tables #
#      source(paste0(pre_dir,"CreateBaselineTables.R"))
      # Creates Retinoid/Valproate treatment episodes #
      source(paste0(pre_dir, "treatment_episodes.R"))
      # Creates KM plots 
#      source(paste0(pre_dir, "KaplanMeier.R")) # COMMENT THIS OUT WITH "#" if you get an error and rerun
      # Creates contraceptive record with all contraceptives and their respective duration (for use in creating contraception treatment episodes)
      source(paste0(pre_dir, "contraception_duration.R"))
      # Creates contraception treatment episodes 
      source(paste0(pre_dir, "treatment_episodes_contracep.R"))
      # Counts of prevalence, incidence, discontinuation - medicines use 
      source(paste0(pre_dir, "medicine_counts_incidence_prevalence_discontinuation.R"))
      # Counts of discontinuation, stratified by reason for discontinuation
      source(paste0(pre_dir, "reasons_for_discontinuation.R"))
      # Counts of pregnancy tests within 90 days before/after medication record 
      source(paste0(pre_dir, "pregnancy_tests_within_90_days_of_medicine_use_counts.R"))
      # Counts of contraception records within 90 days before medication record 
      source(paste0(pre_dir, "contraceptive_use_within_90_days_of_medicine_use_counts.R"))
      # Counts of medicine records during contraception episodes 
      source(paste0(pre_dir, "med_use_during_contraception_episode_counts.R"))
      # Counts of all pregnancies # No rates, only counts, not stratified by subpops
      source(paste0(pre_dir, "all_pregnancies_counts.R")) 
      # Counts of pregnancies started during a treatment episode 
      source(paste0(pre_dir, "pregnancies_started_during_treatment_episode_counts.R"))
      # Counts of medicines used during a pregnancy
      source(paste0(pre_dir, "med_use_during_pregnancy_counts.R"))
      # Alternative medicine counts
      source(paste0(pre_dir, "altmeds_final_counts.R")) # Rates rounded up to decimal 5
      # Counts of patients who switched from Retinoid/Valproate use to alt med use
      source(paste0(pre_dir, "switched_to_alt_meds_counts.R"))
      # Makes plots of all counts files
      source(paste0(pre_dir, "plots_mask.R"))
      # Converts all .rds files into .csv or .xlsx (indicated by user)
      source(paste0(pre_dir, "write_output.R"))
    } else {
      print(paste0("There are no Retinoid/Valproate records for subpopulation: ", pop_prefix))
    }
  }
}

### Creates csv/xslx folders inside main folders ###
# Creates csv/xslx folder inside baseline tables, pregnancy counts and medicines counts folders
invisible(ifelse(!dir.exists(paste0(baseline_tables_dir,"/",my_format,"_files")), dir.create(paste0(baseline_tables_dir,"/",my_format,"_files")), FALSE))
baseline_tables_csv_xlsx <- paste0(baseline_tables_dir,"/",my_format,"_files")
# Create folder inside medicines folder for csv or excel file format
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/",my_format,"_files")), dir.create(paste0(medicines_counts_dir,"/",my_format,"_files")), FALSE))
medicines_counts_csv_xlsx <- paste0(medicines_counts_dir,"/",my_format,"_files")
# Create folder inside pregnancy test folder for csv or excel file format
invisible(ifelse(!dir.exists(paste0(pregnancy_test_counts_dir,"/",my_format,"_files")), dir.create(paste0(pregnancy_test_counts_dir,"/",my_format,"_files")), FALSE))
pregnancy_test_counts_csv_xlsx <- paste0(pregnancy_test_counts_dir,"/",my_format,"_files")
# Create folder inside contraceptives folder for csv or excel file format
invisible(ifelse(!dir.exists(paste0(contraceptive_counts_dir ,"/",my_format,"_files")), dir.create(paste0(contraceptive_counts_dir ,"/",my_format,"_files")), FALSE))
contraceptive_counts_csv_xlsx <- paste0(contraceptive_counts_dir ,"/",my_format,"_files")
# Create folder inside pregnancy folder for csv or excel file format
invisible(ifelse(!dir.exists(paste0(preg_med_counts_dir,"/",my_format,"_files")), dir.create(paste0(preg_med_counts_dir,"/",my_format,"_files")), FALSE))
preg_med_counts_csv_xlsx <- paste0(preg_med_counts_dir,"/",my_format,"_files")

### Creates plot folders inside main folders
# Create plots folder inside medicines counts folder
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/","plots")), dir.create(paste0(medicines_counts_dir,"/","plots")), FALSE))
medicines_counts_plots <- paste0(medicines_counts_dir,"/","plots")
# Create plots folder inside pregnancy test counts folder
invisible(ifelse(!dir.exists(paste0(pregnancy_test_counts_dir,"/","plots")), dir.create(paste0(pregnancy_test_counts_dir,"/","plots")), FALSE))
pregnancy_test_counts_plots <- paste0(pregnancy_test_counts_dir,"/","plots")
# Create plots folder inside contraceptives counts folder
invisible(ifelse(!dir.exists(paste0(contraceptive_counts_dir,"/","plots")), dir.create(paste0(contraceptive_counts_dir,"/","plots")), FALSE))
contraceptive_counts_plots <- paste0(contraceptive_counts_dir,"/","plots")
# Create plots folder inside pregnancy counts folder
invisible(ifelse(!dir.exists(paste0(preg_med_counts_dir,"/","plots")), dir.create(paste0(preg_med_counts_dir,"/","plots")), FALSE))
preg_med_counts_plots <- paste0(preg_med_counts_dir,"/","plots")

### Moves csv/xslx/plot files with matching pattern to corresponding folders
# baseline tables
for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="baseline", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file),baseline_tables_csv_xlsx)}
# medicine_counts_incidence_prevalence_discontinuation/med_use_during_contraception_episode_counts
for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern=paste0(c("prevalence", "incidence", "discontinued", "med_use_during_contraception_episodes", "switched_to_alt_meds", "alt_med_retin", "alt_med_valp"), collapse="|"), ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file), medicines_counts_csv_xlsx)}
#for (file in list.files(path=paste0(output_dir,"plots"), pattern=paste0(c("prevalence", "incidence", "discontinued", "med_use_during_contraception_episodes", "switched_to_alt_meds", "kaplan"), collapse="|"), ignore.case = T)){file.copy(paste0(output_dir,"plots/",file), medicines_counts_plots)}
for (file in list.files(path=paste0(output_dir,"plots"), pattern=paste0(c("prevalence", "incidence", "discontinued", "med_use_during_contraception_episodes", "switched_to_alt_meds","alt_med_retin", "alt_med_valp"), collapse="|"), ignore.case = T)){file.copy(paste0(output_dir,"plots/",file), medicines_counts_plots)}
# pregnancy_tests_within_90_days_of_medicine_use_counts
for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="pgtest", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file),pregnancy_test_counts_csv_xlsx)}
for (file in list.files(path=paste0(output_dir,"plots"), pattern="pgtest", ignore.case = T)){file.copy(paste0(output_dir,"plots/",file), pregnancy_test_counts_plots)}
# contraceptive_use_within_90_days_of_medicine_use_counts
for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="contraception_prior", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file), contraceptive_counts_csv_xlsx)}
for (file in list.files(path=paste0(output_dir,"plots"), pattern="contraception_prior", ignore.case = T)){file.copy(paste0(output_dir,"plots/",file), contraceptive_counts_plots)}
#pregnancies_started_during_treatment_episode_counts/med_use_during_pregnancy_counts/all_pregnancies_counts
for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="preg_starts_during_tx_episodes|med_use_during_pregnancy|all_pregnancies", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file),preg_med_counts_csv_xlsx )}
for (file in list.files(path=paste0(output_dir,"plots"), pattern="preg_starts_during_tx_episodes|med_use_during_pregnancy|all_pregnancies", ignore.case = T)){file.copy(paste0(output_dir,"plots/",file),preg_med_counts_plots )}

# Removes csv/xlsx, plots and monthly counts folders from LOT4_script (after everything has been copied to corresponding folders)
for (file in list.files(path=paste0(output_dir), pattern=paste0(c("plots", paste0(my_format,"_files"), "monthly_counts"), collapse="|"), ignore.case = T)){unlink(paste0(output_dir,file), recursive = TRUE)}

# Deletes temp files
for(file in list.files(path = tmp, pattern ="events_")){unlink(paste0(tmp, file), recursive = TRUE)}

