#Author: Magda Gamba M.D.,Ema Alsina MSc.
#email: m.a.gamba@uu.nl,e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 31/01/2022

#This script sets and saves paths to folders needed for all subsequent scripts
# setwd('..') #in Data Characterisation

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

setwd('..') #in ConcePTION
dir_base<-getwd()
# set the name of the study
StudyName <- "LOT4"
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")
# path_dir<-paste0(dir_base,"/CDMInstances_preselect/") # use this option if you want to use the preselection files
# path<-path_dir

# Checks if folders exist. If they do not, creates them 
# Main folders (g_intermediate, g_output)
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate")), dir.create(paste0(projectFolder, "/g_intermediate")), FALSE))
g_intermediate <- paste0(projectFolder, "/g_intermediate/")
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output")), dir.create(paste0(projectFolder, "/g_output")), FALSE))
output_dir     <- paste0(projectFolder, "/g_output/")
# Sets path to p_steps (to read codelists)
pre_dir        <- paste0(projectFolder,"/p_steps/")
# folders + paths in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/populations")), dir.create(paste0(g_intermediate, "/populations")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/tmp", sep="")), dir.create(paste0(g_intermediate, "/tmp")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/counts_dfs", sep="")), dir.create(paste0(g_intermediate, "/counts_dfs")), FALSE))
counts_dfs_dir <-paste0(g_intermediate,"counts_dfs/")
# Folders in g_intermediate -> tmp
# CONCEPT SET FOLDERS
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_dx")), dir.create(paste0(tmp, "conceptsets_dx")), FALSE))
conceptsets_DX_dir<-paste0(tmp, "conceptsets_dx/")
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_atc")), dir.create(paste0(tmp, "conceptsets_atc")), FALSE))
conceptsets_ATC_dir<-paste0(tmp, "conceptsets_atc/")
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_proc")), dir.create(paste0(tmp, "conceptsets_proc")), FALSE))
conceptsets_PROC_dir<-paste0(tmp, "conceptsets_proc/")

# EVENTS TABLES FOLDERS
# Temporary folder 
invisible(ifelse(!dir.exists(paste0(tmp, "events_dx")), dir.create(paste0(tmp, "events_dx")), FALSE))
events_tmp_DX<-paste0(tmp, "events_dx/")
# Permanent folder
invisible(ifelse(!dir.exists(paste0(tmp, "diagnoses")), dir.create(paste0(tmp, "diagnoses")), FALSE))
diagnoses_pop <- paste0(tmp, "diagnoses/")
# Monthly counts
invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_dxcodes")), dir.create(paste0(output_dir, "monthly_counts_dxcodes")), FALSE))
monthly_counts_dx <- paste0(output_dir, "monthly_counts_dxcodes")

# MEDICINES TABLES FOLDER
# Temporary folder 
invisible(ifelse(!dir.exists(paste0(tmp, "events_atc")), dir.create(paste0(tmp, "events_atc")), FALSE))
events_tmp_ATC <- paste0(tmp, "events_atc/")
# Permanent folder
invisible(ifelse(!dir.exists(paste0(tmp, "medications")), dir.create(paste0(tmp, "medications")), FALSE))
medications_pop <- paste0(tmp, "medications/")
# Monthly counts
invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_atc")), dir.create(paste0(output_dir, "monthly_counts_atc")), FALSE))
monthly_counts_atc <- paste0(output_dir, "monthly_counts_atc")

# PROCEDURES TABLES FOLDERS
# There may or may not be procedures tables 
# Temporary folder
invisible(ifelse(!dir.exists(paste0(tmp, "events_proc")), dir.create(paste0(tmp, "events_proc")), FALSE))
events_tmp_PROC <- paste0(tmp, "events_proc/")
invisible(ifelse(!dir.exists(paste0(tmp, "events_proc_dxcodes")), dir.create(paste0(tmp, "events_proc_dxcodes")), FALSE))
events_tmp_PROC_dxcodes <- paste0(tmp, "events_proc_dxcodes/")
# Permanent folder
invisible(ifelse(!dir.exists(paste0(tmp, "procedures")), dir.create(paste0(tmp, "procedures")), FALSE))
procedures_pop <- paste0(tmp, "procedures/")
invisible(ifelse(!dir.exists(paste0(tmp, "procedures_dxcodes")), dir.create(paste0(tmp, "procedures_dxcodes")), FALSE))
procedures_dxcodes_pop <- paste0(tmp, "procedures_dxcodes/")
# Monthly counts
invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_proc")), dir.create(paste0(output_dir, "monthly_counts_proc")), FALSE))
monthly_counts_proc <- paste0(output_dir, "monthly_counts_proc")
invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_proc_dxcodes")), dir.create(paste0(output_dir, "monthly_counts_proc_dxcodes")), FALSE))
monthly_counts_proc_dxcodes <- paste0(output_dir, "monthly_counts_proc_dxcodes")


# STERILITY FOLDERS
# Temporary folder
invisible(ifelse(!dir.exists(paste0(tmp, "events_sterility")), dir.create(paste0(tmp, "events_sterility")), FALSE))
events_tmp_sterility <- paste0(tmp, "events_sterility/")
# Permanent folder
invisible(ifelse(!dir.exists(paste0(tmp, "sterility")), dir.create(paste0(tmp, "sterility")), FALSE))
sterility_pop <- paste0(tmp, "sterility/")

# PLOT FOLDER
invisible(ifelse(!dir.exists(paste0(output_dir, "plots")), dir.create(paste0(output_dir, "plots")), FALSE))
plot_folder <- paste0(output_dir, "plots")

# MAIN OUTPUT FOLDERS
# 1. PRELIMINARY COUNTS 
invisible(ifelse(!dir.exists(paste0(output_dir, "preliminary_counts")), dir.create(paste0(output_dir, "preliminary_counts")), FALSE))
preliminary_counts_dir <- paste0(output_dir, "preliminary_counts")
# 2. BASELINE TABLES 
invisible(ifelse(!dir.exists(paste0(output_dir, "baseline_tables")), dir.create(paste0(output_dir, "baseline_tables")), FALSE))
baseline_tables_dir <- paste0(output_dir, "baseline_tables")
# 3. MED USE DURING PREGNANCY 
invisible(ifelse(!dir.exists(paste0(output_dir, "pregnancy_counts")), dir.create(paste0(output_dir, "pregnancy_counts")), FALSE))
preg_med_counts_dir <- paste0(output_dir, "pregnancy_counts")

# 4. CONTRACEPTIVE COUNTS 
invisible(ifelse(!dir.exists(paste0(output_dir, "contraceptive_counts")), dir.create(paste0(output_dir, "contraceptive_counts")), FALSE))
contraceptive_counts_dir <- paste0(output_dir, "contraceptive_counts")
# 5. MEDICINES COUNTS 
invisible(ifelse(!dir.exists(paste0(output_dir, "medicines_counts")), dir.create(paste0(output_dir, "medicines_counts")), FALSE))
medicines_counts_dir <- paste0(output_dir, "medicines_counts")
# 6. PREGNANCY TEST COUNTS 
invisible(ifelse(!dir.exists(paste0(output_dir, "pregnancy_test_counts")), dir.create(paste0(output_dir, "pregnancy_test_counts")), FALSE))
pregnancy_test_counts_dir <- paste0(output_dir, "pregnancy_test_counts")

# Path to pregnancy records (created by running pregnancy script) 
preg_dir <-paste0(dir_base, "/ConcePTIONAlgorithmPregnancies-version_2.0/")

# Move stratified records into stratified folders
# Create stratified folder
invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/","stratified")), dir.create(paste0(medicines_counts_dir,"/","stratified")), FALSE))
medicines_stratified_dir <- paste0(medicines_counts_dir,"/","stratified")
# Create stratified by age groups folder
invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","age_group")), dir.create(paste0(medicines_stratified_dir,"/","age_group")), FALSE))
medicines_stratified_age_groups <- paste0(medicines_stratified_dir ,"/","age_group")
# Create stratified by tx_duration folder 
invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","tx_duration")), dir.create(paste0(medicines_stratified_dir,"/","tx_duration")), FALSE))
medicines_stratified_tx_dur <- paste0(medicines_stratified_dir ,"/","tx_duration")
# Create stratified by indication folder 
invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","indication")), dir.create(paste0(medicines_stratified_dir,"/","indication")), FALSE))
medicines_stratified_indication <- paste0(medicines_stratified_dir ,"/","indication")
# Create stratified by indication folder 
invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","reasons_for_discontinuation")), dir.create(paste0(medicines_stratified_dir,"/","reasons_for_discontinuation")), FALSE))
medicines_stratified_reasons <- paste0(medicines_stratified_dir ,"/","reasons_for_discontinuation")
# Create stratified by contraception type folder
invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","contra_type")), dir.create(paste0(medicines_stratified_dir,"/","contra_type")), FALSE))
medicines_stratified_contra_type <- paste0(medicines_stratified_dir ,"/","contra_type")



# Move stratified records into stratified folders
# Create stratified folder
invisible(ifelse(!dir.exists(paste0(contraceptive_counts_dir,"/","stratified")), dir.create(paste0(contraceptive_counts_dir,"/","stratified")), FALSE))
contraceptives_stratified_dir <- paste0(contraceptive_counts_dir,"/","stratified")
# Create stratified by age groups folder
invisible(ifelse(!dir.exists(paste0(contraceptives_stratified_dir,"/","age_group")), dir.create(paste0(contraceptives_stratified_dir,"/","age_group")), FALSE))
contraceptives_stratified_age_groups <- paste0(contraceptives_stratified_dir ,"/","age_group")
# Create stratified by indication folder
invisible(ifelse(!dir.exists(paste0(contraceptives_stratified_dir,"/","indication")), dir.create(paste0(contraceptives_stratified_dir,"/","indication")), FALSE))
contraceptives_stratified_indication <- paste0(contraceptives_stratified_dir ,"/","indication")



