# RecoverAllRecordsOfAPregnanciesList
# Authors: Giorgio Limoncella, Claudia Bartolini, Romin Pajouheshnia

# This script supports post-hoc analyses of the pregnancies during which a retinoid or valproate was prescribed.
# It should be run after the pregnancy and analysis scripts

# Input: it extracts the data frame of pregnancies where there was a prescription/dispensing of valproate or retinoid
# Output: it saves a .csv file with all records relating to pregnancy that were used in the pregnancy script to identify the pregnancies in the input data. This permits groups to review the records
# The script will be updated in a later release to provide more analyses of these records.

library(data.table)

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###############################################
### Below you must source the following
source("99_path.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir,"functions/RecoverAllRecordsOfAPregnanciesList.R"))

### Choose study type
# study_type <- "Retinoid"
 study_type <- "Valproate"
# study_type <- "Both" # NOTE: If you are participating in both the retinoids and valproate studies but only detect records of one or the other during pregnancies, the script will fail if you select "both". In such a case, select your study type as either "Retinoid" or "Valproate" depending on which medicine you identified/dispensed as being prescribed during a pregnancy (for which there is corresponding a file in g_intermediate\counts_dfs)

#user input parameter
# Chose format to save files 
my_format <- "csv"
# my_format <- "xlsx"
 
# Select the pregnancy IDs where there were records of valproate/retinoid dispensing
# Create a data.table/data.frame with the pregnancy_id to be examined using this structure

if (study_type=="Retinoid") {
  pregdata <- readRDS(paste0(projectFolder,"/g_intermediate/counts_dfs/ALL_Retinoid_med_use_during_pregnancy.rds"))
  dataset_of_pregnancy_to_be_checked <- data.table(pregnancy_id = pregdata$pregnancy_id)
  
  #Extract all relevant records to validate the pregnancy and save a CSV file
  DatasetPregnancyRecords <- RecoverAllRecordsOfAPregnanciesList(DatasetInput = dataset_of_pregnancy_to_be_checked,
                                                                 PregnancyIdentifierVariable = "pregnancy_id",
                                                                 DirectoryPregnancyScript = paste0(dir_base,"/ConcePTIONAlgorithmPregnancies-version_2.0"),
                                                                 DatasourceNameConceptionCDM = "ARS",
                                                                 SaveOutputInCsv = TRUE,
                                                                 SaveOriginalSampleInCsv = FALSE,
                                                                 DirectoryOutputCsv = paste0(dir_base,"/LOT4_scripts/g_intermediate/counts_dfs"),
                                                                 anonymous = FALSE,
                                                                 validation_variable = FALSE,
                                                                 output_file_name = "retin")
}

if (study_type=="Valproate") {
  pregdata <- readRDS(paste0(projectFolder,"/g_intermediate/counts_dfs/ALL_Valproate_med_use_during_pregnancy.rds"))
  dataset_of_pregnancy_to_be_checked <- data.table(pregnancy_id = pregdata$pregnancy_id)
  
  #Extract all relevant records to validate the pregnancy and save a CSV file
  DatasetPregnancyRecords <- RecoverAllRecordsOfAPregnanciesList(DatasetInput = dataset_of_pregnancy_to_be_checked,
                                                                 PregnancyIdentifierVariable = "pregnancy_id",
                                                                 DirectoryPregnancyScript = paste0(dir_base,"/ConcePTIONAlgorithmPregnancies-version_2.0"),
                                                                 DatasourceNameConceptionCDM = "ARS",
                                                                 SaveOutputInCsv = TRUE,
                                                                 SaveOriginalSampleInCsv = FALSE,
                                                                 DirectoryOutputCsv = paste0(dir_base,"/LOT4_scripts/g_intermediate/counts_dfs"),
                                                                 anonymous = FALSE,
                                                                 validation_variable = FALSE,
                                                                 output_file_name = "valp")
}

if (study_type=="Both") {
  pregdata_retin <- readRDS(paste0(projectFolder,"/g_intermediate/counts_dfs/ALL_Retinoid_med_use_during_pregnancy.rds"))
  dataset_of_pregnancy_to_be_checked_retin <- data.table(pregnancy_id = pregdata_retin$pregnancy_id)
  pregdata_valp <- readRDS(paste0(projectFolder,"/g_intermediate/counts_dfs/ALL_Valproate_med_use_during_pregnancy.rds"))
  dataset_of_pregnancy_to_be_checked_valp <- data.table(pregnancy_id = pregdata_valp$pregnancy_id)
  
  #Extract all relevant records to validate the pregnancy and save a CSV file
  DatasetPregnancyRecords_retin <- RecoverAllRecordsOfAPregnanciesList(DatasetInput = dataset_of_pregnancy_to_be_checked_retin,
                                                                 PregnancyIdentifierVariable = "pregnancy_id",
                                                                 DirectoryPregnancyScript = paste0(dir_base,"/ConcePTIONAlgorithmPregnancies-version_2.0"),
                                                                 DatasourceNameConceptionCDM = "ARS",
                                                                 SaveOutputInCsv = TRUE,
                                                                 SaveOriginalSampleInCsv = FALSE,
                                                                 DirectoryOutputCsv = paste0(dir_base,"/LOT4_scripts/g_intermediate/counts_dfs"),
                                                                 anonymous = FALSE,
                                                                 validation_variable = FALSE,
                                                                 output_file_name = "retin")
  
  DatasetPregnancyRecords_retin <- RecoverAllRecordsOfAPregnanciesList(DatasetInput = dataset_of_pregnancy_to_be_checked_valp,
                                                                       PregnancyIdentifierVariable_valp = "pregnancy_id",
                                                                       DirectoryPregnancyScript = paste0(dir_base,"/ConcePTIONAlgorithmPregnancies-version_2.0"),
                                                                       DatasourceNameConceptionCDM = "ARS",
                                                                       SaveOutputInCsv = TRUE,
                                                                       SaveOriginalSampleInCsv = FALSE,
                                                                       DirectoryOutputCsv = paste0(dir_base,"/LOT4_scripts/g_intermediate/counts_dfs"),
                                                                       anonymous = FALSE,
                                                                       validation_variable = FALSE,
                                                                       output_file_name = "valp")
  
  
}

# Clean up empty folders in g_intermediate and g_output made by re-running 99_path.
 
# Removes csv/xlsx, plots and monthly counts folders from LOT4_script (after everything has been copied to corresponding folders)
for (file in list.files(path=paste0(output_dir), pattern=paste0(c("plots", paste0(my_format,"_files"), "denominator", "monthly_counts"), collapse="|"), ignore.case = T)){unlink(paste0(output_dir,file), recursive = TRUE)}
# Deletes temp files
for(file in list.files(path = tmp, pattern ="events_")){unlink(paste0(tmp, file), recursive = TRUE)}
 