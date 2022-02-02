# RecoverAllRecordsOfAPregnanciesList: example

# Create a data.table/data.frame with the pregnancy_id to be examined using this structure
library(data.table)

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###############################################
### Below you must set
source("99_path.R")
source(paste0(pre_dir,"packages.R"))

# Call the function
source(paste0(pre_dir,"functions/RecoverAllRecordsOfAPregnanciesList.R"))

# Select the pregnancy IDs where there were records of valproate/retinoid dispensing

pregdata <- readRDS(paste0(projectFolder,"/g_intermediate/counts_dfs/ALL_med_use_during_pregnancy.rds"))

dataset_of_pregnancy_to_be_checked <- data.table(pregnancy_id = pregdata$pregnancy_id)

DatasetPregnancyRecords <- RecoverAllRecordsOfAPregnanciesList(DatasetInput = dataset_of_pregnancy_to_be_checked,
                                                               PregnancyIdentifierVariable = "pregnancy_id",
                                                               DirectoryPregnancyScript = paste0(dir_base,"/ConcePTIONAlgorithmPregnancies-version_2.0"),
                                                               DatasourceNameConceptionCDM = "ARS",
                                                               SaveOutputInCsv = TRUE,
                                                               SaveOriginalSampleInCsv = FALSE,
                                                               DirectoryOutputCsv = "g_intermediate",
                                                               anonymous = FALSE,
                                                               validation_variable = FALSE)