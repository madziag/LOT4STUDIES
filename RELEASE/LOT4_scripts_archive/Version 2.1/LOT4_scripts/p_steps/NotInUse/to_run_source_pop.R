rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###############################################
### NOTE FOR DAPS: If you have run the preselection script and would like to use the subsetted data sets that it produces instead of your full ETL'd data files, you need to go to the "99_path.R" file and choose the second path option, by adding a "#" symbol at the start of line 7, and removing the "#" symbol at the start of line 8. If the preselection files have been stored elsewhere, then the path will need to be set manually.

### Below you must set
source("99_path.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir,"info.R"))
source(paste0(pre_dir,"study_parameters.R"))
setwd(projectFolder)

#################################################
#Study_source_population
#################################################
source(paste0(pre_dir,"study_source_population_script.R"))
source(paste0(pre_dir,"CreateSterilityList.R"))
source(paste0(pre_dir, "CreateEntryExit.R"))

