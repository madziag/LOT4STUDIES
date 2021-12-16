# rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###############################################
### NOTE FOR DAPS: If you have run the preselection script and would like to use the subsetted data sets that it produces instead of your full ETL'd data files, you need to go to the "99_path.R" file and choose the second path option, by adding a "#" symbol at the start of line 7, and removing the "#" symbol at the start of line 8. If the preselection files have been stored elsewhere, then the path will need to be set manually.

## Turn the statement below to = T instead of = F if your data has sub populations (BIFAP)
SUBP = F

### Below you must set
source("99_path.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir,"info.R"))
source(paste0(pre_dir,"study_parameters.R"))

#user input parameters
## MULTIPLE REGIONS
multiple_regions = F
# multiple_regions = T # BIFAP
# multiple_regions_dir <- paste0(path_dir, "BIFAP/")
## STUDY TYPE 
# study_type <- "Retinoids"
#study_type <- "Valproates"
study_type <- "Both"
## MASKING 
mask<-T
# mask <- F
# Source script which sources the following-
## Source Baseline table
source(paste0(pre_dir,"run_all_counts_final.R"))
## Source treatment episodes
## Source create study variables
## Source counts script


