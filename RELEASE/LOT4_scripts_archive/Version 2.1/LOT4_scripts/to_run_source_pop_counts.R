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

#user input parameter
mask<-T
# mask <- F

#user input parameter
# Analysis for multiple regions # BIFAP
multiple_regions = F
# multiple_regions = T # BIFAP
# multiple_regions_dir <- paste0(path_dir, "BIFAP/")

#user input parameter
# Chose format to save files 
my_format<- "csv"
#my_format<-"xlsx"

#################################################
#Study_source_population + counts + plots
#################################################
source(paste0(pre_dir,"run_analysis.R"))


# # ############################################
# # #clear g_intermediate 
# # #set to TRUE to clear out intermediate files PLEASE REPLACE T WITH F IF YOU WANT TO SAVE INTERMEDIATE DATA SETS, I.E. TO REDUCE AMOUNT OF STORED DATA"
clear_int_files<-F
#user input parameter

if(clear_int_files==T){
  unlink(paste0(g_intermediate, "/tmp"), recursive = TRUE)
  unlink(paste0(g_intermediate, "/populations"), recursive = TRUE)
}




