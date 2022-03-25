#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 21/03/2022

# THIS IS A REDUCED VERION OF THE SCRIPT (V4.1) which runs corrected counts using information already generated from the 02 script, which is correct.

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###############################################
### NOTE FOR DAPS: If you have run the preselection script and would like to use the subsetted data sets that it produces instead of your full ETL'd data files, you need to go to the "99_path.R" file and choose the second path option, by adding a "#" symbol at the start of line 7, and removing the "#" symbol at the start of line 8. If the preselection files have been stored elsewhere, then the path will need to be set manually.

### Please indicate DAP NAME 
DAP_name <- "ARS"
# DAP_name <- "BIFAP"
# DAP_name <- "CASERTA"
# DAP_name <- "CPRD"
# DAP_name <- "DNR"
# DAP_name <- "FISABIO" 
# DAP_name <- "PHARMO"
#user input parameter

### Below you must set
source("99_path.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir,"set_DAP_params.R"))

#user input parameter
## Choose study type

# study_type <- "Retinoid"
# study_type <- "Valproate"
study_type <- "Both"

#user input parameter
## Turn statement to T if multiple regions #BIFAP
multiple_regions <- F
# multiple_regions <- T # BIFAP
# multiple_regions_dir <- paste0(path_dir, "BIFAP/")
#user input parameter
## Turn the statement to T instead of = F if data has sub populations #BIFAP
SUBP <- F
# SUBP <- T

#user input parameter
## MASKING 
# mask <- T
mask <- F

#user input parameter
# Chose format to save files 
my_format <- "csv"
# my_format <- "xlsx"

#user input parameter
### Sensitivity analysis ###
# Set discontinuation period
discontinuation_window <- 90
# discontinuation_window <- 30
#user input parameter
# Set contraceptives look back window
contraceptives_window <- 90
# contraceptives_window <- 30
#user input parameter
# Set DAP_specific assumed treatment duration value
# DAP_specific_DOT <- F   #### assumed treatment duration for creating treatment episodes == 30
DAP_specific_DOT <- T   #### assumed treatment duration for creating treatment episodes is DAP specific 

############[PLEASE NOTE] ############################################
# THERE ARE NO DAP SPECIFIC ASSUMED DURATION VALUES FOR:
# - ARS (use default 30 days) 
############[PLEASE NOTE] ###########################################

################################################
# Final counts + plots
#################################################
source(paste0(pre_dir,"run_counts_final.R"))
# BIFAP ONLY pooled masking
# run_pooling(mask= T)
###########################################
# clear g_intermediate 
#set to TRUE to clear out intermediate files PLEASE REPLACE T WITH F IF YOU WANT TO SAVE INTERMEDIATE DATA SETS, I.E. TO REDUCE AMOUNT OF STORED DATA"
clear_int_files <- F
#user input parameter

if(clear_int_files==T){
  unlink(paste0(g_intermediate, "/tmp"), recursive = TRUE)
  unlink(paste0(g_intermediate, "/populations"), recursive = TRUE)
}



