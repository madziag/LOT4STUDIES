rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###############################################
### NOTE FOR DAPS: If you have run the preselection script and would like to use the subsetted data sets that it produces instead of your full ETL'd data files, you need to go to the "99_path.R" file and choose the second path option, by adding a "#" symbol at the start of line 7, and removing the "#" symbol at the start of line 8. If the preselection files have been stored elsewhere, then the path will need to be set manually.

### Below you must set
source("paths_shortcut.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir,"info.R"))
source(paste0(pre_dir,"study_parameters.R"))
setwd(projectFolder)

#################################################
#COUNTS
#################################################

source(paste0(pre_dir,"denominator_monthly.R"))
source(paste0(pre_dir,"monthly_counts_dxcodes.R"))
source(paste0(pre_dir,"monthly_counts_ATC.R"))
source(paste0(pre_dir,"monthly_counts_procedures.R"))

source(paste0(pre_dir,"CreateBaselineTables.R"))

##############################################
#plot output
#############################################
#user input parameter
mask<-T

#to see unmasked plots, set mask<-F
source(paste0(pre_dir,"plots.R"))

############################################
#user input parameter
my_format<- "csv"
#my_format<-"xlsx"

source(paste0(pre_dir,"write_output.R"))

############################################
#clear g_intermediate
#set to TRUE to clear out intermediate files PLEASE REPLACE T WITH F IF YOU WANT TO SAVE INTERMEDIATE DATA SETS, I.E. TO REDUCE AMOUNT OF STORED DATA"
clear_int_files<-F
#user input parameter

if(clear_int_files==T){
unlink(paste0(g_intermediate, "/tmp"), recursive = TRUE)
unlink(paste0(g_intermediate, "/populations"), recursive = TRUE)  
}

