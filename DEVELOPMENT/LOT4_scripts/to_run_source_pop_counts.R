rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###############################################

source("99_path.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir,"info.R"))
source(paste0(pre_dir,"study_parameters.R"))
setwd(projectFolder)

#################################################
#Study_source_population
#################################################
source(paste0(pre_dir,"study_source_population_script.R"))
source(paste0(pre_dir,"denominator_monthly.R"))
source(paste0(pre_dir,"monthly_counts_dxcodes.R"))
source(paste0(pre_dir,"monthly_counts_ATC.R"))

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
# my_format<-"xlxs"

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
