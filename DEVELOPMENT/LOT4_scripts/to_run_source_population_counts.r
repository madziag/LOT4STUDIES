rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
#######################################################################################################
######################################################################################################
#set to TRUE to clear out intermediate files PLEASE REPLACE T WITH F IF YOU WANT TO SAVE INTERMEDIATE DATA SETS, I.E. TO REDUCE AMOUNT OF STORED DATA”
clear_int_files<-T
################################################

source("99_path.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir, "info.R"))
source(paste0(pre_dir,"study_parameters.R"))
setwd(projectFolder)

#################################################
#Study_source_population
#################################################
source(paste0(pre_dir,"study_source_population_script.R"))
source(paste0(pre_dir,"monthly_counts_dxcodes.R"))
source(paste0(pre_dir,"monthly_counts_ATC.R"))
source(paste0(pre_dir, "denominator.r"))
##############################################
#plot output
#############################################

mask<-T

#to see unmasked plots, set mask<-F

source(paste0(pre_dir,"LOT4plots.r"))

############################
#CLEAN UP
if(clear_int_files==T){
unlink(paste0(g_intermediate, "/tmp"), recursive = TRUE)
unlink(paste0(g_intermediate, "/populations"), recursive = TRUE)  
}
