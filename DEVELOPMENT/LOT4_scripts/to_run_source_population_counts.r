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
source(paste0(pre_dir,"denominator.r"))
source(paste0(pre_dir,"monthly_counts_dxcodes.R"))
source(paste0(pre_dir,"monthly_counts_ATC.R"))

##############################################
#plot output
#############################################

mask<-T

#to see unmasked plots, set mask<-F
source(paste0(pre_dir,"LOT4plots.r"))

