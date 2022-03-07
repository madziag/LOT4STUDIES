#Author: Magda Gamba M.D.,Ema Alsina MSc.
#email: m.a.gamba@uu.nl, e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 31/01/2022

# sources preselect script which applies very simple exclusion criteria to reduce data size for the study to source step

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

########################################################################################################################
#first rounds of elligibility criteria
#1) sex == FEMALE
#2) date of birth 
#3) contain elligible base ATC codes 
###############################################
source("99_path.R")
source(paste0(projectFolder, "/p_steps/preselect.R"))

#writes new files to CDMInstances_preselect
