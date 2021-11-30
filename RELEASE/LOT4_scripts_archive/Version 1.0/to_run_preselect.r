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

source(paste0(projectFolder, "/p_steps/preselect.r"))

#writes new files to CDMInstances_preselect
