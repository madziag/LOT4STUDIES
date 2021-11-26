rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
########################################################################################################################
#Specify all meanings that refer to birth registry in the SURVEY_ID table(if applicable) for identifying pregnancy
########################################################################################################################
#example meanings_birth_registry<-c("birth_registry", "birth_registry_meanings")
#keep in mind this records will be classified as end_of_pregnancy so no spontaneous abortion registry should be included
# meanings_birth_registry<-c("birth_registry_mother")
########################################################################################################################
#Specify all variables of interest to generate the Lifestyle report
########################################################################################################################
#Variables of interest:Smoking, Folic acid use, Alcohol abuse, BMI, SES
#1.Identify the CDM table you used to save the information about the variables of interest.
#2.Identify the original name of the variable of interest.
#3.Use the information above to complete the list below.
#4.CDM_table:name of the CDM table where you saved the information.
#5.CDM_column: name of the CDM column where you saved the information about the name of the variable of interest.
#6.value: name of the original variable.
#7.c.voc: name of the CDM column where you saved the vocabulary representing the variable of interest.If no vocabulary fill NULL
#8.v.voc: the vocabulary used for the variable of interest.If no vocabulary fill NULL
#9.v.date: name of the CDM column which saves the date of recording.
#10.If you don't have information about a variable then delete that section and use Lifestyle <- list()

###############################################

source("99_path.R")
source(paste0(pre_dir,"packages.R"))
source(paste0(pre_dir, "info.R"))
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

