rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

#######################################################################################################################
#preselection of the datasource
#######################################################################################################################
#If you want to presselect data in your data source based on age, time of entry, sex(study population) and atc codes
#for medicines set the preselect filter to "Yes" otherwise is set to "No"

pre_select<-"No"

########################################################################################################################
#Specify all meanings that refer to birth registry in the SURVEY_ID table(if applicable) for identifying pregnancy
########################################################################################################################
#example meanings_birth_registry<-c("birth_registry", "birth_registry_meanings")
#keep in mind this records will be classified as end_of_pregnancy so no spontaneous abortion registry should be included
meanings_birth_registry<-c("birth_registry_mother")
##################################################################################################

###############################################
source("packages.R")
source("99_path.R")
#source(paste0(pre_dir,"folders_preselect.R"))
#source(paste0(pre_dir,"preselect_COMPLETE.R"))
source(paste0(pre_dir, "info.R"))
source(paste0(pre_dir,"study_parameters.R"))
setwd(projectFolder)

#################################################
#Study_source_population
#################################################
system.time(source(paste0(pre_dir,"study_source_population_script.R")))

source(paste0(pre_dir,"save_environment.R"))
####################################################
#Medicine exposure
####################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_MEDICINES<-paste0(pre_dir,"/MEDICINES_L3.Rmd")
system.time(source(paste0(pre_dir,"MEDICINES_L3.R")))

if(length(actual_tables$MEDICINES)>0){
if(subpopulations_present=="No"){
  system.time(render(Rmd_MEDICINES, output_dir = paste0(output_dir,"MEDICINES/"), output_file = "MEDICINES_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_MEDICINES, output_dir = paste0(output_dir,"MEDICINES/"), output_file = paste0(subpopulations_names[a],"_MEDICINES_L3.html")))  
  }
}
}
source(paste0(pre_dir,"save_environment.R"))
##################################################
#Vaccine exposure
##################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_VACCINES<-paste0(pre_dir,"/VACCINES_L3.Rmd")
system.time(source(paste0(pre_dir,"VACCINES_L3.R")))

if(length(actual_tables$VACCINES)>0){
if(subpopulations_present=="No"){
  system.time(render(Rmd_VACCINES, output_dir = paste0(output_dir,"VACCINES/"), output_file = "VACCINES_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_VACCINES, output_dir = paste0(output_dir,"VACCINES/"), output_file = paste0(subpopulations_names[a],"_VACCINES_L3.html")))  
  }
}
}
source(paste0(pre_dir,"save_environment.R"))
#################################################
#Diagnoses
#################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_DIAGNOSES<-paste0(pre_dir,"/DIAGNOSES_L3.Rmd")
system.time(source(paste0(pre_dir,"DIAGNOSES_L3.R")))

if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS))>0){
if(subpopulations_present=="No"){
system.time(render(Rmd_DIAGNOSES, output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = "DIAGNOSES_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_DIAGNOSES, output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = paste0(subpopulations_names[a],"_DIAGNOSES_L3.html")))  
  }
}
}
source(paste0(pre_dir,"save_environment.R"))
#################################################
#Pregnancy
#################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_PREGNANCY<-paste0(pre_dir,"/PREGNANCY_L3.Rmd")
source(paste0(pre_dir,"PREGNANCY_L3.R"))

if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS), length(actual_tables$SURVEY_ID))>0){
if(subpopulations_present=="No"){
  system.time(render(Rmd_PREGNANCY, output_dir = paste0(output_dir,"PREGNANCY/"), output_file = "PREGNANCY_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_PREGNANCY, output_dir = paste0(output_dir,"PREGNANCY/"), output_file = paste0(subpopulations_names[a],"_PREGNANCY_L3.html")))  
  }
}
}
source(paste0(pre_dir,"save_environment.R"))
#################################################
#Populations of interest
#################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_POI<-paste0(pre_dir,"/POI_L3.Rmd")
system.time(source(paste0(pre_dir,"POI_L3.R")))

if(subpopulations_present=="No"){
  system.time(render(Rmd_POI, output_dir = paste0(output_dir,"POI/"), output_file = "POI_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_POI, output_dir = paste0(output_dir,"POI/"), output_file = paste0(subpopulations_names[a],"_POI_L3.html")))  
  }
}
source(paste0(pre_dir,"save_environment.R"))

#################################################
#EUROCAT INDICATORS
#################################################
rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
source("99_path.R")
load(paste0(g_intermediate,"environment.RData"))
Rmd_EUROCAT<-paste0(pre_dir,"/EUROCAT_DQI_L3.Rmd")
system.time(source(paste0(pre_dir,"eurocat_dqi.R")))

if(subpopulations_present=="No"){
  system.time(render(Rmd_EUROCAT, output_dir = paste0(output_dir,"EUROCAT/"), output_file = "EUROCAT_DQI_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_EUROCAT, output_dir = paste0(output_dir,"EUROCAT/"), output_file = paste0(subpopulations_names[a],"_EUROCAT_DQI_L3.html")))  
  }
}
source(paste0(pre_dir,"save_environment.R"))

####################################################
#Create ForDashboard folder
####################################################

source(paste0(pre_dir,"for_dashboard.R"))


