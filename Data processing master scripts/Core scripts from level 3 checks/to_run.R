
#Author: Vjola Hoxhaj Drs./Roel Elbers MSc.
#email: v.hoxhaj@umcutrecht.nl/r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

###################################################
#Parameters
#################################################
#females age
min_age_preg<-12
max_age_preg<-55
#Set parameters basic parameters
start_study_date <- "19950101"
end_study_date <- "20181231"
lookback_period <- 365
Age_min <- 0
Age_max <- 56

######################################################################################################
#Specify all meanings that refer to birth registry in the SURVEY_ID table(if applicable) for identifying pregnancy
#example meanings_birth_registry<-c("birth_registry", "birth_registry_meanings")
#keep in mind this records will be classified as end_of_pregnancy so no spontaneous abortion registry should be included
meanings_birth_registry<-c("birth_registry_mother")
#######################################################################################################

source("packages.R")
source("99_path.R")
source(paste0(pre_dir, "info.R"))
setwd(projectFolder)


#################################################
#Study_source_population
#################################################
system.time(source(paste0(pre_dir,"study_source_population_script.R")))

#Create report
for(i in readRDS(paste0(std_pop_tmp,"SCHEME_06.rds"))[["subpopulations"]]){
  rmarkdown::render(paste0(pre_dir,"Report_01_StudyPopulation.Rmd"),
    output_file = paste0(std_source_pop_dir,"Report_01_Study_population",i,".html"),
    output_dir = std_source_pop_dir
  )
}


####################################################
#Medicine exposure
####################################################
Rmd_MEDICINES<-paste0(pre_dir,"/MEDICINES_L3.Rmd")
source(paste0(pre_dir,"MEDICINES_L3.R"))

if(length(actual_tables$MEDICINES)>0){
if(subpopulations_present=="No"){
  system.time(render(Rmd_MEDICINES, output_dir = paste0(output_dir,"MEDICINES/"), output_file = "MEDICINES_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_MEDICINES, output_dir = paste0(output_dir,"MEDICINES/"), output_file = paste0(subpopulations_names[a],"_MEDICINES_L3.html")))  
  }
}
}
##################################################
#Vaccine exposure
##################################################
Rmd_VACCINES<-paste0(pre_dir,"/VACCINES_L3.Rmd")
source(paste0(pre_dir,"VACCINES_L3.R"))
if(length(actual_tables$VACCINES)>0){
if(subpopulations_present=="No"){
  system.time(render(Rmd_VACCINES, output_dir = paste0(output_dir,"VACCINES/"), output_file = "VACCINES_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_VACCINES, output_dir = paste0(output_dir,"VACCINES/"), output_file = paste0(subpopulations_names[a],"_VACCINES_L3.html")))  
  }
}
}

#################################################
#Diagnoses
#################################################
Rmd_DIAGNOSES<-paste0(pre_dir,"/DIAGNOSES_L3.Rmd")
source(paste0(pre_dir,"DIAGNOSES_L3.R"))
if(sum(length(actual_tables$EVENTS),length(actual_tables$MEDICAL_OBSERVATIONS),length(actual_tables$SURVEY_OBSERVATIONS))>0){
if(subpopulations_present=="No"){
system.time(render(Rmd_DIAGNOSES, output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = "DIAGNOSES_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_DIAGNOSES, output_dir = paste0(output_dir,"DIAGNOSES/"), output_file = paste0(subpopulations_names[a],"_DIAGNOSES_L3.html")))  
  }
}
}

#################################################
#Pregnancy
#################################################
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
#################################################
#Populations of interest
#################################################
Rmd_POI<-paste0(pre_dir,"/POI_L3.Rmd")
source(paste0(pre_dir,"POI_L3.R"))
if(subpopulations_present=="No"){
  system.time(render(Rmd_POI, output_dir = paste0(output_dir,"POI/"), output_file = "POI_L3.html")) 
} else {
  for (a in 1: length(subpopulations_names)){
    system.time(render(Rmd_POI, output_dir = paste0(output_dir,"POI/"), output_file = paste0(subpopulations_names[a],"_POI_L3.html")))  
  }
}



