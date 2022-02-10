# this script stores all the path strings in variables used by other scripts WITHOUT creating (overwriting) the directory stored on that path

rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

dir_base<-getwd()

# set the name of the study
StudyName <- "LOT4"
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")

pre_dir<-paste0(projectFolder,"/p_steps/")

g_intermediate<-paste(projectFolder, "/g_intermediate/", sep="")

output_dir<-paste(projectFolder, "/g_output/", sep="")

populations_dir<-paste0(g_intermediate,"populations/")

tmp<-paste0(g_intermediate,"tmp/")

conceptsets_DX_dir<-paste(tmp, "conceptsets_dx/", sep="")

conceptsets_ATC_dir<-paste(tmp, "conceptsets_atc/", sep="")

conceptsets_PROC_dir<-paste(tmp, "conceptsets_proc/", sep="")

conceptsets_MO_dir<-paste(tmp, "conceptsets_mo/", sep="")

events_tmp_DX <- paste0(tmp, "events_dx/")

events_tmp_ATC <- paste0(tmp, "events_atc/")

events_tmp_PROC <- paste0(tmp, "events_proc/")

events_tmp_MO <- paste0(tmp, "events_mo/")

events_tmp_sterility <- paste0(tmp, "events_sterility/")

diagnoses_pop <- paste0(tmp, "diagnoses/")

medications_pop <- paste0(tmp, "medications/")

procedures_pop <- paste0(tmp, "procedures/")

medicalobservations_pop <- paste0(tmp, "medical_observations/")

sterility_pop <- paste0(tmp, "sterility/")

monthly_counts_dx <- paste0(output_dir, "monthly_counts_dxcodes")   

monthly_counts_atc <- paste0(output_dir, "monthly_counts_atc")

monthly_counts_proc <- paste0(output_dir, "monthly_counts_proc")

monthly_counts_mo <- paste0(output_dir, "monthly_counts_mo")

plot_folder <- paste0(output_dir, "plots")

baseline_tables_dir <- paste0(output_dir, "baseline_tables")



