rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

dir_base<-getwd()
# set the name of the study
StudyName <- "LOT4"
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")

g_intermediate<-paste(projectFolder, "/g_intermediate/", sep="")
output_dir<-paste(projectFolder, "/g_output/", sep="")

populations_dir<-paste0(g_intermediate,"populations/")

tmp<-paste0(g_intermediate,"tmp/")