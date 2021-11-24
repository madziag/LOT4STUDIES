#Directory
# setwd('..') #in Data Characterisation
setwd('..') #in ConcePTION
dir_base<-getwd()
# set the name of the study
StudyName <- "LOT4"
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")
# path<-path_dir

#Set the path to where you want your report to be saved(make sure that the output folder already exists)
# Set path to p-steps
pre_dir<-paste0(projectFolder,"/p_steps/")
# Check if file exists. If it does, delete and create new folder: g_output
if ("g_output" %in% list.files(projectFolder)){
  unlink(paste0(projectFolder,"/g_output"), recursive = T)#delete folder
  dir.create(paste(projectFolder, "/g_output", sep=""))
  output_dir<-paste(projectFolder, "/g_output/", sep="")
}else{
  dir.create(paste(projectFolder, "/g_output", sep=""))
  output_dir<-paste(projectFolder, "/g_output/", sep="")
}
# Stores monthly counts per code group: dx codes
invisible(ifelse(!dir.exists(file.path(output_dir, "monthly_counts_dxcodes")), dir.create(paste(output_dir, "monthly_counts_dxcodes", sep="")), FALSE))
monthly_counts_dx <- paste0(output_dir, "monthly_counts_dxcodes")
# Stores monthly counts per code group: dx codes
invisible(ifelse(!dir.exists(file.path(output_dir, "monthly_count_atc")), dir.create(paste(output_dir, "monthly_counts_atc", sep="")), FALSE))
monthly_counts_atc <- paste0(output_dir, "monthly_counts_atc")

# Check if file exists. If it does, delete and create new folder: g_intermediate
if ("g_intermediate" %in% list.files(projectFolder)){
  unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)#delete folder
  dir.create(paste(projectFolder, "/g_intermediate", sep=""))
  g_intermediate<-paste(projectFolder, "/g_intermediate/", sep="")
}else{
  #Create the STUDY_SOURCE_POPULATION folder in the output dir
  dir.create(paste(projectFolder, "/g_intermediate", sep=""))
  g_intermediate<-paste(projectFolder, "/g_intermediate/", sep="")
}
# Create subfolders tmp and populations in g_intermediate
invisible(ifelse(!dir.exists(file.path(g_intermediate, "/populations")), dir.create(paste(g_intermediate, "/populations", sep="")), FALSE))
populations_dir<-paste0(g_intermediate,"populations/")
invisible(ifelse(!dir.exists(file.path(g_intermediate, "/tmp")), dir.create(paste(g_intermediate, "/tmp", sep="")), FALSE))
tmp<-paste0(g_intermediate,"tmp/")
# Create folders and set paths for monthly counts
# For Dx codes concept sets
invisible(ifelse(!dir.exists(file.path(tmp, "conceptsets_dx")), dir.create(paste(tmp, "conceptsets_dx", sep="")), FALSE))
conceptsets_DX_dir<-paste(tmp, "conceptsets_dx/", sep="")
# For ATC codes concept sets
invisible(ifelse(!dir.exists(file.path(tmp, "conceptsets_atc")), dir.create(paste(tmp, "conceptsets_atc", sep="")), FALSE))
conceptsets_ATC_dir<-paste(tmp, "conceptsets_atc/", sep="")
# For individually filtered records: dx codes
invisible(ifelse(!dir.exists(file.path(tmp, "events_dx")), dir.create(paste(tmp, "events_dx", sep="")), FALSE))
events_tmp_dx <- paste0(tmp, "events_dx/")
# For individually filtered records: ATC
invisible(ifelse(!dir.exists(file.path(tmp, "events_atc")), dir.create(paste(tmp, "events_atc", sep="")), FALSE))
events_tmp_ATC <- paste0(tmp, "events_atc/")
# stores 1 df per matched group code (if matches exist): dx codes
invisible(ifelse(!dir.exists(file.path(tmp, "diagnoses")), dir.create(paste(tmp, "diagnoses", sep="")), FALSE))
diagnoses_tmp <- paste0(tmp, "diagnoses/")
# stores 1 df per matched group code (if matches exist): ATC
invisible(ifelse(!dir.exists(file.path(tmp, "medications")), dir.create(paste(tmp, "medications", sep="")), FALSE))
medications_pop <- paste0(tmp, "medications/")


