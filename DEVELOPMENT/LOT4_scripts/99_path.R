#Directory
# setwd('..') #in Data Characterisation
setwd('..') #in ConcePTION
dir_base<-getwd()
# set the name of the study
StudyName <- "LOT4"
### NOTE FOR DAPS: If you have run the preselection script and would like to use the subsetted data sets that it produces instead of your full ETL'd data files, you should change the text "/CDMInstances/" to "/CDMInstances_preselect/"
### Below you must set
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")
# path<-path_dir

# If g_intermediate and g_output folders exist remove them 
if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
# Create main output folders 
dir.create(paste(projectFolder, "/g_intermediate", sep=""))
dir.create(paste(projectFolder, "/g_output", sep=""))
# Set paths to main folders 
pre_dir<-paste0(projectFolder,"/p_steps/")
g_intermediate<-paste(projectFolder, "/g_intermediate/", sep="")
output_dir<-paste(projectFolder, "/g_output/", sep="")
# Create subfolders and corresponding paths 
# Subfolders under g_intermediate
dir.create(paste(g_intermediate, "/populations", sep=""))
populations_dir<-paste0(g_intermediate,"populations/")
dir.create(paste(g_intermediate, "/tmp", sep="")) 
tmp<-paste0(g_intermediate,"tmp/")
dir.create(paste(tmp, "conceptsets_dx", sep=""))
conceptsets_DX_dir<-paste(tmp, "conceptsets_dx/", sep="")
dir.create(paste(tmp, "conceptsets_atc", sep=""))
conceptsets_ATC_dir<-paste(tmp, "conceptsets_atc/", sep="")
dir.create(paste(tmp, "events_dx", sep=""))
events_tmp_dx <- paste0(tmp, "events_dx/")
dir.create(paste(tmp, "events_atc", sep=""))
events_tmp_ATC <- paste0(tmp, "events_atc/")
dir.create(paste(tmp, "diagnoses", sep=""))
diagnoses_tmp <- paste0(tmp, "diagnoses/")
dir.create(paste(tmp, "medications", sep=""))
medications_pop <- paste0(tmp, "medications/")
dir.create(paste(output_dir, "monthly_counts_dxcodes", sep=""))
monthly_counts_dx <- paste0(output_dir, "monthly_counts_dxcodes")   
dir.create(paste(output_dir, "monthly_counts_atc", sep=""))
monthly_counts_atc <- paste0(output_dir, "monthly_counts_atc")
dir.create(paste(output_dir, "plots", sep=""))
plot_folder <- paste0(output_dir, "plots")






