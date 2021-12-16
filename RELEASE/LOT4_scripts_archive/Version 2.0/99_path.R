#Directory
# setwd('..') #in Data Characterisation


projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

setwd('..') #in ConcePTION
dir_base<-getwd()
# set the name of the study
StudyName <- "LOT4"
path_dir<-paste0(dir_base,"/CDMInstances/",StudyName,"/")
# path_dir<-paste0(dir_base,"/CDMInstances_preselect/") # use this option if you want to use the preselection files
# path<-path_dir

# Check if files exist. If they do not, create them 
# Main folders (g_intermediate, g_output)
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_intermediate")), dir.create(paste0(projectFolder, "/g_intermediate")), FALSE))
invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output")), dir.create(paste0(projectFolder, "/g_output")), FALSE))
# Set paths to main folders 
pre_dir        <-paste0(projectFolder,"/p_steps/")
g_intermediate <-paste0(projectFolder, "/g_intermediate/")
output_dir     <-paste0(projectFolder, "/g_output/")

# folders in g_intermediate
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/populations")), dir.create(paste0(g_intermediate, "/populations")), FALSE))
invisible(ifelse(!dir.exists(paste0(g_intermediate, "/tmp", sep="")), dir.create(paste0(g_intermediate, "/tmp")), FALSE))
# Paths 
populations_dir<-paste0(g_intermediate,"populations/")
tmp<-paste0(g_intermediate,"tmp/")

# Folders in g_intermediate -> tmp
# CONCEPT SET FOLDERS
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_dx")), dir.create(paste0(tmp, "conceptsets_dx")), FALSE))
conceptsets_DX_dir<-paste0(tmp, "conceptsets_dx/")
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_atc")), dir.create(paste0(tmp, "conceptsets_atc")), FALSE))
conceptsets_ATC_dir<-paste0(tmp, "conceptsets_atc/")
invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_proc")), dir.create(paste0(tmp, "conceptsets_proc")), FALSE))
conceptsets_PROC_dir<-paste0(tmp, "conceptsets_proc/")

# EVENTS_TMP FOLDERS (TO BE DELETED)
invisible(ifelse(!dir.exists(paste0(tmp, "events_dx")), dir.create(paste0(tmp, "events_dx")), FALSE))
events_tmp_DX<-paste0(tmp, "events_dx/")
invisible(ifelse(!dir.exists(paste0(tmp, "events_atc")), dir.create(paste0(tmp, "events_atc")), FALSE))
events_tmp_ATC <- paste0(tmp, "events_atc/")
invisible(ifelse(!dir.exists(paste0(tmp, "events_proc")), dir.create(paste0(tmp, "events_proc")), FALSE))
events_tmp_PROC <- paste0(tmp, "events_proc/")
invisible(ifelse(!dir.exists(paste0(tmp, "events_sterility")), dir.create(paste0(tmp, "events_sterility")), FALSE))
events_tmp_sterility <- paste0(tmp, "events_sterility/")

# DIAGNOSIS/MEDICATION/PROCEDURES/STERILITY FOLDERS FOR COMBINED EVENTS
invisible(ifelse(!dir.exists(paste0(tmp, "diagnoses")), dir.create(paste0(tmp, "diagnoses")), FALSE))
diagnoses_pop <- paste0(tmp, "diagnoses/")
invisible(ifelse(!dir.exists(paste0(tmp, "medications")), dir.create(paste0(tmp, "medications")), FALSE))
medications_pop <- paste0(tmp, "medications/")
invisible(ifelse(!dir.exists(paste0(tmp, "procedures")), dir.create(paste0(tmp, "procedures")), FALSE))
procedures_pop <- paste0(tmp, "procedures/")
invisible(ifelse(!dir.exists(paste0(tmp, "sterility")), dir.create(paste0(tmp, "sterility")), FALSE))
sterility_pop <- paste0(tmp, "sterility/")

# MONTHLY COUNTS 
invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_dxcodes")), dir.create(paste0(output_dir, "monthly_counts_dxcodes")), FALSE))
monthly_counts_dx <- paste0(output_dir, "monthly_counts_dxcodes") 
invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_atc")), dir.create(paste0(output_dir, "monthly_counts_atc")), FALSE))
monthly_counts_atc <- paste0(output_dir, "monthly_counts_atc")
invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_proc")), dir.create(paste0(output_dir, "monthly_counts_proc")), FALSE))
monthly_counts_proc <- paste0(output_dir, "monthly_counts_proc")

# PLOTS
invisible(ifelse(!dir.exists(paste0(output_dir, "plots")), dir.create(paste0(output_dir, "plots")), FALSE))
plot_folder <- paste0(output_dir, "plots")

# BASELINE TABLES 
invisible(ifelse(!dir.exists(paste0(output_dir, "baseline_tables")), dir.create(paste0(output_dir, "baseline_tables")), FALSE))
baseline_tables_dir <- paste0(output_dir, "baseline_tables")








