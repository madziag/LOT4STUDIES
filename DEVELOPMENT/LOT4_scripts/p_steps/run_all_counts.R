# Accomodates for analysis for subpopulations 

#Load study population/populations 
populations <- list.files(populations_dir, pattern = "study_population")

# Run Analysis for all 
for(pop in 1:length(populations)){
  study_population <- readRDS(paste0(populations_dir, populations[pop]))
  source(paste0(pre_dir,"CreateSterilityList.R"))
  source(paste0(pre_dir,"CreateEntryExit.R"))
  source(paste0(pre_dir,"denominator_monthly.R"))
  source(paste0(pre_dir,"monthly_counts_dxcodes.R"))
  source(paste0(pre_dir,"monthly_counts_ATC.R"))
  source(paste0(pre_dir,"monthly_counts_procedures.R"))
  source(paste0(pre_dir, "monthy_counts_dxcodes_in_procedures.R"))
  source(paste0(pre_dir,"plots.R")) # This also implements the masking of values <5 so they are not included in the output data
  source(paste0(pre_dir,"write_output.R"))
}

unlink(paste0(tmp, "/events_sterility"), recursive = TRUE)
unlink(paste0(tmp, "/events_dx"), recursive = TRUE)
unlink(paste0(tmp, "/events_atc"), recursive = TRUE)
unlink(paste0(tmp, "/events_proc"), recursive = TRUE)
unlink(paste0(tmp, "/events_proc_dxcodes"), recursive = TRUE)

# Move folders to corresponding main files 
# 
# pattern1 = c("monthly_counts", "plots", "csv_files")
# files_to_move <- list.files(path=output_dir, pattern=paste0(pattern1, collapse="|"))
# 
# for(file in files_to_move){
#   file.copy(paste0(output_dir, file), paste0(preliminary_counts_dir, "/"))
# }
# 
# file.copy(paste0(projectFolder, "/g_intermediate"), paste0(projectFolder, "/", regions[reg]), recursive=TRUE)
# file.copy(paste0(projectFolder, "/g_output"), paste0(projectFolder, "/", regions[reg]), recursive=TRUE)
# if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
# if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}