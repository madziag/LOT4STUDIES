# Accomodates for analysis for subpopulations 

#Load study population/populations 
populations <- list.files(populations_dir, pattern = "study_population")

# Run Analysis for all 
for(pop in 1:length(populations)){
  study_population <- readRDS(paste0(populations_dir, populations[pop]))
  source(paste0(pre_dir,"CreateSterilityList.R"))
  source(paste0(pre_dir, "CreateEntryExit.R"))
  source(paste0(pre_dir,"denominator_monthly.R"))
  source(paste0(pre_dir,"monthly_counts_dxcodes.R"))
  source(paste0(pre_dir,"monthly_counts_ATC.R"))
  source(paste0(pre_dir,"monthly_counts_procedures.R"))
  source(paste0(pre_dir,"plots.R"))
  source(paste0(pre_dir,"write_output.R"))
}

unlink(paste0(tmp, "/events_sterility"), recursive = TRUE)
unlink(paste0(tmp, "/events_dx"), recursive = TRUE)
unlink(paste0(tmp, "/events_atc"), recursive = TRUE)
unlink(paste0(tmp, "/events_proc"), recursive = TRUE)
