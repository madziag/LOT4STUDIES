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
  if (mask <- T){  
    source(paste0(pre_dir,"plots_mask.R")) # This also implements the masking of values <5 so they are not included in the output data
  } else {
      source(paste0(pre_dir,"plots.R")) # This also implements the masking of values <5 so they are not included in the output data
    }
  source(paste0(pre_dir,"write_output.R"))
}

unlink(paste0(tmp, "/events_sterility"), recursive = TRUE)
unlink(paste0(tmp, "/events_dx"), recursive = TRUE)
unlink(paste0(tmp, "/events_atc"), recursive = TRUE)
unlink(paste0(tmp, "/events_proc"), recursive = TRUE)
unlink(paste0(tmp, "/events_proc_dxcodes"), recursive = TRUE)

# Move folders to corresponding main files
pattern1 = c("monthly_counts_atc", "monthly_counts_dxcodes","monthly_counts_proc","monthly_counts_proc_dxcodes", "plots", paste0(my_format,"_files") )
files_to_move <- list.files(path=output_dir, pattern=paste0(pattern1, collapse="|"))

for(file in files_to_move){
  file.move(paste0(output_dir,file), paste0(paste0(preliminary_counts_dir, "/") ,file))
}


