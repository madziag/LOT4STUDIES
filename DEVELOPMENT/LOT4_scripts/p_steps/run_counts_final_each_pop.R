# Sources scripts for each population 
# Paths and folders set up in run_all_counts_final.R

for(pop in 1:length(populations)){
  # Reads in study_population file
  if(multiple_regions == TRUE){
    study_pop_reg <- readRDS(paste0(study_pop_dir_reg, "/",populations[pop]))
  } else {
    study_population <- readRDS(paste0(populations_dir, populations[pop]))
  }
  # Runs file to create baseline tables
  source(paste0(pre_dir,"CreateBaselineTables.R"))
  source(paste0(pre_dir, "monthly_counts_med_use_in_pregnancy.R"))
  # source(paste0(pre_dir,"plots.R")) # This also implements the masking of values <5 so they are not included in the output data
  source(paste0(pre_dir,"plots_mask.R")) # This also implements the masking of values <5 so they are not included in the output data
  source(paste0(pre_dir, "write_output_2.R"))
}






