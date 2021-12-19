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
  source(paste0(pre_dir, "write_output.R"))
}



