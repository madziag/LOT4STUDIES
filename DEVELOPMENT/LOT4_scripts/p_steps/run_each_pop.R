# Sources scripts for each population 
# Paths and folders set up in run_all_counts_final.R

  for(pop in 1:length(populations)){
    # Runs file to create baseline tables
    source(paste0(pre_dir,"CreateBaselineTables.R"))
  }



