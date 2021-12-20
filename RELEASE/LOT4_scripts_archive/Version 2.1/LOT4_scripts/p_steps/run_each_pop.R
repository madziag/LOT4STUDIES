## Accommodates subpopulations 




if (multiple_regions == T){
  populations <- list.files(study_pop_dir_reg, pattern = "study_population")
  for(pop in 1:length(populations)){
    # Reads in study_population file
    study_pop_reg <- readRDS(paste0(study_pop_dir_reg, "/",populations[pop]))
    # Runs file to create baseline tables 
    source(paste0(pre_dir,"CreateBaselineTables.R"))
    }
  }else {

    populations <- list.files(populations_dir, pattern = "study_population")
    for(pop in 1:length(populations)){
      # Reads in study_population file
      study_population <- readRDS(paste0(populations_dir, populations[pop]))
      # Runs file to create baseline tables 
      source(paste0(pre_dir,"CreateBaselineTables.R"))
  }
}



