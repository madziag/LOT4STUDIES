# Accommodates for analysis from multiple regions 

if(multiple_regions == T){
  # Gets a list of all the regions
  multiple_regions_dir <- multiple_regions_dir
  regions <- list.files(multiple_regions_dir)
  
  for(reg in 1:length(regions)){
    # MED_list <-list.files(paste0(multiple_regions_dir,"/", regions[reg]), pattern="^MEDICINES") # for select_RET_VAL.R
    # output_dir <- paste0(projectFolder, "/", regions[reg], "/g_output") # for select_RET_VAL.R
  
    # Sets path to study population
    study_pop_dir_reg <- paste0(projectFolder, "/", regions[reg], "/g_intermediate/populations")
    populations <- list.files(study_pop_dir_reg, pattern = "study_population")
    # Reads in study_population file
    study_pop_reg <- readRDS(paste0(study_pop_dir_reg, "/",populations[pop]))
    # Sets path to baseline folders
    baseline_dir <- paste0(projectFolder, "/", regions[reg], "/g_output/baseline_tables")
    # Sets paths to medication folders (gets a list of patients with either retinoid or valproate id's)
    med_pop_reg <- paste0(projectFolder, "/", regions[reg], "/g_intermediate/tmp/medications")
    # Based on the type of study (specified in to_run_final_counts.R, loads corresponding medication file)
    if (study_type == "Retinoids"){
      pattern1 = c("Retinoid")
      } else if (study_type == "Valproates") {
        pattern1 = c("Valproate")
        } else if (study_type == "Both") {
          pattern1 = c("Retinoid","Valproate")
          } else {
            print ("Please enter correct Study Type")
            }
    files <- list.files(path=med_pop_reg, pattern=paste0(pattern1, collapse="|"))
    study_pop_meds <- do.call(rbind,lapply(paste0(med_pop_reg,"/",files), readRDS))

    # Source file to run code for every population found 
    source(paste0(pre_dir,"run_each_pop.R"))
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
    }
  }else{
    populations <- list.files(populations_dir, pattern = "study_population")
    study_population <- readRDS(paste0(populations_dir, populations[pop]))
    if (study_type == "Retinoids"){
      pattern1 = c("Retinoid")
      } else if (study_type == "Valproates") {
        pattern1 = c("Valproate")
        } else if (study_type == "Both") {
          pattern1 = c("Retinoid","Valproate")
          } else {
            print ("Please indicate correct Study Type")
            }
    files <- list.files(path=medications_pop, pattern=paste0(pattern1, collapse="|"))
    study_pop_meds <- do.call(rbind,lapply(paste0(medications_pop,"/",files), readRDS))
    source(paste0(pre_dir,"run_each_pop.R"))

}


