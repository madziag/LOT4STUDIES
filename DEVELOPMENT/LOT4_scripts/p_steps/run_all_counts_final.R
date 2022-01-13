# Accommodates for analysis from multiple regions 

if(multiple_regions == T){
  # Gets a list of all the regions
  multiple_regions_dir <- multiple_regions_dir
  regions <- list.files(multiple_regions_dir)
  
  for(reg in 1:length(regions)){
    # Sets path to study population
    study_pop_dir_reg <- paste0(projectFolder, "/", regions[reg], "/g_intermediate/populations")
    populations <- list.files(study_pop_dir_reg, pattern = "study_population")
    
    # Sets path to baseline folders
    baseline_dir <- paste0(projectFolder, "/", regions[reg], "/g_output/baseline_tables")
    # Sets paths to medication folders (gets a list of patients with either retinoid or valproate id's)
    med_pop_reg <- paste0(projectFolder, "/", regions[reg], "/g_intermediate/tmp/medications")
    # Sets path to read rds files
    output_dir <- paste0(projectFolder, "/", regions[reg], "/g_output/")
    # Sets path to save monthly counts for medication use in pregnancies 
    preg_med_counts <- paste0(output_dir, "objective3_2_counts_per_month") 
      
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


