# Accommodates for analysis from multiple regions 

if(multiple_regions == T){
  multiple_regions_dir <- multiple_regions_dir
  regions <- list.files(multiple_regions_dir)
  
  for(reg in 1:length(regions)){
    # Create directory for region
    print("##################################################")
    print("##################################################")
    print(paste("############ RUNNING ANALYSIS FOR ", regions[reg], "############"))
    print("##################################################")
    print("##################################################")
    
    invisible(ifelse(!dir.exists(paste0(projectFolder, "/", regions[reg])), dir.create(paste0(projectFolder, "/", regions[reg])), FALSE))
    reg_dir <-paste(projectFolder, "/", regions[reg], "/", sep="")
    # Create all folders (start afresh with each iteration )
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
    # Create main output folders 
    dir.create(paste(projectFolder, "/g_intermediate", sep=""))
    dir.create(paste(projectFolder, "/g_output", sep=""))
    # Set paths to main folders 
    pre_dir<-paste0(projectFolder,"/p_steps/")
    g_intermediate<-paste(projectFolder, "/g_intermediate/", sep="")
    output_dir<-paste(projectFolder, "/g_output/", sep="")
    # Create folders and corresponding paths 
    # folders under g_intermediate
    dir.create(paste(g_intermediate, "/populations", sep=""))
    populations_dir<-paste0(g_intermediate,"populations/")
    dir.create(paste(g_intermediate, "/tmp", sep="")) 
    tmp<-paste0(g_intermediate,"tmp/")
    # Directories for concept sets 
    dir.create(paste(tmp, "conceptsets_dx", sep=""))
    conceptsets_DX_dir<-paste(tmp, "conceptsets_dx/", sep="")
    dir.create(paste(tmp, "conceptsets_atc", sep=""))
    conceptsets_ATC_dir<-paste(tmp, "conceptsets_atc/", sep="")
    dir.create(paste(tmp, "conceptsets_proc", sep=""))
    conceptsets_PROC_dir<-paste(tmp, "conceptsets_proc/", sep="")
    # temp directories for events -> will be deleted 
    dir.create(paste(tmp, "events_dx", sep=""))
    events_tmp_DX <- paste0(tmp, "events_dx/")
    dir.create(paste(tmp, "events_atc", sep=""))
    events_tmp_ATC <- paste0(tmp, "events_atc/")
    dir.create(paste(tmp, "events_proc", sep=""))
    events_tmp_PROC <- paste0(tmp, "events_proc/")
    dir.create(paste(tmp, "events_sterility", sep=""))
    events_tmp_sterility <- paste0(tmp, "events_sterility/")
    # Directories for combined events -> for counts 
    dir.create(paste(tmp, "diagnoses", sep=""))
    diagnoses_pop <- paste0(tmp, "diagnoses/")
    dir.create(paste(tmp, "medications", sep=""))
    medications_pop <- paste0(tmp, "medications/")
    dir.create(paste(tmp, "procedures", sep=""))
    procedures_pop <- paste0(tmp, "procedures/")
    dir.create(paste(tmp, "sterility", sep=""))
    sterility_pop <- paste0(tmp, "sterility/")
    #  Directories for monthly counts 
    dir.create(paste(output_dir, "monthly_counts_dxcodes", sep=""))
    monthly_counts_dx <- paste0(output_dir, "monthly_counts_dxcodes")   
    dir.create(paste(output_dir, "monthly_counts_atc", sep=""))
    monthly_counts_atc <- paste0(output_dir, "monthly_counts_atc")
    dir.create(paste(output_dir, "monthly_counts_proc", sep=""))
    monthly_counts_proc <- paste0(output_dir, "monthly_counts_proc")
    # Directories for plots
    dir.create(paste(output_dir, "plots", sep=""))
    plot_folder <- paste0(output_dir, "plots")
    # Directories for baseline tables 
    dir.create(paste(output_dir, "baseline_tables", sep=""))
    baseline_tables_dir <- paste0(output_dir, "baseline_tables")
    
    # Sets paths to each region folder 
    path_dir<- paste0(multiple_regions_dir, regions[reg], "/")
    # Creates the study population
    source(paste0(pre_dir,"study_source_population_script.R"))
    # Creates a patient files with relevant codes (for the counts) + count files + plots
    source(paste0(pre_dir,"run_all_counts.R"))
    
    # Moves files from g_intermediate + g_output  to individual region folders
    file.copy(paste(projectFolder, "/g_intermediate", sep=""), paste(projectFolder, "/", regions[reg], sep=""), recursive=TRUE)
    file.copy(paste(projectFolder, "/g_output", sep=""), paste(projectFolder, "/", regions[reg], sep=""), recursive=TRUE)
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
  }
}else{
  # creates study population + patient files with relevant codes + counts + plots 
  source(paste0(pre_dir,"study_source_population_script.R"))
  source(paste0(pre_dir,"run_all_counts.R"))
}


