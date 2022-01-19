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
    reg_dir <-paste0(projectFolder, "/", regions[reg], "/")
    # Create all folders (start afresh with each iteration )
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
    # Create main output folders 
    dir.create(paste0(projectFolder, "/g_intermediate"))
    dir.create(paste0(projectFolder, "/g_output"))
    # Set paths to main folders 
    pre_dir<-paste0(projectFolder,"/p_steps/")
    g_intermediate<-paste0(projectFolder, "/g_intermediate/")
    output_dir<-paste0(projectFolder, "/g_output/")
    # Create folders and corresponding paths 
    # folders under g_intermediate
    dir.create(paste0(g_intermediate, "/populations"))
    populations_dir<-paste0(g_intermediate,"populations/")
    dir.create(paste0(g_intermediate, "/tmp")) 
    tmp<-paste0(g_intermediate,"tmp/")
    # Directories for concept sets 
    dir.create(paste0(tmp, "conceptsets_dx"))
    conceptsets_DX_dir<-paste0(tmp, "conceptsets_dx/")
    dir.create(paste0(tmp, "conceptsets_atc"))
    conceptsets_ATC_dir<-paste0(tmp, "conceptsets_atc/")
    dir.create(paste0(tmp, "conceptsets_proc"))
    conceptsets_PROC_dir<-paste0(tmp, "conceptsets_proc/")
    # temp directories for events -> will be deleted 
    dir.create(paste0(tmp, "events_dx"))
    events_tmp_DX <- paste0(tmp, "events_dx/")
    dir.create(paste0(tmp, "events_atc"))
    events_tmp_ATC <- paste0(tmp, "events_atc/")
    dir.create(paste0(tmp, "events_sterility"))
    events_tmp_sterility <- paste0(tmp, "events_sterility/")
    
    # Directories for combined events -> for counts 
    dir.create(paste0(tmp, "diagnoses"))
    diagnoses_pop <- paste0(tmp, "diagnoses/")
    dir.create(paste0(tmp, "medications"))
    medications_pop <- paste0(tmp, "medications/")
    dir.create(paste0(tmp, "sterility"))
    sterility_pop <- paste0(tmp, "sterility/")
    #  Directories for monthly counts 
    dir.create(paste0(output_dir, "monthly_counts_dxcodes"))
    monthly_counts_dx <- paste0(output_dir, "monthly_counts_dxcodes")   
    dir.create(paste0(output_dir, "monthly_counts_atc"))
    monthly_counts_atc <- paste0(output_dir, "monthly_counts_atc")
    
    # Creates folders only if procedures tables are available 
    if(length(list.files(path=paste0(multiple_regions_dir, regions[reg]), pattern = "PROCEDURES", ignore.case = TRUE)) > 0){
      dir.create(paste0(tmp, "events_proc"))
      events_tmp_PROC <- paste0(tmp, "events_proc/")
      dir.create(paste0(tmp, "events_proc_dxcodes"))
      events_tmp_PROC_dxcodes <- paste0(tmp, "events_proc_dxcodes/")
      
      dir.create(paste0(tmp, "procedures"))
      procedures_pop <- paste0(tmp, "procedures/")
      dir.create(paste0(tmp, "procedures_dxcodes"))
      procedures_dxcodes_pop <- paste0(tmp, "procedures_dxcodes/")
      
      dir.create(paste0(output_dir, "monthly_counts_proc"))
      monthly_counts_proc <- paste0(output_dir, "monthly_counts_proc")
      dir.create(paste0(output_dir, "monthly_counts_proc_dxcodes"))
      monthly_counts_proc_dxcodes <- paste0(output_dir, "monthly_counts_proc_dxcodes")
      
    }
    
    # Directories for plots
    dir.create(paste0(output_dir, "plots"))
    plot_folder <- paste0(output_dir, "plots")
    # Directories for baseline tables 
    dir.create(paste0(output_dir, "baseline_tables"))
    baseline_tables_dir <- paste0(output_dir, "baseline_tables")
    #Directories for monthly counts medication use during pregnancies
    dir.create(paste0(output_dir, "pregnancy_counts"))
    preg_med_counts <- paste0(output_dir, "pregnancy_counts")
    # Directories for preliminary counts
    dir.create(paste0(output_dir, "preliminary_counts"))
    preliminary_counts_dir <- paste0(output_dir, "preliminary_counts")
    
    # Sets paths to each region folder 
    path_dir<- paste0(multiple_regions_dir, regions[reg], "/")
    # Creates the study population
    source(paste0(pre_dir,"study_source_population_script.R"))
    # Creates a patient files with relevant codes (for the counts) + count files + plots
    # source(paste0(pre_dir,"run_all_counts.R"))
    source(paste0(pre_dir,"run_counts_prelim_each_pop.R"))
    # Moves files from g_intermediate + g_output  to individual region folders
    file.copy(paste0(projectFolder, "/g_intermediate"), paste0(projectFolder, "/", regions[reg]), recursive=TRUE)
    file.copy(paste0(projectFolder, "/g_output"), paste0(projectFolder, "/", regions[reg]), recursive=TRUE)
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
  }
}else{
  # creates study population + patient files with relevant codes + counts + plots 
  path_dir <- path_dir
  source(paste0(pre_dir,"study_source_population_script.R"))
  # source(paste0(pre_dir,"run_all_counts.R"))
  source(paste0(pre_dir,"run_counts_prelim_each_pop.R"))
}


