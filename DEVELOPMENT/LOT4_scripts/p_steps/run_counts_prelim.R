# Takes into account if user_input: multiple_regions = T/F
# If multiple_regions = T, for each region with records in CDMInstances folder 
## 1. Creates folder with name of corresponding region
## 2. Creates g_output, g_intermediate + respective subfolders in LOT4_scripts folder
## 3. Sources study_source_population_script.R for corresponding region 
## 4. Sources run_counts_prelim_each_pop for corresponding region (which runs individual scripts for each subpopulation)
## 5. Moves g_output, g_intermediate + respective subfolders to folder with corresponding region name
## Result: List of folders for every region with records in CDMInstances folder: each has g_intermediate and g_output folders + respective subfolders + results of individual scripts
# If multiple_regions = F
## 1. Sources study_source_population_script.R
## 2. Sources run_counts_prelim_each_pop for corresponding region (which runs individual scripts for each subpopulation)
## Result: g_intermediate, g_output + respective subfolders + results of individual scripts in LOT4_scripts folder 

# Checks for multiple regions 
if(multiple_regions == T){
  # Gets a list of region names from the CDMInstances folder 
  regions <- list.dirs(path = multiple_regions_dir, full.names = FALSE, recursive = FALSE)
  # Loops over each region
  for(reg in 1:length(regions)){
    # Prints region loop is currently working on
    print("##################################################")
    print("##################################################")
    print(paste("############ RUNNING ANALYSIS FOR ", regions[reg], "############"))
    print("##################################################")
    print("##################################################")
    # Sets paths to data folder for each region
    path_dir<- paste0(multiple_regions_dir, regions[reg], "/")
    # Sources folders for each region 
    source(paste0(pre_dir,"info.R"))
    source(paste0(pre_dir,"study_parameters.R"))
    # Creates folder for each region 
    invisible(ifelse(!dir.exists(paste0(projectFolder, "/", regions[reg])), dir.create(paste0(projectFolder, "/", regions[reg])), FALSE))
    reg_dir <-paste0(projectFolder, "/", regions[reg], "/")
    # Creates main output folders 
    ## First removes g_intermediate/g_output
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
    # Creates main output folders + paths
    dir.create(paste0(projectFolder, "/g_intermediate"))
    g_intermediate<-paste0(projectFolder, "/g_intermediate/")
    dir.create(paste0(projectFolder, "/g_output"))
    output_dir<-paste0(projectFolder, "/g_output/")
    # Creates g_intermediate subfolders
    dir.create(paste0(g_intermediate, "/populations"))
    populations_dir<-paste0(g_intermediate,"populations/")
    dir.create(paste0(g_intermediate, "/tmp")) 
    tmp<-paste0(g_intermediate,"tmp/")
    # Creates g_output subfolders
    ## for Concept sets 
    dir.create(paste0(tmp, "conceptsets_dx"))
    conceptsets_DX_dir<-paste0(tmp, "conceptsets_dx/")
    dir.create(paste0(tmp, "conceptsets_atc"))
    conceptsets_ATC_dir<-paste0(tmp, "conceptsets_atc/")
    dir.create(paste0(tmp, "conceptsets_proc"))
    conceptsets_PROC_dir<-paste0(tmp, "conceptsets_proc/")
    ## for records from EVENTS TABLES 
    # Creates folders only if events tables are available 
    if(length(list.files(path=paste0(multiple_regions_dir, regions[reg]), pattern = "EVENTS", ignore.case = TRUE)) > 0){
      # Temporary folder - will be deleted 
      dir.create(paste0(tmp, "events_dx"))
      events_tmp_DX <- paste0(tmp, "events_dx/")
      # Permanent folder
      dir.create(paste0(tmp, "diagnoses"))
      diagnoses_pop <- paste0(tmp, "diagnoses/")
      # Monthly counts
      dir.create(paste0(output_dir, "monthly_counts_dxcodes"))
      monthly_counts_dx <- paste0(output_dir, "monthly_counts_dxcodes") 
    }
    ## for records from MEDICINES TABLES
    if(length(list.files(path=paste0(multiple_regions_dir, regions[reg]), pattern = "MEDICINES", ignore.case = TRUE)) > 0){
      # Temporary folder - will be deleted 
      dir.create(paste0(tmp, "events_atc"))
      events_tmp_ATC <- paste0(tmp, "events_atc/")
      # Permanent folder
      dir.create(paste0(tmp, "medications"))
      medications_pop <- paste0(tmp, "medications/")
      # Monthly counts
      dir.create(paste0(output_dir, "monthly_counts_atc"))
      monthly_counts_atc <- paste0(output_dir, "monthly_counts_atc")
    }
    ## for records from PROCEDURES TABLES 
    if(length(list.files(path=paste0(multiple_regions_dir, regions[reg]), pattern = "PROCEDURES", ignore.case = TRUE)) > 0){
      # Temporary folder - will be deleted 
      dir.create(paste0(tmp, "events_proc"))
      events_tmp_PROC <- paste0(tmp, "events_proc/")
      dir.create(paste0(tmp, "events_proc_dxcodes"))
      events_tmp_PROC_dxcodes <- paste0(tmp, "events_proc_dxcodes/")
      # Permanent folder
      dir.create(paste0(tmp, "procedures"))
      procedures_pop <- paste0(tmp, "procedures/")
      dir.create(paste0(tmp, "procedures_dxcodes"))
      procedures_dxcodes_pop <- paste0(tmp, "procedures_dxcodes/")
      # Monthly counts
      dir.create(paste0(output_dir, "monthly_counts_proc"))
      monthly_counts_proc <- paste0(output_dir, "monthly_counts_proc")
      dir.create(paste0(output_dir, "monthly_counts_proc_dxcodes"))
      monthly_counts_proc_dxcodes <- paste0(output_dir, "monthly_counts_proc_dxcodes")
    }
    ## for sterility records
    # Temporary folder - will be deleted 
    dir.create(paste0(tmp, "events_sterility"))
    events_tmp_sterility <- paste0(tmp, "events_sterility/")
    # Permanent folder
    dir.create(paste0(tmp, "sterility"))
    sterility_pop <- paste0(tmp, "sterility/")
    ## for plots
    dir.create(paste0(output_dir, "plots"))
    plot_folder <- paste0(output_dir, "plots")
    # Creates folders for final storage
    ## For all preliminary counts
    dir.create(paste0(output_dir, "preliminary_counts"))
    preliminary_counts_dir <- paste0(output_dir, "preliminary_counts")
    # Sources study_source_population_script.R
    source(paste0(pre_dir,"study_source_population_script.R"))
    # Sources run_counts_prelim_each_pop.R 
    source(paste0(pre_dir,"run_counts_prelim_each_pop.R"))

    # Moves g_intermediate, g_output folders from LOT4_script folder to respective regional folders
    file.move(paste0(projectFolder,"/g_intermediate"), paste0(projectFolder, "/", regions[reg], "/g_intermediate"))
    file.move(paste0(projectFolder,"/g_output"), paste0(projectFolder, "/", regions[reg], "/g_output"))
  }
}else{
  # Sources folders for each region 
  source(paste0(pre_dir,"info.R"))
  source(paste0(pre_dir,"study_parameters.R"))
  # Sources study_source_population_script.R
  source(paste0(pre_dir,"study_source_population_script.R"))
  # Sources run_counts_prelim_each_pop.R 
  source(paste0(pre_dir,"run_counts_prelim_each_pop.R"))
}


