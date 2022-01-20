# Takes into account if user_input: multiple_regions = T/F
# If multiple_regions = T, for each region with records in CDMInstances folder 
## 1. Moves g_intermediate and g_output folders back into LOT4_scripts folder
## 2. Sources run_counts_final_each_pop for corresponding region (which runs individual scripts for each subpopulation)
## 3. Moves g_intermediate and g_output folders back to corresponding region

# Checks for multiple regions 
if(multiple_regions == T){
  # Gets a list of region names from the CDMInstances folder 
  regions <- list.files(multiple_regions_dir)
  # Loops over each region
  for(reg in 1:length(regions)){
    # Prints region loop is currently working on
    print("##################################################")
    print("##################################################")
    print(paste("############ RUNNING ANALYSIS FOR ", regions[reg], "############"))
    print("##################################################")
    print("##################################################")
    ## First removes g_intermediate/g_output
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
    # Moves g_intermediate and g_output folders from corresponding region folder into LOT4_scripts folder
    file.move(paste0(projectFolder, "/", regions[reg], "/g_intermediate"), paste0(projectFolder,"/g_intermediate"))
    file.move(paste0(projectFolder, "/", regions[reg], "/g_output"), paste0(projectFolder,"/g_output"))
    # Creates folders for final storage
    # Baseline tables folders
    invisible(ifelse(!dir.exists(paste0(output_dir, "baseline_tables")), dir.create(paste0(output_dir, "baseline_tables")), FALSE))
    baseline_tables_dir <- paste0(output_dir, "baseline_tables")
    # Pregnancy Counts 
    invisible(ifelse(!dir.exists(paste0(output_dir, "pregnancy_counts")), dir.create(paste0(output_dir, "pregnancy_counts")), FALSE))
    preg_med_counts <- paste0(output_dir, "pregnancy_counts")
    # Contraceptive counts (not yet in use)
    # invisible(ifelse(!dir.exists(paste0(output_dir, "contraceptive_counts")), dir.create(paste0(output_dir, "contraceptive_counts")), FALSE))
    # contraceptive_counts_dir <- paste0(output_dir, "contraceptive_counts")
    # Medicines Counts (not yet in use)
    # invisible(ifelse(!dir.exists(paste0(output_dir, "medicines_counts")), dir.create(paste0(output_dir, "medicines_counts")), FALSE))
    # medicines_counts_dir <- paste0(output_dir, "medicines_counts")
    # Pregnancy Test Counts(not yet in use)
    # invisible(ifelse(!dir.exists(paste0(output_dir, "pregnancy_test_counts")), dir.create(paste0(output_dir, "pregnancy_test_counts")), FALSE))
    # pregnancy_test_counts_dir <- paste0(output_dir, "pregnancy_test_counts")
    # Creates new plot folder
    invisible(ifelse(!dir.exists(paste0(output_dir, "plots")), dir.create(paste0(output_dir, "plots")), FALSE))
    plot_folder <- paste0(output_dir, "plots")
    # Sources run_counts_final_each_pop.R 
    source(paste0(pre_dir,"run_counts_final_each_pop.R"))
    # Moves g_intermediate, g_output folders from LOT4_script folder to respective regional folders
    file.move(paste0(projectFolder,"/g_intermediate"), paste0(projectFolder, "/", regions[reg], "/g_intermediate"))
    file.move(paste0(projectFolder,"/g_output"), paste0(projectFolder, "/", regions[reg], "/g_output"))
  }
} else {
  # Remove empty files (monthly_counts folders that are created again when to_run_final_counts.R is run)
  for (file in list.files(path=paste0(output_dir), pattern= "monthly_counts", ignore.case = T)){unlink(paste0(output_dir,file), recursive = TRUE)}
  # Sources run_counts_final_each_pop.R 
  source(paste0(pre_dir,"run_counts_final_each_pop.R"))
}


