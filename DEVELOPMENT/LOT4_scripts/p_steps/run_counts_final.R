#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 07/12/2021
# Takes into account if user_input: multiple_regions = T/F
# If multiple_regions = T, for each region with records in CDMInstances folder 
## 1. Moves g_intermediate and g_output folders back into LOT4_scripts folder
## 2. Sources run_counts_final_each_pop for corresponding region (which runs individual scripts for each subpopulation)
## 3. Moves g_intermediate and g_output folders back to corresponding region

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
    path_dir <- paste0(multiple_regions_dir, regions[reg], "/")
    # Sources folders for each region 
    source(paste0(pre_dir,"info.R"))
    source(paste0(pre_dir,"study_parameters.R"))
    rm(actual_tables, METADATA_subp)
    ## First removes g_intermediate/g_output
    if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
    if("g_output"       %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output")      , recursive = T)}
    # Moves g_intermediate and g_output folders from corresponding region folder into LOT4_scripts folder
    file.move(paste0(projectFolder, "/", regions[reg], "/g_intermediate"), paste0(projectFolder,"/g_intermediate"))
    file.move(paste0(projectFolder, "/", regions[reg], "/g_output"), paste0(projectFolder,"/g_output"))
    ## Create g_output folder if not there (e.g. for sensitivity analysis it has been taken out)
    invisible(ifelse(!dir.exists(paste0(projectFolder, "/g_output")), dir.create(paste0(projectFolder, "/g_output")), FALSE))
    output_dir <- paste0(projectFolder, "/g_output/")
    # Baseline tables folders
    invisible(ifelse(!dir.exists(paste0(output_dir, "baseline_tables")), dir.create(paste0(output_dir, "baseline_tables")), FALSE))
    baseline_tables_dir <- paste0(output_dir, "baseline_tables")
    # Inside baseline tables folders - for storing records to be pulled for pooling
    invisible(ifelse(!dir.exists(paste0(g_intermediate, "recs_for_baseline_table_pooling")), dir.create(paste0(g_intermediate, "recs_for_baseline_table_pooling")), FALSE))
    baseline_pooling_dir <- paste0(g_intermediate, "recs_for_baseline_table_pooling")
    # Pregnancy Counts 
    invisible(ifelse(!dir.exists(paste0(output_dir, "pregnancy_counts")), dir.create(paste0(output_dir, "pregnancy_counts")), FALSE))
    preg_med_counts <- paste0(output_dir, "pregnancy_counts")
    # Contraceptive counts (not yet in use)
    invisible(ifelse(!dir.exists(paste0(output_dir, "contraceptive_counts")), dir.create(paste0(output_dir, "contraceptive_counts")), FALSE))
    contraceptive_counts_dir <- paste0(output_dir, "contraceptive_counts")
    # Medicines Counts
    invisible(ifelse(!dir.exists(paste0(output_dir, "medicines_counts")), dir.create(paste0(output_dir, "medicines_counts")), FALSE))
    medicines_counts_dir <- paste0(output_dir, "medicines_counts")
    # Pregnancy Test Counts(not yet in use)
    invisible(ifelse(!dir.exists(paste0(output_dir, "pregnancy_test_counts")), dir.create(paste0(output_dir, "pregnancy_test_counts")), FALSE))
    pregnancy_test_counts_dir <- paste0(output_dir, "pregnancy_test_counts")
    # Creates new plot folder
    invisible(ifelse(!dir.exists(paste0(output_dir, "plots")), dir.create(paste0(output_dir, "plots")), FALSE))
    plot_folder <- paste0(output_dir, "plots")
    
    invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/","stratified")), dir.create(paste0(medicines_counts_dir,"/","stratified")), FALSE))
    medicines_stratified_dir <- paste0(medicines_counts_dir,"/","stratified")
    # Create stratified by age groups folder
    invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","age_group")), dir.create(paste0(medicines_stratified_dir,"/","age_group")), FALSE))
    medicines_stratified_age_groups <- paste0(medicines_stratified_dir ,"/","age_group")
    # Create stratified by tx_duration folder 
    invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","tx_duration")), dir.create(paste0(medicines_stratified_dir,"/","tx_duration")), FALSE))
    medicines_stratified_tx_dur <- paste0(medicines_stratified_dir ,"/","tx_duration")
    # Create stratified by indication folder 
    invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","indication")), dir.create(paste0(medicines_stratified_dir,"/","indication")), FALSE))
    medicines_stratified_indication <- paste0(medicines_stratified_dir ,"/","indication")
    # Create stratified by reason folder
    invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","reasons_for_discontinuation")), dir.create(paste0(medicines_stratified_dir,"/","reasons_for_discontinuation")), FALSE))
    medicines_stratified_reasons <- paste0(medicines_stratified_dir ,"/","reasons_for_discontinuation")
    # Move stratified records into stratified folders
    # Create stratified folder
    invisible(ifelse(!dir.exists(paste0(contraceptive_counts_dir,"/","stratified")), dir.create(paste0(contraceptive_counts_dir,"/","stratified")), FALSE))
    contraceptives_stratified_dir <- paste0(contraceptive_counts_dir,"/","stratified")
    # Create stratified by age groups folder
    invisible(ifelse(!dir.exists(paste0(contraceptives_stratified_dir,"/","age_group")), dir.create(paste0(contraceptives_stratified_dir,"/","age_group")), FALSE))
    contraceptives_stratified_age_groups <- paste0(contraceptives_stratified_dir ,"/","age_group")
    # Create stratified by indication folder
    invisible(ifelse(!dir.exists(paste0(contraceptives_stratified_dir,"/","indication")), dir.create(paste0(contraceptives_stratified_dir,"/","indication")), FALSE))
    contraceptives_stratified_indication <- paste0(contraceptives_stratified_dir ,"/","indication")
    # Create stratified by contraception type folder
    invisible(ifelse(!dir.exists(paste0(contraceptives_stratified_dir,"/","contra_type")), dir.create(paste0(contraceptives_stratified_dir,"/","contra_type")), FALSE))
    contraceptives_stratified_contra_type <- paste0(contraceptives_stratified_dir ,"/","contra_type")
    
    # Path to diagnosis, procedures and procedures_dx folders 
    diagnoses_pop <- paste0(projectFolder,"/g_intermediate/tmp/diagnoses/")
    procedures_pop <- paste0(projectFolder,"/g_intermediate/tmp/procedures/")
    procedures_dxcodes_pop <- paste0(projectFolder,"/g_intermediate/tmp/procedures_dxcodes/")
    medications_pop <- paste0(projectFolder,"/g_intermediate/tmp/medications/")
    mo_pop <- paste0(projectFolder,"/g_intermediate/tmp/med_obs/")
    # Source file
    source(paste0(pre_dir,"run_counts_final_each_pop.R"))
    # Delete g_intermediate/g_output folders before moving the modified ones back 
    if("g_intermediate" %in% list.files(paste0(projectFolder,"/", regions[reg]))){unlink(paste0(projectFolder,"/", regions[reg],"/g_intermediate"), recursive = T)}
    if("g_output"       %in% list.files(paste0(projectFolder,"/", regions[reg]))){unlink(paste0(projectFolder,"/", regions[reg],"/g_output")      , recursive = T)}
    # Moves g_intermediate, g_output folders from LOT4_script folder to respective regional folders
    file.move(paste0(projectFolder,"/g_intermediate"), paste0(projectFolder, "/", regions[reg], "/g_intermediate"))
    file.move(paste0(projectFolder,"/g_output"), paste0(projectFolder, "/", regions[reg], "/g_output"))
  }
} else {
  # Sources files 
  source(paste0(pre_dir,"info.R"))
  source(paste0(pre_dir,"study_parameters.R"))
  # Clean up
  rm(actual_tables, METADATA_subp)
  # Sources run_counts_final_each_pop.R 
  source(paste0(pre_dir,"run_counts_final_each_pop.R"))
}


