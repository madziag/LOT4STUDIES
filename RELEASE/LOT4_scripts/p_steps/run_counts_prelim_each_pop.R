#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 16/02/2022

# Takes into account subpopulations 
# Runs individual scripts for each subpopulation
# Result: If SUBP -> TRUE then each folder will contain (if present) results coming from all indicated subpops. Resulting files are prefixed with the name of the subpop

# Loads study population/populations 
populations <- list.files(populations_dir, pattern = "study_population")
# Loops over each subpopulation
for(pop in 1:length(populations)){
  # Loads study population
  study_population <- readRDS(paste0(populations_dir, populations[pop]))
  # Assign study population prefix name
  pop_prefix <- gsub("_study_population.rds", "", populations[pop])
  # Creates sterility list 
  source(paste0(pre_dir,"CreateSterilityList.R"))
  # Creates entry and exit dates
  source(paste0(pre_dir,"CreateEntryExit.R"))
  # Plots age distribution of study population
  source(paste0(pre_dir, "plot_age_distribution.R"))
  # Creates denominator file
  source(paste0(pre_dir,"denominator_monthly.R"))
  # Finds matching dx codes from dx concept set in EVENTS tables
  source(paste0(pre_dir,"monthly_counts_dxcodes.R"))
  # Finds matching ATC from ATC concept set in MEDICINES tables
  source(paste0(pre_dir,"monthly_counts_ATC.R"))
  # Finds matching procedure codes from procedures concept set in EVENTS tables 
  source(paste0(pre_dir,"monthly_counts_procedures.R"))
  # Finds matching dx codes from dx concept set in PROCEDURES tables
  source(paste0(pre_dir,"monthly_counts_dxcodes_in_procedures.R"))
  # Finds matching pregnancy test codes in MEDICAL OBSERVATIONS tables (for BIFAP only)
  source(paste0(pre_dir, "monthly_counts_medical_observations.R"))
  # Makes plots of all counts files
  source(paste0(pre_dir,"plots_mask.R"))
  # Converts all .rds files into .csv or .xlsx (indicated by user)
  source(paste0(pre_dir,"write_output.R"))
}

# Moves all counts, plots, formatted files to preliminary_counts folder
pattern1 = c("monthly_counts", "plots", paste0(my_format,"_files"))
# files_to_move <- list.files(path=output_dir, pattern=paste0(pattern1, collapse="|"))
for(file in list.files(path=output_dir, pattern=paste0(pattern1, collapse="|"), ignore.case = T)){file.move(paste0(output_dir,file), paste0(paste0(preliminary_counts_dir, "/") ,file))}
# Deletes temp files
for(file in list.files(path = tmp, pattern ="events_")){unlink(paste0(tmp, file), recursive = TRUE)}
# Delete Flowchart files 
for(file in list.files(path = output_dir, pattern ="FlowChart")){unlink(paste0(output_dir, file), recursive = TRUE)}
# Delete Study_population_folder 
for(file in list.files(path = output_dir, pattern ="STUDY_SOURCE_POPULATION")){unlink(paste0(output_dir, file), recursive = TRUE)}




