# Takes into account subpopulations 
# Loads records with Retinoid/Valproate use (depending on study type)
# Loads study_population 
# Runs individual scripts for each subpopulation
# Result: If SUBP -> TRUE then each folder will contain (if present) results coming from all indicated subpops. Resulting files are prefixed with the name of the subpop

# Loads study population/populations 
populations <- list.files(populations_dir, pattern = "study_population")
# Checks for study type
if(study_type == "Retinoid"){
  pattern1 = c("Retinoid")
} else if (study_type == "Valproate") {
  pattern1 = c("Valproate")
} else if (study_type == "Both") {
  pattern1 = c("Retinoid","Valproate")
} else {
  print ("Please indicate Study Type")
}
# Loads files with matching pattern 
med_files <- list.files(path=medications_pop, pattern=paste0(pattern1, collapse="|"))
# Reads in records of population with indicated study type
study_pop_meds <- do.call(rbind,lapply(paste0(medications_pop,"/",med_files), readRDS))

if (is_Denmark == T){
  # Loads study population
  study_population <- readRDS(paste0(populations_dir, populations))
  # Creates baseline tables 
  source(paste0(pre_dir,"CreateBaselineTables.R"))
  # Converts all .rds files into .csv or .xlsx (indicated by user)
  source(paste0(pre_dir,"write_output.R"))
  
} else {

# Loops over each subpopulation
for(pop in 1:length(populations)){
  # Loads study population
  study_population <- readRDS(paste0(populations_dir, populations[pop]))
  # Creates baseline tables 
  source(paste0(pre_dir,"CreateBaselineTables.R"))
  # Looks for medication use during pregnancies
  source(paste0(pre_dir, "monthly_counts_med_use_in_pregnancy.R"))
  # Makes plots of all counts files
  source(paste0(pre_dir,"plots_mask.R"))
  # Converts all .rds files into .csv or .xlsx (indicated by user)
  source(paste0(pre_dir,"write_output.R"))
}
}

# Creates csv/xslx folder inside baseline tables and pregnancy counts folders
invisible(ifelse(!dir.exists(paste0(baseline_tables_dir,"/",my_format,"_files")), dir.create(paste0(baseline_tables_dir,"/",my_format,"_files")), FALSE))
baseline_tables_csv_xlsx <- paste0(baseline_tables_dir,"/",my_format,"_files")
# Create folder inside pregnancy folder for csv or excel file format
invisible(ifelse(!dir.exists(paste0(preg_med_counts,"/",my_format,"_files")), dir.create(paste0(preg_med_counts,"/",my_format,"_files")), FALSE))
preg_med_counts_csv_xlsx <- paste0(preg_med_counts,"/",my_format,"_files")
# Create plots folder inside pregnancy counts folder
invisible(ifelse(!dir.exists(paste0(preg_med_counts,"/","plots")), dir.create(paste0(preg_med_counts,"/","plots")), FALSE))
preg_med_counts_plots <- paste0(preg_med_counts,"/","plots")

# Moves csv/xslx files with matching pattern to corresponding folders
for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern="baseline", ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file),baseline_tables_csv_xlsx)}
for (file in list.files(path=paste0(output_dir,my_format,"_files"), pattern=paste0(c("Retinoid_preg", "Valproate_preg", "Pregnancy_ALL"), collapse="|"), ignore.case = T)){file.copy(paste0(output_dir,my_format,"_files/", file),preg_med_counts_csv_xlsx )}
# Moves plot files with matching pattern to corresponding folders
for (file in list.files(path=paste0(output_dir,"plots"), pattern=paste0(c("Retinoid_preg", "Valproate_preg", "Pregnancy_ALL"), collapse="|"), ignore.case = T)){file.copy(paste0(output_dir,"plots/",file),preg_med_counts_plots )}
# Removes csv/xlsx, plots and monthly counts folders from LOT4_script (after everything has been copied to corresponding folders)
for (file in list.files(path=paste0(output_dir), pattern=paste0(c("plots", paste0(my_format,"_files")), collapse="|"), ignore.case = T)){unlink(paste0(output_dir,file), recursive = TRUE)}
# Deletes temp files 
for(file in list.files(path = tmp, pattern ="events_")){unlink(paste0(tmp, file), recursive = TRUE)}




