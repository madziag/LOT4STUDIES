# writes count output into csv or excel format 

if (multiple_regions == T){
  all_rds_outputs<-list.files(paste0(projectFolder, "/", regions[reg], "/g_output/"), recursive=T, pattern=".rds")
  # exclude already completed folders
  all_rds_outputs<- all_rds_outputs[ !grepl("preliminary_counts", all_rds_outputs) ]
  all_rds_outputs<- all_rds_outputs[ !grepl("denominator", all_rds_outputs) ]
  all_rds_outputs<- all_rds_outputs[ !grepl("FlowChart", all_rds_outputs) ]
  invisible(ifelse(!dir.exists(paste0(projectFolder, "/", regions[reg], "/g_output/",my_format, "_files")),  dir.create(paste0(projectFolder, "/", regions[reg], "/g_output/",my_format, "_files")), FALSE))
  my_output_folder<-paste0(projectFolder, "/", regions[reg], "/g_output/",my_format, "_files")
  
  for (i in 1:length(all_rds_outputs)){
    my_name<-gsub(".*/","",all_rds_outputs[i])
    my_name<-substr(my_name, 1,nchar(my_name)-4)
    
    my_table<-data.frame(readRDS(paste0(projectFolder, "/", regions[reg], "/g_output/", all_rds_outputs[i])))
    if(my_format=="csv"){write.csv(my_table, paste0(my_output_folder,"/", my_name,".csv"), row.names = F )}
    else{writexl::write_xlsx(my_table,paste0(my_output_folder, "/", my_name,".xlsx"))}

  }
  # Move baseline csv files to baseline folder
  # Create folder for csv file within baseline
  invisible(ifelse(!dir.exists(paste0(baseline_dir, "/",my_format, "_files")),  dir.create(paste0(baseline_dir,"/",my_format, "_files")), FALSE))
  my_output_folder_baseline<-paste0(baseline_dir, "/",my_format, "_files")
  
  files_to_move_baseline <- list.files(path=my_output_folder, pattern = "baseline")
  
  for(file in files_to_move_baseline){
    file.move(paste0(my_output_folder,"/",file), paste0(my_output_folder_baseline,"/" ,file))
  }
  
  # Move med use in pregnancy csv files to pregnancy folder
  # Create folder for csv file within pregnancy folder 
  invisible(ifelse(!dir.exists(paste0(preg_med_counts, "/",my_format, "_files")),  dir.create(paste0(preg_med_counts,"/",my_format, "_files")), FALSE))
  my_output_folder_preg_meds <-paste0(preg_med_counts, "/",my_format, "_files")
  files_to_move_preg_meds <- list.files(path=my_output_folder, pattern = "counts")
  
  for(file in   files_to_move_preg_meds){
    file.move(paste0(my_output_folder,"/",file), paste0(my_output_folder_preg_meds,"/" ,file))
  }
  
  unlink(paste0(projectFolder, "/", regions[reg], "/g_output/", paste0(my_format,"_files")), recursive = TRUE)
  
} else {
  print("I am HERE!")
  all_rds_outputs<-list.files(output_dir, recursive=T, pattern=".rds")

  # exclude already completed folders 
  all_rds_outputs<- all_rds_outputs[ !grepl("preliminary_counts", all_rds_outputs) ]
  all_rds_outputs<- all_rds_outputs[ !grepl("denominator", all_rds_outputs) ]
  all_rds_outputs<- all_rds_outputs[ !grepl("FlowChart", all_rds_outputs) ]
  
  invisible(ifelse(!dir.exists(paste0(output_dir,my_format, "_files")),  dir.create(paste0(output_dir,my_format, "_files")), FALSE))
  my_output_folder<-paste0(output_dir,my_format, "_files/")
  
  
  for (i in 1:length(all_rds_outputs)){
    my_name<-gsub(".*/","",all_rds_outputs[i])
    my_name<-substr(my_name, 1,nchar(my_name)-4)
    
    my_table<-data.frame(readRDS(paste0(output_dir, all_rds_outputs[i])))
    if(my_format=="csv"){write.csv(my_table, paste0(my_output_folder, my_name,".csv"), row.names = F )}
    else{writexl::write_xlsx(my_table,paste0(my_output_folder, my_name,".xlsx"))}
    
  }

  # Move baseline csv files to baseline folder
  # Create folder for csv file within baseline
  invisible(ifelse(!dir.exists(paste0(baseline_tables_dir,"/",my_format, "_files")),  dir.create(paste0(baseline_tables_dir,"/",my_format, "_files")), FALSE))
  my_output_folder_baseline<-paste0(baseline_tables_dir,"/",my_format, "_files")
  
  files_to_move_baseline <- list.files(path=my_output_folder, pattern = "baseline")
  
  for(file in files_to_move_baseline){
    file.move(paste0(my_output_folder,"/",file), paste0(my_output_folder_baseline,"/" ,file))
  }
  
  # Move med use in pregnancy csv files to pregnancy folder
  # Create folder for csv file within pregnancy folder 
  invisible(ifelse(!dir.exists(paste0(preg_med_counts, "/",my_format, "_files")),  dir.create(paste0(preg_med_counts,"/",my_format, "_files")), FALSE))
  my_output_folder_preg_meds <-paste0(preg_med_counts, "/",my_format, "_files")
  files_to_move_preg_meds <- list.files(path=my_output_folder, pattern = "counts")
  
  for(file in   files_to_move_preg_meds){
    file.move(paste0(my_output_folder,"/",file), paste0(my_output_folder_preg_meds,"/" ,file))
  }
  
   unlink(paste0(output_dir, paste0(my_format,"_files")), recursive = TRUE)
}







