# Writes count output into csv or excel format 
# Creates folder for output + path
invisible(ifelse(!dir.exists(paste0(output_dir,my_format, "_files")),  dir.create(paste0(output_dir,my_format, "_files")), FALSE))
my_output_folder <-paste0(output_dir,my_format, "_files/")

# Gets list of .rds files for conversion 
all_rds_outputs<-list.files(output_dir, recursive=T, pattern=".rds")
# Excludes already converted folders 
all_rds_outputs<- all_rds_outputs[!grepl("preliminary_counts", all_rds_outputs) ]
# Loops over files, converting each into desired output
for (i in 1:length(all_rds_outputs)){
  my_name<-gsub(".*/","",all_rds_outputs[i])
  my_name<-substr(my_name, 1,nchar(my_name)-4)
  # Reads in file
  my_table<-data.frame(readRDS(paste0(output_dir, all_rds_outputs[i])))
  # Saves file into desired output
  if(my_format=="csv"){write.csv(my_table, paste0(my_output_folder, my_name,".csv"), row.names = F )}
  else{writexl::write_xlsx(my_table,paste0(my_output_folder, my_name,".xlsx"))}
  
}

rm(all_rds_outputs)
