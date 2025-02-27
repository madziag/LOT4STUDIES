# writes count output into csv or excel format 


if(my_format=="xlsx"){
if(!require(writexl)){install.packages("writexl")}
library(writexl)}

all_rds_outputs<-list.files(output_dir, recursive=T, pattern="rds")
dir.create(paste0(output_dir,my_format, "_files"))
my_output_folder<-paste0(output_dir,my_format, "_files/")


for (i in 1:length(all_rds_outputs)){
  my_name<-gsub(".*/","",all_rds_outputs[i])
  my_name<-substr(my_name, 1,nchar(my_name)-4)
  my_table<-readRDS(paste0(output_dir, all_rds_outputs[i]))
  if(my_format=="csv"){write.csv(my_table, paste0(my_output_folder, my_name,".csv"), row.names = F )}
  else{writexl::write_xlsx(my_table,paste0(my_output_folder, my_name,".xlsx"))}

  my_table<-data.frame(readRDS(paste0(output_dir, all_rds_outputs[i])))
  if(my_format=="csv"){write.csv(my_table, paste0(my_output_folder, my_name, ".csv"))}
  else{writexl::write_xlsx(my_table,paste0(my_output_folder, my_name, ".xlsx"))}

}
