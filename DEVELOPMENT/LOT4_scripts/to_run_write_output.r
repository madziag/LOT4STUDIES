rm(list=ls())
if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)


projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)

my_format<- "csv"
# my_format<-"xlxs"

if(my_format=="xlxs"){
if(!require(xlxs)){install.packages("xlxs")}
library(xlxs)}

all_rds_outputs<-list.files(output_dir, recursive=T, pattern="rds")
dir.create(paste0(output_dir,"/",my_format, "_files"))
my_output_folder<-paste0(output_dir,my_format, "_files/")

for (i in 1:length(all_rds_outputs)){
  my_table<-readRDS(paste0(output_dir, all_rds_outputs[1]))
  if(my_format=="csv"){write.csv(my_table, my_output_folder)}
  else{writexl::write_xlsx(mytable,my_output_folder )}
}