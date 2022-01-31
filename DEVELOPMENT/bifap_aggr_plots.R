#BIFAP plots per outcome with all regions

#data is organized with each counts of each outcome in a folder, containing all regions
#i in 1:length(my_folders)
#containing 
#j in 1:length(list.files(my_folders[i]))

all_regions_dir<-pasteo(path_dir,"ALL_regions/")

all_regions_dir<-(output_dir)

my_folders<-(list.files(all_regions_dir, pattern="counts"))

#using g_output "counts" as a development example

for(i in 1:length(my_folders)){
  #read in list of RDS files
  my_files<-lapply(paste0(all_regions_dir,my_folders[1]), readRDS)
                   
  for(j in 1:length(my_files)){
    my_data<-readRDS(paste0(my_files))
  }
}
