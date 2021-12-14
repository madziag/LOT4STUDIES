#import study population data

#for BIFAP with multiple regions and subpopulation meanings PC and PChosp extract multiple study population files to list

if(multiple_regions==T){
  study_population<-list()
  
  my_regions<-list.dirs(projectFolder, recursive = F)
  for (i in 1:length(my_regions)){
      study_subpop<-list
      study_subpop<-list_files(paste0(my_regions, "/g_intermediate/populations"))
      for (j in 1:length(study_subpop)){
        study_population[i][j]<-study_subpop[j]
      }
  }
  study_population<-unlist(study_population)
  readRDS(study_population)
}else{readRDS(readRDS(paste0(populations_dir, "ALL_study_population.rds"))}