##### Instructions #####
# 1. Save file in p_steps
# 2. Run lines 1-75 of 3_to_run_final_counts.R (to set paths and load packages) 
# 3. Run this file 

# A file (preg_tx_distance_summary.rds) will be created in g_intermediate/DAP_pregnancies.R - this needs to be uploaded to YODA

# Reads in pregnancy files
preg_files<-list.files(paste0(g_intermediate,"DAP_pregnancies"))
preg_files<-preg_files[!grepl("summary",preg_files)]
# Creates list for the dfs
sum_list<-list()

for(file in 1:length(preg_files)){
  # Reads in file
  df<-readRDS(paste0(g_intermediate,"DAP_pregnancies/",preg_files[file]))
  # Creates column distance (distance between medication date and pregnancy start date)
  if(str_detect(preg_files[file],"med_use_during_pregnancy")){
    setnames(df,"prescribing/dispensing_date","dispensing")
    df<-df[,distance:=dispensing-pregnancy_start_date]
  }
  if(str_detect(preg_files[file],"preg_starts_during_tx_episodes")){
    df<-df[,distance:=pregnancy_start_date-episode.start]
  }
  # Get summary statistics of distance column
  dist_summary<-summary(df$distance)
  # Assign values to variables
  min<-dist_summary[[1]]
  lower_quartile<-dist_summary[[2]]
  median<-dist_summary[[3]]
  mean<-dist_summary[[4]]
  upper_quartile<-dist_summary[[5]]
  max<-dist_summary[[6]]
  # Create a data frame
  names<-c("min","lower quartile","median","mean","upper quartile","max")
  values<-c(min,lower_quartile,median,mean,upper_quartile,max)
  dist_summary_df<-data.table(names, values)
  # Rename values to name of df
  setnames(dist_summary_df,"values",gsub(".rds","",paste0(preg_files[file])))
  # Save df in a list
  sum_list[[file]]<-dist_summary_df
 }

# Join all the summaries into one  
all_summaries<-Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "names", all.x = TRUE), sum_list)

# Save into pregnancy folder
saveRDS(all_summaries,paste0(g_intermediate,"DAP_pregnancies/preg_tx_distance_summary.rds"))
