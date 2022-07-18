# Run the first 50 lines of 3_to_run_final_counts.R -> to initialize paths 

regions<-list.dirs(path=multiple_regions_dir,full.names=FALSE,recursive=FALSE)

# Loops over each region
for(reg in 1:length(regions)){
  
  if(length(list.files(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/"), pattern = "PC_Retinoid_incidence_counts.rds"))>0){
    
    denominator_file<-list.files(paste0(projectFolder,"/",regions[reg],"/"), recursive = T, pattern = "PC_denominator.rds", full.names = T)
    denominator<-readRDS(denominator_file)
    # Drop columns you do not need
    denominator[,studyFUmonths:=NULL]
    # Split Y-M variable to year - month columns (for creating empty df & and getting min and max of data available)
    denominator[,c("year","month"):= tstrsplit(YM,"-",fixed=TRUE)]
    denominator[,year:=as.integer(year)][,month:=as.integer(month)]
    # Get min and max data available from denominator
    min_data_available<-min(denominator$year)
    max_data_available<-max(denominator$year)
    # Create empty df using these min and max values
    empty_df<-as.data.table(expand.grid(seq(min_data_available,max_data_available),seq(1,12)))
    names(empty_df) <- c("year","month")
    
    # Load all incident counts
    incidence_all_counts<-readRDS(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_Retinoid_incidence_counts.rds"))
    # Read in retinoid incidence file
    df_incidence<-readRDS(paste0(projectFolder,"/",regions[reg],"/g_intermediate/counts_dfs/objective_1/PC_Retinoid_incidence_counts_df.rds"))
    # Performs incidence counts - stratified by ATC (for retinoids)
    incidence_by_ATC<-df_incidence[,.N, by = .(year,month,ATC)]
    # Get unique values of age groups - for the for loop
    ATC_unique<-unique(incidence_by_ATC$ATC)
    
    for(group in 1:length(ATC_unique)){
      # Create a subset of age group
      each_group<-incidence_by_ATC[ATC==ATC_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(ATC),ATC:=ATC_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      # Prepare denominator (all prevalence counts )
      incidence_all_counts_min<-incidence_all_counts[,c("YM","N")]
      setnames(incidence_all_counts_min,"N","Freq")
      # Create counts file
      incidence_ATC_counts<-merge(x=each_group,y=incidence_all_counts_min,by=c("YM"),all.x=TRUE)
      # Masking no longer applied
      incidence_ATC_counts[,masked:=0]
      # Calculates rates
      incidence_ATC_counts<-incidence_ATC_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Keeps necessary columns
      incidence_ATC_counts<-incidence_ATC_counts[,c("YM","N","Freq","rates","masked","true_value")]
      # Saves files in medicine counts folder
      saveRDS(incidence_ATC_counts,(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_Retionoid_ATC_", ATC_unique[group],"_incidence_counts.rds")))
      write.csv(incidence_ATC_counts,(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_Retionoid_ATC_", ATC_unique[group],"_incidence_counts.csv")))
    }
    
    ### PREVALENCE 
    # Load all prevalent counts
    prevalence_all_counts<-readRDS(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_Retinoid_prevalence_counts.rds"))
    # Read in retinoid incidence file
    df_prevalence<-readRDS(paste0(projectFolder,"/",regions[reg],"/g_intermediate/counts_dfs/objective_1/PC_Retinoid_prevalence_counts_df.rds"))
    # Performs prevalence counts - stratified by ATC (for retinoids)
    prevalence_by_ATC<-df_prevalence[,.N, by = .(year,month,ATC)]
    # Get unique values of age groups - for the for loop
    ATC_unique<-unique(prevalence_by_ATC$ATC)
    
    for(group in 1:length(ATC_unique)){
      # Create a subset of age group
      each_group<-prevalence_by_ATC[ATC==ATC_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(ATC),ATC:=ATC_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      # Prepare denominator (all prevalence counts )
      prevalence_all_counts_min<-prevalence_all_counts[,c("YM","N")]
      setnames(prevalence_all_counts_min,"N","Freq")
      # Create counts file
      prevalence_ATC_counts<-merge(x=each_group,y=prevalence_all_counts_min,by=c("YM"),all.x=TRUE)
      # Masking no longer applied
      prevalence_ATC_counts[,masked:=0]
      # Calculates rates
      prevalence_ATC_counts<-prevalence_ATC_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Keeps necessary columns
      prevalence_ATC_counts<-prevalence_ATC_counts[,c("YM","N","Freq","rates","masked","true_value")]
      # Saves files in medicine counts folder
      saveRDS(prevalence_ATC_counts,(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_Retionoid_ATC_", ATC_unique[group],"_prevalence_counts.rds")))
      write.csv(prevalence_ATC_counts,(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_Retionoid_ATC_", ATC_unique[group],"_prevalence_counts.csv")))
    }
    
    # Create folder inside medicines folder for csv or excel file format
    invisible(ifelse(!dir.exists(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/retinoids_by_ATC/")),
                     dir.create(paste0(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/retinoids_by_ATC"))), FALSE))
    medicines_counts_add <- paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/retinoids_by_ATC")
    
    for(file in list.files(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/"),pattern="_ATC_",ignore.case = F)){file.copy(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/", file),medicines_counts_add)}
    for(file in list.files(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/"),pattern="_ATC_",ignore.case = F)){unlink(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/", file))}
  }
}



############### 
# PC_HOSP
# Loops over each region
for(reg in 1:length(regions)){
    
  if(length(list.files(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/"), pattern = "PC_HOSP_Retinoid_incidence_counts.rds"))>0){
    
    denominator_file<-list.files(paste0(projectFolder,"/",regions[reg],"/"), recursive = T, pattern = "PC_HOSP_denominator.rds", full.names = T)
    denominator<-readRDS(denominator_file)
    # Drop columns you do not need
    denominator[,studyFUmonths:=NULL]
    # Split Y-M variable to year - month columns (for creating empty df & and getting min and max of data available)
    denominator[,c("year","month"):= tstrsplit(YM,"-",fixed=TRUE)]
    denominator[,year:=as.integer(year)][,month:=as.integer(month)]
    # Get min and max data available from denominator
    min_data_available<-min(denominator$year)
    max_data_available<-max(denominator$year)
    # Create empty df using these min and max values
    empty_df<-as.data.table(expand.grid(seq(min_data_available,max_data_available),seq(1,12)))
    names(empty_df) <- c("year","month")
    
    # Load all incident counts
    incidence_all_counts<-readRDS(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_HOSP_Retinoid_incidence_counts.rds"))
    # Read in retinoid incidence file
    df_incidence<-readRDS(paste0(projectFolder,"/",regions[reg],"/g_intermediate/counts_dfs/objective_1/PC_HOSP_Retinoid_incidence_counts_df.rds"))
    # Performs incidence counts - stratified by ATC (for retinoids)
    incidence_by_ATC<-df_incidence[,.N, by = .(year,month,ATC)]
    # Get unique values of age groups - for the for loop
    ATC_unique<-unique(incidence_by_ATC$ATC)
    
    for(group in 1:length(ATC_unique)){
      # Create a subset of age group
      each_group<-incidence_by_ATC[ATC==ATC_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(ATC),ATC:=ATC_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      # Prepare denominator (all prevalence counts )
      incidence_all_counts_min<-incidence_all_counts[,c("YM","N")]
      setnames(incidence_all_counts_min,"N","Freq")
      # Create counts file
      incidence_ATC_counts<-merge(x=each_group,y=incidence_all_counts_min,by=c("YM"),all.x=TRUE)
      # Masking no longer applied
      incidence_ATC_counts[,masked:=0]
      # Calculates rates
      incidence_ATC_counts<-incidence_ATC_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Keeps necessary columns
      incidence_ATC_counts<-incidence_ATC_counts[,c("YM","N","Freq","rates","masked","true_value")]
      # Saves files in medicine counts folder
      saveRDS(incidence_ATC_counts,(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_HOSP_Retionoid_ATC_", ATC_unique[group],"_incidence_counts.rds")))
      write.csv(incidence_ATC_counts,(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_HOSP_Retionoid_ATC_", ATC_unique[group],"_incidence_counts.csv")))
    }
    
    ### PREVALENCE 
    # Load all prevalent counts
    prevalence_all_counts<-readRDS(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_HOSP_Retinoid_prevalence_counts.rds"))
    # Read in retinoid incidence file
    df_prevalence<-readRDS(paste0(projectFolder,"/",regions[reg],"/g_intermediate/counts_dfs/objective_1/PC_HOSP_Retinoid_prevalence_counts_df.rds"))
    # Performs prevalence counts - stratified by ATC (for retinoids)
    prevalence_by_ATC<-df_prevalence[,.N, by = .(year,month,ATC)]
    # Get unique values of age groups - for the for loop
    ATC_unique<-unique(prevalence_by_ATC$ATC)
    
    for(group in 1:length(ATC_unique)){
      # Create a subset of age group
      each_group<-prevalence_by_ATC[ATC==ATC_unique[group]]
      # Adjust for PHARMO
      if(is_PHARMO){each_group<-each_group[year<2020,]}else{each_group<-each_group[year<2021,]}
      # Merge with empty df (for counts that do not have counts for all months and years of study)
      each_group<-as.data.table(merge(x=empty_df,y=each_group,by=c("year","month"),all.x=TRUE))
      # Fills in missing values with 0
      each_group[is.na(N),N:=0][is.na(ATC),ATC:=ATC_unique[group]]
      # Column detects if data is available this year or not #3-> data is not available, 0 values because data does not exist; 16-> data is available, any 0 values are true
      each_group[year<min_data_available|year>max_data_available,true_value:=3][year>=min_data_available&year<=max_data_available,true_value:=16]
      # Create YM variable 
      each_group<-within(each_group,YM<-sprintf("%d-%02d",year,month))
      # Prepare denominator (all prevalence counts )
      prevalence_all_counts_min<-prevalence_all_counts[,c("YM","N")]
      setnames(prevalence_all_counts_min,"N","Freq")
      # Create counts file
      prevalence_ATC_counts<-merge(x=each_group,y=prevalence_all_counts_min,by=c("YM"),all.x=TRUE)
      # Masking no longer applied
      prevalence_ATC_counts[,masked:=0]
      # Calculates rates
      prevalence_ATC_counts<-prevalence_ATC_counts[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
      # Keeps necessary columns
      prevalence_ATC_counts<-prevalence_ATC_counts[,c("YM","N","Freq","rates","masked","true_value")]
      # Saves files in medicine counts folder
      saveRDS(prevalence_ATC_counts,(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_HOSP_Retionoid_ATC_", ATC_unique[group],"_prevalence_counts.rds")))
      write.csv(prevalence_ATC_counts,(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/PC_HOSP_Retionoid_ATC_", ATC_unique[group],"_prevalence_counts.csv")))
    }
    
    # Create folder inside medicines folder for csv or excel file format
    invisible(ifelse(!dir.exists(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/retinoids_by_ATC/")),
                     dir.create(paste0(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/retinoids_by_ATC"))), FALSE))
    medicines_counts_add <- paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/retinoids_by_ATC")
    
    for(file in list.files(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/"),pattern="_ATC_",ignore.case = F)){file.copy(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/", file),medicines_counts_add)}
    for(file in list.files(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/"),pattern="_ATC_",ignore.case = F)){unlink(paste0(projectFolder,"/",regions[reg],"/g_output/medicines_counts/", file))}
  }
}

