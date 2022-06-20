library(data.table); library(stringr)
aggregate_folders<-list.files("C:/Users/31653/Desktop/BIFAP_AGGREGATE/")

## Loops through all the numerator folders 
for (i in 1:length(aggregate_folders)){
  record_dir<-paste0("C:/Users/31653/Desktop/BIFAP_AGGREGATE/",aggregate_folders[i],"/") 
  # Get list of all files in folder 
  all_files<-lapply(paste0(record_dir,list.files(record_dir)),read.csv,header=TRUE) 
  all_df<-as.data.table(do.call(rbind,all_files))
  if(str_detect(aggregate_folders[i], "PY")){
    # Add up by  group
    all_df[,Sum_N:=sum(N),by=list(year)][,Sum_Freq:=sum(Freq),by=list(year)]
    all_df[,N:=NULL][,Freq:=NULL][,rates:=NULL][,masked:=NULL][,true_value:=NULL]
    setnames(all_df, "Sum_N","N")
    setnames(all_df, "Sum_Freq","Freq")
    # Recalculate rates
    all_df[,rates:=round(as.numeric(N)/as.numeric(Freq),10)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
    all_df<-all_df[!duplicated(all_df),] # Removes any duplicates
    write.csv(all_df, paste0(record_dir,aggregate_folders[i],"_Pooled.csv")) # Saves pooled file
  }
  if(str_detect(aggregate_folders[i], "PP")){
    # Add up by  group
    all_df[,Sum_N:=sum(N),by=list(intervention)][,Sum_Freq:=sum(Freq),by=list(intervention)]
    all_df[,N:=NULL][,Freq:=NULL][,rates:=NULL][,masked:=NULL][,true_value:=NULL]
    setnames(all_df, "Sum_N","N")
    setnames(all_df, "Sum_Freq","Freq")
    # Recalculate rates
    all_df[,rates:=round(as.numeric(N)/as.numeric(Freq),10)][,rates:=rates*1000][is.nan(rates)|is.na(rates),rates:=0]
    all_df<-all_df[!duplicated(all_df),] # Removes any duplicates
    write.csv(all_df, paste0(record_dir,aggregate_folders[i],"_Pooled.csv")) # Saves pooled file
  }
}
