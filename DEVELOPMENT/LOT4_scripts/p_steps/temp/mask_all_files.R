library(data.table)
library(stringr)
library(readxl)
# Set paths

folder_name <- "DENMARK_90"

file_path <- paste0("C:/Users/31653/Desktop/prep_for_masking/", folder_name)

# # Need to be corrected: discontinued by age, tx_dur, indication (divide by 100)
# files_to_correct <- list.files(file_path, pattern="age_group_12-20.99_discontinued_counts|age_group_21-30.99_discontinued_counts|age_group_31-40.99_discontinued_counts|age_group_41-55.99_discontinued_counts|tx_dur_group_0-182_discontinued_counts|tx_dur_group_182-365_discontinued_counts|tx_dur_group_over365_discontinued_counts|indication_bipolar_discontinued_counts|indication_epilepsy_discontinued_counts|indication_migraine_discontinued_counts|indication_multiple_discontinued_counts|indication_unknown_discontinued_counts")
# 
# for(i in 1:length(files_to_correct)){
#   df<-as.data.table(read.csv(paste0(file_path,"/",files_to_correct[i])))
#   df[,rates:=as.numeric(N)/as.numeric(Freq)][is.nan(rates)|is.na(rates),rates:=0]
#   fwrite(df,paste0(file_path,"/",files_to_correct[i]))
# }

# Mask all files 
files_to_mask <- list.files(file_path)

# No change: Switched/Contra Prior/Med use during Contra/Preg tests
# Prevalence/Incidence/Preg Starts/Med use during pregs: [,rates:=round(as.numeric(N)/as.numeric(Freq), 5)][,rates:=rates*1000]
# Discontinued: [,rates:=round(as.numeric(N)/as.numeric(Freq),5)][,rates:=rates*100]

for(i in 1:length(files_to_mask)){
  # df <- as.data.table(read.csv(paste0(file_path,"/",files_to_mask[i])))
  df <- as.data.table(read_excel(paste0(file_path,"/",files_to_mask[i])))
  # df[,masked:=ifelse(N>0 & N<5, 1, 0)]
  df[,N:=as.character(N)][,Freq:=as.character(Freq)][,rates:=as.character(rates)]
  # df[masked==1, N:="<5"][masked==1 & Freq>0 & Freq<5, Freq:="<5"][masked==1,rates:="N/A"]
  
  df[masked=="yes", N:="<5"][masked=="yes",rates:="N/A"]
  if(length(unique(df$masked))>1){print(files_to_mask[i])}
  # Save file 
  # fwrite(df,paste0(file_path,"/PHARMO_",files_to_mask[i]))
  fwrite(df,paste0(file_path,"/DNR_",gsub("xlsx", "csv", files_to_mask[i])))
}



