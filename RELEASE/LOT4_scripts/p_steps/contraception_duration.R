#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 05/01/2022

#this section finds files matching any contraception vocabulary from the three possible folders: medications, procedures and diagnoses
# Lists all contraception files (depending on population )
# ### From Medications folder 
# contracep_med_list<-list.files(paste0(tmp,"medications/"), full.names = T,pattern="contracep")
# if(pop_prefix == "PC"){contracep_med_list <- contracep_med_list[!grepl("PC_HOSP",contracep_med_list)]}
# if(pop_prefix == "PC_HOSP"){contracep_med_list <- contracep_med_list[grepl("PC_HOSP",contracep_med_list)]}
# ### From procedures folder 
# contracep_proc_list<-list.files(paste0(tmp,"procedures/"), full.names = T,pattern="iud") 
# if(pop_prefix == "PC"){contracep_proc_list <- contracep_proc_list[!grepl("PC_HOSP",contracep_proc_list)]}
# if(pop_prefix == "PC_HOSP"){contracep_proc_list <- contracep_proc_list[grepl("PC_HOSP",contracep_proc_list)]}
# ### From diagnoses folder 
# contracep_diag_list<-list.files(paste0(tmp,"diagnoses/"), full.names = T, pattern="iud")   
# if(pop_prefix == "PC"){contracep_diag_list <- contracep_diag_list[!grepl("PC_HOSP",contracep_diag_list)]}
# if(pop_prefix == "PC_HOSP"){contracep_diag_list <- contracep_diag_list[grepl("PC_HOSP",contracep_diag_list)]}
# ### From procedures_dxcodes folder 
# contracep_proc_diag_list<-list.files(paste0(tmp,"procedures_dxcodes/"), full.names = T, pattern="iud")   
# if(pop_prefix == "PC"){contracep_proc_diag_list <- contracep_proc_diag_list[!grepl("PC_HOSP",contracep_proc_diag_list)]}
# if(pop_prefix == "PC_HOSP"){contracep_proc_diag_list <- contracep_proc_diag_list[grepl("PC_HOSP",contracep_proc_diag_list)]}
# contracep_tables<-c(unlist(contracep_med_list), unlist(contracep_proc_list), unlist(contracep_diag_list), unlist(contracep_proc_diag_list))
# 
# contracep_med_list<-list.files(paste0(tmp,"medications/"), full.names = F,pattern="contracep")
# if(pop_prefix == "PC"){contracep_med_list <- contracep_med_list[!grepl("PC_HOSP",contracep_med_list)]}
# if(pop_prefix == "PC_HOSP"){contracep_med_list <- contracep_med_list[grepl("PC_HOSP",contracep_med_list)]}
# ### From procedures folder 
# contracep_proc_list<-list.files(paste0(tmp,"procedures/"), full.names = F,pattern="iud") 
# if(pop_prefix == "PC"){contracep_proc_list <- contracep_proc_list[!grepl("PC_HOSP",contracep_proc_list)]}
# if(pop_prefix == "PC_HOSP"){contracep_proc_list <- contracep_proc_list[grepl("PC_HOSP",contracep_proc_list)]}
# ### From diagnoses folder 
# contracep_diag_list<-list.files(paste0(tmp,"diagnoses/"), full.names = F, pattern="iud")   
# if(pop_prefix == "PC"){contracep_diag_list <- contracep_diag_list[!grepl("PC_HOSP",contracep_diag_list)]}
# if(pop_prefix == "PC_HOSP"){contracep_diag_list <- contracep_diag_list[grepl("PC_HOSP",contracep_diag_list)]}
# ### From procedures_dxcodes folder 
# contracep_proc_diag_list<-list.files(paste0(tmp,"procedures_dxcodes/"), full.names = F, pattern="iud")   
# if(pop_prefix == "PC"){contracep_proc_diag_list <- contracep_proc_diag_list[!grepl("PC_HOSP",contracep_proc_diag_list)]}
# if(pop_prefix == "PC_HOSP"){contracep_proc_diag_list <- contracep_proc_diag_list[grepl("PC_HOSP",contracep_proc_diag_list)]}
# 
# contracep_names<-c(unlist(contracep_med_list), unlist(contracep_proc_list), unlist(contracep_diag_list), unlist(contracep_proc_diag_list))

contracep_all_list<-list.files(contraceptive_dir, pattern=paste0(pop_prefix, "_contracep_"), full.names = T)   
contracep_tables<-c(unlist(contracep_all_list))

contracep_all_list<-list.files(contraceptive_dir, pattern=paste0(pop_prefix, "_contracep_"), full.names = F)   
contracep_names<-c(unlist(contracep_all_list))

# In each dataset, create a new column called assumed_duration.
# diagnoses\iud_diag.rds: assumed_duration = 1095
# procedures\iud.rds: assumed_duration = 1095

# medications\contracep_fixedcomb.rds: assumed_duration = 28
# medications\contracep_implant.rds: assumed_duration = 1095
# medications\contracep_injection.rds: assumed_duration = 84
# medications\contracep_IUD.rds: assumed_duration = 1095
# medications\contracep_patch.rds: assumed_duration = 28
# medications\contracep_progest.rds: assumed_duration = 28
# medications\contracep_sequenprep.rds: assumed_duration = 28
# medications\contracep_vaginalring.rds: assumed_duration = 28

types_contra<-c("iud_diag.rds", "iud.rds", "fixedcomb.rds", "implant.rds","injection.rds","IUD.rds",
                "patch.rds","progest.rds", "sequenprep.rds", "vaginalring.rds")
duration_contra<-c(1095, 1095, 28, 1095,84, 1095, 28,28,28,28)

contra_type_dur<-as.data.frame(cbind(types_contra, duration_contra))

# #folder to store output
# invisible(ifelse(!dir.exists(paste0(tmp, "all_contraception/")), dir.create(paste0(tmp, "all_contraception/")), FALSE))
# contra_folder <- paste0(tmp, "all_contraception/")

# In each data set, create a new column called start_contraception, where you copy over the value from 
# the record date column as follows
# 
# diagnoses\iud_diag.rds: start_contraception := date_record variable
# procedures\iud.rds: start_contraception := procedure_date variable
# medications\...: start_contraception := date_dispending variable or date_prescription variable

# start_vars<-c("date_record", "procedure_date", "date_dispensing", "date_prescription")

#  empty dataframe to store loop results
all_contra=data.frame()
#empty vector to check dataset collation
my_rows<-vector()

for (i in 1:length(contracep_tables)){
  #get data
  my_contra<-as.data.table(readRDS(contracep_tables[i]))
  my_rows[i]<-nrow(my_contra)
  #match type of contraception in dataframe to options
  my_dur<-contra_type_dur[stringr::str_detect(contracep_tables[i],types_contra),]
  #directly impute the duration of contraception
  my_contra$assumed_duration<-rep(my_dur$duration_contra,nrow(my_contra))
  #standardize start date of contraception
  # event_date is a composite variable indicating the onset of treatment --> rename to contraception_record_date
  #IUD does not have event_date, but procedure_date  ### event_date and procedure_dates have been changed to Date for uniformity's sake
  if("Date" %in%names(my_contra)) {names(my_contra)[names(my_contra)=="Date"]<-"contraception_record_date"}
  ###################################################################################
  # rename meaning column to contraception_meaning,
  ###################################################################################
  # should only have one per record- but this could be multiple within the data? 
  meaning_contra<-names(my_contra)[(suppressWarnings( stringr::str_detect(names(my_contra),"Meaning")))]
  ##warning message not relevant, suppressed
  names(my_contra)[names(my_contra)==meaning_contra]<-"contraception_meaning"
  contra_name <- gsub(".rds", "", contracep_names[i])
  contra_name <- gsub(paste0(pop_prefix, "_"), "", contra_name)
  my_contra[,contra_type:=contra_name]
  # Save record
  saveRDS(my_contra,(paste0(contraceptive_dir,contracep_names[i])))
  #make "master" contraception dataframe for treatment episodes 
  new_df<-my_contra[,c("person_id","contraception_record_date", "assumed_duration", "Code", "contraception_meaning", "contra_type")]
  all_contra<-rbind(all_contra, new_df)
 
}

# Check 
if(nrow(all_contra)==sum(my_rows)){print("all_contra OK")}else{print("all contra incomplete")}
# Saves file
saveRDS(all_contra,(paste0(contraceptive_dir, pop_prefix, "_all_contra.rds" )))
# Clean up 
rm(list = grep("^all_|^my_rows|^my_contra|contra_type_dur|new_df|my_dur", ls(), value = TRUE))
