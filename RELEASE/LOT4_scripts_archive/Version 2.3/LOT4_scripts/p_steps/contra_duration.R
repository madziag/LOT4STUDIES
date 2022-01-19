# Using the contraception concept set datasets in g_intermediate/temp

# diagnoses\iud_diag.rds
# procedures\iud.rds
# medications\contracep_fixedcomb.rds
# medications\contracep_implant.rds
# medications\contracep_injection.rds
# medications\contracep_IUD.rds
# medications\contracep_patch.rds
# medications\contracep_progest.rds
# medications\contracep_sequenprep.rds
# medications\contracep_vaginalring.rds



contracep_med_list<-list.files(paste0(tmp,"medications/"), full.names = T,pattern="contracep")
contracep_proc_list<-list.files(paste0(tmp,"procedures/"), full.names = T,pattern="iud")                               
contracep_diag_list<-list.files(paste0(tmp,"diagnoses/"), full.names = T, pattern="iud")                               

contracep_tables<-c(unlist(contracep_med_list), unlist(contracep_proc_list), unlist(contracep_diag_list))

contracep_med_list<-list.files(paste0(tmp,"medications/"), full.names = F,pattern="contracep")
contracep_proc_list<-list.files(paste0(tmp,"procedures/"), full.names = F,pattern="iud")                               
contracep_diag_list<-list.files(paste0(tmp,"diagnoses/"), full.names = F, pattern="iud")                               

contracep_names<-c(unlist(contracep_med_list), unlist(contracep_proc_list), unlist(contracep_diag_list))


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

#how do I do this... different DAPS have different tables available... need to match based on file name... oof
#Redundat IUD labels- may get multiple matches...
#use stringr match

types_contra<-c("iud_diag", "iud", "fixedcomb", "implant","injection","IUD","patch","progest", "sequenprep", "vaginalring")
duration_contra<-c(1095, 1095, 28, 1095,84, 1095, 28,28,28,28)

contra_type_dur<-as.data.frame(cbind(types_contra, duration_contra))

#folder to store output

dir.create(paste0(tmp, "all_contraception/"))

for (i in 1:length(contracep_tables)){
  my_contra<-readRDS(contracep_tables[i])
  my_dur<-contra_type_dur[stringr::str_detect(contracep_tables[i],types_contra),]
  my_contra$assumed_duration<-rep(my_dur$duration_contra,nrow(my_contra))
  saveRDS(my_contra, path=paste0(tmp, "all_contraception/"), contracep_names[i])
}