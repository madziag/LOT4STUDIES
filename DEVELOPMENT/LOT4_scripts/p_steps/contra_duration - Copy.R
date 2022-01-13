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

#this section finds files matching any contraception vocabulary from the three possible folders: medications, procedures and diagnoses

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


types_contra<-c("iud_diag", "iud", "fixedcomb", "implant","injection","IUD","patch","progest", "sequenprep", "vaginalring")
duration_contra<-c(1095, 1095, 28, 1095,84, 1095, 28,28,28,28)

contra_type_dur<-as.data.frame(cbind(types_contra, duration_contra))

#folder to store output

dir.create(paste0(tmp, "all_contraception/"))

contra_folder<-(paste0(tmp, "all_contraception/"))

# In each data set, create a new column called start_contraception, where you copy over the value from 
# the record date column as follows
# 
# diagnoses\iud_diag.rds: start_contraception := date_record variable
# procedures\iud.rds: start_contraception := procedure_date variable
# medications\...: start_contraception := date_dispending variable or date_prescription variable

start_vars<-c("date_record", "procedure_date", "date_dispensing", "date_prescription")

#  empty dataframe to store loop results

all_contra=data.frame()

#empty vector to check dataset collation

my_rows<-vector()

# sapply(substring(contracep_names, ""), function(x) paste0(x[1:(nchar(contracep_names)-4)], collapse=""))
# 
# gsub('.{2}$', '', name)
# apply(x=contracep_names, FUN=substr(contracep_names,1,nchar(contracep_names)-4))

for (i in 1:length(contracep_tables)){
  my_contra<-readRDS(contracep_tables[1])
  my_rows[i]<-nrow(my_contra)
  #match type of contraception in dataframe to options
  my_dur<-contra_type_dur[stringr::str_detect(contracep_tables[4],types_contra),]
  #directly impute the duration of contraception
  my_contra$assumed_duration<-rep(my_dur$duration_contra,nrow(my_contra))
  #find all possible start dates of contraception
  start_contra<-names(my_contra)[(suppressWarnings( stringr::str_detect(names(my_contra),start_vars)))]
  #warning message not relevant, supressed
  print(start_contra)
  #should give me the columns which match... why not? >_<
  if (length(start_contra)>1){
    #here I need a logical test to determine which of the matching columns to use... 
    #if all but one is empty (NA) then this will work
    start_contra<- my_contra[,start_contra][,(apply(X = my_contra[start_contra], MARGIN = 2, FUN = anyNA)==F)]
  }else{start_contra<-my_contra[start_contra]}
  my_contra$start_contraception<-start_contra
  
  ###################################################################################
  # In each data set, create a new column called contraception_meaning, where you copy over the value from 
  # the record date column as follows:
  ###################################################################################
  
  meaning_contra<-my_contra[,(suppressWarnings( stringr::str_detect(names(my_contra),"meaning")))]
  ##warning message not relevant, supressed
  print(names(meaning_contra))
  
  my_contra$contraception_meaning<-meaning_contra[1]
  
  all_contra<-rbind(all_contra, my_contra)
}

#test correct collation of contraception dataframes

if((nrow(all_contra))-(sum(my_rows))==0){print("contraception data ready")
  saveRDS(all_contra,paste0(contra_folder,"all_contra.rds"))}else{print("contact programmer")}
