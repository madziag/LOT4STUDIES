# Load functions
source(paste0(pre_dir,"/functions/LoadCodelist.R"))
# Load code lists
codelist_list<- load_codelist(paste0(pre_dir,filename), matches <- matches)
# Create lists for saving Dx codes concept sets
var_names <- names(codelist_list)
vocabularies_list <- list()
conditions_read_all <- list()
conditions_snomed_all <- list()
conditions_start_all <- list()
conditions_all <- list()
# Data Cleaning 
for(i in seq_along(codelist_list)) {
  codelist <- rbindlist(codelist_list[i])
  # Keep necessary columns 
  codelist <-codelist[,c("Concept name","Coding system", "Code")]
  # Rename columns
  setnames(codelist,"Coding system", "Coding_system")
  setnames(codelist,"Concept name", "Condition")
  # Delete records where code is missing (-)
  codelist <- codelist[ !Code == '-',]
  #Create variable dot_present
  codelist[,dot_present:=str_detect(codelist[,Code],"\\.")]
  #Create variable code_no_dot by removing dot from all codes
  codelist[,code_no_dot:=gsub("\\.","",codelist[,Code])]
  # Create vocabularies list
  vocabularies_list <- c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES", "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP", "RCD", "RCD2", "READ", "CPRD_Read", "SNOMED_US", "SCTSPA", "SNOMED" )
  #put all information in a list
  conditions<-vector(mode="list", length=length(unique(na.omit(codelist[,Condition]))))
  names(conditions)<-unique(na.omit(codelist[,Condition]))
  for (l in 1:length(conditions)){
    vocabularies<-vector(mode="list", length=length(unique(na.omit(codelist[,Coding_system]))))
    names(vocabularies)<-unique(na.omit(codelist[,Coding_system]))
    for (j in 1:length(vocabularies)){
      vocabularies[[j]]<-codelist[Condition==names(conditions)[l] & Coding_system==names(vocabularies)[j], Code]
    }
    conditions[[l]]<-list.append(conditions[[l]],vocabularies)
    rm(vocabularies)
  }
  #remove empty vocabularies
  conditions<-lapply(conditions, function(x) Filter(length, x))
  # Creare a list of all conditions
  conditions_all[[i]] <- conditions
  #################################################################################################################
  #Rule: start with
  #Coding system: ICD9, ICD9CM, ICD9PROC, MTHICD9,, ICD10, ICD-10, ICD10CM, ICD10/CM, ICD10ES, ICPC, ICPC2, ICPC-2, ICPC2P, CIAP
  #################################################################################################################
  #vocabularies that will be filtered with start with
  conditions_start<-list()
  for(l in 1:length(conditions_all[[i]])){
    conditions_start[[l]]<-conditions_all[[i]][[l]][names(conditions_all[[i]][[l]])%!in% c("SNOMEDCT_US", "SCTSPA", "RCD", "RCD2")]
  }
  names(conditions_start)<-names(conditions_all[[i]])
  for(l in 1:length(conditions_start)){
    lapply(conditions_start[[l]], function(x) x[names(x) %in% c("Code")])
  }
  # Remove empty vocabularies
  conditions_start <- Filter(function(x) length(x) > 0, conditions_start)
  # Create a list of all ICD/ICPC conditions + codes
  conditions_start_all[[i]] <- conditions_start
  ###############################################################################################################
  #Rule:Remove dot, start with
  #Coding system: RCD, RCD2, READ, CPRD_Read 
  ###############################################################################################################
  conditions_read<-vector(mode="list", length=length(unique(na.omit(codelist[,Condition]))))
  names(conditions_read)<-unique(na.omit(codelist[,Condition]))
  for (l in 1:length(conditions_read)){
    vocabularies_RCD<-vector(mode="list", length=2)
    vocabularies_RCD$RCD2<-codelist[Condition==names(conditions_read)[l] & Coding_system=="RCD2", code_no_dot]
    vocabularies_RCD$RCD<-codelist[Condition==names(conditions_read)[l] & Coding_system=="RCD", code_no_dot]
    conditions_read[[l]]<-list.append(conditions_read[[l]],vocabularies_RCD)
    rm(vocabularies_RCD)
  }
  # Remove empty vocabularies
  conditions_read<-lapply(conditions_read, function(x) Filter(length, x))
  conditions_read <- Filter(function(x) length(x) > 0, conditions_read)
  # Create a list of all Read Code conditions + codes 
  conditions_read_all[[i]] <- conditions_read
  ################################################################################################################
  #Rule: match exactly
  #Coding system: SNOMED_US, SCTSPA, SNOMED
  #################################################################################################################
  #SNOMED codes
  conditions_snomed<-list()
  for(l in 1:length(conditions_all[[i]])){
    conditions_snomed[[l]]<-conditions_all[[i]][[l]][names(conditions_all[[i]][[l]])%in% c("SNOMEDCT_US", "SCTSPA")]
  }
  # Remove empty vocabularies 
  names(conditions_snomed)<-names(conditions_all[[i]])
  conditions_snomed <- Filter(function(x) length(x) > 0, conditions_snomed)
  # Creare a list of all Snomed conditions + codes
  conditions_snomed_all[[i]] <- conditions_snomed
  
  codelist[,comb:=paste(Condition, Coding_system,"_")]
  codelist[,dot_present:=NULL][,code_no_dot:=NULL]
  codelist[,Codes:=paste0(Code, collapse = ", "), by="comb"]
  codelist<-codelist[!duplicated(comb)]
  codelist[,Code:=NULL][,comb:=NULL]
  setnames(codelist,"Condition","event_definition")
  
  assign(var_names[i], as.data.table(codelist))
  # Save concept sets in concept set folder 
  write.csv(get(var_names[i]), paste0(conceptsets_DX_dir, var_names[i], ".csv"), row.names = FALSE)
  
}
names(conditions_all) <- names(codelist_list)
names(conditions_read_all) <- names(codelist_list)
names(conditions_snomed_all) <- names(codelist_list)
names(conditions_start_all) <- names(codelist_list)
rm(codelist_list, conditions_read, conditions_snomed, conditions_start, conditions, codelist, i,j,l)

