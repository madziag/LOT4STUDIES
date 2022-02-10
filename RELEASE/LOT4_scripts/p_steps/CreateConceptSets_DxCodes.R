#Author: Magda Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022

# Creates concept sets of diagnosis codes
# Loads functions
source(paste0(pre_dir,"/functions/LoadCodelist.R"))
# # Loads concept sets
filename <- "CodeLists/Lot4_completediagnosis_codelist_20211110.xlsx"
# Prints Message
print("Loading Concept Sets")
# Loads code lists 
codelist_list <- load_codelist(paste0(pre_dir,filename), matches <- matches)
# Creates lists for saving Info
var_names <- names(codelist_list)
codelist_all <- list()
codelist_start_all <- list()
codelist_read_all <- list()
codelist_snomed_all <- list()
# Data cleaning
for(i in seq_along(codelist_list)) {
  # Creates df
  codelist <- rbindlist(codelist_list[i])
  # Keeps necessary columns 
  codelist <-codelist[,c("Coding system","Concept name", "Code")]
  # Renames columns
  setnames(codelist,"Coding system", "Coding_System")
  setnames(codelist,"Concept name", "Concept_name")
  # Deletes records where code is missing (-)
  codelist <- codelist[!Code == '-',]
  #Creates variable dot_present
  codelist[,dot_present:=str_detect(codelist[,Code],"\\.")]
  #Creates variable code_no_dot by removing dot from all codes
  codelist[,code_no_dot:=gsub("\\.","",codelist[,Code])]
  # Creates col that specifies vocab type
  codelist[,vocab:= ifelse(codelist[,Coding_System] %chin% c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES" , "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP"), "start",
                           ifelse(codelist[,Coding_System] %chin% c("RCD","RCD2", "READ", "CPRD_Read"), "READ", 
                                  ifelse(codelist[,Coding_System] %chin% c("SNOMEDCT_US", "SCTSPA", "SNOMED"), "SNOMED", "UNKNOWN")))]
  # Creates df based on vocab group types and presence or absence of dots
  codelist_start_dot_1   <- codelist[dot_present==TRUE & vocab=="start"]
  codelist_start_nodot_1 <- codelist[dot_present==FALSE & vocab=="start" ]
  codelist_start_dot_2   <- codelist[dot_present==TRUE & vocab=="start"]
  codelist_start_nodot_2 <- codelist[dot_present==FALSE & vocab=="start" ]
  codelist_read        <- codelist[dot_present==TRUE & vocab=="READ"]
  codelist_snomed      <- codelist[vocab=="SNOMED"]
  
  # Creates code lists  
  # ICD/ICPC codes (has variants with dots and without dots)
  codelist_start_dot_1[,dot_present:=NULL][,code_no_dot:=NULL][,vocab:=NULL]
  # setnames(codelist_start_dot,"code_no_dot", "Code")
  codelist_start_nodot_1[,dot_present:=NULL][,code_no_dot:=NULL][,vocab:=NULL]
  # For saving the codelist (dots + no dots)
  codelist_start_1 <- rbind(codelist_start_dot_1, codelist_start_nodot_1)
  # For use in monthly counts
  # ICD/ICPC codes (has variants with dots and without dots)
  codelist_start_dot_2[,dot_present:=NULL][,Code:=NULL][,vocab:=NULL]
  setnames(codelist_start_dot_2,"code_no_dot", "Code")
  codelist_start_nodot_2[,dot_present:=NULL][,code_no_dot:=NULL][,vocab:=NULL]
  codelist_start <- rbind(codelist_start_dot_2, codelist_start_nodot_2)
  # READ codes
  codelist_read[,dot_present:=NULL][,code_no_dot:=NULL][,vocab:=NULL]
  # SNOMED 
  codelist_snomed[,dot_present:=NULL][,code_no_dot:=NULL][,vocab:=NULL]
  # ALL 
  codelist <- rbind(codelist_start_1, codelist_read, codelist_snomed)
  # Creates list of codes
  codelist[,comb:=paste(Concept_name, Coding_System,"_")]
  codelist[,Codes:=paste0(Code, collapse = ", "), by="comb"]
  codelist<-codelist[!duplicated(comb)]
  codelist[,Code:=NULL][,comb:=NULL]
  setnames(codelist,"Codes", "Code")
  # Saves codelists
  assign(var_names[i], as.data.table(codelist))
  write.csv(get(var_names[i]), paste0(conceptsets_DX_dir, var_names[i], ".csv"), row.names = FALSE)
  # Creates a list of all codelists by vocabulary type 
  codelist_start_all[[i]] <- codelist_start
  codelist_read_all[[i]] <- codelist_read
  codelist_snomed_all[[i]] <- codelist_snomed
  codelist_all[[i]] <- codelist
}
# Assigns  names to the codelists in the lists
names(codelist_start_all) <- names(codelist_list)
names(codelist_read_all) <- names(codelist_list)
names(codelist_snomed_all) <- names(codelist_list)
names(codelist_all) <- names(codelist_list)
# Cleanup 
rm(list = noquote(names(codelist_list)))
rm(codelist, codelist_list, codelist_read, codelist_snomed, codelist_start, codelist_start_1, codelist_start_dot_1, codelist_start_nodot_1, codelist_start_dot_2, codelist_start_nodot_2)
