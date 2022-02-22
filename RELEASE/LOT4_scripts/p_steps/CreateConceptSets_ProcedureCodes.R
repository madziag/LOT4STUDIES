# Creates concept sets of Procedure codes

# Loads functions
source(paste0(pre_dir,"/functions/LoadCodelist.R"))
# Prints Message
print("Loading Concept Sets")
# Loads code lists 
filename <- "CodeLists/Procedure_codes.xlsx"
codelist_list <- load_codelist(paste0(pre_dir,filename), matches <- matches)
# Creates lists for saving Info
var_names <- names(codelist_list)
codelist_all <- list()
codelist_CPRD_all <- list() # CPRD
codelist_PHARMO_all <- list() # PHARMO
codelist_CPRD_names <- c()
codelist_PHARMO_names <- c()
# Data cleaning
for(i in seq_along(codelist_list)) {
  # Creates a df
  codelist <- rbindlist(codelist_list[i])
  # Renames columns
  setnames(codelist,"Coding System", "Coding_System")
  setnames(codelist,"Description", "Concept_name")
  # Deletes records where code is missing (-)
  codelist <- codelist[!Code == '-',]
  # Creates df based on vocab group types and presence or absence of dots
  codelist_CPRD   <- codelist[Origin =="CPRD"]
  codelist_PHARMO <- codelist[Origin =="PHARMO"]
  
  # Drops origin column 
  codelist_CPRD <- codelist_CPRD[,c("Coding_System","Concept_name", "Code")]
  codelist_PHARMO <- codelist_PHARMO[,c("Coding_System","Concept_name", "Code")]
  # Gets names of codelists used by DAP 
  if(nrow(codelist_CPRD)>0){codelist_CPRD_names <- append(codelist_CPRD_names, names(codelist_list[i]))}
  if(nrow(codelist_PHARMO)>0){codelist_PHARMO_names <- append(codelist_PHARMO_names, names(codelist_list[i]))}
  # Creates code lists  
  codelist <- rbind(codelist_CPRD, codelist_PHARMO)
  # Creates list of codes
  codelist[,comb:=paste(Concept_name, Coding_System,"_")]
  codelist[,Codes:=paste0(Code, collapse = ", "), by="comb"]
  codelist<-codelist[!duplicated(comb)]
  codelist[,Code:=NULL][,comb:=NULL]
  setnames(codelist,"Codes", "Code")
  # Saves codelists
  assign(var_names[i], as.data.table(codelist))
  write.csv(get(var_names[i]), paste0(conceptsets_PROC_dir, var_names[i], ".csv"), row.names = FALSE)
  # Creates a list of all codelists by vocabulary type 
  if(nrow(codelist_CPRD)>0){codelist_CPRD_all[[i]] <- codelist_CPRD}
  if(nrow(codelist_PHARMO)>0){codelist_PHARMO_all[[i]] <- codelist_PHARMO}
  if(nrow(codelist)>0){codelist_all[[i]] <- codelist}
}
# Removes empty lists 
codelist_CPRD_all<-codelist_CPRD_all[!sapply(codelist_CPRD_all,is.null)]
codelist_PHARMO_all<-codelist_PHARMO_all[!sapply(codelist_PHARMO_all,is.null)]
# Assigns  names to the codelists in the lists
names(codelist_CPRD_all) <- codelist_CPRD_names
names(codelist_PHARMO_all) <- codelist_PHARMO_names
names(codelist_all) <- names(codelist_list)
# Cleanup 
rm(list = noquote(names(codelist_list)))
rm(codelist, codelist_list, codelist_CPRD, codelist_PHARMO)

