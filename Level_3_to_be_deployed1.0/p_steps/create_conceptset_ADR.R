`%!in%` = Negate(`%in%`)

# Load function: 'load_codelist'
source(paste0(pre_dir,"/functions/LoadCodelist.R"))

# Read ADR codelist -> gives a list of all tabs with matching (partial or full names)
codelist_ADR<- load_codelist(paste0(pre_dir,"ANNEX2a_Oral_Retinoids_Code_List_20201124.xlsx"), matches <- c("ADR"))

# Create a datatable with all ADRs
complete_codelist_ADR <- rbindlist(codelist_ADR) 
# Select only necessary columns 
complete_codelist_ADR <-complete_codelist_ADR[,c("Concept name","Coding system", "Code")]
# Rename columns
setnames(complete_codelist_ADR,"Coding system", "Coding_system")
setnames(complete_codelist_ADR,"Concept name", "Concept_name")
# Coding_System clean up
# Transforms ICD10/CM -> ICD10CM
complete_codelist_ADR[,Coding_system:=gsub("\\/","",complete_codelist_ADR[,Coding_system])]
# Transforms ICPC2EENG -> ICPC2
complete_codelist_ADR[,Coding_system:=gsub("EENG","",complete_codelist_ADR[,Coding_system])]
# Deletes records where code is missing (-)
complete_codelist_ADR <- complete_codelist_ADR[ !Code == '-',] 
#Create variable dot_present
complete_codelist_ADR[,dot_present:=str_detect(complete_codelist_ADR[,Code],"\\.")]
#Create variable code_no_dot by removing dot from all codes
complete_codelist_ADR[,code_no_dot:=gsub("\\.","",complete_codelist_ADR[,Code])]
# Create a list of unique coding system types
vocabularies_list<-complete_codelist_ADR[!duplicated(Coding_system), Coding_system]

#put all information in a list
concept_name<-vector(mode="list", length=length(unique(na.omit(complete_codelist_ADR[,Concept_name]))))
names(concept_name)<-unique(na.omit(complete_codelist_ADR[,Concept_name]))
for (i in 1:length(concept_name)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(complete_codelist_ADR[,Coding_system]))))
  names(vocabularies)<-unique(na.omit(complete_codelist_ADR[,Coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-complete_codelist_ADR[Concept_name==names(concept_name)[i] & Coding_system==names(vocabularies)[j], Code]
  }
  concept_name[[i]]<-list.append(concept_name[[i]],vocabularies)
  rm(vocabularies)
}
#remove empty vocabularies
concept_name<-lapply(concept_name, function(x) Filter(length, x))

#################################################################################################################
#Rule: start with
#Coding system: ICD9CM, MTHICD9, ICD10CM, ICPC, ICPC2, ICPC2P
#################################################################################################################
#vocabularies that will be filtered with start with
concept_name_start<-list()
for(i in 1:length(concept_name)){
  concept_name_start[[i]]<-concept_name[[i]][names(concept_name[[i]])%!in% c("SNOMEDCT_US", "SCTSPA", "RCD", "RCD2")]
}
names(concept_name_start)<-names(concept_name)
for(i in 1:length(concept_name_start)){
  lapply(concept_name_start[[i]], function(x) x[names(x) %in% c("Code")])
}

################################################################################################################
#Rule:Remove dot, start with
#Coding system: Read codes (RCD, RCD2)
###############################################################################################################
concept_name_read<-vector(mode="list", length=length(unique(na.omit(complete_codelist_ADR[,Concept_name]))))
names(concept_name_read)<-unique(na.omit(complete_codelist_ADR[,Concept_name]))
for (i in 1:length(concept_name_read)){
  vocabularies_RCD<-vector(mode="list", length=2)
  vocabularies_RCD$RCD2<-complete_codelist_ADR[Concept_name==names(concept_name_read)[i] & Coding_system=="RCD2", code_no_dot]
  vocabularies_RCD$RCD <-complete_codelist_ADR[Concept_name==names(concept_name_read)[i] & Coding_system=="RCD", code_no_dot]
  concept_name_read[[i]]<-list.append(concept_name_read[[i]], vocabularies_RCD)
  rm(vocabularies_RCD)
}

################################################################################################################
#Rule: match exactly
#Coding system: SNOMEDCT_US & SCTSPA (SNOMED CT SPANISH EDITION)
#################################################################################################################
#SNOMED codes
concept_name_snomed<-list()
for(i in 1:length(concept_name)){
  concept_name_snomed[[i]]<-concept_name[[i]][names(concept_name[[i]])%in% c("SNOMEDCT_US", "SCTSPA")]
}
names(concept_name_snomed)<-names(concept_name)

################################################################################################################
#output folder for Info report in g_output
if ("Info" %in% list.files(output_dir)){
  info_dir<-paste(output_dir, "Info/",sep="")
  do.call(file.remove, list(list.files(info_dir, full.names = T)))
} else {
  #Create the Info folder in the output dir
  dir.create(paste(output_dir, "Info", sep=""))
  info_dir<-paste(output_dir, "Info/", sep="")
} 

complete_codelist_ADR[,comb:=paste(Concept_name, Coding_system,"_")]
complete_codelist_ADR[,dot_present:=NULL][,code_no_dot:=NULL]
complete_codelist_ADR[,Codes:=paste0(Code, collapse = ", "), by="comb"]
complete_codelist_ADR<-complete_codelist_ADR[!duplicated(comb)]
complete_codelist_ADR[,Code:=NULL][,comb:=NULL]
setnames(complete_codelist_ADR,"Concept_name","ADR")

# Save copy of complete codelist
write.csv(complete_codelist_ADR, paste0(info_dir, "adr_codelist.csv"), row.names = F)
rm(complete_codelist_ADR)

