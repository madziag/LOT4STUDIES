# Run to_run.R file
## Use with DIAGNOSES_L3_pre_script_2.R

# Cleans up global environment after running to_run.R script ()
# rm(list=ls()[! ls() %in% c("pre_dir", "output_dir")])

`%!in%` = Negate(`%in%`)

# Reads in specified sheets from excel file
# filename -> name of the excel file to be read
# matches -> name/names (partial or whole) of the tabs that you want to upload e.g. ADR will read in all tabs with the word ADR in them
#         -> This is a vector so values need to be in the form of c("a", "b").
read_excel_tabs <- function(filename, matches, tibble = FALSE) {
  if(!require(readxl)){install.packages("readxl")}
  library(readxl)
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  toMatch <- matches
  x <- x[unique(grep(paste(toMatch,collapse="|"), sheets, ignore.case = TRUE))]
  x
}

# Run function read_excel_tabs()
# codelist_sterilization  <- read_excel_tabs(paste0(pre_dir, "ANNEX2a_Oral_Retinoids_Code_List_20201124.xlsx"), matches <- c("pregnancy_testing"))
codelist_sterilization  <- read_excel_tabs(paste0(pre_dir, "ANNEX2a_Valproate_Code_List_20201125.xlsx"), matches <- c("sterilization"))
# Create a df with all the ADR's
complete_codelist_sterilization <- rbindlist(codelist_sterilization) 
#select only necessary columns
complete_codelist_sterilization <-complete_codelist_sterilization[,c("Concept name","Coding system", "Code")]
# Rename columns
setnames(complete_codelist_sterilization,"Coding system", "Coding_system")
setnames(complete_codelist_sterilization,"Concept name", "Concept_name")
#### Clean up of Coding_System & Code (Should this be done in the original excel file?)
complete_codelist_sterilization[,Coding_system:=gsub("\\/","",complete_codelist_sterilization[,Coding_system])]
complete_codelist_sterilization <- complete_codelist_sterilization[ !Code == '-',] 
#Create variable dot_present
complete_codelist_sterilization[,dot_present:=str_detect(complete_codelist_sterilization[,Code],"\\.")]
#Create variable code_no_dot by removing dot from all codes
complete_codelist_sterilization[,code_no_dot:=gsub("\\.","",complete_codelist_sterilization[,Code])]
vocabularies_list<-complete_codelist_sterilization[!duplicated(Coding_system), Coding_system]
#put all information in a list
concept_name<-vector(mode="list", length=length(unique(na.omit(complete_codelist_sterilization[,Concept_name]))))
names(concept_name)<-unique(na.omit(complete_codelist_sterilization[,Concept_name]))
for (i in 1:length(concept_name)){
  vocabularies<-vector(mode="list", length=length(unique(na.omit(complete_codelist_sterilization[,Coding_system]))))
  names(vocabularies)<-unique(na.omit(complete_codelist_sterilization[,Coding_system]))
  for (j in 1:length(vocabularies)){
    vocabularies[[j]]<-complete_codelist_sterilization[Concept_name==names(concept_name)[i] & Coding_system==names(vocabularies)[j], Code]
  }
  concept_name[[i]]<-list.append(concept_name[[i]],vocabularies)
  rm(vocabularies)
}
#remove empty vocabularies
concept_name<-lapply(concept_name, function(x) Filter(length, x))

#################################################################################################################
#Rule: start with
#Coding system: ICD9CM, ICD10CM, ICPC2P, MTHICD9

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
concept_name_read<-vector(mode="list", length=length(unique(na.omit(complete_codelist_sterilization[,Concept_name]))))
names(concept_name_read)<-unique(na.omit(complete_codelist_sterilization[,Concept_name]))
for (i in 1:length(concept_name_read)){
  vocabularies_RCD<-vector(mode="list", length=2)
  vocabularies_RCD$RCD2<-complete_codelist_sterilization[Concept_name==names(concept_name_read)[i] & Coding_system=="RCD2", code_no_dot]
  vocabularies_RCD$RCD <-complete_codelist_sterilization[Concept_name==names(concept_name_read)[i] & Coding_system=="RCD", code_no_dot]
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

complete_codelist_sterilization[,comb:=paste(Concept_name, Coding_system,"_")]
complete_codelist_sterilization[,dot_present:=NULL][,code_no_dot:=NULL]
complete_codelist_sterilization[,Codes:=paste0(Code, collapse = ", "), by="comb"]
complete_codelist_sterilization<-complete_codelist_sterilization[!duplicated(comb)]
complete_codelist_sterilization[,Code:=NULL][,comb:=NULL]
setnames(complete_codelist_sterilization,"Concept_name","Sterilization")

# Save csv file
write.csv(complete_codelist_sterilization, paste0(info_dir, "sterilization_codelist.csv"), row.names = F)
# rm(complete_codelist_sterilization)

