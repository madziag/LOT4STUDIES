dirinput<-path_dir
diroutput<-g_intermediate
# Loads function
source(paste0(pre_dir,"functions/", "CreateItemsetDatasets.R"))
#### Get list of medical observation tables
files<-list.files(path=path_dir,pattern="^MEDICAL_OB",ignore.case = TRUE)
# Define the parameters
# 1. EAVtables: ConcePTION_CDM_EAV_tables_retrieve_mo
ConcePTION_CDM_EAV_tables_retrieve_mo <- vector(mode="list")
for (i in 1:length(files)) {
  if (str_detect(files[i],"^MEDICAL_OB")) {ConcePTION_CDM_EAV_tables_retrieve_mo[[files[i]]]<-list("mo_source_table", "mo_source_column")
  }
}


source(paste0(pre_dir,"02_itemsets.R"))
source(paste0(pre_dir,"02_parameters_CDM.R")) # to get ConcePTION_CDM_datevar_retrieve?


# Runs function
CreateItemsetDatasets(EAVtables = ConcePTION_CDM_EAV_tables_retrieve_mo,
                      datevar= ConcePTION_CDM_datevar_retrieve,
                      dateformat= "YYYYmmdd",
                      rename_col = list(person_id=person_id,date=date),
                      study_variable_names = study_itemset_pregnancy,
                      itemset = itemsetMED_AVpair_pregnancy_this_datasource,
                      dirinput = dirinput,
                      diroutput = diroutput,
                      discard_from_environment = FALSE,
                      extension = c("rds")
                      )













