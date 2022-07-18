#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022

# Creates concept sets of ATC codes

# Loads functions
source(paste0(pre_dir,"/functions/LoadCodelist.R"))
# Loads code file
filename<-"CodeLists/ATC_lot4_formatted.xlsx"
# Prints Message
print("Loading concept sets")
# Loads code lists 
codelist_list<-load_codelist(paste0(pre_dir,filename), matches<-matches)
# Creates lists for saving Info_ATC
var_names<-names(codelist_list)
codelist_all<-list()
# Data cleaning
for(i in seq_along(codelist_list)) {
  # Creates a df
  codelist<-rbindlist(codelist_list[i])
  # Keeps necessary columns 
  codelist<-codelist[,c("Code", "Code name")]
  # Renames columns
  setnames(codelist,"Code name", "Medication")
  # Deletes records where code is missing (-)
  codelist<-codelist[ !Code == '-',]
  # Saves codelist
  assign(var_names[i], as.data.table(codelist))
  write.csv(get(var_names[i]), paste0(conceptsets_ATC_dir, var_names[i], ".csv"), row.names = FALSE)
  # Saves all imported codelists in a list
  codelist_all[[i]]<-codelist
}
# Assigns  names to the codelists in the lists
names(codelist_all)<-names(codelist_list)
# Cleanup 
rm(list = noquote(names(codelist_list)))

