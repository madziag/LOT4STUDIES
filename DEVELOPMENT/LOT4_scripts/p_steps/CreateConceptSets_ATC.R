# Load functions
source(paste0(pre_dir,"/functions/LoadCodelist.R"))
# Load code file
filename <- "CodeLists/ATC_lot4_formatted.xlsx"
# Print Message
print("Loading concept sets")
# Load code lists 
codelist_list<- load_codelist(paste0(pre_dir,filename), matches <- matches)
# Create lists for saving Info_ATC
var_names <- names(codelist_list)
codelist_all <- list()
# Data cleaning
for(i in seq_along(codelist_list)) {
  # Create a df
  codelist <- rbindlist(codelist_list[i])
  # Keep necessary columns 
  codelist <-codelist[,c("Code", "Code name")]
  # Rename columns
  setnames(codelist,"Code name", "Medication")
 # Delete records where code is missing (-)
  codelist <- codelist[ !Code == '-',]
  # Save codelist
  assign(var_names[i], as.data.table(codelist))
  write.csv(get(var_names[i]), paste0(conceptsets_ATC_dir, var_names[i], ".csv"), row.names = FALSE)
  # Save all imported codelists in a list
  codelist_all[[i]] <- codelist
}
# Assign  names to the codelists in the lists
names(codelist_all) <- names(codelist_list)
# Cleanup 
rm(list = noquote(names(codelist_list)))

