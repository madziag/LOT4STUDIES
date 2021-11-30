# Load functions
source(paste0(pre_dir,"/functions/LoadCodelist.R"))
# Load code lists 
codelist_list<- load_codelist(paste0(pre_dir,filename), matches <- matches)
# Create lists for saving Info_ATC
var_names <- names(codelist_list)
codelist_all <- list()
# Data cleaning
for(i in seq_along(codelist_list)) {
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

names(codelist_all) <- names(codelist_list)
rm(list= ls(pattern = "altmed"))
rm(list= ls(pattern = "contracep_"))
rm(Retinoid, Valproate, folic_acid)