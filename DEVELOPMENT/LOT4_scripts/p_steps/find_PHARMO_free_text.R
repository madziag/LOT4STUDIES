# df_free_text <- as.data.table(read_xlsx("C:/Users/31653/Desktop/LOT4_Sourcetree/DEVELOPMENT/CDMInstances/LOT4/PHARMO/PHARMO_free_text_examples.xlsx"))
# Initializes free text concept sets 
source(paste0(pre_dir, "CreateConceptSets_free_text_PHARMO.R"))
# Checks for each of the above single terms if present in the events free text column
for(i in 1:length(concept_set_terms)){
  for(j in 1:length(concept_set_terms[[i]])){
    df_free_text_subset <- df_free_text[grepl(event_free_text, pattern = concept_set_terms[[i]][j], ignore.case = T),]
    df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
    if(nrow(df_free_text_subset)>0){
      saveRDS(df_free_text_subset, paste0(events_tmp_DX, pop_prefix, "_", names(concept_set_terms[i]), "-", concept_set_terms[[i]][j], "_",events_prefix, "_free_text_dutch.rds"))
      free_text_files <- list.files(events_tmp_DX, pattern = ".rds")
      file.move(paste0(events_tmp_DX, free_text_files), paste0(events_tmp_DX, names(concept_set_terms[i]), "/", free_text_files))
    }
  }
}

# Checks if any of the combination words are present in the events free text column
# concept_set_terms[["adr_cardiovasc"]][["free_text_dutch"]]=c("cerebro + accident")
df_free_text_subset <- df_free_text[grepl("^(?=.*cerebro)(?=.*accident)", event_free_text, perl = TRUE),]
df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
if(nrow(df_free_text_subset)>0){
  saveRDS(df_free_text_subset, paste0(events_tmp_DX, pop_prefix, "_adr_cardiovasc-cerebro+accident_",events_prefix, "_free_text_dutch.rds"))
  file.move(paste0(events_tmp_DX, list.files(events_tmp_DX, pattern = ".rds")), paste0(events_tmp_DX, "adr_cardiovasc/", list.files(events_tmp_DX, pattern = ".rds")))
}
# concept_set_terms[["adr_gastroint"]][["free_text_dutch"]]= c("bloed + overg", "coli + ulcer", "chron + enteritis", "prikkel + darm + synd")
df_free_text_subset <- df_free_text[grepl("^(?=.*bloed)(?=.*overg)", event_free_text, perl = TRUE),]
df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
if(nrow(df_free_text_subset)>0){saveRDS(df_free_text_subset, paste0(events_tmp_DX, pop_prefix, "_adr_gastroint-bloed+overg_",events_prefix, "_free_text_dutch.rds"))}
df_free_text_subset <- df_free_text[grepl("^(?=.*coli)(?=.*ulcer)", event_free_text, perl = TRUE),]
df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
if(nrow(df_free_text_subset)>0){saveRDS(df_free_text_subset, paste0(events_tmp_DX, pop_prefix, "_adr_gastroint-coli+ulcer_",events_prefix, "_free_text_dutch.rds"))}
df_free_text_subset <- df_free_text[grepl("^(?=.*chron)(?=.*enteritis)", event_free_text, perl = TRUE),]
df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
if(nrow(df_free_text_subset)>0){saveRDS(df_free_text_subset, paste0(events_tmp_DX, pop_prefix, "_adr_gastroint-chron+enteritis_",events_prefix, "_free_text_dutch.rds"))}
df_free_text_subset <- df_free_text[grepl("^(?=.*prikkel)(?=.*darm)(?=.*synd)", event_free_text, perl = TRUE),]
df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
if(nrow(df_free_text_subset)>0){saveRDS(df_free_text_subset, paste0(events_tmp_DX, pop_prefix, "_adr_gastroint-prikkel+darm+synd_",events_prefix, "_free_text_dutch.rds"))}
if (length(list.files(events_tmp_DX, pattern = ".rds")>0)){file.move(paste0(events_tmp_DX, free_text_files <- list.files(events_tmp_DX, pattern = ".rds")), paste0(events_tmp_DX, "adr_gastroint/", free_text_files <- list.files(events_tmp_DX, pattern = ".rds")))}
## concept_set_terms[["adr_immuno"]][["free_text_dutch"]]=c("allerg + reactie")
df_free_text_subset <- df_free_text[grepl("^(?=.*allerg)(?=.*reactie)", event_free_text, perl = TRUE),]
df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
if(nrow(df_free_text_subset)>0){
  saveRDS(df_free_text_subset, paste0(events_tmp_DX, pop_prefix, "_adr_immuno-allerg+reactie_",events_prefix, "_free_text_dutch.rds"))
  file.move(paste0(events_tmp_DX, list.files(events_tmp_DX, pattern = ".rds")), paste0(events_tmp_DX, "adr_immuno/", list.files(events_tmp_DX, pattern = ".rds")))
}
# concept_set_terms[["adr_epilepsy"]][["free_text_dutch"]]=c("epilep + aanval")
df_free_text_subset <- df_free_text[grepl("^(?=.*epilep)(?=.*aanval)", event_free_text, perl = TRUE),]
df_free_text_subset <- df_free_text_subset[,-c("event_free_text")]
if(nrow(df_free_text_subset)>0){
  saveRDS(df_free_text_subset, paste0(events_tmp_DX, pop_prefix, "_ind_epilepsy-epilep+aanval_",events_prefix, "_free_text_dutch.rds"))
  file.move(paste0(events_tmp_DX, list.files(events_tmp_DX, pattern = ".rds")), paste0(events_tmp_DX, "ind_epilepsy/", list.files(events_tmp_DX, pattern = ".rds")))
}

rm(list = grep("^concept_set", ls(), value = TRUE))

