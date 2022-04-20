#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 29/03/2022

# Merges data created in 2_to_run_source_pop_counts.R
# Puts data (pregnancy tests, contraceptives, indications, adrs, alternative medicines) into respective folders 

##### Binds Pregnancy Tests #####
###### 1. pregtest: created using diagnosis codes on events and procedures tables, initially saved in the diagnoses and procedures_dxcodes folders
###### 2. preg_test: created using procedure codes on procedure tables (only for CPRD and PHARMO), initially saved in the procedures folders. 

# Removes all folders - if any
# for(file in list.files(counts_dfs_dir, recursive = T)){unlink(paste0(counts_dfs_dir, file))}
#Creates a folder for pregnancy tests 
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"all_pregtests")),dir.create(paste0(counts_dfs_dir,"all_pregtests")),FALSE))
pregtest_dir<-paste0(counts_dfs_dir,"all_pregtests/")
#Copies pregtest files to pregtest folder
for(file in list.files(path=diagnoses_pop,pattern=paste0(pop_prefix, "_pregtest"))){file.copy(paste0(diagnoses_pop,file),pregtest_dir)}
for(file in list.files(path=procedures_dxcodes_pop,pattern=paste0(pop_prefix,"_pregtest"))){file.copy(paste0(procedures_dxcodes_pop,file),pregtest_dir)}
for(file in list.files(path=procedures_pop,pattern=paste0(pop_prefix, "_preg_test"))){file.copy(paste0(procedures_pop,file),pregtest_dir)}

if(length(list.files(pregtest_dir, pattern=paste0(pop_prefix, "_preg")))>0){
  #Reads in all moved files 
  pregtest_df<-do.call(rbind,lapply(list.files(pregtest_dir,pattern=paste0(pop_prefix, "_preg"), full.names=T),readRDS))
  # Removes duplicates 
  pregtest_df<-pregtest_df[!duplicated(pregtest_df[,c("person_id","Date","Code")])]
  #Removes individual pregtest files 
  for(file in list.files(path=pregtest_dir,pattern="EVENTS|PROC")){unlink(paste0(pregtest_dir,file))}
  #Saves as one file
  saveRDS(pregtest_df,paste0(pregtest_dir,pop_prefix,"_pregtests.rds"))
}
##### Contraceptives 
###### 1. iud_diag (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
###### 2. iud (from procedures folder - proc codes in procedures tables)
###### 3. contracep_IUD (from medicines folder - ATC codes in medicines tables)
###### 4. contracep_progest (from medicines folder - ATC codes in medicines tables)
###### 5. contracep_fixedcomb (from medicines folder - ATC codes in medicines tables)
###### 6. contracep_sequenprep (from medicines folder - ATC codes in medicines tables)
###### 7. contracep_vaginalring (from medicines folder - ATC codes in medicines tables)
###### 8. contracep_patch (from medicines folder - ATC codes in medicines tables)
###### 9. contracep_implant (from medicines folder - ATC codes in medicines tables)
###### 10. contracep_injection (from medicines folder - ATC codes in medicines tables)

#Creates a folder for contraceptives
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"all_contraceptives")),dir.create(paste0(counts_dfs_dir,"all_contraceptives")),FALSE))
contraceptive_dir<-paste0(counts_dfs_dir,"all_contraceptives/")
#Copy contraceptive files to contraceptives folder
for(file in list.files(path=diagnoses_pop,pattern=paste0(pop_prefix, "_iud_diag"))){file.copy(paste0(diagnoses_pop,file),contraceptive_dir)}
for(file in list.files(path=procedures_dxcodes_pop,pattern=paste0(pop_prefix, "_iud_diag"))){file.copy(paste0(procedures_dxcodes_pop,file),contraceptive_dir)}
for(file in list.files(path=procedures_pop,pattern=paste0(pop_prefix, "_iud"))){file.copy(paste0(procedures_pop,file),contraceptive_dir)}
for(file in list.files(path=medications_pop,pattern=paste0(pop_prefix, "_contracep"))){file.copy(paste0(medications_pop,file),contraceptive_dir)}

#Combine iud files
iud_files<-list.files(contraceptive_dir,pattern="iud",ignore.case=T,full.names=T)
iud_files<-iud_files[!grepl("contracep_IUD.rds",iud_files)]
if(pop_prefix=="PC"){iud_files<-iud_files[!grepl("PC_HOSP",iud_files)]}
if(pop_prefix=="PC_HOSP"){iud_files<-iud_files[grepl("PC_HOSP",iud_files)]}

if(length(iud_files)>0){
  for(iud in 1:length(iud_files)){
    df<-readRDS(iud_files[iud])
    df<-df[,c("person_id","Date","Code","Meaning")]
    saveRDS(df,iud_files[iud])
  }

  #Joins all the iud files
  iud_df<-do.call(rbind,lapply(iud_files,readRDS))
  iud_df<-iud_df[!duplicated(iud_df[,c("person_id","Date","Code")])]
  #Remove all individual iud files from folder
  for(file in iud_files){unlink(file)}
  #Save combined iud file
  saveRDS(iud_df,paste0(contraceptive_dir,pop_prefix,"_contracep_IUD.rds"))
}
# Renames other files in folder
contracep_files<-list.files(contraceptive_dir,pattern="MEDS",full.names=T)
if(pop_prefix=="PC"){contracep_files<-contracep_files[!grepl("PC_HOSP",contracep_files)]}
if(pop_prefix=="PC_HOSP"){contracep_files<-contracep_files[grepl("PC_HOSP",contracep_files)]}
# Renames contracep files 
invisible(file.rename(contracep_files, gsub("_MEDS", "", contracep_files)))

#### Indications
##### 1. ind_bipolar (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 2. ind_epilepsy (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 3. ind_migraine (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)

#Creates a folder for indications
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"all_indications")),dir.create(paste0(counts_dfs_dir,"all_indications")),FALSE))
indications_dir<-paste0(counts_dfs_dir,"all_indications/")
#Copy indication files to indication folder
for(file in list.files(path=diagnoses_pop,pattern=paste0(pop_prefix,"_ind_"),ignore.case=T)){file.copy(paste0(diagnoses_pop,file),indications_dir)}
for(file in list.files(path=procedures_dxcodes_pop,pattern=paste0(pop_prefix,"ind_"),ignore.case=T)){file.copy(paste0(procedures_dxcodes_pop,file),indications_dir)}

if(length(list.files(indications_dir)>0)){

  indications_list<-c("ind_bipolar","ind_epilepsy","ind_migraine")

  for(ind in 1:length(indications_list)){

    if(length(list.files(indications_dir,pattern=paste0(pop_prefix,"_", indications_list[ind], "_EVENTS.rds"),ignore.case=T))){
      #Combine same named indications
      df<-do.call(rbind,lapply(list.files(indications_dir,full.names=T,pattern=paste0(pop_prefix,"_", indications_list[ind], "_EVENTS.rds"),ignore.case=T),readRDS))
      #Remove duplicates
      df<-df[!duplicated(df[,c("person_id","Date","Code")])]
      #Remove individual files before saving final combined file
      for(file in list.files(path=indications_dir,pattern=paste0(pop_prefix, "_", indications_list[ind], "_EVENTS.rds"),ignore.case=T)){unlink(paste0(indications_dir,file))}
      #Save combined file
      saveRDS(df,paste0(indications_dir,pop_prefix,"_",indications_list[ind],".rds"))
    }
  }
}

#### ADR's
##### 1. adr_tremor (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 2. adr_nausea (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 3. adr_depression (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 4. adr_cardiovasc (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 5. adr_dermatol (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 6. adr_gastroint (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 7. adr_hematol (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 8. adr_hepatic (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 9. adr_immuno (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 10. adr_musculoskel (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 11. adr_neuro (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 12. adr_otic (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 13. adr_opthal (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 14. adr_psych (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 15. adr_respir (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)
##### 16. adr_other (from diagnoses folder - dxcodes in event tables/from procedures_dxcodes folder - dxcodes in procedures tables)

#Creates a folder for adrs
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"all_adrs")),dir.create(paste0(counts_dfs_dir,"all_adrs")),FALSE))
adr_dir<-paste0(counts_dfs_dir,"all_adrs/")
#Copy adr files to adr folder
for(file in list.files(path=diagnoses_pop,pattern=paste0(pop_prefix, "_adr_"),ignore.case=T)){file.copy(paste0(diagnoses_pop,file),adr_dir)}
for(file in list.files(path=procedures_dxcodes_pop,pattern=paste0(pop_prefix, "_adr_"),ignore.case=T)){file.copy(paste0(procedures_dxcodes_pop,file),adr_dir)}

if(length(list.files(adr_dir)>0)){

  adrs_list<-c("adr_tremor","adr_nausea","adr_depression","adr_cardiovasc","adr_dermatol","adr_gastroint","adr_hematol","adr_hepatic","adr_immuno","adr_musculoskel","adr_neuro","adr_otic","adr_opthal","adr_psych","adr_respir","adr_other")

  for(adr in 1:length(adrs_list)){

    if(length(list.files(adr_dir,pattern=paste0(pop_prefix,"_",adrs_list[adr], "_EVENTS.rds"),ignore.case=T))>0 |
       length(list.files(adr_dir,pattern=paste0(pop_prefix,"_",adrs_list[adr], "_PROC.rds"),ignore.case=T))>0){
      #Combine same named adrs
      df_events <-do.call(rbind,lapply(list.files(adr_dir,full.names=T,pattern=paste0(pop_prefix,"_",adrs_list[adr], "_EVENTS.rds"),ignore.case=T),readRDS))
      df_procs  <-do.call(rbind,lapply(list.files(adr_dir,full.names=T,pattern=paste0(pop_prefix,"_",adrs_list[adr], "_PROC.rds"),ignore.case=T),readRDS))
      df<-rbind(df_events, df_procs)
      rm(df_events, df_procs)
      #Remove duplicates
      df<-df[!duplicated(df[,c("person_id","Date","Code")])]
      #Remove individual files before saving final combined file
      for(file in list.files(path=adr_dir,pattern=paste0(pop_prefix,"_",adrs_list[adr], "_EVENTS.rds"),ignore.case=T)){unlink(paste0(adr_dir,file))}
      for(file in list.files(path=adr_dir,pattern=paste0(pop_prefix,"_",adrs_list[adr], "_PROC.rds"),ignore.case=T)){unlink(paste0(adr_dir,file))}
      
      #Save combined file
      saveRDS(df,paste0(adr_dir,pop_prefix,"_",adrs_list[adr],".rds"))
    }
  }
}

##### Altmeds
##### 1. altmed_valp_epilepsy (from medicines folder - ATC codes in medicines tables)
##### 2. altmed_valp_bipolar (from medicines folder - ATC codes in medicines tables)
##### 3. altmed_valp_migraine (from medicines folder - ATC codes in medicines tables)
##### 4. altmed_retin_psoriasis (from medicines folder - ATC codes in medicines tables)
##### 5. altmed_retin_acne (from medicines folder - ATC codes in medicines tables)
##### 6. altmed_retin_dermatitis (from medicines folder - ATC codes in medicines tables)

#Creates a folder for alternative medications
invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"all_altmeds")),dir.create(paste0(counts_dfs_dir,"all_altmeds")),FALSE))
altmeds_dir<-paste0(counts_dfs_dir,"all_altmeds/")
#Copy altmed files to altmed folder
for(file in list.files(path=medications_pop,pattern="altmed_",ignore.case=T)){file.copy(paste0(medications_pop,file),altmeds_dir)}
# Renames files
alt_meds_files<-list.files(path=altmeds_dir,pattern="altmed_",ignore.case=T, full.names = T)
invisible(file.rename(alt_meds_files, gsub("_MEDS", "", alt_meds_files)))

