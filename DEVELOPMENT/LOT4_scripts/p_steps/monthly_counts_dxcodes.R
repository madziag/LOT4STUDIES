# Create empty table for counts 
empty_counts_df <- expand.grid(seq(2009, 2019), seq(1, 12))
names(empty_counts_df) <- c("year", "month")
# Load study population
# study_population_list<-list.files(populations_dir, pattern="population")
# study_population <-lapply(paste0(populations_dir,"/", study_population_list), readRDS)
# study_population <- rbindlist(study_population)

study_population <- readRDS(paste0(populations_dir, "ALL_study_population.rds"))
# Load code file
filename <- "CodeLists/Lot4_completediagnosis_codelist_20211110.xlsx"
matches <- c()
print("Loading Concept Sets")
source(paste0(pre_dir,"CreateConceptSets_DxCodes.R"))
# Reads in study population
if(length(actual_tables$EVENTS)>0){
  # Lists for saving info
  count_list <- list() # Saves counts for all variables in a list 
  events_variable <- list() # For folder creation (for each code group)
  # Reads in events table/tables 
  w<-1
  for (y in 1:length(actual_tables$EVENTS)){
    #Load the table
    df<-fread(paste(path_dir, actual_tables$EVENTS[y], sep=""), stringsAsFactors = FALSE)
    # Data cleaning of df
    df<-df[,c("person_id", "start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event")]
    df<-df[, lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))] #make sure missing data is read appropriately
    setnames(df,"meaning_of_event","meaning")
    setnames(df,"start_date_record","event_date")
    setnames(df,"event_record_vocabulary","event_vocabulary")
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    #Merge with the study_population table(there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    df[,event_date:=as.IDate(event_date,"%Y%m%d")] #transform to date variables
    #create year variable
    df[,year:=year(event_date)]
    #remove records with both dates missing
    df<-df[!is.na(year)]
    df<-df[year>2009 | year<2019]
    #identify persons that have an event before start_of_follow_up
    df[,date_dif:=start_follow_up-event_date][,filter:=fifelse(date_dif<=365 & date_dif>=1,1,0)]
    #get person_id
    persons_event_prior<-unique(na.omit(df[filter==1,person_id]))
    df[,date_dif:=NULL][,filter:=NULL]
    #remove records that are outside the obs_period for all subjects
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] #remove records outside study period
    df[,obs_out:=NULL]
    df<-df[!is.na(meaning)] #remove records with empty meaning
    df<-df[!is.na(event_code) | !is.na(event_vocabulary)]# remove records with both event code and event record vocabulary missing
    df<-df[!is.na(event_vocabulary)] #remove empty vocabularies
    df<-df[is.na(event_vocabulary) | event_vocabulary %in% vocabularies_list] #remove records where vocabularies are not of interest
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"]#remove unspecified sex
    print(paste0("Finding matching records in ", actual_tables$EVENTS[y]))
    # Create a new folder for each code group type (to store records with matching codes)
    for (z in 1:length(conditions_all)){
      ifelse(!dir.exists(file.path(events_tmp_dx, names(conditions_all[z]))), dir.create(paste(events_tmp_dx, names(conditions_all[z]), sep="")), FALSE)
      events_variable[[z]] <- paste(events_tmp_dx, names(conditions_all[z]), sep="")
    }
    #################################################################
    #Match codes based on coding system and code: algorithm start with
    #################################################################
    # Filters records with ICD/ICPC codes   
    if(df[,.N]>0){
      years_study_events<-df[!duplicated(year), year]
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("ICD9", "ICD9CM", "ICD9PROC", "MTHICD9", "ICD10", "ICD-10", "ICD10CM", "ICD10/CM", "ICD10ES" , "ICPC", "ICPC2", "ICPC2P", "ICPC-2", "CIAP"))>0){
        for(l in 1:length(conditions_start_all)) {
          for (i in 1:length(conditions_start_all[[l]])){
            for(j in 1:length(conditions_start_all[[l]][[i]])){
              z<-1
              repeat{
                if(df[grepl(paste0("^",paste(conditions_start_all[[l]][[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_start_all[[l]][[i]])[j]][,.N]>0){
                  df[grepl(paste0("^",paste(conditions_start_all[[l]][[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_start_all[[l]][[i]])[j],filter:=1]
                } 
                z<-z+1
                if(z>length(conditions_start_all[[l]][[i]][[j]])){
                  break
                } 
              } 
              if("filter" %!in% names(df)){
                df[,filter:=0]
              }
              m<-1
              repeat{
                if(df[filter==1 & year==years_study_events[m],.N]>0){
                  saveRDS(data.table(df[filter==1 & year==years_study_events[m]], condition=names(conditions_start_all[[l]][i])),
                          paste0(events_tmp_dx,years_study_events[m], "_", names(conditions_start_all[[l]][i]), "_",actual_tables$EVENTS[y], "_start.rds"))
                } 
                m<-m+1
                if(m >length(years_study_events)){
                  break
                } 
              }
              df[,filter:=NULL]
            } 
            # Get list of saved files
            conditions_files<-c(list.files(events_tmp_dx, "\\_start.rds$"))
            # Move files to correspondingly named file
            move_files <- function(x){file.rename( from = file.path(events_tmp_dx, x), to = file.path(paste0(events_tmp_dx, names(conditions_all[l])), x))}
            lapply(conditions_files, move_files)
          }
        }
      }  
      # Filters records with READ codes 
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("RCD","RCD2", "READ", "CPRD_Read"))>0){
        for(l in 1:length(conditions_read_all)) {
          for (i in 1:length(conditions_read_all[[l]])){
            for(j in 1:length(conditions_read_all[[l]][[i]])){
              z<-1 
              repeat{
                if(df[grepl(paste0("^",paste(conditions_read_all[[l]][[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_read_all[[l]][[i]])[j]][,.N]>0){
                  df[grepl(paste0("^",paste(conditions_read_all[[l]][[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_read_all[[l]][[i]])[j],filter:=1]
                } 
                z<-z+1
                if(z>length(conditions_read_all[[l]][[i]][[j]])){
                  break 
                }
              } 
              if("filter" %!in% names(df)){
                df[,filter:=0]
              }
              m<-1
              repeat{
                if(df[filter==1 & year==years_study_events[m],.N]>0){
                  saveRDS(data.table(df[filter==1 & year==years_study_events[m]], condition=names(conditions_read_all[[l]][i])), paste0(events_tmp_dx,years_study_events[m],"_", names(conditions_read_all[[l]][i]), "_",actual_tables$EVENTS[y], "_RCD.rds"))
                } 
                m<-m+1
                if(m >length(years_study_events)){
                  break
                }
              } 
              df[,filter:=NULL]
            } 
          } 
          # Get list of saved files
          conditions_files<-c(list.files(events_tmp_dx, "\\_RCD.rds$"))
          # Move files to correspondingly named file
          move_files <- function(x){file.rename( from = file.path(events_tmp_dx, x), to = file.path(paste0(events_tmp_dx, names(conditions_all[l])), x))}
          lapply(conditions_files, move_files)
        }
      }
      #  Filter records with SNOMED codes 
      if(sum(df[!duplicated(event_vocabulary), event_vocabulary] %in% c("SNOMEDCT_US", "SCTSPA", "SNOMED"))>0){
        for(l in 1:length(conditions_snomed_all)) { 
          for (i in 1:length(conditions_snomed_all[[l]])){
            for(j in 1:length(conditions_snomed_all[[l]][[i]])){
              z<-1
              repeat{
                if(df[grepl(paste0("^",paste(conditions_snomed_all[[l]][[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_snomed_all[[l]][[i]])[j]][,.N]>0){
                  df[grepl(paste0("^",paste(conditions_snomed_all[[l]][[i]][[j]][z])), df[["event_code"]]) & event_vocabulary==names(conditions_snomed_all[[l]][[i]])[j],filter:=1]
                }
                z<-z+1
                if(z>length(conditions_snomed_all[[l]][[i]][[j]])){
                  break
                }
              }
              if("filter" %!in% names(df)){df[,filter:=0]}
              m<-1
              repeat{
                if(df[filter==1 & year==years_study_events[m],.N]>0){
                  saveRDS(data.table(df[filter==1 & year==years_study_events[m]], condition=names(conditions_snomed_all[[l]][i])), paste0(events_tmp_dx,years_study_events[m],"_", names(conditions_snomed_all[[l]][i]), "_",actual_tables$EVENTS[y], "_SNOMED.rds"))
                }
                m<-m+1
                if(m >length(years_study_events)){
                  break
                }
              }
              df[,filter:=NULL]
            }
          }
          # Get list of saved files
          conditions_files<-c(list.files(events_tmp_dx, "\\_SNOMED.rds$"))
          # Move files to correspondingly named file
          move_files <- function(x){file.rename( from = file.path(events_tmp_dx, x), to = file.path(paste0(events_tmp_dx, names(conditions_all[l])), x))}
          lapply(conditions_files, move_files)
        }
      }
    } 
    w<-w+1
  } 
  # Load files from all folders to form a df for each code group type
  # Count the number of patients per code type per month per year, store as a list 
  print("Counting records")
  folders_list <- list.files(events_tmp_dx)
  for (i in 1:length(folders_list)){
    file_list <-list.files(paste0(events_tmp_dx, folders_list[i]), "\\.rds$")
    if (length(file_list)>0){
      combined_events<-lapply(paste0(paste0(events_tmp_dx, folders_list[i], "/"), file_list), readRDS)
      combined_events<-do.call(rbind,combined_events)
      count <- combined_events[,.N, by = .(year, month(event_date))]
      count <- merge(x = empty_counts_df, y = count, by = c("year", "month"), all.x = TRUE)
      count[is.na(count$N),]$N <- 0

      if (subpopulations_present=="Yes"){
        if(combined_events[,.N]>0){
          saveRDS(combined_events, paste0(diagnoses_tmp ,subpopulations_names[s], "/", folders_list[i],".rds"))
          saveRDS(count, paste0(monthly_counts_dx,"/", subpopulations_names[s], "/", folders_list[i],"_counts.rds"))
          } else {print(paste("There are no matching records for", folders_list[i]))}
        } else {
          if(combined_events[,.N]>0){
            saveRDS(combined_events, paste0(diagnoses_tmp ,folders_list[i],".rds"))
            saveRDS(count, paste0(monthly_counts_dx,"/",folders_list[i],"_counts.rds"))
            } else {print(paste("There are no matching records for", folders_list[i]))}
          }
      } else {
        print(paste("There are no matching records for", folders_list[i]))
      }
    }
  } 


# Add rates to counts tables 
count_names_diag<-list.files(monthly_counts_dx, pattern="count")
count_files_diag<-lapply(paste0(monthly_counts_dx,"/", count_names_diag), readRDS)
names(count_files_diag) <- count_names_diag

for(i in 1:length(count_names_diag)){
  count_files_diag[[i]] <- within(count_files_diag[[i]], YM<- sprintf("%d-%02d", year, month))
  count_files_diag[[i]] <- merge(x = count_files_diag[[i]], y = FUmonths_df, by = c("YM"), all.x = TRUE)   
  count_files_diag[[i]] <-count_files_diag[[i]][,c("YM", "N", "Freq")]
  count_files_diag[[i]]$rates <- as.integer(count_files_diag[[i]]$N)/as.integer(count_files_diag[[i]]$Freq)
  saveRDS(count_files_diag[[i]], paste0(monthly_counts_dx,"/",names(count_files_diag[i])))
}
