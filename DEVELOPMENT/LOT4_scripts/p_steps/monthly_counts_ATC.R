# Create empty table for counts 
empty_counts_df <- expand.grid(seq(2009, 2019), seq(1, 12))
names(empty_counts_df) <- c("year", "month")
# Load study population
study_population <- readRDS(paste0(populations_dir, "ALL_study_population.rds"))
# Load code file
filename <- "CodeLists/ATC_lot4_formatted.xlsx"
# matches <- c("altmed", "contracep", "folic_acid", "valproate", "retinoid")
matches <- c()
print("Loading concept sets")
source(paste0(pre_dir,"CreateConceptSets_ATC.R"))
# List of Retinoid and Valproates for individual counts
codelist_ind <- Filter(function(x) names(codelist_all)== "Valproate" | names(codelist_all) == "Retinoid", codelist_all)
codelist_ind <- Filter(function(x) length(x) > 0, codelist_ind)

# Create subsets of data of the study population with ATC codes corresponding to the ATC group loaded above
if(length(actual_tables$MEDICINES)>0){
  # Create lists for saving info
  df_subset <- list() # List to save subsets of data for each ATC group indicated in the match argument above
  file_list <- list() # List to save files in Events folder with pattern corresponding to ATC group
  comb_meds <- list() # List to save concatenated files belonging to the same ATC group
  counts <- list() # List to save counts per ATC group
  counts_ind <- list() # List to save counts per ATC 
  df_ind <- list() # List of Retinoid and Valproate dfs
  sub_ind <- list() # List of subserts

  # Loads each medicine table, joins it with study population, then uses ATC groups to create subsets of data
  for (y in 1:length(actual_tables$MEDICINES)){
    # Loads medicine table
    df<-fread(paste(path_dir, actual_tables$MEDICINES[y], sep=""), stringsAsFactors = FALSE)
    # Data cleaning: Choose necessary columns, rename accordingly
    df<-df[,c("person_id", "medicinal_product_atc_code", "date_dispensing", "date_prescription", "meaning_of_drug_record")]
    setnames(df,"medicinal_product_atc_code", "Code")
    #make sure missing data is read appropriately
    df<-df[,lapply(.SD, FUN=function(x) gsub("^$|^ $", NA, x))]
    # Create new column event_date. It takes the date from date_dispensing. If value from date_dispensing is missing, it takes the date value from date_prescription
    df<-df[,event_date:= ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)]
    # To be used to drop records with missing values in these columns
    colnames_events<-names(df)
    std_names_events<-names(study_population)
    colnames_events<-colnames_events[!colnames_events %in% "person_id"]
    # Merge medicine table with study population table (there is no missing data in this table)
    df[,person_id:=as.character(person_id)]
    study_population[,person_id:=as.character(person_id)]
    df<-df[study_population,on=.(person_id)]#left join, keeps all people in the study population even if they didn't have an event
    # Transform to numeric variables  
    df<-df[,age_start_follow_up:=as.numeric(age_start_follow_up)]
    # Remove records with missing values in the medicine table 
    df<-df[!rowSums(is.na(df[,..colnames_events]))==length(colnames_events)]
    # Transform to date variables
    df[,event_date:=as.IDate(event_date,"%Y%m%d")] 
    # Create variable year (year of event_date)
    df[,year:=year(event_date)]
    # Remove records with missing year, or year is less than 2009 or more than 2019
    df<-df[!is.na(year)]
    df<-df[year>2009 | year<2019]
    # remove records that are outside the obs_period for all subjects
    df[(event_date<start_follow_up | event_date>end_follow_up), obs_out:=1]
    df<-df[is.na(obs_out)] 
    df[,obs_out:=NULL]
    # remove records with ATC code missing 
    df<-df[!is.na(Code)]
    # remove unspecified sex
    df<-df[sex_at_instance_creation == "M" | sex_at_instance_creation == "F"]
    print(paste0("Finding matching records in ", actual_tables$MEDICINES[y]))
    # Match codes: Saves df for each ATC code group, per Medicine Table
    for (i in 1:length(codelist_all)){
      df_subset[[i]] <- setDT(df)[Code %chin% codelist_all[[i]][,Code]]
      saveRDS(data.table(df_subset[[i]]), paste0(events_tmp_ATC, names(codelist_all[i]), "_",actual_tables$MEDICINES[y], ".rds"))

      }
  }
    print("Counting records")
    # Concatenate files belonging to the same ATC group. Perform counts per year/month
    for(i in 1:length(codelist_all)){
      # Find files in folder with the same name as the variable
      file_list[[i]]<- list.files(path=events_tmp_ATC, pattern=names(codelist_all[i]))
      if (length(file_list[[i]])>0){
        # Loads all dfs with corresponding variable name
        comb_meds[[i]]<-lapply(paste0(events_tmp_ATC, file_list[[i]]), readRDS)
        comb_meds[[i]]<-do.call(rbind,comb_meds[[i]])
        # Counts records per month/year
        counts[[i]]<- comb_meds[[i]][,.N, by = .(year,month(event_date))]
        counts[[i]]<- as.data.table(merge(x = empty_counts_df, y = counts[[i]], by = c("year", "month"), all.x = TRUE))
        counts[[i]][is.na(counts[[i]][,N]), N:=0]
        # Saves results
        if (subpopulations_present=="Yes"){
          if(comb_meds[[i]][,.N]>0){
            saveRDS(comb_meds[[i]], paste0(medications_pop,subpopulations_names[s], "/", names(codelist_all[i]),".rds"))
            saveRDS(counts[[i]], paste0(monthly_counts_atc,"/",subpopulations_names[s], "/", names(codelist_all[i]),"_counts.rds"))
            } else {print(paste("There are no matching records for", names(codelist_all[i])))}
        } else {
          if(comb_meds[[i]][,.N]>0){
            saveRDS(comb_meds[[i]], paste0(medications_pop,names(codelist_all[i]),".rds"))
            saveRDS(counts[[i]], paste0(monthly_counts_atc,"/",names(codelist_all[i]),"_counts.rds"))
          } else {print(paste("There are no matching records for", names(codelist_all[i])))}
        }
      } else {
        print(paste("There are no matching records for", names(codelist_all[i])))
      }

    }
  # Individual counts for Valproates and Retinoids
  for(i in 1:length(codelist_ind)){
    df_ind[[i]] <- readRDS(paste0(medications_pop, list.files(path=medications_pop, pattern = paste0(names(codelist_ind)[i], ".rds"))))
    for(j in 1:length(unique(codelist_ind[[i]]$Code))){
      sub_ind[[j]] <- setDT(df_ind[[i]])[Code %chin% codelist_ind[[i]][j][,Code]]
      counts_ind[[j]]<- sub_ind[[j]][,.N, by = .(year,month(event_date))]
      counts_ind[[j]]<- as.data.table(merge(x = empty_counts_df, y = counts_ind[[j]], by = c("year", "month"), all.x = TRUE))
      counts_ind[[j]][is.na(counts_ind[[j]][,N]), N:=0]

      if (subpopulations_present=="Yes"){
        if(comb_meds[[i]][,.N]>0){
          saveRDS(counts_ind[[j]], paste0(medications_pop,subpopulations_names[s], "/",tolower(names(codelist_ind[i])),"_",codelist_ind[[i]][j][,Medication],codelist_ind[[i]][j][,Code],"_counts.rds"))
        } else {print(paste("There are no matching records for", names(codelist_all[i])))}
          } else {
            if(comb_meds[[i]][,.N]>0){
          saveRDS(counts_ind[[j]], paste0(monthly_counts_atc,"/",tolower(names(codelist_ind[i])),"_",codelist_ind[[i]][j][,Medication],codelist_ind[[i]][j][,Code],"_counts.rds"))
          } else {print(paste("There are no matching records for", names(codelist_all[i])))}
            }
  }

  }
}

# Add rates to counts tables
count_names_med<-list.files(monthly_counts_atc, pattern="count")
count_files_med<-lapply(paste0(monthly_counts_atc,"/", count_names_med), readRDS)
names(count_files_med) <- count_names_med

for(i in 1:length(count_names_med)){
  count_files_med[[i]] <- within(count_files_med[[i]], YM<- sprintf("%d-%02d", year, month))
  count_files_med[[i]] <- merge(x = count_files_med[[i]], y = FUmonths_df, by = c("YM"), all.x = TRUE)   
  count_files_med[[i]] <-count_files_med[[i]][,c("YM", "N", "Freq")]
  count_files_med[[i]]$rates <- as.integer(count_files_med[[i]]$N)/as.integer(count_files_med[[i]]$Freq)
  saveRDS(count_files_med[[i]], paste0(monthly_counts_atc,"/",names(count_files_med[i])))
}


