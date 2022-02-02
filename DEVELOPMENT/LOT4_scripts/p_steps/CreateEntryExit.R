# Creates entry and exit dates of study population
# Adds spell_start and spell_end
print("merge spell_start spell_end from OBS_SPELLS")
# # Gets study population prefix to be used to read in corresponding OBS_SPELLS data
# pop_prefix <- str_replace(populations[pop], "study_population.rds", "")
# Reads in OBS_SPELLS  
OBS_SPELLS <- readRDS(paste0(g_intermediate,"tmp/STUDY_SOURCE_POPULATION/", pop_prefix, "_OBS_SPELLS.rds"))
# Renames columns
names(OBS_SPELLS)[names(OBS_SPELLS) == 'op_start_date'] <- 'spell_start_date'
names(OBS_SPELLS)[names(OBS_SPELLS) == 'op_end_date'] <- 'spell_end_date'
# Drops unnecessary columns
OBS_SPELLS<-OBS_SPELLS[,c("person_id","spell_start_date", "spell_end_date")]
# Merges OBS_SPELLS with study population
study_population[,person_id:=as.character(person_id)]
OBS_SPELLS[,person_id:=as.character(person_id)]
study_population<-OBS_SPELLS[study_population,on=.(person_id)] # Left join
# Creates Entry Date 
## entry date <- latest date at which ANY of the following conditions are met: age > 12, observation starts, study starts
# Looks for max date of all chosen columns
study_population$start_date <- as.Date(as.character(20100101), "%Y%m%d")
study_population$entry_date <- pmax(study_population$date_min, study_population$spell_start_date, study_population$start_date, na.rm = TRUE)
study_population$entry_date <- as.Date(study_population$entry_date, "%Y%m%d")
summary(study_population$entry_date)
print("entry date OK")
# Creates Exit date
## exit date <- earliest date at which ANY of the following conditions are met: age>55, observation ends, study ends, sterilization record date (if present)
# Checks if there are sterilization records. 
## If there are, loads the file and merges with study population, and uses this data to create exit date
## If there are no sterilization records, uses age>55, observation date end and study end date to determine exit date
if (length(list.files(paste0(g_intermediate,"tmp/sterility/"), pattern = "sterility_all_first_occurrence.rds"))>0){
  # Loads sterilization records
  # if(SUBP == TRUE){sterility<-readRDS(paste0(g_intermediate,"tmp/sterility/", populations[pop], "_sterility_all_first_occurrence.rds"))}else {sterility<-readRDS(paste0(g_intermediate,"tmp/sterility/sterility_all_first_occurrence.rds"))}
  sterility <- readRDS(paste0(sterility_pop, pop_prefix, "_sterility_all_first_occurrence.rds"))
  # Data Cleaning 
  sterility<-sterility[,c("person_id", "Date")]
  setnames(sterility,"Date","Sterility_Date") # Renames column names
  # Merges sterilization file with study population
  study_population[,person_id:=as.character(person_id)]
  sterility[,person_id:=as.character(person_id)]
  study_population<-sterility[study_population,on=.(person_id)] # Left join
  # Looks for min date of all chosen columns
  if (is_PHARMO){study_population$end_date  <- as.Date(as.character(20191231), "%Y%m%d")} else {study_population$end_date  <- as.Date(as.character(20201231), "%Y%m%d")}
  study_population$exit_date <- pmin(study_population$date_max, study_population$spell_end_date, study_population$end_date,study_population$Sterility_Date, na.rm = TRUE)
  study_population$exit_date <- as.Date(study_population$exit_date, "%Y%m%d")
  summary(study_population$exit_date)
  print("exit date OK")
} else {
  # Determines exit date (excluding sterility records )
  study_population$end_date  <- as.Date(as.character(20201231), "%Y%m%d")
  study_population$exit_date <- pmin(study_population$date_max, study_population$spell_end_date, study_population$end_date, na.rm = TRUE)
  study_population$exit_date <- as.Date(study_population$exit_date, "%Y%m%d")
  summary(study_population$exit_date)
  print("exit date OK")
}
# Exclude any individuals who have exit_date before Jan 01 2010, as they will never be eligible
print(paste0(dim(study_population[study_population$exit_date <= study_population$entry_date,])[1], " individuals excluded as their exit date was before their entry date, i.e. before 01 Jan 2010"))
study_population <- study_population[study_population$exit_date > study_population$entry_date,]
print("save study_population.rds with entry, exit and spells start and end")
saveRDS(study_population, paste0(populations_dir, populations[pop]))

rm(OBS_SPELLS, sterility)