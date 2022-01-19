# creates entry and exit dates
#add spell_start and spell_end
print("merge spell_start spell_end from ALL_OBS_SPELLS")

pop_prefix <- str_replace(populations[pop], "study_population.rds", "")

# Read in OBS_SPELLS  
OBS_SPELLS <- readRDS(paste0(g_intermediate,"tmp/STUDY_SOURCE_POPULATION/", pop_prefix, "OBS_SPELLS.rds"))
names(OBS_SPELLS)[names(OBS_SPELLS) == 'op_start_date'] <- 'spell_start_date'
names(OBS_SPELLS)[names(OBS_SPELLS) == 'op_end_date'] <- 'spell_end_date'
OBS_SPELLS<-OBS_SPELLS[,c("person_id","spell_start_date", "spell_end_date")]
# Merge OBS_SPELLS with study population
study_population[,person_id:=as.character(person_id)]
OBS_SPELLS[,person_id:=as.character(person_id)]
# Merge OBS file with study population file 
study_population<-OBS_SPELLS[study_population,on=.(person_id)] # Left join

# Create Entry Date 
study_population$start_date <- as.Date(as.character(20100101), "%Y%m%d")
study_population$entry_date <- pmax(study_population$date_min, study_population$spell_start_date, study_population$start_date, na.rm = TRUE)
study_population$entry_date <- as.Date(study_population$entry_date, "%Y%m%d")

summary(study_population$entry_date)
print("entry date OK")

#exit date: earliest date at which ANY of the following conditions are met: age>55, observation ends, study ends, sterilization record
# using study_populations AND sterilization

if (length(list.files(paste0(g_intermediate,"tmp/sterility/"), pattern = "sterility_all_first_occurrence.rds"))>0){
  if(SUBP == TRUE){
    sterility<-readRDS(paste0(g_intermediate,"tmp/sterility/", populations[pop], "_sterility_all_first_occurrence.rds"))
  }else {
    sterility<-readRDS(paste0(g_intermediate,"tmp/sterility/sterility_all_first_occurrence.rds"))
  }
  
  # Data Cleaning 
  sterility<-sterility[,c("person_id", "Date")]
  setnames(sterility,"Date","Sterility_Date") # Rename column names
  study_population[,person_id:=as.character(person_id)]
  sterility[,person_id:=as.character(person_id)]
  # Merge Sterility file with study population file 
  study_population<-sterility[study_population,on=.(person_id)] # Left join
  
  # Determine the exit date (including sterility records )
  study_population$end_date  <- as.Date(as.character(20201231), "%Y%m%d")
  study_population$exit_date <- pmin(study_population$date_max, study_population$spell_end_date, study_population$end_date,study_population$Sterility_Date, na.rm = TRUE)
  study_population$exit_date <- as.Date(study_population$exit_date, "%Y%m%d")
  
  summary(study_population$exit_date)
  print("exit date OK")
  
} else {
  # Determine the exit date (excluding sterility records )
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

