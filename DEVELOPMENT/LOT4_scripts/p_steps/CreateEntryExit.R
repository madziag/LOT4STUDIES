# creates entry and exit dates

#entry date: latest date at which all of the following conditions are met: age>12, observation period starts, study starts
#using study_population
# study_population<-readRDS(paste0(populations_dir, "study_population.rds"))

# start_date<-as.Date(as.character(20100101), "%Y%m%d")
# 
# # study_population$entry_date<-as.Date(as.character("18000101"), "%Y%m%d")
# 
# study_population$entry_date <- as.Date(as.character(20100101), "%Y%m%d")
# 
# print("calculating entry date")
# for(i in 1:nrow(study_population)){
#   entry_dates<-c(study_population$date_min[i], study_population$spell_start_date[i], start_date)
#   max_date<-max(entry_dates)
#   study_population$entry_date[i]<-as.Date(max_date, "%Y%m%d")
# }

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
# Merge Sterility file with study population file 
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


# Remove records where exit date is < entry date
study_population <- study_population[study_population$entry_date < study_population$exit_date,]
# sterility<-sterility[,c("person_id", "Date")]
# study_population<-dplyr::left_join(study_population, sterility)
# end_date<-as.Date(as.character(20201231), "%Y%m%d")

# study_population$exit_date <- as.Date(as.character(20201231), "%Y%m%d")
# 
# print("calculating exit date")
# for(i in 1:nrow(study_population)){
#   exit<-c(study_population$date_max[i], study_population$spell_end_date[i], study_population$sterility_date[i], end_date)
#   min_date<-min(exit,na.rm = T)
#   study_population$exit_date[i]<-as.Date(min_date, "%Y%m%d")
# }

<<<<<<< Updated upstream
=======
study_population$end_date  <- as.Date(as.character(20201231), "%Y%m%d")
study_population$exit_date <- pmin(study_population$date_max, study_population$spell_end_date, study_population$end_date,study_population$Sterility_Date, na.rm = TRUE)
study_population$exit_date <- as.Date(study_population$exit_date, "%Y%m%d")
>>>>>>> Stashed changes


# Exclude any individuals who have exit_date before Jan 01 2010, as they will never be eligible
study_population <- study_population[study_population$exit_date > study_population$entry_date,]
print(paste0(dim(study_population[study_population$exit_date <= study_population$entry_date,])[1], " individuals excluded as their exit date was before their entry date, i.e. before 01 Jan 2010"))

#add spell_start and spell_end
# print("merge spell_start spell_end from ALL_OBS_SPELLS")
# 
# ALL_OBS_SPELLS<-readRDS(paste0(g_intermediate,"tmp/STUDY_SOURCE_POPULATION/ALL_OBS_SPELLS.rds"))
# names(ALL_OBS_SPELLS)[names(ALL_OBS_SPELLS) == 'op_start_date'] <- 'spell_start_date'
# names(ALL_OBS_SPELLS)[names(ALL_OBS_SPELLS) == 'op_end_date'] <- 'spell_end_date'
# ALL_OBS_SPELLS<-ALL_OBS_SPELLS[,c("person_id","spell_start_date", "spell_end_date")]
# study_population<-dplyr::left_join(study_population, ALL_OBS_SPELLS)

print("save study_population.rds with entry, exit and spells start and end")
# saveRDS(study_population, paste0(populations_dir, "study_population.rds"))
saveRDS(study_population, paste0(populations_dir, populations[pop]))

