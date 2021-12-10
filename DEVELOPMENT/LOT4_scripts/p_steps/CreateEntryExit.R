# creates entry and exit dates

#entry date: latest date at which all of the following conditions are met: age>12, observation period starts, study starts
#using ALL_study_population
ALL_study_population<-readRDS(paste0(populations_dir, "ALL_study_population.rds"))
start_date<-as.Date(as.character(20100101), "%Y%m%d")

ALL_study_population$entry_date<-as.Date(as.character("18000101"), "%Y%m%d")
print("calculating entry date")
for(i in 1:nrow(ALL_study_population)){
  entry_dates<-c(ALL_study_population$date_min[i], ALL_study_population$op_start_date[i], start_date)
  max_date<-max(entry_dates)
  ALL_study_population$entry_date[i]<-as.Date(max_date, "%Y%m%d")
}

summary(ALL_study_population$entry_date)
print("entry date OK")

#exit date: earliest date at which ANY of the following conditions are met: age>55, observation ends, study ends, sterilization record
# using ALL_study_populations AND sterilization
sterility<-readRDS(paste0(g_intermediate,"tmp/sterility/sterility_all_first_occurrence.rds"))
sterility<-sterility[,c("person_id", "sterility_date")]
ALL_study_population<-dplyr::left_join(ALL_study_population, sterility)
end_date<-as.Date(as.character(20201231), "%Y%m%d")

print("calculating exit date")
for(i in 1:nrow(ALL_study_population)){
  exit<-c(ALL_study_population$date_max[i], ALL_study_population$op_end_date[i], ALL_study_population$sterility_date[i], end_date)
  min_date<-min(exit,na.rm = T)
  ALL_study_population$exit_date[i]<-as.Date(min_date, "%Y%m%d")
}

summary(ALL_study_population$exit_date)
print("exit date OK")
saveRDS(ALL_study_population, paste0(populations_dir, "ALL_study_population.rds"))

