# creates entry and exit dates

#entry date: latest date at which all of the following conditions are met: age>12, observation period starts, study starts
#using ALL_study_population

#dplyr/tidyverse 
entry_vars<-cbind(ALL_study_population$date_min, ALL_study_population$op_start_date, rep(01012010, nrow(ALL_study_population)))
ALL_study_population$entry_date <- apply(entry_vars, 1, max)


entry_vars<-c("date_min", "op_start_date", "study_start_date")
entry_date<-max(entry_vars)

apply(ALL_study_population[, entry_vars], 1, max)


#exit date: earliest date at which ANY of the following conditions are met: age>55, observation ends, study ends, sterilization record
# using ALL_study_populations AND sterilization

exit_vars<-c(date_max, op_end_date, study_end_date, sterilization)

