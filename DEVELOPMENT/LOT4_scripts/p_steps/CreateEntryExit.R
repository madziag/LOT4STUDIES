# creates entry and exit dates

#entry date: latest date at which all of the following conditions are met: age>12, observation period starts, study starts
#using ALL_study_population
entry_vars<-c(date_min, op_start_date, study_start_date)

#exit date: earliest date at which ANY of the following conditions are met: age>55, observation ends, study ends, sterilization record
# using ALL_study_populations AND sterilization
