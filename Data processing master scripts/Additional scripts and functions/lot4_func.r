# Lot4 functions for study population selection and imputation


library(data.table)

path <- "C:\\Users\\Acer\\OneDrive\\Documents\\work\\"
setwd(path)

mydt<-data.table::fread (file=(paste0(path, "lot4_mock.csv")))

class(mydt$DOB)
#character--> first thing is to convert into dates

#how does the CDM process dates? what format is used?


as.Date(mydt$DOB, format = "m/d/y")


# keep tally of which cells have which imputation
imp_record<-data.frame("end_date"=NA, "DOB"=NA, "MOB"=NA, "DOD"=NA, "MOD"=NA)

# imp_end_date
# If observation end date is missing AND logical checks are passed (i.e. the person is alive) then impute observation end date as end of study.
# Assumption: this person's observation is ongoing


imp_end_date<- function(end_date, study_end_date, mydt){
  for(i in 1:nrow(mydt)){
  if((is.na(end_date[i])|(end_date[i]==""))==T){
    end_date[i]<-study_end_date
    imp_record$end_date[i]<-1} else{imp_record$end_date[i]<-0}
  }
}

# imp_MOB
# If year of birth is present, but month is missing, impute by sampling 1-12

imp_MOB<-function(DOB, mydt){
  for(i in 1:nrow(mydt)){
  if((is.na(month(DOB[i]))|(DOB[i]==""))==T){
    month(DOB[i])<-(sample(1:12,1))
    imp_record$MOB[i]<-1} else{imp_record$MOB[i]<-0} 
  }
}

# imp_DOB
# If year of birth is present, but day is missing, impute by sampling 1-28

imp_DOB<-function(DOB, mydt){
  for(i in 1:nrow(mydt)){
    if((is.na(day(DOB[i]))|(DOB[i]==""))==T){
      day(DOB[i])<-(sample(1:28,1))
      imp_record$DOB[i]<-1}else {imp_record$DOB[i]<-0}
  }
}

# Imp_DOD
# If day of death is missing, impute last day of quarter.  
# This prevents any observations from occurring after imputed death
imp_DOD<-function(DOD, mydt){
  for(i in 1:nrow(mydt)){
    if((is.na(day(DOD[i]))|(DOD[i]==""))==T){
      day(DOD[i])<-(day(end_quarter))
      imp_record$DOD[i]<-1}else{imp_record$DOD[i]<-0}
  }
}


# Imp_MOD
# If month of death is missing, impute last month in quarter.  
# This prevents any observations from occurring after imputed death

imp_MOD<-function(DOD,end_quarter, mydt){
  for(i in 1:nrow(mydt)){
    if((is.na(month(DOD[i]))|(DOD[i]==""))==T){
      month(DOD[i])<-(month(end_quarter))
      imp_record$MOD[i]<-1} else {imp_record$MOD[i]<-0}
  }
}


