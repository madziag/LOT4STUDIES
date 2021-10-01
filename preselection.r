#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 30/9/2021

#LOT 4 preselection
#the goal of this function is to remove the first layers of uneccessary cases from datasets, reducing the size of data for DAPs
#filter keeps only:
#persons table
  #female
  #correct age range
# using medicines table
  #applicable ATC 


numloc<-(1:100)
gender<-(sample(c(1,0), 100, replace = T))
datebirth<-sample(c(1990, 1980, 2010, 1950, 1945, 1955, 2015), 100, replace =T) 

mydata<-as.data.frame(cbind(numloc, gender, datebirth))
rm(datebirth)
rm(gender)
rm(numloc)
colnames(mydata)

#firstflter function selects females within age range from the persons table and stores the selected IDs

firstfilter<-function(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female=0, dob= "year_of_birth", dobmin=1954, dobmax=2008) {
newdata<-personstable[(personstable[,sex]==female),]
newdata<-newdata[(newdata[,dob]>=dobmin),]
filtered_data<-newdata[(newdata[,dob]<=dobmax),]
  return(filtered_data)
}

example_filtered_data<-firstfilter(personstable=mydata, caseid="numloc", sex="gender", female=0, dob = "datebirth", dobmin = 1954, dobmax = 2008)

#check on sample data

PERSONS<-read.csv("C:\\Users\\Acer\\OneDrive\\Documents\\GitHub\\LOT4\\CDMInstances\\LOT4\\PERSONS.csv")
colnames(PERSONS)
filtered_data<-firstfilter(personstable = PERSONS, caseid = "person_id", sex = "sex_at_instance_creation", 
                           female = "F", dob = "year_of_birth", dobmin = 1954, dobmax=2008)
summary(filtered_data)


#ATC filter
#match on the first 4 cijfers in the code

#need to 
  #define ATC codes vector
    #manually extract ATC codes from SAP >_< 
  #select first 4 cijfers
  #concatenate ATC columns from MEDICINES
  # select first 4 cijfers
  #use %in% to select rows with at least 1 match

#simulate MEDICINES table

ATC1<-sample(c("A10AE01", "P16AE01", "B20AE01", "A37GE01", "N10AE01"),100, replace = T)
ATC2<-sample(c("A10AE01", "P16AE01", "B20AE01", "A37GE01", "N10AE01"),100, replace = T)
ATC3<-sample(c("A10AE01", "P16AE01", "B20AE01", "A37GE01", "N10AE01"),100, replace = T)

MEDICINES<-as.data.frame(cbind(numloc, ATC1, ATC2, ATC3))

rm(numloc)
rm(ATC1)
rm(ATC2)
rm(ATC3)

#first step, match to filtered data

