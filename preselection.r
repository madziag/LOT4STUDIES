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

#firstflter function selects females within age range from the persons table and stores the selected IDs to use for subsequent filtering

firstfilter<-function(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female="F", dob= "year_of_birth", dobmin=1954, dobmax=2008) {
newdata<-personstable[(personstable[,sex]==female),]
flowchart_gender<-c(nrow(personstable), nrow(newdata))
newdata<-newdata[(newdata[,dob]>=dobmin),]
filtered_data<-newdata[(newdata[,dob]<=dobmax),]
flowchart_age<-nrow(filtered_data)
flowchart_steps<-c("original", "females only", "1954<=DOB<=2008")
filter_ID<-(filtered_data[[caseid]])
flowchart_filter<-as.data.frame(cbind(flowchart_steps, c(flowchart_gender, flowchart_age)))
filter_output<-list(filter_ID, flowchart_filter)
  return(filter_output)
}

example_filtered_data<-firstfilter(personstable=mydata, caseid="numloc", sex="gender", female=0, dob = "datebirth", dobmin = 1954, dobmax = 2008)
example_filtered_data

plot(example_filtered_data$flowchart_steps, example_filtered_data$V2, lwd=3)

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

#only 1 column for ATC codes (nice)


#isolate first 4 cijfers
substr(MEDICINES$ATC1, 1, 4)

#first step, establish ATC library

#mix of SAP ATCs and random ATCs
# random
ATC1<-sample(c("A10AE01", "P16AE01", "B20AE01", "A37GE01", "N10AE01"),100, replace = T)
# SAP
ATC_lib<-as.data.frame(read.csv("C:\\Users\\Acer\\OneDrive\\Documents\\work\\lot4\\ATC_lot4.csv"))
colnames(ATC_lib)<-c("drug_name", "ATC")
ATC_lib<-unique(ATC_lib)
ATC_lib<-ATC_lib[complete.cases(ATC_lib)==T,]
ATC_lot4<-ATC_lib$ATC

#something going wrong here concatenate causing integers 

ATC<-(c(ATC1[1:50], ATC_lot4[1:50]))
numloc<-(1:100)
MEDICINES<-as.data.frame(cbind(numloc, ATC))

rm(numloc)
rm(ATC1)
rm(ATC)
#second match to data

MEDICINES[MEDICINES[,"ATC"]
          
          %in%ATC_lib$ATC]

