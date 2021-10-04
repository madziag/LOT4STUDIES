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
# substr(MEDICINES$ATC1, 1, 4)

#establish ATC library

# SAP ATCs
ATC_lib<-as.data.frame(read.csv("C:\\Users\\Acer\\OneDrive\\Documents\\work\\lot4\\ATC_lot4.csv"))
colnames(ATC_lib)<-c("drug_name", "ATC")
ATC_lib<-unique(ATC_lib)
ATC_lib<-ATC_lib[complete.cases(ATC_lib)==T,]
ATC_lot4<-ATC_lib$ATC

ATCfirst4<-substr(ATC_lot4, 1,4)

#here are the 4 digit codes from the SAP to filter the MEDICINES table
ATCfirst4<-unique(ATCfirst4)

#one ATC from LOT4 lib (G03AA13), and one random (A10AE01) to test selection
ATCex<-rep(c("G03AA13", "A10AE01"),50)  

#create sample dataset
numloc<-(1:100)
MEDICINES<-as.data.frame(cbind(numloc, ATCex))

rm(numloc)
rm(ATCex)
#second match to data


ATCfilter<-function(medtable=MEDICINES, ID="person_id", ATC="medicinal_product_atc_code", Lot4ATC= 
                      c( "N03A","N05B", "N05A", "C07A", "N06A", "N07C", "N02C", "C02A", "G03A",
                          "G03D", "G02B", "B03B", "B03A", "D10A", "J01F", "S01A", "J01A",
                            "D07A", "H02A", "D11A", "L04A", "D05A", "D05B")){
  medATC4<-substr((medtable[,ATC]), 1,4)
  newMED<-medtable[(medATC4%in%Lot4ATC),]
  
  medID<-as.vector(newMED[,ID])
  return(medID)
  }          

#should return odd rows
medID<-ATCfilter(medtable = MEDICINES, ID="numloc", ATC="ATCex")
#good
rm(medID)
rm(MEDICINES)

myMED<-as.data.frame(read.csv("C:\\Users\\Acer\\OneDrive\\Documents\\GitHub\\LOT4\\CDMInstances\\LOT4\\MEDICINES_2019_SPF.csv"))

med_ID<-ATCfilter(medtable = myMED)

