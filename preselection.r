#Author: Ema Alsina, MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 30/9/2021



# READ.ME
#LOT 4 preselection
#this script contains 3 functions which filter DAP data down to elligible cases based on broad criteria: gender, age, and ATC
#the goal of this function is to remove the first layers of uneccessary cases from datasets, reducing the size of data for DAPs
#functions are
    #persons filter
    #input: persons table
      #selects: female &correct age range
      #output: ID vector & dataframe with attrition
# ATCfilter
  #input: medicines table
    #selects: applicable ATC 
    #output: ID vector & dataframe with attrition
#combine_filter


numloc<-(1:100)
gender<-(sample(c(1,0), 100, replace = T))
datebirth<-sample(c(1990, 1980, 2010, 1950, 1945, 1955, 2015), 100, replace =T) 

mydata<-as.data.frame(cbind(numloc, gender, datebirth))
rm(datebirth)
rm(gender)
rm(numloc)
colnames(mydata)

#firstflter function selects females within age range from the persons table and stores the selected IDs to use for subsequent filtering
personsfilter<-function(personstable=PERSONS, caseid="person_id", sex="sex_at_instance_creation", female="F", dob= "year_of_birth", dobmin=1954, dobmax=2008) {
newdata<-personstable[(personstable[,sex]==female),]
flowchart_gender<-as.numeric(c(nrow(personstable), nrow(newdata)))
newdata<-newdata[(newdata[,dob]>=dobmin),]
filtered_data<-newdata[(newdata[,dob]<=dobmax),]
flowchart_age<-as.numeric(c(flowchart_gender, nrow(filtered_data)))
flowchart_steps<-c("original", "females only", "1954<=DOB<=2008")
filter_ID<-(filtered_data[[caseid]])
flowchart_filter<-as.data.frame(cbind(flowchart_steps, flowchart_age))
colnames(flowchart_filter)<-c("filter_step","cases_number" )
persons_filter_output<-list(filter_ID, flowchart_filter, filtered_data)
  return(persons_filter_output)
}

example_filtered_data<-personsfilter(personstable=mydata, caseid="numloc", sex="gender", female=0, dob = "datebirth", dobmin = 1954, dobmax = 2008)
example_filtered_data[[1]]
example_filtered_data[[2]]
example_filtered_data[[3]]

plot(x=(as.factor(example_filtered_data[[2]]$filter_step)), y=example_filtered_data[[2]]$cases_number)

#check on sample data

PERSONS<-read.csv("C:\\Users\\Acer\\OneDrive\\Documents\\GitHub\\LOT4\\CDMInstances\\LOT4\\PERSONS.csv")
table(PERSONS$sex_at_instance_creation)
# > table(PERSONS$sex_at_instance_creation)
# WHAT IS GOING ON WITH THIS VARIABLE?
#      F   M 
# 861  61  59 


filtered_data<-personsfilter(personstable = PERSONS, caseid = "person_id", sex = "sex_at_instance_creation", 
                           female = "F", dob = "year_of_birth", dobmin = 1954, dobmax=2008)
summary(filtered_data)


#ATC filter
#match on the first 4 cijfers in the code

#need to 
  #define ATC codes vector
    #manually extract ATC codes from SAP --> ATClib
  #select first 4 cijfers
  # select first 4 cijfers with substr()
  #use %in% to select rows 

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

#simulate test data
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
  
  medID<-newMED[[ID]]
  flowchart_ATC<-c(nrow(medtable), nrow(newMED))
  ATC_filter_output<-list(medID, flowchart_ATC, newMED)
  return(ATC_filter_output)
  }          

#should return odd rows
medID<-ATCfilter(medtable = MEDICINES, ID="numloc", ATC="ATCex")
#good
rm(medID)
rm(MEDICINES)

myMED<-as.data.frame(read.csv("C:\\Users\\Acer\\OneDrive\\Documents\\GitHub\\LOT4\\CDMInstances\\LOT4\\MEDICINES_2019_SPF.csv"))

med_ID<-ATCfilter(medtable = myMED)

#last step, combine

combine_filter<-function(person_filter_ID= filtered_data[[1]], med_filter_data= med_ID[[3]])
{final_med_data<- med_filter_data[(med_filter_data[["person_id"]]%in%person_filter_ID==T),]
  final_med_ID<-unique(final_med_data[["person_id"]])
  final_flowchart<-c(nrow(med_filter_data), nrow(final_med_data))
  final_output<-list(final_med_ID, final_flowchart, final_med_data)
          return(final_output)}


ex_output<-combine_filter(person_filter_ID = filtered_data[[1]], med_filter_data= med_ID[[3]])


length(ex_output[[1]])
ex_output[[2]]
ex_output[[3]]

cases<-c(981, 61, 39,19)
steps<- c("original", "female", "DOB", "ATC")

plot(x=(1:4), y=cases, type= "b", lwd=3)

      