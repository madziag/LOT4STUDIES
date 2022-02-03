#boxplots per year of age of study pop

# load study population with entry and exit dates
# mydata<-readRDS(paste0(populations_dir,list.files(populations_dir)))
# Study population is already loaded in the wrapper script
mydata <- study_population

# dataframe to store ids
myplotdata<-data.frame(matrix(ncol=length(seq(2010,2020, by=1))))
colnames(myplotdata)<-as.character(seq(2010,2020, by=1))
myyears<-(seq(2010,2020, by=1))

#assign personIDs to all years of participation
myentry<-lubridate::year(mydata$entry_date)
myexit<-lubridate::year(mydata$exit_date)

for(i in 1:ncol(myplotdata)){
  for(j in 1:nrow(mydata)){
    if(between(x=myyears[i], left=myentry[j], right=myexit[j])==T){myplotdata[j,i]<-(myyears[i]-mydata$year_of_birth[j])}else{myplotdata[j,i]<-NA}
  }
}

summary(myplotdata)
pdf((paste0(plot_folder,"/", pop_prefix, "_age_distribution_boxplot.pdf")), width=8, height=4)
boxplot(myplotdata, main="Entry Age Distribution of Participants by Year")
dev.off() 

rm(i,j, mydata, myplotdata, myentry, myexit, myyears)