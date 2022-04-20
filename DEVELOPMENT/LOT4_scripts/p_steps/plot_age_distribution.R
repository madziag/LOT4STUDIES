#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 05/01/2022

#creates boxplots per study year of age distribution of study pop to check for unexpected patterns

# Study population is already loaded in the wrapper script
mydata <- study_population
if(nrow(mydata)>0){
  mydata <- mydata[,c("person_id", "birth_date", "entry_date", "exit_date")]
  mydata <- mydata[,rowID:=.I]
  
  mydata_expanded <- setDT(mydata)[,list(idnum = person_id, year_in_study = year(seq(entry_date, exit_date, by = "year"))), by = "rowID"]
  # Merges back with original df
  mydata_expanded <-  merge(mydata, mydata_expanded, by = "rowID")
  # Age in every year of study 
  mydata_expanded[,age:= year_in_study - year(birth_date)]

  pdf((paste0(plot_folder,"/", pop_prefix, "_age_distribution_boxplot.pdf")), width=8, height=4)
  boxplot(age ~ year_in_study, data = mydata_expanded, main="Entry Age Distribution of Participants by Year")
  dev.off() 
}
# Clean up
rm(list = grep("^mydata", ls(), value = TRUE))



