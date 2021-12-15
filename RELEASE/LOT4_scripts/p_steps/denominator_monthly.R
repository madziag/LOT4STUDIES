#this script counts the number of active/elligible participants per month in the study (denominator for events)

# if(!require(rstudioapi)){install.packages("rstudioapi")}
# library(rstudioapi)
# if(!require(rstudioapi)){install.packages("lubridate")}
# library(lubridate)
if(!require(rstudioapi)){install.packages("zoo")}
library(zoo)


# 
# projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(projectFolder)
# setwd('..')
# base_folder<-getwd()

study_population <- readRDS(paste0(populations_dir, "ALL_study_population.rds"))

start.date<-as.Date(study_population$start_follow_up)
end.date<-as.Date(study_population$end_follow_up)


ym1 <- as.yearmon(as.character(start.date), "%Y-%m-%d") # convert to yearmon
ym2 <- as.yearmon(as.character(end.date), "%Y-%m-%d") # ditto

#get the seq of months between start dates and end dates in a list 
#easy with a for loop, but better with apply 

#start with a loop and then see how slow it is, make more elegant later

FUmonths<-list()

for (i in 1:length(ym1)){
  s <- seq(ym1[i], ym2[i], (1/12)) # create yearmon sequence
  s<-as.numeric(format(s, "%Y%m"))
  FUmonths[[i]]<-s
}

#doesn't take too long

FUmonths<-unlist(FUmonths)
studyFUmonths<-FUmonths[(FUmonths>=200901)&(FUmonths<=202012)]

FUmonths_df<-as.data.frame(table(studyFUmonths))
FUmonths_df$YM<-as.Date(paste0(as.character(FUmonths_df$studyFUmonths),"01"), format="%Y%m%d")
FUmonths_df$YM<-format(as.Date(FUmonths_df$YM),"%Y-%m")

# for plots need to split(FUmonths,by=year)?

saveRDS(data.table(FUmonths_df), paste0(output_dir, "denominator.rds"))


pdf((paste0(output_dir, "plots/denominator.pdf")), width=8, height=4)
plot(FUmonths_df$studyFUmonths, FUmonths_df$Freq, ylab="Persons Observed per Month", xlab="Year and Month")
invisible(dev.off())

