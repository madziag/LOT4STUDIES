#Author: Romin Pajouheshnia
#email: r.pajouheshnia@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 03/02/2022

# Script for converting Excel files for UCPH to 
#input: denominator file for UCPH, excel file counts/rates
#output: pdf plots

if(!require(readxl)){install.packages("readxl")}
library(readxl)
if(!require(Rcpp)){install.packages("Rcpp")}
library(Rcpp)

# Extracts denominator and counts files
FOLDERS<-c("contraceptive_counts", "medicines_counts", "preliminary_counts") 

for (k in length(FOLDERS)){
  plot_folder <- paste0("Y:/research-ucph/Analysis scripts/g_output_2022.02.15/", FOLDERS[k])
  setwd(plot_folder)
  denom<-read.csv(paste0(plot_folder,"/denominator.csv"))
  count_names_all = list.files(pattern="*.xlsx")
  count_files_all = lapply(count_names_all, read_excel)

# PLOTS
## COUNTS
  for (i in 1:length(count_files_all)){
    main_name<-substr(count_names_all[[i]], 1,nchar(count_names_all[[i]])-11)
    pdf((paste0(plot_folder,"/", main_name, ".pdf")), width=8, height=4)
    my_data<-as.data.frame(count_files_all[[i]])
    if (length(my_data[!is.finite(my_data$N),]$N)>0) my_data[!is.finite(my_data$N),]$N <- 0
    my_ymax <- ifelse(max(my_data$N) < 1, 1, max(my_data$N))
  
    plot(x=1:nrow(my_data), y=my_data$N,ylim=c(0,my_ymax), xaxt="n", xlab="", ylab="counts", main=main_name, pch=16, type="b", lwd=2, cex.main=1.5)
    axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
    dev.off()
  }

## RATES
  for (i in 1:length(count_files_all)){
    main_name<-substr(count_names_all[[i]], 1,nchar(count_names_all[[i]])-11)
    pdf((paste0(plot_folder,"/", main_name, "rates.pdf")), width=8, height=4)
    my_data<-as.data.frame(count_files_all[[i]])
  #Set NA/inf rate values to 0
    if (length(my_data[!is.finite(my_data$rates),]$rate)>0) my_data[!is.finite(my_data$rates),]$rates <- 0
    my_ymax <- ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
  
    plot(x=1:nrow(my_data), y=my_data$rates,ylim=c(0,my_ymax), xaxt="n", xlab="", ylab="rates", main=main_name, pch=16, type="b", lwd=2, cex.main=1.5)
    axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
    dev.off()
  }
}