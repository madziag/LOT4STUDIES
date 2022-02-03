# Script for converting Excel files for UCPH to 
#input: denominator file for UCPH, excel file counts/rates
#output: pdf plots
#03 Deb 2022
#R Pajouheshnia
install.packages("readxl")
library(readxl)
install.packages("Rccp")
library(Rccp)
#if(!require(rstudioapi)){install.packages("rstudioapi")}
#library(rstudioapi)
#projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(projectFolder)
#setwd('..')

yoda_path <- "Y:/research-ucph/Analysis scripts/g_output_020222/preliminary_counts"
plot_folder <- paste0(yoda_path,"/plots")
setwd(yoda_path)
denominator<-read.csv(paste0(yoda_path,"/denominator.csv"))

# Extracts counts files
count_names_all = list.files(pattern="*.xlsx")
count_files_all = lapply(count_names_all, read_excel)

for (i in 1:length(count_files_all)){
      
      main_name<-substr(count_names_all[[i]], 1,nchar(count_names_all[[i]])-11)
      
      pdf((paste0(plot_folder,"/", main_name, ".pdf")), width=8, height=4)
      
      my_data<-as.data.frame(count_files_all[[i]])
      #indicate masked values with stars
      my_pch<-count_files_all[[i]]$masked
      my_pch[my_pch==0]<-16
      my_pch[my_pch==1]<-8
      
      plot(x=1:nrow(my_data), y=my_data$N, xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
      axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
      dev.off()
}

pdf((paste0(plot_folder,"/", denom, ".pdf")), width=8, height=4)
my_data<-as.data.frame(denominator)
plot(x=1:nrow(my_data), y=my_data$Freq, xaxt="n", xlab="", ylab="counts", main="denominator", type="b", lwd=2, cex.main=1.5)
axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
dev.off()