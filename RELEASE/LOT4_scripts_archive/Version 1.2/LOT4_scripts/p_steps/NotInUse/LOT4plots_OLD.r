#generate lineplots of counts of variables
#monthly counts by year
#total counts over whole study period

#plot groups of variables on one plot
# can't use count list because doesn't have variable names


# find correct folders

if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(projectFolder)
plotFolder_med<-paste0(projectFolder,"/g_intermediate/populations/MEDICINES/monthly_counts")
plotFolder_diag<-paste0(projectFolder,"/g_intermediate/populations/DIAGNOSES/monthly_counts")



#extract count files

count_names_diag<-list.files(plotFolder_diag, pattern="count")
count_names_med<-list.files(plotFolder_med, pattern="count")

count_files_diag<-lapply(paste0(plotFolder_diag,"/", count_names_diag), readRDS)
count_files_med<-lapply(paste0(plotFolder_med,"/", count_names_med), readRDS)

#masking

# mask<-T

# group_names<-c("alt_med", "contracep", "etc...")



# medicines 

for(i in 1:length(count_names_med)){
  main_name<-substr(count_names_med[[i]], 1,nchar(count_names_med[[i]])-11)
  
  pdf((paste0(projectFolder, "/g_output/plots/", main_name, ".pdf")), width=8, height=4)
  
  var_counts<-count_files_med[[i]]$N
  if(mask==T){var_counts[(0<var_counts)&(var_counts<=5)]<-5}
  
  mycounts<-ts(var_counts, frequency = 12, start = 2009,end = 2020)
  mydates<-paste0(15,"-",count_files_med[[i]]$month, "-", count_files_med[[i]]$year)
  count_files_med[[i]]$date<-as.Date(mydates, "%d-%m-%y")

  plot(mycounts, xaxt="n", yaxt="n", xlab="", ylab="counts", main=main_name, lwd=2, cex.main=1.5)
  tsp = attributes(mycounts)$tsp
  dates = seq(as.Date("2009-01-01"), by = "month", along = mycounts)
  axis(1, at = seq(tsp[1], tsp[2], along = mycounts),las=2, labels = format(dates, "%Y-%m"))
  axis(2, seq(0:(max(count_files_med[[i]]$N)+1)))
  dev.off()

}

#diagnoses 

for(i in 1:length(count_names_diag)){
  main_name<-substr(count_names_diag[[i]], 1,nchar(count_names_diag[[i]])-11)
  
  pdf((paste0(projectFolder, "/g_output/plots/", main_name, ".pdf")), width=8, height=4)
  
  var_counts<-count_files_diag[[i]]$N
  if(mask==T){var_counts[(0<var_counts)&(var_counts<=5)]<-5}
  
  mycounts<-ts(var_counts, frequency = 12, start = 2009,end = 2020)
  mydates<-paste0(15,"-",count_files_diag[[i]]$month, "-", count_files_diag[[i]]$year)
  count_files_diag[[i]]$date<-as.Date(mydates, "%d-%m-%y")
  
  plot(mycounts, xaxt="n", yaxt="n", xlab="", ylab="counts", main=main_name, lwd=2, cex.main=1.5)
  tsp = attributes(mycounts)$tsp
  dates = seq(as.Date("2009-01-01"), by = "month", along = mycounts)
  axis(1, at = seq(tsp[1], tsp[2], along = mycounts),las=2, labels = format(dates, "%Y-%m"))
  axis(2, seq(0:(max(count_files_diag[[i]]$N)+1)))
  dev.off()
  
}
