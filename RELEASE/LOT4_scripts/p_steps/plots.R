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

#get denominators from denominator.r in p_steps
# source("denominator.r") ran in to_run_source_population
#this also creates base_folder 

#plot_folder created in 99_path

#extract denominators

denominator<-readRDS(paste0(output_dir,"denominator.rds"))

#extract count files

count_names_diag<-list.files(monthly_counts_dx, pattern="count")
count_names_med<-list.files(monthly_counts_atc, pattern="count")

count_files_diag<-lapply(paste0(monthly_counts_dx,"/", count_names_diag), readRDS)
count_files_med<-lapply(paste0(monthly_counts_atc,"/", count_names_med), readRDS)

count_names_all<-c(count_names_diag, count_names_med)
count_files_all<-c(count_files_diag, count_files_med)
# calculate rates

for(i  in 1: length (count_files_all)){
  var_counts<-denominator$Freq
  if(mask==T){var_counts[(0<var_counts)&(var_counts<=5)]<-5}
  count_files_all[[i]]$rate<-(count_files_all[[i]]$N)/var_counts
  count_files_all[[i]]$ratep1000<-round((count_files_all[[i]]$rate*1000),2)
}


for(i in 1:length(count_files_all)){
  main_name<-substr(count_names_all[[i]], 1,nchar(count_names_all[[i]])-11)
  
  pdf((paste0(plot_folder,"/", main_name, ".pdf")), width=8, height=4)
  
  var_counts<-count_files_all[[i]]$N
  if(mask==T){var_counts[(0<var_counts)&(var_counts<=5)]<-5}
  
  mycounts<-ts(var_counts, frequency = 12, start = 2009,end = 2020)
  mydates<-paste0(15,"-",count_files_all[[i]]$month, "-", count_files_all[[i]]$year)
  count_files_all[[i]]$date<-as.Date(mydates, "%d-%m-%y")

  plot(mycounts, xaxt="n", yaxt="n", xlab="", ylab="counts", main=main_name, lwd=2, cex.main=1.5)
  tsp = attributes(mycounts)$tsp
  dates = seq(as.Date("2009-01-01"), by = "month", along = mycounts)
  axis(1, at = seq(tsp[1], tsp[2], along = mycounts),las=2, labels = format(dates, "%Y-%m"))
  axis(2, seq(0:(max(count_files_all[[i]]$N)+1)))
  dev.off()

}

for(i in 1:length(count_files_all)){
  main_name<-substr(count_names_all[[i]], 1,nchar(count_names_all[[i]])-11)
  
  pdf((paste0(plot_folder,"/", main_name, "_rate.pdf")), width=8, height=4)
  
  var_counts<-count_files_all[[i]]$ratep1000
  
  
  mycounts<-ts(var_counts, frequency = 12, start = 2009,end = 2020)
  mydates<-paste0(15,"-",count_files_all[[i]]$month, "-", count_files_all[[i]]$year)
  count_files_all[[i]]$date<-as.Date(mydates, "%d-%m-%y")
  
  plot(mycounts, xaxt="n", xlab="", ylab="rate per 1000 persons", main=main_name, lwd=2, cex.main=1.5)
  tsp = attributes(mycounts)$tsp
  dates = seq(as.Date("2009-01-01"), by = "month", along = mycounts)
  axis(1, at = seq(tsp[1], tsp[2], along = mycounts),las=2, labels = format(dates, "%Y-%m"))
  dev.off()
  
}


