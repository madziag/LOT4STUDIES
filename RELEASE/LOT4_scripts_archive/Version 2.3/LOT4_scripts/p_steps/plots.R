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

monthly_counts_folders <- list.files(path = output_dir, pattern= "monthly_counts")

count_names_all <- list()
count_files_all <- list()

for (folder in 1:length(monthly_counts_folders)){
  #extract count files
  if(length(list.files(paste0(output_dir, monthly_counts_folders[folder], "/"), pattern="count")) > 0){
    
    count_names<-list.files(paste0(output_dir, monthly_counts_folders[folder]), pattern = "counts")
    count_files<-lapply(paste0(output_dir, monthly_counts_folders[folder],"/", count_names), readRDS)
    count_names_all[[folder]] <- count_names
    count_files_all[[folder]] <- count_files
  }
}

# Remove empty lists
count_names_all <- count_names_all[lapply(count_names_all,length)>0]
count_files_all <- count_files_all[lapply(count_files_all,length)>0]

# calculate rates
for (i in 1:length(count_files_all)){
  
  for (j  in 1: length(count_files_all[[i]])){
    
    var_counts<-denominator$Freq
    if(mask==T){var_counts[(0<var_counts)&(var_counts<=5)]<-5}
    count_files_all[[i]][[j]]$rate<-(count_files_all[[i]][[j]]$N)/var_counts
    count_files_all[[i]][[j]]$ratep1000<-round((count_files_all[[i]][[j]]$rate*1000),2)
  }
}



for (i in 1:length(count_files_all)){
  
  for (j  in 1: length(count_files_all[[i]])){
    
    main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
    
    pdf((paste0(plot_folder,"/", main_name, ".pdf")), width=8, height=4)
    
    var_counts<-count_files_all[[i]][[j]]$N
    if(mask==T){var_counts[(0<var_counts)&(var_counts<=5)]<-5}
    
    mycounts<-ts(var_counts, frequency = 12, start = 2009,end = 2021)
    mydates<-paste0(15,"-",count_files_all[[i]][[j]]$month, "-", count_files_all[[i]][[j]]$year)
    count_files_all[[i]][[j]]$date<-as.Date(mydates, "%d-%m-%y")
    
    plot(mycounts, xaxt="n", yaxt="n", xlab="", ylab="counts", main=main_name, lwd=2, cex.main=1.5)
    tsp = attributes(mycounts)$tsp
    dates = seq(as.Date("2009-01-01"), by = "month", along = mycounts)
    axis(1, at = seq(tsp[1], tsp[2], along = mycounts),las=2, labels = format(dates, "%Y-%m"))
    axis(2, seq(0:(max(count_files_all[[i]][[j]]$N)+1)))
    dev.off()
    
  }
}




for (i in 1:length(count_files_all)){
  
  for (j  in 1: length(count_files_all[[i]])){
    
    main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
    
    pdf((paste0(plot_folder,"/", main_name, "_rate.pdf")), width=8, height=4)
    
    var_counts<-count_files_all[[i]][[j]]$ratep1000
    
    mycounts<-ts(var_counts, frequency = 12, start = 2009,end = 2021)
    mydates<-paste0(15,"-",count_files_all[[i]][[j]]$month, "-", count_files_all[[i]][[j]]$year)
    count_files_all[[i]][[j]]$date<-as.Date(mydates, "%d-%m-%y")
    
    plot(mycounts, xaxt="n", xlab="", ylab="rate per 1000 persons", main=main_name, lwd=2, cex.main=1.5)
    tsp = attributes(mycounts)$tsp
    dates = seq(as.Date("2009-01-01"), by = "month", along = mycounts)
    axis(1, at = seq(tsp[1], tsp[2], along = mycounts),las=2, labels = format(dates, "%Y-%m"))
    dev.off()
    
  }
  
}
