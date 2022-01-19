#generate lineplots of counts of variables
#monthly counts by year
#total counts over whole study period

#plot groups of variables on one plot

# count files already masked to_run_study_population

# find correct folders
# 
# if(!require(rstudioapi)){install.packages("rstudioapi")}
# library(rstudioapi)
# 
# projectFolder<-dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(projectFolder)

#get denominators from denominator.r in p_steps
# source("denominator.r") ran in to_run_source_population
#this also creates base_folder 

#plot_folder created in 99_path

#extract denominators

denominator<-readRDS(paste0(output_dir,"denominator.rds"))

pattern1 = c("monthly_counts", "pregnancy_counts")
# monthly_counts_folders <- list.files(path = output_dir, pattern= "monthly_counts")
monthly_counts_folders <- list.files(path = output_dir, pattern=paste0(pattern1, collapse="|"))

# monthly_counts_folders <- list.files(path = output_dir, pattern= "monthly_counts")

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

# # calculate rates
# for (i in 1:length(count_files_all)){
#   
#   for (j  in 1: length(count_files_all[[i]])){
#     
#     fu_months<-denominator$Freq
#     
#     count_files_all[[i]][[j]]$rate<-(count_files_all[[i]][[j]]$N)/fu_months
#     count_files_all[[i]][[j]]$ratep1000<-round((count_files_all[[i]][[j]]$rate*1000),2)
#   }
# }



for (i in 1:length(count_files_all)){
  
  for (j  in 1: length(count_files_all[[i]])){
    
    main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
    
    pdf((paste0(plot_folder,"/", main_name, ".pdf")), width=8, height=4)
    
    my_data<-as.data.frame(count_files_all[[i]][[j]])
    
    #indicate masked values with stars
    my_pch<-count_files_all[[i]][[j]]$masked
    my_pch[my_pch==0]<-16
    my_pch[my_pch==1]<-8
    
    
    plot(x=1:nrow(my_data), y=my_data$N, xaxt="n", yaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
    
    
    axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
    axis(2, seq(0:(max(count_files_all[[1]][[1]]$N)+1)))
    dev.off()
    
  }
}




for (i in 1:length(count_files_all)){
  
  for (j  in 1: length(count_files_all[[i]])){
    
    main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
    
    pdf((paste0(plot_folder,"/", main_name, "_rate.pdf")), width=8, height=4)
    
    my_data<-count_files_all[[i]][[j]]
    
    #indicate masked values with stars
    my_pch<-my_data$masked
    my_pch[my_pch==0]<-16
    my_pch[my_pch==1]<-8
    
    
    plot(x=1:nrow(my_data), y=my_data$rates, xaxt="n",type="b", xlab="", ylab="rate per 1000 persons", main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
    axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
    dev.off()
    
  }
  
}
