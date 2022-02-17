#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 20/01/2022

# Generates lineplots of counts of variables
# monthly counts by year
# total counts over whole study period
# plot groups of variables on one plot
# count files already masked to_run_study_population
# Loads denominator file  ### THIS NEEDS TO BE GENERALIZED OTHERWISE IT WILL NOT WORK WITH THE 2 BIFAP SUBPOPULATION DENOMINATORS
# denominator<-readRDS(paste0(output_dir, pop_prefix, "_denominator.rds"))
# Gets lists of files for plotting 

##################################################################
##################################################################
############### PRELIMINARY COUNTS: RATES ########################
##################################################################
##################################################################
monthly_counts_folders <- list.files(path = output_dir, pattern = "monthly_counts")
# Creates lists 
count_names_all <- list()
count_files_all <- list()
# Extracts counts files
for (folder in 1:length(monthly_counts_folders)){
  if(length(list.files(paste0(output_dir, monthly_counts_folders[folder], "/"), pattern="count")) > 0){
    count_names<-list.files(paste0(output_dir, monthly_counts_folders[folder]), pattern = "count")
    count_files<-lapply(paste0(output_dir, monthly_counts_folders[folder],"/", count_names), readRDS)
    count_names_all[[folder]] <- count_names
    count_files_all[[folder]] <- count_files
  }
}
# Removes empty lists
count_names_all <- count_names_all[lapply(count_names_all,length)>0]
count_files_all <- count_files_all[lapply(count_files_all,length)>0]
# Plots counts 
if (length(count_files_all)>0){
  for (i in 1:length(count_files_all)){
    for (j  in 1: length(count_files_all[[i]])){
      # Assigns names to plots 
      main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
      pdf((paste0(plot_folder,"/", main_name, ".pdf")), width=8, height=4)
      my_data<-as.data.frame(count_files_all[[i]][[j]])
      #indicate masked values with stars
      my_pch<-count_files_all[[i]][[j]]$masked
      my_pch[my_pch==0]<-16
      my_pch[my_pch==1]<-8
      # Makes plots 
      plot(x=1:nrow(my_data), y=my_data$N, xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
      axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
      dev.off()
    }
  }
  # Plots Rates 
  for (i in 1:length(count_files_all)){
    for (j  in 1: length(count_files_all[[i]])){
      # Assigns names to plots 
      main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
      pdf((paste0(plot_folder,"/", main_name, "_rate.pdf")), width=8, height=4)
      my_data<-count_files_all[[i]][[j]]
      #indicate masked values with stars
      my_pch<-my_data$masked
      my_pch[my_pch==0]<-16
      my_pch[my_pch==1]<-8
      # Makes plots 
      plot(x=1:nrow(my_data), y=my_data$rates, xaxt="n",type="b", xlab="", ylab="nr. records/1000 person-months", main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
      axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
      dev.off()
    }
  }
} else {
  print("There are no files to plot")
}

##################################################################
##################################################################
##################### FINAL COUNTS: RATES  #######################
##################################################################
##################################################################
final_counts_rates_folders <- list.files(path = output_dir, pattern = "medicines_count|pregnancy_counts")
# Creates lists
count_names_all <- list()
count_files_all <- list()
# Extracts counts files
for (folder in 1:length(final_counts_rates_folders)){
  if(length(list.files(paste0(output_dir, final_counts_rates_folders[folder], "/"), pattern="count")) > 0){
    count_names<-list.files(paste0(output_dir, final_counts_rates_folders[folder]), pattern = "count")
    # Removes files that calculate proportions
    count_names <- count_names[!grepl("discontinue",count_names)]
    count_names <- count_names[!grepl("switch",count_names)]
    count_files<-lapply(paste0(output_dir, final_counts_rates_folders[folder],"/", count_names), readRDS)
    count_names_all[[folder]] <- count_names
    count_files_all[[folder]] <- count_files
  }
}
# Removes empty lists
count_names_all <- count_names_all[lapply(count_names_all,length)>0]
count_files_all <- count_files_all[lapply(count_files_all,length)>0]
# Plots counts 
if (length(count_files_all)>0){
  for (i in 1:length(count_files_all)){
    for (j  in 1: length(count_files_all[[i]])){
      main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
      pdf((paste0(plot_folder,"/", main_name, ".pdf")), width=8, height=4)
      my_data<-as.data.frame(count_files_all[[i]][[j]])
      #indicate masked values with stars
      my_pch<-count_files_all[[i]][[j]]$masked
      my_pch[my_pch==0]<-16
      my_pch[my_pch==1]<-8
      # Makes plots 
      plot(x=1:nrow(my_data), y=my_data$N, xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
      axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
      dev.off()
    }
  }
  # Plots Rates
  for (i in 1:length(count_files_all)){
    for (j  in 1: length(count_files_all[[i]])){
      main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
      if(str_detect(main_name, "all_pregnancies")){
        print(paste0("There are no rates plots for: ", main_name))
      } else {
        pdf((paste0(plot_folder,"/", main_name, "_rate.pdf")), width=8, height=4)
        my_data<-count_files_all[[i]][[j]]
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        # Assigns ylab name
        ylab_props <- ""
        if(str_detect(main_name, "prevalence")){ylab_rates <- "Number current users per 1000 pm"}
        if(str_detect(main_name, "incidence")){ylab_rates <- "Number new users per 1000 pm"}
        if(str_detect(main_name, "med_use_during_pregnancy")){ylab_rates <- "Number presc/disp during pregnancy per 1000 pm"}
        if(str_detect(main_name, "preg_start")){ylab_rates <- "Number pregnancies during exposure per 1000 pm"}
        # Makes plots
        plot(x=1:nrow(my_data), y=my_data$rates, xaxt="n",type="b", xlab="", ylab= ylab_rates, main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
  }
} else {
  print("There are no files to plot")
}

##################################################################
##################################################################
##################### FINAL COUNTS: PROPORTIONS  #################
##################################################################
##################################################################
final_counts_props_folders <- list.files(path = output_dir, pattern = "contraceptive_counts|medicines_counts|pregnancy_test_counts")
# Creates lists
count_names_all <- list()
count_files_all <- list()
# Extracts counts files
for (folder in 1:length(final_counts_props_folders)){
  if(length(list.files(paste0(output_dir, final_counts_props_folders[folder], "/"), pattern="count")) > 0){
    count_names<-list.files(paste0(output_dir, final_counts_props_folders[folder]), pattern = "count")
    count_names <- count_names[!grepl("prevalence",count_names)]
    count_names <- count_names[!grepl("incidence",count_names)]
    count_files<-lapply(paste0(output_dir, final_counts_props_folders[folder],"/", count_names), readRDS)
    count_names_all[[folder]] <- count_names
    count_files_all[[folder]] <- count_files
  }
}
# Removes empty lists
count_names_all <- count_names_all[lapply(count_names_all,length)>0]
count_files_all <- count_files_all[lapply(count_files_all,length)>0]
# Plots counts 
if (length(count_files_all)>0){
  for (i in 1:length(count_files_all)){
    for (j  in 1: length(count_files_all[[i]])){
      main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
      pdf((paste0(plot_folder,"/", main_name, ".pdf")), width=8, height=4)
      my_data<-as.data.frame(count_files_all[[i]][[j]])
      #indicate masked values with stars
      my_pch<-count_files_all[[i]][[j]]$masked
      my_pch[my_pch==0]<-16
      my_pch[my_pch==1]<-8
      # Makes plots 
      plot(x=1:nrow(my_data), y=my_data$N, xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
      axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
      dev.off()
    }
  }
  # Plots Rates
  for (i in 1:length(count_files_all)){
    for (j  in 1: length(count_files_all[[i]])){
      main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
        pdf((paste0(plot_folder,"/", main_name, "_proportion.pdf")), width=8, height=4)
        my_data<-count_files_all[[i]][[j]]
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        # Assigns ylab name
        ylab_props <- ""
        if(str_detect(main_name, "contraception_prior")){ylab_props <- "Proportion presc/disp. with contraceptive before"}
        if(str_detect(main_name, "med_use_during_contra_episodes")){ylab_props <- "Proportion prec/disp. within contraception period"}
        if(str_detect(main_name, "discontinued")){ylab_props <- "Proportion discontinued"}
        if(str_detect(main_name, "switched")){ylab_props <- "Proportion switched to alternative"}
        if(str_detect(main_name, "before")){ylab_props <- "Proportion presc/disp. with pregnancy test before"}
        if(str_detect(main_name, "after")){ylab_props <- "Proportion presc/disp. with pregnancy test after"}
        # Makes plots
        plot(x=1:nrow(my_data), y=my_data$rates, xaxt="n",type="b", xlab="", ylab= ylab_props, main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
    }
  }
} else {
  print("There are no files to plot")
}






