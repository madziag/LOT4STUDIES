#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 20/01/2022

# Generates lineplots of counts of variables
# monthly counts by year
# total counts over whole study period
# plot groups of variables on one plot
# count files already masked to_run_study_population

##################################################################
##################################################################
############### PRELIMINARY COUNTS: RATES ########################
##################################################################
##################################################################
monthly_counts_folders<-list.files(path = output_dir, pattern = "monthly_counts")
if(length(monthly_counts_folders)>0){
  # Creates lists 
  count_names_all<-list()
  count_files_all<-list()
  # Extracts counts files
  for (folder in 1:length(monthly_counts_folders)){
    if(length(list.files(paste0(output_dir, monthly_counts_folders[folder], "/"), pattern="count")) > 0){
      count_names<-list.files(paste0(output_dir, monthly_counts_folders[folder]), pattern = "count")
      count_files<-lapply(paste0(output_dir, monthly_counts_folders[folder],"/", count_names), readRDS)
      count_names_all[[folder]]<-count_names
      count_files_all[[folder]]<-count_files
    }
  }
  # Removes empty lists
  count_names_all<-count_names_all[lapply(count_names_all,length)>0]
  count_files_all<-count_files_all[lapply(count_files_all,length)>0]
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
        my_ymax<-ifelse(max(my_data$N) < 1, 1, max(my_data$N))
        plot(x=1:nrow(my_data), y=my_data$N, ylim=c(0,my_ymax), xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
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
        
        #Set NA/inf rate values to 0
        my_data[!is.finite(my_data$rates),]$rates<-0
        
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
        # Makes plots 
        if(str_detect(main_name, "altmed")){ylab_prelim<-"Number presc/disp per 1000 pm"} else {ylab_prelim<-"nr. records/1000 person-months"}
        plot(x=1:nrow(my_data), y=my_data$rates, ylim=c(0,my_ymax), xaxt="n",type="b", xlab="", ylab=ylab_prelim, main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
  } else {
    print("There are no files to plot")
  }
}
##################################################################
##################################################################
##################### FINAL COUNTS: RATES  #######################
##################################################################
##################################################################
final_counts_rates_folders<-list.files(path = output_dir, pattern = "medicines_count|pregnancy_counts")
if(length(final_counts_rates_folders)>0){
  # Creates lists
  count_names_all<-list()
  count_files_all<-list()
  # Extracts counts files
  for (folder in 1:length(final_counts_rates_folders)){
    if(length(list.files(paste0(output_dir, final_counts_rates_folders[folder], "/"), pattern="count")) > 0){
      count_names<-list.files(paste0(output_dir, final_counts_rates_folders[folder]), pattern = "count")
      # Removes files that calculate proportions
      count_names<-count_names[!grepl("discontinue",count_names)]
      count_names<-count_names[!grepl("switch",count_names)]
      count_files<-lapply(paste0(output_dir, final_counts_rates_folders[folder],"/", count_names), readRDS)
      count_names_all[[folder]]<-count_names
      count_files_all[[folder]]<-count_files
    }
  }
  # Removes empty lists
  count_names_all<-count_names_all[lapply(count_names_all,length)>0]
  count_files_all<-count_files_all[lapply(count_files_all,length)>0]
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
        my_ymax<-ifelse (max(my_data$N) < 1, 1, max(my_data$N))
        # Makes plots 
        plot(x=1:nrow(my_data), y=my_data$N, ylim=c(0,my_ymax), xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
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
          
          #Set NA/inf rate values to 0
          my_data[!is.finite(my_data$rates),]$rates<-0
          
          #indicate masked values with stars
          my_pch<-my_data$masked
          my_pch[my_pch==0]<-16
          my_pch[my_pch==1]<-8
          my_ymax<-ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
          # Assigns ylab name
          ylab_props<-""
          if(str_detect(main_name, "prevalence")){ylab_rates<-"Number current users per 1000 pm"}
          if(str_detect(main_name, "incidence")){ylab_rates<-"Number new users per 1000 pm"}
          if(str_detect(main_name, "med_use_during_pregnancy")){ylab_rates<-"Number presc/disp during pregnancy per 1000 pm"}
          if(str_detect(main_name, "preg_start")){ylab_rates<-"Number pregnancies during exposure per 1000 pm"}
          if(str_detect(main_name, "alt_med")){ylab_rates<-"nr. records/1000 person-months"}
          
          # Makes plots
          plot(x=1:nrow(my_data), y=my_data$rates,ylim=c(0,my_ymax), xaxt="n",type="b", xlab="", ylab= ylab_rates, main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
          axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
          dev.off()
        }
      }
    }
  } else {
    print("There are no files to plot")
  }
}
##################################################################
##################################################################
##################### FINAL COUNTS: PROPORTIONS  #################
##################################################################
##################################################################
final_counts_props_folders<-list.files(path = output_dir, pattern = "contraceptive_counts|medicines_counts|pregnancy_test_counts")
if(length(final_counts_props_folders)>0){
  # Creates lists
  count_names_all<-list()
  count_files_all<-list()
  # Extracts counts files
  for (folder in 1:length(final_counts_props_folders)){
    if(length(list.files(paste0(output_dir, final_counts_props_folders[folder], "/"), pattern="count")) > 0){
      count_names<-list.files(paste0(output_dir, final_counts_props_folders[folder]), pattern = "count")
      count_names<-count_names[!grepl("prevalence",count_names)]
      count_names<-count_names[!grepl("incidence",count_names)]
      count_files<-lapply(paste0(output_dir, final_counts_props_folders[folder],"/", count_names), readRDS)
      count_names_all[[folder]]<-count_names
      count_files_all[[folder]]<-count_files
    }
  }
  # Removes empty lists
  count_names_all<-count_names_all[lapply(count_names_all,length)>0]
  count_files_all<-count_files_all[lapply(count_files_all,length)>0]
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
        my_ymax<-ifelse (max(my_data$N) < 1, 1, max(my_data$N))
        # Makes plots 
        plot(x=1:nrow(my_data), y=my_data$N,ylim=c(0,max(my_data$N)), xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
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
        
        #Set NA/inf rate values to 0
        my_data[!is.finite(my_data$rates),]$rates<-0
        
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
        # Assigns ylab name
        ylab_props<-""
        if(str_detect(main_name, "contraception_prior")){ylab_props<-"Proportion presc/disp. with contraceptive before"}
        if(str_detect(main_name, "med_use_during_contra_episodes")){ylab_props<-"Proportion prec/disp. within contraception period"}
        if(str_detect(main_name, "discontinued")){ylab_props<-"Proportion discontinued"}
        if(str_detect(main_name, "switched")){ylab_props<-"Proportion switched to alternative"}
        if(str_detect(main_name, "before")){ylab_props<-"Proportion presc/disp. with pregnancy test before"}
        if(str_detect(main_name, "after")){ylab_props<-"Proportion presc/disp. with pregnancy test after"}
        # Makes plots
        plot(x=1:nrow(my_data), y=my_data$rates, ylim=c(0,my_ymax),xaxt="n",type="b", xlab="", ylab= ylab_props, main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
  } else {
    print("There are no files to plot")
  }
}



########################## STRATIFIED PLOTS ######################

##################################################################
##################################################################
##################### FINAL COUNTS: PROPORTIONS  #################
##################################################################
##################################################################

final_counts_props_folders_strat<-list.files(path = medicines_stratified_dir, pattern = "ATC|indication|tx_duration|age_group|reason|contra_type")
if(length(final_counts_props_folders_strat)>0){
  # Creates lists
  count_names_all<-list()
  count_files_all<-list()
  # Extracts counts files
  for (folder in 1:length(final_counts_props_folders_strat)){
    if(length(list.files(paste0(medicines_stratified_dir, "/",final_counts_props_folders_strat[folder], "/"), pattern="count")) > 0){
      count_names <-list.files(paste0(medicines_stratified_dir, "/",final_counts_props_folders_strat[folder]), pattern = "count")
      count_names<-count_names[!grepl("prevalence",count_names)]
      count_names<-count_names[!grepl("incidence",count_names)]
      count_files<-lapply(paste0(medicines_stratified_dir, "/",final_counts_props_folders_strat[folder],"/", count_names), readRDS)
      count_names_all[[folder]]<-count_names
      count_files_all[[folder]]<-count_files
    }
  }
  # Removes empty lists
  count_names_all<-count_names_all[lapply(count_names_all,length)>0]
  count_files_all<-count_files_all[lapply(count_files_all,length)>0]
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
        my_ymax<-ifelse (max(my_data$N) < 1, 1, max(my_data$N))
        # Makes plots 
        plot(x=1:nrow(my_data), y=my_data$N,ylim=c(0,max(my_data$N)), xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
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
        
        #Set NA/inf rate values to 0
        my_data[!is.finite(my_data$rates),]$rates<-0
        
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
        # Assigns ylab name
        ylab_props<-""
        if(str_detect(main_name, "contraception_prior")){ylab_props<-"Proportion presc/disp. with contraceptive before"}
        if(str_detect(main_name, "med_use_during_contra_episodes")){ylab_props<-"Proportion prec/disp. within contraception period"}
        if(str_detect(main_name, "discontinued")){ylab_props<-"Proportion discontinued"}
        if(str_detect(main_name, "switched")){ylab_props<-"Proportion switched to alternative"}
        if(str_detect(main_name, "before")){ylab_props<-"Proportion presc/disp. with pregnancy test before"}
        if(str_detect(main_name, "after")){ylab_props<-"Proportion presc/disp. with pregnancy test after"}
        # Makes plots
        plot(x=1:nrow(my_data), y=my_data$rates, ylim=c(0,my_ymax),xaxt="n",type="b", xlab="", ylab= ylab_props, main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
  } else {
    print("There are no files to plot")
  }
}


##################################################################
##################################################################
########### STRATIFIED COUNTS: CONTRACEPTIVES FOLDER  ############
##################################################################
##################################################################

final_counts_props_folders_strat_contra<-list.files(path = contraceptives_stratified_dir, pattern = "indication|age_group")
if(length(final_counts_props_folders_strat_contra)>0){
  # Creates lists
  count_names_all<-list()
  count_files_all<-list()
  # Extracts counts files
  for (folder in 1:length(final_counts_props_folders_strat_contra)){
    if(length(list.files(paste0(contraceptives_stratified_dir, "/",final_counts_props_folders_strat_contra[folder], "/"), pattern="count")) > 0){
      count_names<-list.files(paste0(contraceptives_stratified_dir, "/",final_counts_props_folders_strat_contra[folder]), pattern = "count")
      count_files<-lapply(paste0(contraceptives_stratified_dir, "/",final_counts_props_folders_strat_contra[folder],"/", count_names), readRDS)
      count_names_all[[folder]]<-count_names
      count_files_all[[folder]]<-count_files
    }
  }
  # Removes empty lists
  count_names_all<-count_names_all[lapply(count_names_all,length)>0]
  count_files_all<-count_files_all[lapply(count_files_all,length)>0]
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
        my_ymax<-ifelse (max(my_data$N) < 1, 1, max(my_data$N))
        # Makes plots 
        plot(x=1:nrow(my_data), y=my_data$N,ylim=c(0,max(my_data$N)), xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
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
        
        #Set NA/inf rate values to 0
        my_data[!is.finite(my_data$rates),]$rates<-0
        
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
        # Assigns ylab name
        ylab_props<-""
        if(str_detect(main_name, "contraception_prior")){ylab_props<-"Proportion presc/disp. with contraceptive before"}
        # Makes plots
        plot(x=1:nrow(my_data), y=my_data$rates, ylim=c(0,my_ymax),xaxt="n",type="b", xlab="", ylab= ylab_props, main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
  } else {
    print("There are no files to plot")
  }
}

# Clean up 
rm(list = grep("^count_|my_data", ls(), value = TRUE))
