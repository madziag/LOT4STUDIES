#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 31/01/2022

#in the case that data is divded by region, this script generates
# aggregated plots per outcome with all regions to analyze the source of trends

#the second section produces pooled plots per outcome

# Set path to
All_regions_dir<-paste0(projectFolder, "/ALL_regions/ALL_regions/")
#Plot directory to deposit loop output
invisible(ifelse(!dir.exists(paste0(All_regions_dir,"/plots/")), dir.create(paste0(All_regions_dir,"plots/")), FALSE))
bifap_plots<-paste0(All_regions_dir,"plots/")


# Creates a region color key
my_regions<-c("AR", "AS","CA", "CL","CM","CN","MA","MU","NA")
my_pallette<-RColorBrewer::brewer.pal(n =length(my_regions), name="Set1" )
#bind colors to regions so that the same color will represent each region in all plots, even when not all regions are present
region_key<-as.data.frame(cbind(my_pallette, my_regions))

##########################################################################################################################
################################################### PLOTS FOR RATES ######################################################
##########################################################################################################################
#For preliminary counts, prevalence, incidence, med_use_during_pregnancy, preg_starts 
my_folders<- list.files(All_regions_dir, pattern="counts")
# RATES - Multiplied by 1000
# 1. All preliminary 
# 2. Prevalence 
# 3. Incidence 
# 4. med_use_during_pregnancy
# 5. Preg_starts
my_folders_rates <- my_folders[!grepl(c("all_pregnancies|discontinued|switched|pgtests_prior|pgtests_after|contraception_prior|med_use_during_contraception_episodes"), my_folders)]
# PROPORTIONS 
# 1. Discontinued
# 2. switched 
# 3. pg test prior 
# 4. pg test after 
# 5. contraception prior 
# 6. med_use_during_contraception 
my_folders_props <- my_folders[grepl(c("discontinued|switched|pgtests_prior|pgtests_after|contraception_prior|med_use_during_contraception_episodes"), my_folders)]

#create ylim max from pooled file

# Combined plots 
for(i in 1:length(my_folders_rates)){
  #read in list of RDS files
  my_files<-grep(list.files(path=paste0(All_regions_dir,my_folders_rates[i])), pattern='Pooled', invert=TRUE, value=TRUE)
  pool_file<-list.files(path=paste0(All_regions_dir,my_folders_rates[i]), pattern='counts_Pooled')
  my_pool<-fread(paste0(All_regions_dir, my_folders_rates[i],"/", pool_file ))
  my_max<- (max(my_pool$rates))*2
  if (length(my_files)>0){
    main_name<-paste0("Each_region_rate_", substr(my_folders_rates[i], 1,nchar(my_folders_rates[i])-7))
    pdf((paste0(bifap_plots,main_name,".pdf")), width=8, height=4)
    plot(x=1:nrow(my_pool),y=rep(0,nrow(my_pool)),ylim=c(0,my_max), main=main_name,type ="n",xaxt="n",xlab="",ylab="rates")
    axis(1, at=1:nrow(my_pool), as.character(my_pool$YM), las=2)
    legend("topright", legend = my_regions, col=my_pallette, lwd=2, bty="n", cex=0.75)
    legend("topleft", legend=c("true values", "no observations"), bty="n",pch=c(16,3))
    for(j in 1:length(my_files)){
      my_data<-fread(paste0(All_regions_dir,my_folders_rates[i],"/",my_files[1]))
      my_data[!is.finite(my_data$rates),]$rates <- 0  #Set NA/inf rate values to 0
      my_reg<-substr(my_files[j],1,2)
      my_col<-as.character(region_key$my_pallette[region_key$my_regions==my_reg])
      #jitterred lines so that overlapping regions are distinguishable
      my_jit<-sample(x=(seq(0,0.001, by=0.00001)), size = 1)
      lines(x=(1:nrow(my_data)), y=((my_data$rates)+my_jit), pch=my_data$true_value, type="b", col=my_col, lwd=2)
    }
    dev.off()
  }
}

#Pooled plots for each output- one pooled file per output folder
for(i in 1:length(my_folders_rates)){
  if (str_detect(my_folders_rates[i], "_preg_starts_during_tx_episodes")){
   my_files<-grep(list.files(path=paste0(All_regions_dir,my_folders_rates[i])), pattern='final_Pooled', invert=FALSE, value=TRUE)
  } else {
   my_files<-grep(list.files(path=paste0(All_regions_dir,my_folders_rates[i])), pattern='counts_Pooled', invert=FALSE, value=TRUE)
  }
  my_data<-my_data<-fread(paste0(All_regions_dir,my_folders_rates[i],"/",my_files))
  my_data[!is.finite(my_data$rates),]$rates <- 0  #Set NA/inf rate values to 0
  my_max <- (max(my_data$rates))*1.1
  main_name<-paste0("Pooled_rate_",substr(my_folders_rates[i], 1,nchar(my_folders_rates[i])-7))
  pdf((paste0(bifap_plots, main_name,".pdf")), width=8, height=4)
  plot(x=1:nrow(my_data),y=my_data$rates, ylim=c(0,my_max), main=main_name, type = "n",xaxt="n", xlab="", ylab="rates")
  lines(x=(1:nrow(my_data)), y=my_data$rates, lwd=2)
  axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
  dev.off()
}

##########################################################################################################################
############################################## PLOTS FOR PROPORTIONS ######################################################
##########################################################################################################################

# Combined plots 
for(i in 1:length(my_folders_props)){
  #read in list of RDS files
  my_files <-grep(list.files(path=paste0(All_regions_dir,my_folders_props[i])), pattern='Pooled', invert=TRUE, value=TRUE)
  pool_file<-list.files(path=paste0(All_regions_dir,my_folders_props[i]), pattern='counts_Pooled')
  my_pool<-fread(paste0(All_regions_dir, my_folders_props[i],"/", pool_file ))
  my_max<- (max(my_pool$rates))*2
  if (length(my_files)>0){
    main_name<-paste0("Each_region_prop_",substr(my_folders_props[i], 1,nchar(my_folders_props[i])-7))
    pdf((paste0(bifap_plots, main_name,".pdf")), width=8, height=4)
    plot(x=1:nrow(my_pool),y=rep(1, nrow(my_pool)), ylim=c(0,1), main=main_name, type = "n",xaxt="n", xlab="", ylab="proportions")
    axis(1, at=1:nrow(my_date_df), as.character(my_pool$YM), las=2)
    legend("topright", legend = my_regions, col=my_pallette, lwd=2, bty="n", cex=0.75)
    legend("topleft", legend=c("true values", "no observations"), bty="n",pch=c(16,3))
    for(j in 1:length(my_files)){
      my_data<-fread(paste0(All_regions_dir,my_folders_props[i],"/",my_files[j]))
      my_data[!is.finite(my_data$rates),]$rates <- 0  #Set NA/inf rate values to 0
      my_reg<-substr(my_files[j],1,2)
      my_col<-as.character(region_key$my_pallette[region_key$my_regions==my_reg])
      #jitterred lines so that overlapping regions are distinguishable
      my_jit<-sample(x=(seq(0,0.001, by=0.00001)), size = 1)
      lines(x=(1:nrow(my_data)), y=((my_data$rates)+my_jit), type="b", pch=my_data$true_value, col=my_col, lwd=2)
    }
    dev.off()
  }
}

#Pooled plots for each output- one pooled file per output folder
for(i in 1:length(my_folders_props)){
  my_files<-grep(list.files(path=paste0(All_regions_dir,my_folders_props[i])), pattern='final_Pooled', invert=FALSE, value=TRUE)
  my_data<-my_data<-fread(paste0(All_regions_dir,my_folders_props[i],"/",my_files))
  my_data[!is.finite(my_data$rates),]$rates <- 0  #Set NA/inf rate values to 0
  main_name<-paste0("Pooled ",substr(my_folders_props[i], 1,nchar(my_folders_props[i])-7))
  pdf((paste0(bifap_plots, main_name,".pdf")), width=8, height=4)
  plot(x=1:nrow(my_date_df),y=my_data$rates, main=main_name, type = "n",xaxt="n", xlab="", ylab="proportions", ylim = c(0,1))
  lines(x=(1:nrow(my_data)), y=my_data$rates, lwd=2)
  axis(1, at=1:nrow(my_date_df), as.character(my_dates), las=2)
  dev.off()
}




