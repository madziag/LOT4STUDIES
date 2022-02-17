#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 31/01/2022

#in the case that data is divded by region, this script generates
# aggregated plots per outcome with all regions to analyze the source of trends

#the second section produces pooled plots per outcome


if(!require(viridis)){install.packages("viridis")}
suppressPackageStartupMessages(library(viridis))

if(!require(RColorBrewer)){install.packages("RColorBrewer")}
suppressPackageStartupMessages(library(RColorBrewer))

## Path to ALL_regions folder already set -> All_regions_dir

my_folders<- list.files(All_regions_dir, pattern="counts")
# Excludes pregnancy counts (there are no rates here )
my_folders<- my_folders[!grepl("_Pregnancy_ALL_counts", my_folders) ]

#plot directory to deposit loop output
invisible(ifelse(!dir.exists(paste0(All_regions_dir,"/plots/")), dir.create(paste0(All_regions_dir,"/plots/")), FALSE))
bifap_plots<- paste0(All_regions_dir,"/plots/")

#get dates vector

denominator <- (list.files(paste0(All_regions_dir,"/ALL_denominator/"), pattern = "denominator_Pooled.csv", recursive = TRUE, ignore.case = T))

my_date_df<-fread(paste0(All_regions_dir,"/ALL_denominator/", denominator))

my_dates<-my_date_df$YM

# Get a dynamic list of region names 
# my_regions <- list.dirs(path = multiple_regions_dir, full.names = FALSE, recursive = FALSE)
my_regions<-c("AR", "AS","CA", "CL","CM","CN","MA","MU","NA")

my_pallette<-RColorBrewer::brewer.pal(n =length(my_regions), name="Set1" )

#bind colors to regions so that the same color will represent each region in all plots, even when not all regions are present
region_key<-as.data.frame(cbind(my_pallette, my_regions))

for(i in 1:length(my_folders)){
  #read in list of RDS files
  my_files<-grep(list.files(path=paste0(All_regions_dir,"/",my_folders[i])), pattern='Pooled', invert=TRUE, value=TRUE)
  my_files <- my_files[!grepl(c("all_pregnancies|discontinued|switched|contraception_prior|med_use_during_pregnancy"), my_files)]
  if (length(my_files)>0){
    main_name<-substr(my_folders[i], 1,nchar(my_folders[i])-7)
    pdf((paste0(bifap_plots, main_name,".pdf")), width=8, height=4)
    plot(x=1:nrow(my_date_df),y=rep(1, nrow(my_date_df)), ylim=c(0, 2), main=main_name, type = "n",xaxt="n", xlab="", ylab="rates", ylim = c(0,0.03))
    axis(1, at=1:nrow(my_date_df), as.character(my_dates), las=2)
    legend("topright", legend = my_regions, col=my_pallette, lwd=2, bty="n", cex=0.75)
    for(j in 1:length(my_files)){
      my_data<-fread(paste0(All_regions_dir,"/",my_folders[i],"/",my_files[j]))
      my_reg<-substr(my_files[j],1,2)
      my_col<-as.character(region_key$my_pallette[region_key$my_regions==my_reg])
      #jitterred lines so that overlapping regions are distinguishable
      my_jit<-sample(x=(seq(0,0.001, by=0.00001)), size = 1)
      lines(x=(1:nrow(my_data)), y=((my_data$rates)+my_jit), col=my_col, lwd=2)
    }
    dev.off()
  }
}



#pooled plots for each output- one pooled file per output folder
my_folders<- list.files(All_regions_dir, pattern="counts")
my_folders<- my_folders[!grepl(c("all_pregnancies|discontinued|switched|contraception_prior|med_use_during_pregnancy"), my_folders)]

for(i in 1:length(my_folders)){
  my_files<-grep(list.files(path=paste0(All_regions_dir,"/",my_folders[i])), pattern='counts_Pooled', invert=FALSE, value=TRUE)
  my_data<-my_data<-fread(paste0(All_regions_dir,"/",my_folders[i],"/",my_files))
  main_name<-paste0("Pooled ",substr(my_folders[i], 1,nchar(my_folders[i])-7))
  pdf((paste0(bifap_plots, main_name,".pdf")), width=8, height=4)
  plot(x=1:nrow(my_date_df),y=my_data$rates, ylim=c(0,max(my_data$rates)), main=main_name, type = "n",xaxt="n", xlab="", ylab="rates", ylim = c(0,0.03))
  lines(x=(1:nrow(my_data)), y=my_data$rates, lwd=2)
  axis(1, at=1:nrow(my_date_df), as.character(my_dates), las=2)
  dev.off()
}


# Proportions: discontinued, switched_to_alt_meds, contraception_prior, med_use_during_pregnancy

for(i in 1:length(my_folders)){
  #read in list of RDS files
  my_files<-grep(list.files(path=paste0(All_regions_dir,"/",my_folders[i])), pattern='Pooled', invert=TRUE, value=TRUE)
  my_files <- my_files[grepl(c("all_pregnancies|discontinued|switched|contraception_prior|med_use_during_pregnancy"), my_files)]
  my_files <- my_files[!grepl(c("all_pregnancies"), my_files)]
  if (length(my_files)>0){
    main_name<-substr(my_folders[i], 1,nchar(my_folders[i])-7)
    pdf((paste0(bifap_plots, main_name,".pdf")), width=8, height=4)
    plot(x=1:nrow(my_date_df),y=rep(1, nrow(my_date_df)), ylim=c(0,2), main=main_name, type = "n",xaxt="n", xlab="", ylab="proportions", ylim = c(0,1))
    axis(1, at=1:nrow(my_date_df), as.character(my_dates), las=2)
    legend("topright", legend = my_regions, col=my_pallette, lwd=2, bty="n", cex=0.75)
    for(j in 1:length(my_files)){
      my_data<-fread(paste0(All_regions_dir,"/",my_folders[i],"/",my_files[j]))
      my_reg<-substr(my_files[j],1,2)
      my_col<-as.character(region_key$my_pallette[region_key$my_regions==my_reg])
      #jitterred lines so that overlapping regions are distinguishable
      my_jit<-sample(x=(seq(0,0.001, by=0.00001)), size = 1)
      lines(x=(1:nrow(my_data)), y=((my_data$rates)+my_jit), col=my_col, lwd=2)
    }
    dev.off()
  }
}



#pooled plots for each output- one pooled file per output folder
my_folders<- list.files(All_regions_dir, pattern="counts")
my_folders<- my_folders[grepl(c("all_pregnancies|discontinued|switched|contraception_prior|med_use_during_pregnancy"), my_folders)]
my_folders<- my_folders[!grepl(c("all_pregnancies"), my_folders)]

for(i in 1:length(my_folders)){
  my_files<-grep(list.files(path=paste0(All_regions_dir,"/",my_folders[i])), pattern='counts_Pooled', invert=FALSE, value=TRUE)
  my_data<-my_data<-fread(paste0(All_regions_dir,"/",my_folders[i],"/",my_files))
  main_name<-paste0("Pooled ",substr(my_folders[i], 1,nchar(my_folders[i])-7))
  pdf((paste0(bifap_plots, main_name,".pdf")), width=8, height=4)
  plot(x=1:nrow(my_date_df),y=my_data$rates,ylim=c(0,max(my_data$rates)), main=main_name, type = "n",xaxt="n", xlab="", ylab="proportions", ylim = c(0,1))
  lines(x=(1:nrow(my_data)), y=my_data$rates, lwd=2)
  axis(1, at=1:nrow(my_date_df), as.character(my_dates), las=2)
  dev.off()
}


