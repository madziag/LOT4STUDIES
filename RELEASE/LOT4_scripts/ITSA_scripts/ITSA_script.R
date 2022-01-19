# Packages 
if(!require(tidyr)){install.packages("tidyr")}
suppressPackageStartupMessages(library(tidyr))
if(!require(lmtest)){install.packages("lmtest")}
suppressPackageStartupMessages(library(lmtest))
if(!require(car)){install.packages("car")}
suppressPackageStartupMessages(library(car))
if(!require(nlme)){install.packages("nlme")}
suppressPackageStartupMessages(library(nlme))

# Create Folder for modified monthly counts records 
invisible(ifelse(!dir.exists(paste0(output_dir, "count_files_for_itsa")), dir.create(paste0(output_dir, "count_files_for_itsa")), FALSE))
monthly_counts_itsa <- paste0(output_dir, "count_files_for_itsa")
# Create Folder for itsa plots
invisible(ifelse(!dir.exists(paste0(output_dir, "itsa_plots")), dir.create(paste0(output_dir, "itsa_plots")), FALSE))
itsa_plots <- paste0(output_dir, "itsa_plots")

# CHOOSE REGION
########################
# region <- "UK"
# region <- "Denmark"
# region <- "Spain"
# region <- "Italy"
region <- "Netherlands"

# Find all folders with monthly counts
monthly_counts_folders <- list.files(path = output_dir, pattern = 'monthly_counts')
# Assign policy intervention date based on region
if (exists('region')){
  policy_int <- ifelse(region == 'UK', '2018-04',
                       ifelse(region == "Denmark" | region == "Spain", '2018-07',
                              ifelse(region == "Italy", '2018-08',
                                     ifelse(region == 'Netherlands', '2018-10', NA ))))
} else {
  print("Please enter name of your region")
}


if (exists('region')){
  # For each folder with monthly counts, read in all files in the folder and do the changes below 
  for(folder in monthly_counts_folders){
    mc_folder_list <- list.files(path = paste0(output_dir, folder, "/"))
    # for each file in each folder of the monthly counts: 
    for(file in mc_folder_list){
      mc_df <- as.data.table(readRDS(paste0(output_dir, folder, "/", file)))
      mc_df$level <-  ifelse(mc_df$YM >= policy_int, 1, 0) 
      # level: 0,1. It starts at 0 but switches to 1 at a specified month, depending on country (this is the month when the policy intervention about retinoids/valproates use began in the country - see below for details)  mc_df_0 <- mc_df[mc_df$level == 0]
      mc_df_0 <- mc_df[mc_df$level == 0]
      mc_df_1 <- mc_df[mc_df$level == 1]
      # trend: 0:n. It starts at zero and switches to 1 as above. However, every month afterwards, the value should be n+1 until the final date (see example output data)
      mc_df_0$trend <- 0
      mc_df_1$trend <-  1:nrow(mc_df_1)
      mc_df <- rbind(mc_df_0, mc_df_1)
      # Year: YYYY, starting at the first year of count output and ending at the end year
      # Month: MM, starting at the first month of count output (01) and ending at the last month
      mc_df_cols <- extract(mc_df, YM, into = c("Year", "Month"), "([0-9]+)-([0-9]+)")
      # incidence: == "rates" from the input data frame
      mc_df_cols <- setnames(mc_df_cols,"rates","incidence_val") # Rename column names
      # time: 1:length(rates). Starts at 1 and continues up the the number of rows in the output table.
      mc_df_cols$time <- 1:nrow(mc_df_cols)
      mc_df_cols <- mc_df_cols[,c("Year", "Month", "incidence_val", "level", "trend", "time")]
      # Save files 
      if (SUBP == TRUE){
        saveRDS(mc_df_cols, paste0(monthly_counts_itsa, "/", population[pop],"_", file))
      } else {
        saveRDS(mc_df_cols, paste0(monthly_counts_itsa, "/", file))
      }
      # Clean Up
      rm(mc_df_0, mc_df_1)
    }
    
  }
}

if (exists('region')){
  # List all amended monthly count files 
  itsa_files <- list.files(path = monthly_counts_itsa)
  
  for (i in itsa_files){
    data <- as.data.table(readRDS(paste0(monthly_counts_itsa, "/", i)))
    
    ## Initial Plot##
    
    # Plot outcome variable versus time
    plot(data$time,data$incidence_val,
         ylab="Incident valproate users",
         ylim=c(0,1),
         xlab="Year",
         type="l",
         col="red",
         xaxt="n")
    
    # Add x-axis year labels
    axis(1, at=1:132, labels=data$Month)
    
    # Add in the points for the figure
    points(data$time,data$incidence_val,
           col="red",
           pch=20)
    
    #Label the intervention#
    abline(v=103.5,lty=2)
    
    #Phase-in#
    include<-c(1:103,109:132)
    data_pi<-data[include,]
    
    ##Modelling-preliminary OLS regression##
    
    model_ols <- lm(incidence_val~time + level + trend,data = data_pi)
    summary(model_ols)
    confint(model_ols)
    
    #Autocorrelation#
    #Durbin-watson test#
    library(lmtest)
    durbinWatsonTest(model_ols,max.lag=12, alternative="two.sided")
    
    #Autocorrelation plots#
    par(mfrow=c(2,1))
    acf(residuals(model_ols))
    acf(residuals(model_ols),type="partial")
    
    #Final model#
    model_final<-gls(incidence_val~time+level+trend,data = data_pi)
    summary(model_final)
    Confint(model_final)
    
    #Checking the model#
    model_q1<-update(model_final,correlation=corARMA(q=1,form = ~time))
    anova(model_final,model_q1)
    
    #Plot results#
    plot(data$time,data$incidence_val,
         ylim=c(0,0.8),
         ylab="Incidence users of oral valproates",
         xlab="Month",
         pch=20,
         col="pink",
         xaxt="n")
    axis(1, at=1:132, labels=data$Month)
    abline(v=103.5,lty=2)
    
    # Plot the first line segment#
    lines(data$time[1:103], fitted(model_final)[1:103], col="red",lwd=2)
    # Plot the second line segment
    lines(data$time[109:137], fitted(model_final)[104:132], col="red",lwd=2)
    
    # And the counterfactual#
    segments(109,
             model_final$coef[1]+model_final$coef[2]*109,
             132,
             model_final$coef[1]+model_final$coef[2]*132,
             lty=2,
             lwd=2,
             col='red')
    
    rect(103.5,-2,108.5,2,border = NA, col = "#00000011")
    
  }
  
}




