#Author: Magdalena Gamba M.D.,Ema Alsina MSc.
#email: m.a.gamba@uu.nl, e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 20/12/2021


# this script counts the number of elligible participants per month (as persons may leave and enter the database)

if (is_Denmark == TRUE){
  # Loads customized denominator
  FUmonths_df <- fread(paste0(projectFolder, "/p_param/denominator.csv"), quote = "")
  # Saves file where it will be accessed by following scripts
  saveRDS(data.table(FUmonths_df), paste0(output_dir, pop_prefix, "_denominator.rds"))
  # plots denominator 
  pdf((paste0(output_dir, "plots/", pop_prefix ,"denominator.pdf")), width=8, height=4)
  plot(FUmonths_df$studyFUmonths, FUmonths_df$Freq, ylab="Persons Observed per Month", xlab="Year and Month")
  invisible(dev.off())
} else {
  
  # Sets start and end dates 
  start.date<-as.Date(study_population$entry_date)
  end.date<-as.Date(study_population$exit_date)
  # Converts to yearmon
  ym1 <- as.yearmon(as.character(start.date), "%Y-%m-%d") 
  ym2 <- as.yearmon(as.character(end.date), "%Y-%m-%d") 
  # Gets seq of months between start dates and end dates in a list 
  FUmonths<-list()
  for (i in 1:length(ym1)){
    s <- seq(ym1[i], ym2[i], (1/12)) # creates yearmon sequence
    s <-as.numeric(format(s, "%Y%m"))
    FUmonths[[i]]<-s
  }
  FUmonths<-unlist(FUmonths)
  if(is_PHARMO) {studyFUmonths<-FUmonths[(FUmonths>=200901)&(FUmonths<=201912)]} else {studyFUmonths<-FUmonths[(FUmonths>=200901)&(FUmonths<=202012)]}
  FUmonths_df<-as.data.frame(table(studyFUmonths))
  FUmonths_df$YM<-as.Date(paste0(as.character(FUmonths_df$studyFUmonths),"01"), format="%Y%m%d")
  FUmonths_df$YM<-format(as.Date(FUmonths_df$YM),"%Y-%m")
  # Saves file
  saveRDS(data.table(FUmonths_df), paste0(output_dir, pop_prefix, "_denominator.rds"))
  # plots denominator 
  pdf((paste0(output_dir, "plots/", pop_prefix ,"denominator.pdf")), width=8, height=4)
  plot(FUmonths_df$studyFUmonths, FUmonths_df$Freq, ylab="Persons Observed per Month", xlab="Year and Month")
  invisible(dev.off())
}

