#Author: Ema Alsina MSc.
#email: e.m.alsina-2@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 25/01/2022

# this script will differentiate between censored CMAs (study ended) and terminated CMAs
#before and after the 2018 intervention date (June 2018) 

#load flexibly for multiple files

my_files<-list.files(paste0(g_intermediate, "treatment_episodes/"), pattern="CMA")
my_files<-my_files[grepl(pop_prefix, my_files)]
if(populations[pop] == "PC_study_population.rds"){
  my_files<-list.files(paste0(g_intermediate, "treatment_episodes/"), pattern="CMA")
  my_files<-my_files[!grepl("PC_HOSP", my_files)]
}

my_data<-lapply(paste0(g_intermediate, "treatment_episodes/",my_files), readRDS)

my_names<-as.data.frame(matrix(ncol = 2, nrow=4))
colnames(my_names)<-c("Code", "Drug")
my_names$Code<-c("D05BB02", "D11AH04","D10BA01", "N03AG01")
my_names$Drug<-c("Retinoid", "Retinoid", "Retinoid", "Valproate")

for(i in 1:length(my_data)){
  my_label<-my_names[(str_detect(my_files[i], my_names$Code)),]
  my_KM<-as.data.frame(my_data[i])
  
  # 1. Create a data frame with all treatment episodes for valp/retin in the whole period
  # 
  # 2. Create the following variables:
  #   
  #   a) time_on_treatment (days) = episode_end - episode_start
  #already present == "episode_duration"
  #   
  #   b) censor (1,2): If episode_end date == study_exit date, censor <- 2; else censor <- 1
  #add death--> study exit date merge on person ID
  
  end_date<- as.Date("31-12-2020", "%d-%m-%Y")
  my_KM$censor<-rep(NA, nrow(my_KM))
  my_KM$censor[my_KM$episode.end>=end_date]<-0
  my_KM$censor[my_KM$episode.end<end_date]<-1
  
  if(length(table(my_KM$censor))>1){
    #   
    #   3. split the data frame into two: episodes that started before the intervention date (in 2018, depending on country).
    int_date<- as.Date("01-06-2018", "%d-%m-%Y")
    
    my_KM$int<-rep(NA, nrow(my_KM))
    my_KM$int[my_KM$episode.start<int_date]<-"before"
    my_KM$int[my_KM$episode.start>int_date]<-"after"
    
    
    my_KM_before<-my_KM[my_KM$int=="before",]
    my_KM_after<-my_KM[my_KM$int=="after",]
    
    
    #   4. Plot kaplan meier (KM) curves in the two data sets ; calculate the "median drug survival" from each KM curve.          
    
    surv_int<-survfit(Surv(my_KM$episode.duration, my_KM$censor)~my_KM$int)
    comparrison<-survdiff(Surv(my_KM$episode.duration, my_KM$censor)~my_KM$int)
    my_median<-summary(surv_int)$table[,'median']
    my_median<-paste0(my_median[1],", ",my_median[2])
    pdf((paste0(plot_folder,"/","kaplan_meir_", my_label[2],"_",my_label[1], ".pdf")), width=8, height=4)
    plot(surv_int, col=c(1,2), lwd=2, main=paste0(my_label[1]," treatment episode duration"), ylab = "proportion still exposed", cex.main=1.5)
    legend("topright", c("before", "after", paste0("median: ", my_median)), col=c(2,1,1), lwd=2,lty=c(1,1,2), bty="n", cex=1.5)
    abline(h=0.5, lty=2)
    dev.off()
    my_surv<-list()
    my_surv[[1]]<-surv_int
    my_surv[[2]]<-comparrison
    saveRDS(my_surv,paste0(output_dir,"medicines_counts/", pop_prefix, "_kaplan_meir_", my_label[2],"_",my_label[1], "_model_data.rds"))}
  else{print("no censored cases, survival model cannot be fit")}
}
