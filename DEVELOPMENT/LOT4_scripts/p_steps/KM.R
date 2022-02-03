# this script will differentiate between censored CMAs (study ended) and terminated CMAs
#before and after the 2018 intervention date (June 2018) 

my_data<-  readRDS(paste0(g_intermediate, "treatment_episodes/",(list.files(paste0(g_intermediate, "treatment_episodes/"), pattern="CMA"))))

# 1. Create a data frame with all treatment episodes for valp/retin in the whole period
# 
# 2. Create the following variables:
#   
#   a) time_on_treatment (days) = episode_end - episode_start
        #already present == "episode_duration"
#   
#   b) km_event (1,2): If episode_end date == study_exit date, km_event <- 2; else km_event <- 1
end_date<- as.Date("31-12-2020", "%d-%m-%Y")
my_data$km_event<-rep(NA, nrow(my_data))
my_data$km_event[my_data$episode.end==end_date]<-2
my_data$km_event[my_data$episode.end!=end_date]<-1
table(my_data$km_event)
#   
#   3. split the data frame into two: episodes that started before the intervention date (in 2018, depending on country).
int_date<- as.Date("01-06-2018", "%d-%m-%Y")

my_data$int<-rep(NA, nrow(my_data))
my_data$int[my_data$episode.start<int_date]<-"before"
my_data$int[my_data$episode.start>int_date]<-"after"


my_data_before<-my_data[my_data$int=="before",]
my_data_after<-my_data[my_data$int=="after",]

table(my_data_before$episode.duration)
table(my_data_after$episode.duration)
   
#   4. Plot kaplan meier (KM) curves in the two data sets ; calculate the "median drug survival" from each KM curve.          

surv_int<-survfit(Surv(my_data$episode.duration, my_data$km_event)~my_data$int)

survdiff(Surv(my_data$episode.duration, my_data$km_event)~my_data$int)


plot(surv_int, col=c(1,2), lwd=2, main="treatment episode duration", cex.main=2)
legend("topright", c("before", "after", "median"), col=c(2,1,1), lwd=2,lty=c(1,1,2), bty="n", cex=2)
abline(h=0.5, lty=2)
       