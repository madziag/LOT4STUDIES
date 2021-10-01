#LOT 4

summary(mydt$birth_date)

firstfilter<-function(sex, dob, dobmin, dobmax, data) {
  data<-data[sex==1,]
  data<-data[dob>=dobmin,]
  data<-data[dob<=dobmax,]
  }


firstfilter(sex=mydt$sex, dob=mydt$birth_date, dobmin=20000, dobmax=50000, data=mydt)


simple_function <- function(dataset, col_name){
  col_name <- as.name(col_name)
  dataset %>%
    group_by(col_name) %>%
    summarise(mean_speed = mean(speed))
}

simple_function(cars, "dist")
(mydt[sex,])

summary(mydt[(sex==2)])
