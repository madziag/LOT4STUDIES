#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022


##############################################################
############### DAP SPECIFIC ASSUMED DURATIONS ###############
##############################################################
### DEFAULT ###
# cma_data$assumed_duration<-rep(30, nrow(cma_data))
cma_data[,assumed_duration:=30]
### DAP	Type algorithm	Rules ###
# 1. ARS: None, No additional analyses, keep 30 days
# - uses default 
# 2. DNR	Fixed duration	Valp/Retin: All records 90 days
if(is_Denmark){cma_data[,assumed_duration:=90]}
# 3. BIFAP: Estimated using local algorithm	Valp/Retin: Use the value in MEDICINES$presc_duration_days --> COLUMN ADDED IN MONTHLY COUNTS ATC
### ARE THERE ANY MISSING VALUES??????
if(is_BIFAP){
  cma_data[,assumed_duration:=presc_duration_days][,assumed_duration:=as.numeric(assumed_duration)]
  # If any NA's, we take the value of 30 days 
  cma_data[is.na(assumed_duration), assumed_duration:=30]
  if(sum(is.na(cma_data$presc_duration_days))>0){
    print("Missing values for duration of treatment detected.")
    print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
  }
}
# 4. FISABIO: Retin: Use the value in MEDICINES$presc_duration_days
if(is_FISABIO){
  cma_data[,assumed_duration:=presc_duration_days][,assumed_duration:=as.numeric(assumed_duration)]
  # If any NA's, we take the value of 30 days 
  cma_data[is.na(assumed_duration), assumed_duration:=30]
  if(sum(is.na(cma_data$presc_duration_days))>0){
    print("Missing values for duration of treatment detected.")
    print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
  }
}
# 5. PHARMO Estimated using local algorithm	Valp/Retin: Use the value in MEDICINES$presc_duration_days
if(is_PHARMO){
  cma_data[,assumed_duration:=presc_duration_days][,assumed_duration:=as.numeric(assumed_duration)]
  # If any NA's, we take the value of 30 days 
  cma_data[is.na(assumed_duration), assumed_duration:=30]
  if(sum(is.na(cma_data$presc_duration_days))>0){
    print("Missing values for duration of treatment detected.")
    print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
  }
}
# 6. CPRD	Estimated using local algorithm	Valp: days_treated = disp_number_medicinal_product  / presc_quantity_per_day
if(is_CPRD){
  cma_data[,disp_number_medicinal_product:=as.numeric(disp_number_medicinal_product)][,presc_quantity_per_day:=as.numeric(presc_quantity_per_day)]
  cma_data[,assumed_duration:= disp_number_medicinal_product/presc_quantity_per_day][,assumed_duration:=as.numeric(assumed_duration)]
  # If any NA's, we take the value of 30 days 
  cma_data[is.na(assumed_duration), assumed_duration:=30]
  if(sum(is.na(cma_data$presc_duration_days))>0){
    print("Missing values for duration of treatment detected.")
    print(paste0(sum(is.na(cma_data$presc_duration_days)), " missing values were imputed with a value of 30 days"))
  }
}
# 7.   CASERTA	Calculation	Retin: For every retinoid record, calculate the value
# - If missing(disp_number_medicinal_product) disp_number_medicinal_product <- 1
# - days_treated = disp_number_medicinal_product (MEDICINES) *  coverage per box 
#### PENDING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# if(is_CASERTA){
#   cma_data[is.na(disp_number_medicinal_product), disp_number_medicinal_product:=1]
#   cma_data[,assumed_duration:=disp_number_medicinal_product * coverage_per_box] ### where do we get this value from?
#   cma_data[is.na(assumed_duration), assumed_duration:=30]
# }
# 

