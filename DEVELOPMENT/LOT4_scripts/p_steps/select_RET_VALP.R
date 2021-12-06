
# fetch data
MED_list<-list.files(paste0(data_folder,"/"), pattern="^MEDICINES")

#ATC of interest

 RETINOID<-c("D05BB02", "D11AH04", "D10BA01")

 VALPROATE<-c("N03AG01","N03AG02")

 # user input RETINOID or VALPROATE for function parameter Lot4ATC
 
ATCfilter<-function(medtable=MEDICINES, ID="person_id", ATC="medicinal_product_atc_code", Lot4ATC= RETINOID){
  
  newMED<-medtable[(medtabke[ATC]%in%Lot4ATC),]
  
  medID<-unique(newMED[[ID]])
  flowchart_ATC<-c(nrow(medtable), nrow(newMED))
  ATC_filter_output<-list(medID, flowchart_ATC, newMED)
  return(ATC_filter_output)
}          

# user input RETINOID or VALPROATE for function parameter Lot4ATC

ATCfilter_ID<- list()
for(i in 1:length(MED_list$MEDICINES)) {
  MEDS<-fread(paste0(data_folder,"/",actual_tables_preselect$MEDICINES[[i]]))
  output<- ATCfilter(medtable = MEDS, Lot4ATC=RETINOID)
  ATCfilter_ID[[i]]<-unique(output[[1]])
}

ATCfilter_ID_unique<-as.vector(unique(unlist(ATCfilter_ID)))

PERSONS_RETINOID<- PERSONS[(PERSONS$person_id%in%ATCfilter_ID_unique),]

# PERSONS_VALPROATE<- PERSONS[(PERSONS$person_id%in%ATCfilter_ID_unique),]

saveRDS(PERSONS_RETINOID,output_folder, "PERSONS_RETINOID.rds")
# saveRDS(PERSONS_RETINOID,output_folder, "PERSONS_VALPROATE.rds")