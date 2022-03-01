
# fetch data
#needs to accomodate BIFAP

MED_list<-list.files(paste0(path_dir,"/"), pattern="^MEDICINES")
# fetch ALL_study_population
ALL_study_population<-readRDS(paste0(populations_dir, "ALL_study_population.rds"))

#ATC of interest

 RETINOID<-c("D05BB02", "D11AH04", "D10BA01")

 VALPROATE<-c("N03AG01","N03AG02")

 # user input RETINOID or VALPROATE for function parameter Lot4ATC
 
ATCfilter<-function(medtable=MEDICINES, ID="person_id", ATC="medicinal_product_atc_code", Lot4ATC= RETINOID){
  
  newMED<-medtable[(medtable[ATC]%in%Lot4ATC),]
  
  medID<-unique(newMED[[ID]])
  flowchart_ATC<-c(nrow(medtable), nrow(newMED))
  ATC_filter_output<-list(medID, flowchart_ATC, newMED)
  return(ATC_filter_output)
}          

# user input RETINOID or VALPROATE for function parameter Lot4ATC

ATCfilter_ID<- list()
for(i in 1:length(MED_list)) {
  MEDS<-fread(paste0(path_dir,MED_list[i]))
  output<- ATCfilter(medtable = MEDS, Lot4ATC=RETINOID)
  ATCfilter_ID[[i]]<-unique(output[[1]])
}

ATCfilter_ID_unique<-as.vector(unique(unlist(ATCfilter_ID)))

STUDY_POP_RETINOID<- ALL_study_population[(ALL_study_population$person_id%in%ATCfilter_ID_unique),]

# STUDY_POP_VALPROATE<- ALL_study_population[(ALL_study_population$person_id%in%ATCfilter_ID_unique),]

saveRDS(STUDY_POP_RETINOID,output_dir, "PERSONS_RETINOID.rds")
# saveRDS(STUDY_POP_RETINOID,output_dir, "PERSONS_VALPROATE.rds")
