# date: TO_ADD
# datasource: CASERTA
# DAP: CASERTA
# author: TO_ADD
# version: 1.0
# changelog: 

####### LOAD MEANING_OF_SURVEY for CASERTA

meaning_of_survey_pregnancy <- vector(mode="list")

meaning_of_survey_pregnancy[["CASERTA"]][["livebirth_or_stillbirth"]]<-list("birth_registry_mother")
meaning_of_survey_pregnancy[["CASERTA"]][["ongoing_pregnancy"]]<-list()
meaning_of_survey_pregnancy[["CASERTA"]][["spontaneous_abortion"]]<-list() 
meaning_of_survey_pregnancy[["CASERTA"]][["induced_termination"]]<-list() 
meaning_of_survey_pregnancy[["CASERTA"]][["other"]]<-list()



####### LOAD MEANING_OF_VISIT for CASERTA

#meaning_of_visit_pregnancy <- vector(mode="list")
#meaning_of_visit_pregnancy[["CASERTA"]]<-list("first_encounter_for_ongoing_pregnancy", "service_before_termination","service_for_ongoing_pregnancy")
