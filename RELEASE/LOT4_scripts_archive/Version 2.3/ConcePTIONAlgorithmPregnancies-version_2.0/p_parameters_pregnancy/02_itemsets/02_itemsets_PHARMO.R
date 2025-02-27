# date: TO_ADD
# datasource: PHARMO
# DAP: PHARMO
# author: TO_ADD
# version: 1.0
# changelog: 

####### LOAD itemsets for PHARMO


########################################## START ######################################################   

### specification GESTAGE_FROM_DAPS_CRITERIA_WEEKS
itemset_AVpair_pregnancy[["GESTAGE_FROM_DAPS_CRITERIA_WEEKS"]][[files[i]]][["PHARMO"]] <- list()

### specification GESTAGE_FROM_DAPS_CRITERIA_DAYS
itemset_AVpair_pregnancy[["GESTAGE_FROM_DAPS_CRITERIA_DAYS"]][[files[i]]][["PHARMO"]] <- list()

### specification GESTAGE_FROM_LMP_WEEK
itemset_AVpair_pregnancy[["GESTAGE_FROM_LMP_WEEKS"]][[files[i]]][["PHARMO"]] <- list()

# specification GESTAGE_FROM_LMP_DAYS
itemset_AVpair_pregnancy[["GESTAGE_FROM_LMP_DAYS"]][[files[i]]][["PHARMO"]] <- list()

# # specification GESTAGE_FROM_USOUNDS_DAYS
itemset_AVpair_pregnancy[["GESTAGE_FROM_USOUNDS_DAYS"]][[files[i]]][["PHARMO"]] <- list()

# # specification GESTAGE_FROM_USOUNDS_WEEKS
itemset_AVpair_pregnancy[["GESTAGE_FROM_USOUNDS_WEEKS"]][[files[i]]][["PHARMO"]]  <- list()

# specification DATESTARTPREGNANCY
itemset_AVpair_pregnancy[["DATESTARTPREGNANCY"]][[files[i]]][["PHARMO"]] <- list(list("perined", "start_zw"))


########################################## END ###################################################### 

itemset_AVpair_pregnancy[["DATEENDPREGNANCY"]][[files[i]]][["PHARMO"]] <- list(list("perined", "eind_zw"))

### specification END_LIVEBIRTH
itemset_AVpair_pregnancy[["END_LIVEBIRTH"]][[files[i]]][["PHARMO"]] <- list()

### specification END_STILLBIRTH
itemset_AVpair_pregnancy[["END_STILLBIRTH"]][[files[i]]][["PHARMO"]] <- list()

# specification END_TERMINATION
itemset_AVpair_pregnancy[["END_TERMINATION"]][[files[i]]][["PHARMO"]] <- list()

### specification END_ABORTION
itemset_AVpair_pregnancy[["END_ABORTION"]][[files[i]]][["PHARMO"]] <- list()


########################################### TYPE #######################################

itemset_AVpair_pregnancy[["TYPE"]][[files[i]]][["PHARMO"]] <- list(list("perined", "birth_status"))

################################ DICTINARY OF TYPE ##################################

dictonary_of_itemset_pregnancy[["TYPE"]][["PHARMO"]][["LB"]]<-list(list("perined", "2")) 
dictonary_of_itemset_pregnancy[["TYPE"]][["PHARMO"]][["SB"]]<-list(list("perined", "3"))
dictonary_of_itemset_pregnancy[["TYPE"]][["PHARMO"]][["SA"]]<-list(list("perined", "0"))
dictonary_of_itemset_pregnancy[["TYPE"]][["PHARMO"]][["T"]]<-list()
dictonary_of_itemset_pregnancy[["TYPE"]][["PHARMO"]][["MD"]]<-list()
dictonary_of_itemset_pregnancy[["TYPE"]][["PHARMO"]][["ECT"]]<-list()
dictonary_of_itemset_pregnancy[["TYPE"]][["PHARMO"]][["UNK"]]<-list(list("perined", "4"))





##### FROM MEDICAL_OBSERVATION

### specification LastMestrualPeriod
itemsetMED_AVpair_pregnancy[["LastMestrualPeriod"]][[files[i]]][["PHARMO"]] <- list()

### specification GestationalAge
itemsetMED_AVpair_pregnancy[["GestationalAge"]][[files[i]]][["PHARMO"]] <- list()

### specification PregnancyTest
itemsetMED_AVpair_pregnancy[["PregnancyTest"]][[files[i]]][["PHARMO"]] <- list(list("gp","gp_exaval1"))



################################ DICTINARY OF PregnancyTest ##################################

dictonary_of_itemset_PregnancyTest[["PregnancyTest"]][["positive"]]<-list(list("positive")) 



################################ PARAMETERS for PregnancyTest ##################################
days_from_start_PregnancyTest <- 30
days_to_end_PregnancyTest <- 280

