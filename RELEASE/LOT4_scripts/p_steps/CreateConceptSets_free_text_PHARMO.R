#Author: Magdalena Gamba M.D.
#email: m.a.gamba@uu.nl
#Organisation: Utrecht University, Utrecht, The Netherlands
#Date: 31/01/2022


# List of free text words used per concept set : Single words
concept_set_terms <- vector(mode="list")

concept_set_terms[["adr_cardiovasc"]]=c("beroerte", "CVA", "flauwvallen", "syncope")
concept_set_terms[["adr_depression"]]=c("depressie", "suicide", "zelfmoord")
concept_set_terms[["adr_gastroint"]]= c("Hematemesis", "geelzucht")
concept_set_terms[["adr_immuno"]]=c("allergie")
concept_set_terms[["adr_tremor"]]=c("tremor", "beving")
concept_set_terms[["adr_otic"]]=c("doof", "slechthorend", "oorsuizen", "tinnitus")
concept_set_terms[["adr_opthal"]]= c("blind", "cataract", "staar")
concept_set_terms[["adr_psych"]]=c("psychose")
concept_set_terms[["ind_epilepsy"]]=c("epilepsie")
concept_set_terms[["ind_migraine"]]=c("migraine", "hoofdpijn")
concept_set_terms[["iud_diag"]]= c("IUD", "I.U.D", "spiraal")
concept_set_terms[["sterility"]]=c("menopause", "overgang", "climac.")

# List of free text words used per concept set : word_combos
concept_set_terms_combo <- vector(mode="list")

concept_set_terms_combo[["adr_cardiovasc"]]=c("cerebro+accident")
concept_set_terms_combo[["adr_gastroint"]]= c("bloed+overg", "coli+ulcer", "chron+enteritis", "prikkel+darm+synd")
concept_set_terms_combo[["adr_immuno"]]=c("allerg+reactie")
concept_set_terms_combo[["ind_epilepsy"]]=c("epilep+aanval")

# Saves concept sets 
