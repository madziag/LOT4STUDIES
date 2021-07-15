
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021


if(SUBP) SCHEME_0102 <- subpopulation_meanings[, 
                                               ':=' 
                                               (
                                                 file_in = paste0(subpopulations,"_study_population.rds"), 
                                                 file_in2 = paste0(subpopulations,"_source_population.rds"),
                                                 #file_in2 = "ALL_source_population.rds",
                                                 file_out = paste0(subpopulations,"_R_01_02_CompareToSource.csv"),
                                                 file_out2 = paste0(subpopulations,"_R_01_02_CompareToSourcePlot.rds"),
                                                 file_out3 = paste0(subpopulations,"_R_01_02_CompareToSourcePlot2.rds"),      
                                                 folder_in = populations_dir, 
                                                 folder_in2 = std_pop_tmp,
                                                 folder_out = std_source_pop_dir) 
                                               ]


if(!SUBP) SCHEME_0102 <- data.frame(subpopulations = c("ALL"), 
                                    
                                    file_in = "ALL_study_population.rds", 
                                    file_in2 = "ALL_source_population.rds",
                                    file_out = "ALL_R_01_02_CompareToSource.csv",
                                    file_out2 = "ALL_R_01_02_CompareToSourcePlot.rds",
                                    file_out3 = "ALL_R_01_02_CompareToSourcePlot2.rds",
                                    folder_in = populations_dir, 
                                    folder_in2 = std_pop_tmp, 
                                    folder_out = std_source_pop_dir)



#i=4
for(i in 1:nrow(SCHEME_0102)){
  
  #j = "ALL"
  STUDY_POPULATION <- readRDS(file = paste0(SCHEME_0102[["folder_in"]][i],SCHEME_0102[["file_in"]][i]))[sex_at_instance_creation %in% c("F","M"),]
  #[,.(person_id,age_op_start_date,op_start_date,start_follow_up,end_follow_up,num_spell)]
  SOURCE_POPULATION <- readRDS(file = paste0(SCHEME_0102[["folder_in2"]][i],SCHEME_0102[["file_in2"]][i]))[sex_at_instance_creation %in% c("F","M"),]
  #[,.(person_id,age_op_start_date,op_start_date,op_end_date,num_spell)]
  
  
  
  #Agebands <- CreateBands(seq(from = 0 , to = 5 + max(SOURCE_POPULATION[!is.na(age_op_start_date),age_op_start_date]), by = 5))
  Agebands <- CreateBands(seq(from = 0 , to = 10 + Age_max, by = 5))
  TEMP_STUDY <- merge(x = STUDY_POPULATION, y = Agebands, by.x = "age_op_start_date",by.y = "INT")
  TEMP_SOURCE <- merge(x = SOURCE_POPULATION, y = Agebands, by.x = "age_op_start_date",by.y = "INT")
  rm(STUDY_POPULATION,SOURCE_POPULATION, Agebands)
  gc()
  
  TEMP_SOURCE <- TEMP_SOURCE[is.na(age_op_start_date) | op_end_date < op_start_date, ':=' (Year_op = 9999, band = 9999, PY_OP = 0,num_spell = 0)]
  COUNT_STUDY <- TEMP_STUDY[,.(No1 = sum(!is.na(person_id)),PY1 = sum(PY), last_spell1 = round(mean(num_spell),2)), keyby = list(Year_op,band)]
  COUNT_SOURCE <- TEMP_SOURCE[,.(No0 = sum(!is.na(person_id)),PY0 = sum(PY_OP),last_spell0 = round(mean(num_spell),2)), keyby = list(Year_op,band,Order)]
  rm(TEMP_STUDY,TEMP_SOURCE)
  gc()
  
  COUNT <- merge(x = COUNT_SOURCE, y = COUNT_STUDY, by = c("Year_op","band"), all = T)
  lapply(c("No0","PY0","No1","PY1"),function(x) COUNT <- COUNT[is.na(get(x)), eval(x) := 0])
  COUNT <- COUNT[PY0 >= PY1,]
  
  COUNT2 <- COUNT[,.(Year_op,Order,band,PY0,PY1)][is.na(PY1), PY1 := 0][,diff_per := ((PY1 - PY0)/PY0)*-1][,diff_abs := (PY1 - PY0)*-100][band != "9999",][PY0 > PY1,]
  #COUNT3 <- COUNT[,.(Year_op,Order,band,PY0,PY1)][is.na(PY1), PY1 := 0][,diff_abs := (PY1 - PY0)*-1][band != "9999",]
  setorderv(COUNT2,c("Order"))
  
  TEMP2 <- INPUTMATRIX(
    d = COUNT2,
    value = "diff_per",
    type = "none",
    var = "Year_op",
    var.v = c(min(COUNT2[["Year_op"]]):max(COUNT2[["Year_op"]])),
    cat = "band",
    cat.v = unique(COUNT2[,band]),
    per = F
  )
  
  saveRDS(TEMP2, file = paste0(SCHEME_0102[["folder_out"]][i],SCHEME_0102[["file_out2"]][i]))
  rm(TEMP2)
  
  TEMP3 <- INPUTMATRIX(
    d = COUNT2,
    value = "diff_abs",
    type = "none",
    var = "Year_op",
    var.v = c(min(COUNT2[["Year_op"]]):max(COUNT2[["Year_op"]])),
    cat = "band",
    cat.v = unique(COUNT2[,band]),
    per = F
  )
  
  saveRDS(TEMP3, file = paste0(SCHEME_0102[["folder_out"]][i],SCHEME_0102[["file_out3"]][i]))
  rm(COUNT2,TEMP3)
  
  
  colls <- c("Year_op","band","No0","PY0","last_spell0","No1","PY1","last_spell1")               
  new <- c("Year op_start_date", "Ageband op_start_date", "No. source", "PY source","Average number of spells source","No. study", "PY study","Average number of spells study" )
  setnames(COUNT, colls, new)
  setorderv(COUNT,c("Year op_start_date", "Ageband op_start_date"))
  
  fwrite(COUNT, file = paste0(SCHEME_0102[["folder_out"]][i],SCHEME_0102[["file_out"]][i]), sep = ";")
  #saveRDS(COUNT, file = paste0(thisdir,SCHEME_0102[["folder_out"]][i],SCHEME_0102[["file_out"]][i]))
  
  rm(COUNT,COUNT_SOURCE,COUNT_STUDY,colls,new)
  gc()
  
}

saveRDS(SCHEME_0102, file = paste0(std_pop_tmp,"SCHEME_0102.rds"))
rm(SCHEME_0102)


#sum(COUNT[!is.na(No0),No0])










