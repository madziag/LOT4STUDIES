
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

source(paste0(pre_dir,"functions/merge_gap.R"))

print('Import and append observation periods files')

#made a dummy duplicate to test append--> works fine
OBSERVATION_PERIODS <- IMPORT_PATTERN(pat = "OBSERVATION_PERIODS", dir = path_dir)


print('Set start and end date to date format and if end date is empty fill with end study date')
lapply(c("op_start_date","op_end_date"), function (x) OBSERVATION_PERIODS <- OBSERVATION_PERIODS[,eval(x) := as.IDate(as.character(get(x)),"%Y%m%d")])

OBSERVATION_PERIODS <- OBSERVATION_PERIODS[is.na(op_end_date), op_end_date := end_study_date]

#deleted 3 start dates in dummy duplicate to test removing start_date=NA
OBSERVATION_PERIODS <- OBSERVATION_PERIODS[(is.na(op_start_date)==F),]
#good
FlowChartCreateSpells <- list()

if(SUBP){
  print("There are subpopulations, so a column with meaning_set is added as specified in metadata")
  for (i in 1:nrow(op_meaning_list_set)){
    OBSERVATION_PERIODS[OBSERVATION_PERIODS[, op_meaning %in% unlist(str_split(op_meaning_list_set[i,op_meanings_list_per_set], pattern = " "))], meaning_set:=op_meaning_list_set[i,op_meaning_sets]]
  }
}


if(SUBP){
  FlowChartOverlap <- list()
  print("Create subpopulations subsets, Create spells and select latest")
  for(i in 1:nrow(subpopulation_meanings)){
          print(subpopulation_meanings[["subpopulations"]][i])
          before <- nrow(OBSERVATION_PERIODS)
          TEMP <- OBSERVATION_PERIODS[meaning_set %in% unlist(str_split(subpopulation_meanings[subpopulations==subpopulations[i],meaning_sets], pattern = " "))]
          TEMP <- TEMP[,c("person_id","op_start_date","op_end_date","meaning_set")]
        
          if(length(strsplit(subpopulation_meanings[["subpopulations"]][i],"-")[[1]]) > 1){
            print("Select only overlapping periods")
            
            FlowChartOverlap[[paste0("Rows at start ",subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
            k=1
            meaning_sets <- unique(TEMP[["meaning_set"]])
            
            while (k < length(meaning_sets)){
              
              if(k == 1) {meaning_sets1 <- meaning_sets[k]} else {meaning_sets1 <- paste0(meaning_sets[1],"_",meaning_sets[2])} 
              meaning_sets2 <- meaning_sets[k+1] 
              
              TEMP <- TEMP[meaning_set %in% c(meaning_sets1,meaning_sets2), ]
              FlowChartOverlap[[paste0("Rows in pair ",meaning_sets1,"|||",meaning_sets2," for",subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
              
              CreateSpells(dataset = TEMP,
                           id = "person_id",
                           start_date = "op_start_date", 
                           end_date = "op_end_date",
                           category = "meaning_set",
                           replace_missing_end_date = "date_creation",
                           overlap = T,
                           #only_overlaps = T,
                           dataset_overlap = "overlap")
              
              setnames(overlap,"category","meaning_set")
              setnames(overlap, "entry_spell_category", "op_start_date")
              setnames(overlap, "exit_spell_category", "op_end_date")
              overlap[,op_start_date := as.IDate(op_start_date)]
              overlap[,op_end_date := as.IDate(op_end_date)]
              FlowChartOverlap[[paste0("Rows with overlap ",meaning_sets1,"|||",meaning_sets2," for",subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(overlap)
              
              if(k < length(meaning_sets)-1){
                meaning_sets3 <- meaning_sets[k + 2:length(meaning_sets)] #repair this
                TEMP <- copy(OBSERVATION_PERIODS)[meaning_set %in% meaning_sets3,c("person_id","op_start_date","op_end_date","meaning_set")]
                FlowChartOverlap[[paste0("Rows in that where not in pair ",meaning_sets[1],"|||",meaning_sets[2]," for",subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
                overlap[,num_spell := NULL]
                TEMP <- rbind(overlap,TEMP,fill = T)
                rm(meaning_sets3)
              } else TEMP <- overlap
              
              FlowChartOverlap[[paste0("Rows at end of round ",meaning_sets1,"|||",meaning_sets2," for",subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
              rm(overlap,meaning_sets1,meaning_sets2) 
              gc()
              
              k = k+1
              if(nrow(TEMP)==0){
                k = length(meaning_sets)
                print(paste0(subpopulation_meanings[["subpopulations"]][i]," Has no overlapping spells so a file with o rows is returned"))
              }
            }
            
            rm(meaning_sets,k)
            gc()
            
          }else{
              
              TEMP <- CreateSpells(
                dataset=TEMP,
                id="person_id" ,
                start_date = "op_start_date",
                end_date = "op_end_date",
                overlap = F,
                dataset_overlap = "overlap",
                replace_missing_end_date = end_study_date,
                only_overlaps = F
              ) 
              
              TEMP <- TEMP[,temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell,][,temp := NULL]
              
              setnames(TEMP, "entry_spell_category", "op_start_date")
              setnames(TEMP, "exit_spell_category", "op_end_date")
              TEMP[,op_start_date := as.IDate(op_start_date)]
              TEMP[,op_end_date := as.IDate(op_end_date)]
              
          }
          
          
          
          TEMP <- TEMP[,temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell,][,temp := NULL]
          saveRDS(TEMP, file = paste0(std_pop_tmp,subpopulation_meanings[["subpopulations"]][i],"_OBS_SPELLS.rds"))
          
          after <- nrow(TEMP)
          FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$step <- "01_CreateSpells"
          FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$population <- subpopulation_meanings[["subpopulations"]][i]
          FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$before <- before
          FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$after <- after
          rm(TEMP)
          gc()
  }
  
}



######################################################################################################################  
# goal:  if observations_periods start_date[j] is less than 7 days after end_date[j-1] then the periods are merged 
# (assuming administrative gap in observation)
#then the merged observations are used to Create_spells

print("order OBSVERVATION_PERIODS by ID and startdate")
OBSERVATION_PERIODS<- OBSERVATION_PERIODS[order(OBSERVATION_PERIODS$person_id, OBSERVATION_PERIODS$op_start_date)]

print("merging gaps<=7")
 
 
 
 before_merge<-nrow(OBSERVATION_PERIODS)
 OBSERVATION_PERIODS_merge<-merge_gap(OBSERVATION_PERIODS, ID="person_id", startdate="op_start_date", enddate="op_end_date", gap=7)
 
 print("Create spells BUT not select latest for ALL")
 before_create_spells <- nrow(OBSERVATION_PERIODS)
 
 #
 OBSERVATION_PERIODS1 <- CreateSpells(
   dataset=OBSERVATION_PERIODS_merge,
   id="person_id" ,
   start_date = "op_start_date",
   end_date = "op_end_date",
   overlap = FALSE,
   only_overlaps = F
)

after_create_spells<-nrow(OBSERVATION_PERIODS1)
#take most recent
OBSERVATION_PERIODS_RECENT<- OBSERVATION_PERIODS1[(duplicated(OBSERVATION_PERIODS1$person_id, fromLast = TRUE)==F),]

after_recent<-nrow(OBSERVATION_PERIODS_RECENT)

# X<-setDT(OBSERVATION_PERIODS1)[, .SD[which.max(OBSERVATION_PERIODS1$entry_spell_category)],(OBSERVATION_PERIODS1$person_id)] 
 
 # if(length(unique(OBSERVATION_PERIODS$person_id)==nrow(OBSERVATION_PERIODS2))){print("single spell per patient-OK")}


OBSERVATION_PERIODS_RECENT <- OBSERVATION_PERIODS_RECENT[,temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell,][,temp := NULL]
setnames(OBSERVATION_PERIODS_RECENT, "entry_spell_category", "op_start_date")
setnames(OBSERVATION_PERIODS_RECENT, "exit_spell_category", "op_end_date")
OBSERVATION_PERIODS_RECENT[,op_start_date := as.IDate(op_start_date)]
OBSERVATION_PERIODS_RECENT[,op_end_date := as.IDate(op_end_date)]
saveRDS(OBSERVATION_PERIODS_RECENT, file = paste0(std_pop_tmp,"ALL_OBS_SPELLS.rds"))
after <- nrow(OBSERVATION_PERIODS_RECENT)
FlowChartCreateSpells[["Spells_ALL"]]$step <- "01_CreateSpells"
FlowChartCreateSpells[["Spells_ALL"]]$population <- "ALL"
FlowChartCreateSpells[["Spells_ALL"]]$before_merge <- before_merge
FlowChartCreateSpells[["Spells_ALL"]]$before_create_spells <- before_create_spells
FlowChartCreateSpells[["Spells_ALL"]]$after_create_spells <- after_create_spells
FlowChartCreateSpells[["Spells_ALL"]]$after_recent<- after_recent
FlowChartCreateSpells[["Spells_ALL"]]$unique_ID<- length(unique(OBSERVATION_PERIODS$person_id))
rm(OBSERVATION_PERIODS1)

###################################################################################################################### 

        

saveRDS(FlowChartCreateSpells, file = paste0(std_pop_tmp,"FlowChartCreateSpells.rds"))

if(exists("FlowChartOverlap")){ 
  saveRDS(FlowChartOverlap, file = paste0(std_pop_tmp,"FlowChartOverlap.rds"))
  rm(FlowChartOverlap)
}
  
# rm(before,after,OBSERVATION_PERIODS,FlowChartCreateSpells)
gc()




