
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

#modified by Ema Alsina MSc. e.m.alsina-2@umcutrecht.nl

# this script generates the observation spells used in the study.
# In the case of Lot4, observations with a gap of 7 days or less are concatenated,
# and in the case of multiple spells, the most recent is taken, and the others are discarded, resulting in one spell per personID

print('Import and append observation periods files')

FlowChartCreateSpells <- list()

OBSERVATION_PERIODS <- IMPORT_PATTERN(pat = "OBSERVATION_PERIODS", dir = path_dir)


step0<-"original data"

step0_nrow<-nrow(OBSERVATION_PERIODS)

step0_unique<-length(unique(OBSERVATION_PERIODS$person_id))

step1<-'Set start and end date to date format and if end date is empty fill with end study date'

print('Set start and end date to date format and if end date is empty fill with end study date')
lapply(c("op_start_date","op_end_date"), function (x) OBSERVATION_PERIODS <- OBSERVATION_PERIODS[,eval(x) := as.IDate(as.character(get(x)),"%Y%m%d")])

OBSERVATION_PERIODS <- OBSERVATION_PERIODS[is.na(op_end_date), op_end_date := end_study_date]

step1_nrow<-nrow(OBSERVATION_PERIODS)

step1_unique<-length(unique(OBSERVATION_PERIODS$person_id))

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
    #TEMP <- TEMP[0]
    original_unique_ID<-length(unique(TEMP$person_id))
    
    if(nrow(TEMP) > 0){
      
      if(length(strsplit(subpopulation_meanings[["subpopulations"]][i],"_")[[1]]) > 1){
        ## THIS IS FOR PC HOSP 
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
                       dataset_overlap = "overlap",
                       gap_allowed = 7)
          
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
          } else {
            TEMP <- overlap
            TEMP <- TEMP[op_start_date<op_end_date,]
            } 
          FlowChartOverlap[[paste0("Rows at end of round ",meaning_sets1,"|||",meaning_sets2," for",subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
          rm(overlap,meaning_sets1,meaning_sets2) 
          gc()
          
          k = k+1
          if(nrow(TEMP)==0){
            k = length(meaning_sets)
            print(paste0(subpopulation_meanings[["subpopulations"]][i]," Has no overlapping spells so a file with 0 rows is returned"))
          }
        }
        
        if(nrow(TEMP) > 0) TEMP <- TEMP[,temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell,][,temp := NULL]

        rm(meaning_sets,k)
        gc()
        ## THIS IS FOR PC HOSP 
      }else{
        ### THIS IS FOR PC ####
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
        ### THIS IS FOR PC ####
      }
    }else{
      # THIS IS WHEN THE TABLE IS EMPTY
      TEMP <- data.table(person_id = as.character(), op_start_date = as.IDate(x = integer(0), origin = "1970-01-01"), op_end_date = as.IDate(x = integer(0), origin = "1970-01-01"), meaning_set = as.character(), num_spell = as.numeric())
      print(paste0(subpopulation_meanings[["subpopulations"]][i]," has no observations. Please check if metadata is filled correctly and if CDM contains observations for this subpopulation"))
    }
    
    saveRDS(TEMP, file = paste0(std_pop_tmp,subpopulation_meanings[["subpopulations"]][i],"_OBS_SPELLS.rds"))
    ############################HERE IS THE PROBLEM ##############################   
    after <- nrow(TEMP)
    FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$step <- "01_CreateSpells"
    FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$original_unique_ID <- original_unique_ID
    FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$population <- subpopulation_meanings[["subpopulations"]][i]
    FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$before <- before
    FlowChartCreateSpells[[paste0("Spells_",subpopulation_meanings[["subpopulations"]][i])]]$after <- after
    saveRDS(FlowChartCreateSpells, file = paste0(output_dir,subpopulation_meanings[["subpopulations"]][i],"_flowchart.rds"))
    rm(TEMP)
    gc()
    

    # THIS IS THE END OF THE FOR LOOP 
  } 
} else{
  ### THIS IS FOR SINGLE POPULATIONS 
  print("Create spells and select latest for ALL")

  before_CreateSpells <- nrow(OBSERVATION_PERIODS)

  OBSERVATION_PERIODS1 <- CreateSpells(
    dataset=OBSERVATION_PERIODS,
    id="person_id" ,
    start_date = "op_start_date",
    end_date = "op_end_date",
    overlap = FALSE,
    only_overlaps = F,
    gap_allowed = 7
  )

  print("CreateSpells run OK")

  after_CreateSpells<-nrow(OBSERVATION_PERIODS1)

  print("select most recent Observation Period")

  OBSERVATION_PERIODS1<- OBSERVATION_PERIODS1[(duplicated(OBSERVATION_PERIODS1$person_id, fromLast = TRUE)==F),]

  select_most_recent<-nrow(OBSERVATION_PERIODS1)

  print("CLEANUP OBSERVATION_PERIODS1")
  OBSERVATION_PERIODS1 <- OBSERVATION_PERIODS1[,temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell,][,temp := NULL]
  setnames(OBSERVATION_PERIODS1, "entry_spell_category", "op_start_date")
  setnames(OBSERVATION_PERIODS1, "exit_spell_category", "op_end_date")
  OBSERVATION_PERIODS1[,op_start_date := as.IDate(op_start_date)]
  OBSERVATION_PERIODS1[,op_end_date := as.IDate(op_end_date)]
  saveRDS(OBSERVATION_PERIODS1, file = paste0(std_pop_tmp,"ALL_OBS_SPELLS.rds"))


  ######################################################################################################################
  print("store FlowChart data on attrition")
  CreateSpellsStep<-c("original number of OBSERVATION PERIODS", "original number of unique personID",
                      "number of OBSERVATION PERIODS after concatenating observations with gaps <= 7 days",
                      "number of OBSERVATION PERIODS after selecting the most recent observation (one spell per unique ID)")

  OBS_number<-c(before_CreateSpells,step0_unique, after_CreateSpells, select_most_recent)

  FlowChartCreateSpells<-as.data.frame(cbind(CreateSpellsStep, OBS_number))

  saveRDS(FlowChartCreateSpells, file = paste0(output_dir,"FlowChartCreateSpells.rds"))

  if(exists("FlowChartOverlap")){
    saveRDS(FlowChartOverlap, file = paste0(std_pop_tmp,"SUBPOP_FlowChartOverlap.rds"))
    rm(FlowChartOverlap)
  }

  rm(before_CreateSpells,after_CreateSpells, select_most_recent, OBSERVATION_PERIODS, OBSERVATION_PERIODS1, FlowChartCreateSpells)
  gc()
}



