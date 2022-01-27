### BIFAP POOLING ###

# 1. Creates ALL_regions folder 
invisible(ifelse(!dir.exists(paste0(output_dir, "ALL_regions")), dir.create(paste0(projectFolder, "/ALL_regions")), FALSE))
All_regions_dir<-paste0(projectFolder, "/ALL_regions/")

# 2. Copies all counts, baseline tables, denominator csv files from each region, prefixing each file with the name of the region
if(multiple_regions == T){
  ## Gets a list of region names from the CDMInstances folder 
  regions <- list.dirs(path = multiple_regions_dir, full.names = FALSE, recursive = FALSE)
  ## Loops over each region
  for(reg in 1:length(regions)){
    ## Find counts, baseline tables and denominator csv files in each regional folder 
    pattern1 = c("counts.csv", "baseline.csv", "denominator.csv")
    csv_files <- list.files(paste0(projectFolder, "/", regions[reg]), pattern = paste0(pattern1, collapse="|"), recursive = TRUE, full.names =  TRUE, ignore.case =  TRUE)
    ## Copy these files to ALL_regions folder 
    f1 <- as.data.frame(csv_files, header = FALSE)
    colnames(f1)  <- 'files.old'
    f1$files.new  <- gsub(paste0(regions[reg], "/g_output/baseline_tables/csv_files/")   , paste0("ALL_regions/", regions[reg], "_"), f1$files.old)
    f1$files.new1 <- gsub(paste0(regions[reg], "/g_output/pregnancy_counts/csv_files/")  , paste0("ALL_regions/", regions[reg], "_"), f1$files.new)
    f1$files.new2 <- gsub(paste0(regions[reg], "/g_output/preliminary_counts/csv_files/"), paste0("ALL_regions/", regions[reg], "_"), f1$files.new1)
    file.copy(as.vector(f1$files.old), as.vector(f1$files.new2))
  }
}

# 3. Moves files within ALL_regions folder to subfolders named after the specific counts that have been performed e.g. Valproate.csv files from every region will be moved into  Valproate folder 
## Gets a list of files in ALL_regions folder (for subfolder creation, the region prefix is removed)
all_regions_files                  <- list.files(paste0(projectFolder, "/ALL_regions/"), full.names = FALSE)
all_regions_files_no_prefix        <- gsub("^[A-Z]{2}_" ,"",all_regions_files)
all_regions_files_no_prefix_unique <- unique(unlist(all_regions_files_no_prefix))
## Loops through all the unique counts files
for (i in 1:length(all_regions_files_no_prefix_unique)){
  ## Removes the .csv from the file name so that it is not created in the name of the newly created folder name
  file_name <- gsub(".csv", "", all_regions_files_no_prefix_unique[i])
  ## Creates folder + path to folder
  invisible(ifelse(!dir.exists(paste0(projectFolder, "/ALL_regions/", file_name)), dir.create(paste0(projectFolder, "/ALL_regions/", file_name)),FALSE))
  record_dir <-paste0(paste0(projectFolder, "/ALL_regions/", file_name, "/"))
  ## Gets list of files with name matching the newly created folder  
  files = list.files(paste0(projectFolder, "/ALL_regions/"), pattern = all_regions_files_no_prefix_unique[i], full.names = TRUE)
  # Moves these files to the newly created folder of the same name  
  f2 <- as.data.frame(files, header = FALSE)
  colnames(f2) <- 'files.old'
  f2$files.new <- gsub("ALL_regions/", paste0("ALL_regions/", file_name, "/"), f2$files.old) 
  file.move(as.vector(f2$files.old), as.vector(f2$files.new))
}

# 4. Aggregate the denominator files to create a denominator file for all the regions  
all_denominator_folders <- list.files(paste0(projectFolder, "/ALL_regions/"), pattern = "denominator", full.names = FALSE)
## Loops through all the denominator folders 
for (i in 1:length(all_denominator_folders)){
  tables <- lapply(paste0(All_regions_dir, all_denominator_folders[i]), read.csv, header = TRUE)
  tables <- lapply("C:/Users/31653/Desktop/LOT4_Sourcetree/DEVELOPMENT/LOT4_scripts/ALL_regions/ALL_denominator", read.csv, header = TRUE)
}


csv.list <- list.files(paste0(All_regions_dir), pattern = "denominator", recursive = TRUE)

lst <- lapply(paste0(All_regions_dir, csv.list), fread)
dt <- rbindlist(lst)

abc <- fread("C:/Users/31653/Desktop/LOT4_Sourcetree/DEVELOPMENT/LOT4_scripts/ALL_regions/ALL_denominator/AS_ALL_denominator.csv")

# join all tables in each folder 
# tables <- lapply(paste0(record_dir,list.files(record_dir)), read.csv, header = TRUE)
# combined_tables <- as.data.table(do.call(rbind , tables))
# combined_tables[ ,sum_N:=sum(N), by = list(YM)]
# # combined_tables <- combined_tables[ , sum_freq:=sum(Freq), by = list(YM)]
# combined_tables[ ,N:= NULL]
# combined_tables[ ,Freq:=NULL]
# combined_tables[ ,rates:=NULL]
# combined_tables[ ,masked:=NULL]
# setnames(combined_tables, "sum_N", "N")
# setnames(combined_tables, "sum_freq", "Freq")
# setcolorder(combined_tables ,c("YM", "N", "Freq"))

# combined_tables <- combined_tables[!duplicated(combined_tables),]
# combined_tables$masked <- ifelse(combined_tables$N<5 & combined_tables$N>0, 1, 0)
# if (mask == T){combined_tables[combined_tables$masked == 1,]$N <- 5} else {combined_tables[combined_tables$masked == 1,]$N <- combined_tables[combined_tables$masked == 1,]$N }
# combined_tables[,rates:=as.numeric(N)/as.numeric(Freq)]
# Save aggregated counts
# saveRDS(combined_tables, paste0(All_regions_dir,all_regions_files_no_prefix_unique[i],".rds"))
