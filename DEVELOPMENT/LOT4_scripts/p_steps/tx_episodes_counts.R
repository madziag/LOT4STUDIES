# Reads in treatment file 
df <- as.data.table(readRDS(paste0(g_intermediate, "treatment_episodes/", pop_prefix,"_", study_type,".rds")))
# Reads in study population data
# Study_population has already been read in by the wrapper script
# Add column that indicates row number (to be used when merging expanded df with original df)
df <- df[, nrow := .I]
# # Create years_months columns for episode.start and episode.end columns
# df$episode.start.YM <-  format(as.Date(df$episode.start), "%Y-%m")
# df$episode.end.YM   <-  format(as.Date(df$episode.end), "%Y-%m")
#################################################################################################################
#################################################################################################################
# 1. Prevalence of valproate / retinoid use per month over the study period:
# Numerator: Number of female subjects in cohort with valproate episode overlapping the month by at least 1 day 
# Denominator: Total number of female subjects in the cohort for at least 1 day in the month
# Stratification: age categories, indication, dose, duration of treatment episode (up until that month) 
#################################################################################################################
#################################################################################################################
# Expand df (1 row for every day of treatment per patient) -> gives 2 cols (person_id + tx day)a
df_expanded <- setDT(df)[ , list(idnum = person_id, episode.day = seq(episode.start, episode.end, by = "day")), by = 1:nrow(df)]
# Merge back with original df
df_expanded <-  merge(df, df_expanded, by = "nrow")
# Create year-months columns episode.day
# df_expanded$episode.day.YM <-  format(as.Date(df_expanded$episode.day), "%Y-%m")
df_expanded$year <- year(df_expanded$episode.day)
df_expanded$month <- month(df_expanded$episode.day)
# Remove episode.day column (we already have the month and year extracted from this column) + other duplicate/unnecessary columns
df_expanded <- df_expanded[,-c("nrow", "idnum")]
# Remove duplicate records
df_expanded <- unique(df_expanded, by=c("person_id", "episode.ID", "episode.start", "end.episode.gap.days" , "episode.duration", "episode.end", "year", "month" )) # where df is a data.table

# Count the number of patients treated per month
# Group by episode.day.YM (gives every month of treatment for each patient)
# df_prevalence_counts <- df_expanded[,.N, by = .(episode.day.YM)]
df_prevalence_counts <- df_expanded[,.N, by = .(year,month(episode.day))]
if(is_PHARMO){df_prevalence_counts <- df_prevalence_counts[df_prevalence_counts$year < 2020,]} else {df_prevalence_counts <- df_prevalence_counts[df_prevalence_counts$year < 2021,]}
# # # Confirm counts 
# sum(df_prevalence_counts$N) == nrow(df_expanded)

#################################################################################################################
#################################################################################################################
# 2 Incidence rates of valproate / retinoid initiation per month:
## Numerator: Number of incident valproate prescriptions/dispensings (i.e. start of a new episode), per month
## Denominator: Total amount of follow-up in the month
## Stratification: age categories, indication, dose
#################################################################################################################
#################################################################################################################
# Group by year_month of episode.start.YM
df_incidence_counts <- df[,.N, by = .(year(episode.start),month(episode.start))]
if(is_PHARMO){df_incidence_counts <- df_incidence_counts[df_incidence_counts$year < 2020,]} else {df_incidence_counts <- df_incidence_counts[df_incidence_counts$year < 2021,]}
# # Confirm counts 
# sum(df_incidence$N) == nrow(df)
#################################################################################################################
#################################################################################################################
# 3. Proportion of discontinuation per month
## Numerator: Number of female subjects in cohort who discontinue valproate in the month 
## Denominator: Total number of female subjects in the cohort who used valproate for at least 1 day in the month
## Stratification: country, indication, age group, duration of treatment and reason for discontinuation (adverse drug reaction, pregnancy wish as defined by folic acid use, pregnancy, other)
## Sensitivity analysis 1: Discontinuation period set to 30 days instead of 90
#################################################################################################################
#################################################################################################################
# join tx episodes df with study population to get exit date 
study_population_min <- study_population[,c("person_id", "exit_date")]
df_with_exit_dates <- merge(x=df, y=study_population_min, by="person_id", all.x = TRUE)
# Sort df 
df_with_exit_dates[order(person_id,episode.start,decreasing=TRUE),]
# Create a column with lead values of episode.start column (so that episode.end can be on the same row as the next episode.start of the same person)
df_with_exit_dates[, next.episode.start:= shift(episode.start, type = "lead" ), by = person_id]
# Get difference between the end of an episode and the start of the next episode per person_id.
df_with_exit_dates[, diff_between_episodes := next.episode.start - episode.end]
df_with_exit_dates$exit_date <-as.Date(df_with_exit_dates$exit_date, format="%Y%m%d")
df_with_exit_dates[, diff_episodes_exit_date := exit_date - episode.end]
# if diff_between_episodes is not NA -> means that there is tx_episode following. To see if pt qualifies for count check if value is > 90 -> tx_discontinued = 1
df_with_exit_dates[, tx_discontinued := ifelse(!is.na(diff_between_episodes) & diff_between_episodes >= 90, 1, 
                                               ifelse(!is.na(diff_between_episodes) & diff_between_episodes < 90, 0, NA))]
# if diff_between_episodes is NA -> means that there is no following tx_episode or it is the only one. To see if pt qualifies for count check if diff between episode end and exit date is > 90 -> tx_discontinued = 1
df_with_exit_dates[, tx_discontinued := ifelse(is.na(diff_between_episodes) & diff_episodes_exit_date >= 90, 1, 
                                               ifelse(is.na(diff_between_episodes) & diff_episodes_exit_date< 90, 0, tx_discontinued))]
# Create subset for discontinued records
df_discontinued <- subset(df_with_exit_dates, df_with_exit_dates$tx_discontinued == 1)
# Group by year_month of episode.end.YM
df_discontinued_counts <- df_discontinued[,.N, by = .(year(episode.end), month(episode.end))]
if(is_PHARMO){df_discontinued_counts <- df_discontinued_counts[df_discontinued_counts$year < 2020,]} else {df_discontinued_counts <- df_discontinued_counts[df_discontinued_counts$year < 2021,]}

# Merge with empty df (for counts that do not have counts for all months and years of study)
# Loads denominator file to get min and max dates for empty file
####################################################################################################
denominator_files <- list.files(output_dir, pattern = "denominator.rds")
denominator <- readRDS(paste0(output_dir, denominator_files[[1]][1]))
denominator[, c("Y", "M") := tstrsplit(YM, "-", fixed=TRUE)]
empty_counts <- as.data.table(expand.grid(seq(min(denominator$Y), max(denominator$Y)), seq(1, 12)))
names(empty_counts) <- c("year", "month")
empty_counts$YM <- paste0(empty_counts$year,"-", empty_counts$month)
# Merge empty counts with df_prevalence counts, df_incidence_counts &  df_discontinuation_counts
## df_prevalence_counts
df_prevalence_counts<- as.data.table(merge(x = empty_counts, y = df_prevalence_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
df_prevalence_counts[is.na(df_prevalence_counts[,N]), N:=0]
# sum(df_prevalence_counts$N)
# Masking values less than 5
# Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
df_prevalence_counts$masked <- ifelse(df_prevalence_counts$N < 5 & df_prevalence_counts$N > 0, 1, 0)
# Changes values less than 5 and more than 0 to 5
if (mask == T){counts[counts$masked == 1,]$N <- 5} else {counts[counts$masked == 1,]$N <- counts[counts$masked == 1,]$N }
# Calculates rates
df_prevalence_counts <- within(df_prevalence_counts, YM<- sprintf("%d-%02d", year, month))
df_prevalence_counts <- merge(x = df_prevalence_counts, y = denominator, by = c("YM"), all.x = TRUE)
df_prevalence_counts <-df_prevalence_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
df_prevalence_counts <-df_prevalence_counts[,c("YM", "N", "Freq", "rates", "masked")]

## df_incidence_counts
df_incidence_counts<- as.data.table(merge(x = empty_counts, y = df_incidence_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
df_incidence_counts[is.na(df_incidence_counts[,N]), N:=0]
# sum(df_incidence_counts$N)

# Masking values less than 5
# Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
df_incidence_counts$masked <- ifelse(df_incidence_counts$N < 5 & df_incidence_counts$N > 0, 1, 0)
# Changes values less than 5 and more than 0 to 5
if (mask == T){counts[counts$masked == 1,]$N <- 5} else {counts[counts$masked == 1,]$N <- counts[counts$masked == 1,]$N }
# Calculates rates
df_incidence_counts <- within(df_incidence_counts, YM<- sprintf("%d-%02d", year, month))
df_incidence_counts <- merge(x = df_incidence_counts, y = denominator, by = c("YM"), all.x = TRUE)
df_incidence_counts <-df_incidence_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
df_incidence_counts <-df_incidence_counts[,c("YM", "N", "Freq", "rates", "masked")]

## df_discontinued_counts
df_discontinued_counts<- as.data.table(merge(x = empty_counts, y = df_discontinued_counts, by = c("year", "month"), all.x = TRUE))
# Fills in missing values with 0
df_discontinued_counts[is.na(df_discontinued_counts[,N]), N:=0]
# sum(df_discontinued_counts$N)

# Masking values less than 5
# Creates column that indicates if count is less than 5 (but more than 0) and value needs to be masked 
df_discontinued_counts$masked <- ifelse(df_discontinued_counts$N < 5 & df_discontinued_counts$N > 0, 1, 0)
# Changes values less than 5 and more than 0 to 5
if (mask == T){counts[counts$masked == 1,]$N <- 5} else {counts[counts$masked == 1,]$N <- counts[counts$masked == 1,]$N }
# Calculates rates
df_discontinued_counts <- within(df_discontinued_counts, YM<- sprintf("%d-%02d", year, month))
df_discontinued_counts <- merge(x = df_discontinued_counts, y = denominator, by = c("YM"), all.x = TRUE)
df_discontinued_counts <-df_discontinued_counts[,rates:=as.numeric(N)/as.numeric(Freq)]
df_discontinued_counts <-df_discontinued_counts[,c("YM", "N", "Freq", "rates", "masked")]

# Save files 
saveRDS(df_prevalence_counts,   (paste0(medicines_counts_dir,"/", pop_prefix,"_","prevalence_counts.rds")))
saveRDS(df_incidence_counts,    (paste0(medicines_counts_dir,"/", pop_prefix,"_","incidence_counts.rds")))
saveRDS(df_discontinued_counts, (paste0(medicines_counts_dir,"/", pop_prefix,"_","discontinued_counts.rds")))

