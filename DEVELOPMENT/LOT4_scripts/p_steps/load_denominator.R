### Creates empty df for expanding counts files (when not all month-year combinations have counts) - uses denominator file min and max year values
# Looks for denominator file in output directory
denominator_file<-list.files(tmp,pattern=paste0(pop_prefix,"_denominator.rds"))
# Loads denominator file
denominator<-readRDS(paste0(tmp,denominator_file))
# Drop columns you do not need
denominator[,studyFUmonths:=NULL]
# Split Y-M variable to year - month columns (for creating empty df & and getting min and max of data available)
denominator[,c("year","month"):= tstrsplit(YM,"-",fixed=TRUE)]
denominator[,year:=as.integer(year)][,month:=as.integer(month)]
# Get min and max data available from denominator
min_data_available<-min(denominator$year)
max_data_available<-max(denominator$year)
# Create empty df using these min and max values
empty_df<-as.data.table(expand.grid(seq(min_data_available,max_data_available),seq(1,12)))
names(empty_df)<-c("year","month")
# Rearrange columns
denominator<-denominator[,c("YM", "year","month","Freq")]




