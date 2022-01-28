#BIFAP plots per outcome with all regions

#load packages

if(!require(lubridate)){install.packages("lubridate")}
suppressPackageStartupMessages(library(lubridate))

if(!require(data.table)){install.packages("data.table")}
suppressPackageStartupMessages(library(data.table))

if(!require(dplyr)){install.packages("dplyr")}
suppressPackageStartupMessages(library(dplyr))

if(!require(viridis)){install.packages("viridis")}
suppressPackageStartupMessages(library(viridis))


if(!require(RColorBrewer)){install.packages("RColorBrewer")}
suppressPackageStartupMessages(library(RColorBrewer))

display.brewer.all()


#desired output is line graphs for each outcome rate and one line per region, color coded
#data is organized with each counts of each outcome in a folder, containing all regions
#i in 1:length(my_folders)
#containing 
#j in 1:length(list.files(my_folders[i]))


all_regions_dir<-dirname(rstudioapi::getSourceEditorContext()$path)

my_folders<-(list.files(all_regions_dir, pattern="counts"))

#plot directory to deposit loop output
dir.create(paste0(all_regions_dir,"/plots/"))
bifap_plots<-(paste0(all_regions_dir,"/plots/"))
#need dates vector

my_date_df<-fread(paste0(output_dir, "preliminary_counts/csv_files/denominator.csv"))

my_dates<-my_date_df$YM

my_regions<-c("AR", "AS","CA", "CL","CM","CN","MA","MU","NA")

my_pallette<-RColorBrewer::brewer.pal(n =length(my_regions), name="Set1" )

region_key<-as.data.frame(cbind(my_pallette, my_regions))

for(i in 1:length(my_folders)){
  #read in list of RDS files
  my_files<-grep(list.files(path=paste0(all_regions_dir,"/",my_folders[i])), pattern='Pooled', invert=TRUE, value=TRUE)
  main_name<-substr(my_folders[i], 1,nchar(my_folders[i])-7)
  pdf((paste0(bifap_plots, main_name,".pdf")), width=8, height=4)
  plot(x=1:nrow(my_date_df),y=rep(1, nrow(my_date_df)), main=main_name, type = "n",xaxt="n", xlab="", ylab="rates", ylim = c(0,0.03))
  axis(1, at=1:nrow(my_date_df), as.character(my_dates), las=2)
  legend("topright", legend = my_regions, col=my_pallette, lwd=2, bty="n", cex=0.75)
  for(j in 1:length(my_files)){
    my_data<-fread(paste0(all_regions_dir,"/",my_folders[i],"/",my_files[j]))
    my_perc<-my_data$rates*100
    my_reg<-substr(my_files[j],1,2)
    my_col<-as.character(region_key$my_pallette[region_key$my_regions==my_reg])
    my_jit<-sample(x=(seq(0,0.001, by=0.00001)), size = 1)
    lines(x=(1:nrow(my_data)), y=((my_data$rates)+my_jit), col=my_col, lwd=2)
  }
  dev.off()
}

#jitterred lines so that overlapping regions are distinguishable

#don't know origin of x y lengths differ error (they are the same) and the plots are made fine anyway, so it's OK I think