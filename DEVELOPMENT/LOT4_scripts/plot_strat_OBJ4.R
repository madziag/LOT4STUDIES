if(!require(rstudioapi)){install.packages("rstudioapi")}
library(rstudioapi)

my_path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(my_path)

# When you open each file, it contains 6 R datasets that contains the stratifications for
# Inc., Prev., and Disc. rates in Obj. 1
# Contracept prior and contraceptive during in Obj. 2,
# Switch to Alt. med. in Obj. 4. 

pattern4 = "alt"

ARS <- list.files(path=paste0(my_path,"/ARS"), pattern=pattern4, ignore.case = T)

BIFAP <- list.files(path=paste0(my_path,"/BIFAP"), pattern=pattern4, ignore.case = T)

CASERTA <- list.files(path=paste0(my_path,"/CASERTA"), pattern=pattern4, ignore.case = T)

CPRD <- list.files(path=paste0(my_path,"/CPRD"), pattern=pattern4, ignore.case = T)

FISABIO <- list.files(path=paste0(my_path,"/FISABIO"), pattern=pattern4, ignore.case = T)

PHARMO <- list.files(path=paste0(my_path,"/PHARMO"), pattern=pattern4, ignore.case = T)

my_names<-c("ARS", "BIFAP", "CASERTA", "CPRD", "FISABIO", "PHARMO")

Obj_4_list<-list(ARS, BIFAP, CASERTA, CPRD, FISABIO, PHARMO)


for(j in 1:length(my_names)){
  dap_path<-(paste0(my_path,"/", my_names[j]))  
  OBJ4_tables<-Obj_4_list[[j]]

for(i in 1:length(OBJ4_tables)){
  my_data<-readRDS(paste0(dap_path,"/", OBJ4_tables[i]))
  
  main_name<-substr(OBJ4_tables[i], 1,nchar(OBJ4_tables[i])-4)
  main_name<-paste0(my_names[j],"_", main_name)
  
  pdf((paste0(my_path,"/Obj_4/plot_age/", main_name, ".pdf")), width=14, height=7)
  
  my_max<-max(my_data$N_overall)
  par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
  
  plot(x=1:nrow(my_data), y=my_data$N_overall, ylim=c(0, my_max), xaxt="n", xlab="", ylab="number of subjects", main=main_name, type="l", lwd=2, cex.main=1.5)
  axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
  lines(x =1:nrow(my_data), y=my_data$N_age_12_21,type="l", lwd=2, col=2 )
  lines(x =1:nrow(my_data), y=my_data$N_age_21_31,type="l", lwd=2, col=3 )
  lines(x =1:nrow(my_data), y=my_data$N_age_31_41,type="l", lwd=2, col=4 )
  lines(x =1:nrow(my_data), y=my_data$N_age_41_56,type="l", lwd=2, col=5 )
  legend("topright", inset=c(-0.13,0), legend=c("Overall","12-21", "21-31", "31-41", "41-56"), col=c(1,2,3,4,5), lwd=2,bty="n", title="Age Group")
  dev.off()
}
}


#need special function for naming
my_substr = function(x,n){
  substr(x,start=n, stop=nchar(x))}

##############################

# redefine the data lists only with Valproate data

my_names<-c("ARS", "BIFAP", "CPRD", "PHARMO")

Obj_4_list<-list(ARS, BIFAP, CPRD, PHARMO)

for(j in 1:length(my_names)){
  dap_path<-(paste0(my_path,"/", my_names[j]))  
  OBJ4_tables<-Obj_4_list[[j]]
  OBJ4_tables<-str_subset(OBJ4_tables, "Valp")
for(i in 1:length(OBJ4_tables)){
  my_data<-readRDS(paste0(dap_path,"/", OBJ4_tables[i]))
  my_vars<-select(my_data,starts_with("N_indication"))
  
  main_name<-substr(OBJ4_tables[i], 1,nchar(OBJ4_tables[i])-4)
  main_name<-paste0(my_names[j],"_", main_name)
  
  pdf((paste0(my_path,"/Obj_4/plot_indication/", main_name, ".pdf")), width=14, height=7)
  
  my_max<-max(my_data$N_indication_unknown)
  par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
  
  plot(x=1:nrow(my_data), y=my_data$N_indication_unknown, ylim=c(0, my_max), xaxt="n", xlab="", ylab="number of subjects", main=main_name, type="n", lwd=2, cex.main=1.5)
  axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
  
  my_vars_names<-colnames(select(my_data,starts_with("N_indication")))
  var_names<-my_substr(x=my_vars_names, n=14)
  my_vars<-as.data.frame(my_vars)
  for(w in 1:length(my_vars)){
    lines(x =1:nrow(my_data), y=my_vars[,w],type="l", lwd=2, col=w )
  }
  
  legend("topright", inset=c(-0.13,0), legend=var_names, col=c(1:5), lwd=2, bty="n", title="Indication")
  dev.off()
}
}