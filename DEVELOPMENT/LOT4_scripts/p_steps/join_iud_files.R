# # CPRD
# iud_files <- list.files(stratified_dir, pattern= "iud", ignore.case = T, full.names = T)
# iud_1 <- fread(iud_files[1]) # iud_diag (from events tables)
# iud_2 <- fread(iud_files[2]) # IUD (from medicines tables )
# 
# all_iud <- rbind(iud_1,iud_2)
# 
# # Masked values events tables # 2020-06/2020-08/2020-10
# # Masked values medicines tables# 2020-09/2020-10
# all_iud<-all_iud[, N_all:=sum(N), by = list(YM)]
# all_iud[,rates:=NULL][,true_value:=NULL][,masked:=NULL][,N:=NULL]
# setnames(all_iud, "N_all", "N")
# all_iud <- all_iud[!duplicated(all_iud)]
# all_iud[, rates:=N/Freq]
# all_iud <- all_iud[,c("YM", "N", "Freq", "rates")]
# for(file in list.files(path = stratified_dir, pattern =".iud", ignore.case = T)){unlink(paste0(stratified_dir, "/", file), recursive = TRUE)}
# write.csv(all_iud, paste0(stratified_dir,"/ALL_Valproate_contra_type_IUD_all_med_use_during_contraception_episodes_counts.csv"))


# PHARMO
iud_files <- list.files(stratified_dir, pattern= "iud", ignore.case = T, full.names = T)
iud_1 <- fread(iud_files[1]) # IUD (from medicines tables)
iud_2 <- fread(iud_files[2]) # iud_diag (from events tables)
iud_3 <- fread(iud_files[3]) # iud (from procedures tables )

all_iud <- rbind(iud_1,iud_2, iud_3)
## No masked values from medicines table 
## Masked values events tables #53 
#2010-01/2010-02/2010-04/2011-04/2011-07/2011-11/2011-12/2012-03/2012-04/2012-06/2012-07/2012-10/2013-01/2013-02/2013-03/2013-04/2013-05/2013-06/2013-07/2013-08/2013-09/2013-10/2013-12/2014-01/2014-03/2014-06/2014-07/2014-09/2015-11/2015-12/2016-01/2016-04/2017-02/2017-04/2017-09/2017-12/2018-02/2018-04/2018-05/2018-07/2018-10/2018-11/2018-12/2019-01/2019-02/2019-04/2019-05/2019-06/2019-07/2019-08/2019-10/2019-11/2019-12 
## Masked values procedures tables #32 
#2010-03/2010-05/2010-07/2010-11/2011-02/2011-04/2011-06/2011-09/2011-10/2011-11/2011-12/2012-02/2012-07/2012-10/2016-08/2016-09/2016-10/2017-06/2017-07/2017-09/2017-11/2017-12/2018-02/2018-03/2018-04/2018-05/2018-06/2018-07/2018-08/2018-09/2019-05/2019-09

all_iud<-all_iud[, N_all:=sum(N), by = list(YM)]
all_iud[,rates:=NULL][,true_value:=NULL][,masked:=NULL][,N:=NULL]
setnames(all_iud, "N_all", "N")
all_iud <- all_iud[!duplicated(all_iud)]
all_iud[, rates:=N/Freq]
all_iud <- all_iud[,c("YM", "N", "Freq", "rates")]
for(file in list.files(path = stratified_dir, pattern =".iud", ignore.case = T)){unlink(paste0(stratified_dir, "/", file), recursive = TRUE)}
write.csv(all_iud, paste0(stratified_dir,"/ALL_Valproate_contra_type_IUD_all_med_use_during_contraception_episodes_counts.csv"))
