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

all_iud<-all_iud[, N_all:=sum(N), by = list(YM)]
all_iud[,rates:=NULL][,true_value:=NULL][,masked:=NULL][,N:=NULL]
setnames(all_iud, "N_all", "N")
all_iud <- all_iud[!duplicated(all_iud)]
all_iud[, rates:=N/Freq]
all_iud <- all_iud[,c("YM", "N", "Freq", "rates")]
for(file in list.files(path = stratified_dir, pattern =".iud", ignore.case = T)){unlink(paste0(stratified_dir, "/", file), recursive = TRUE)}
write.csv(all_iud, paste0(stratified_dir,"/ALL_Valproate_contra_type_IUD_all_med_use_during_contraception_episodes_counts.csv"))
