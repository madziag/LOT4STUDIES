# df_march <- `ALL_Valproate_all_preg_starts_during_tx_episodes_counts (1)`
# df_march <- df_march[,c("YM", "N", "Freq")]
# df_april <- ALL_Valproate_all_preg_starts_during_tx_episodes_counts
# df_april <- df_april[,c("YM", "N", "Freq")]

df_march <- `ALL_Retinoid_all_preg_starts_during_tx_episodes_counts (1)`
df_march <- df_march[,c("YM", "N", "Freq")]
df_april <- ALL_Retinoid_all_preg_starts_during_tx_episodes_counts
df_april <- df_april[,c("YM", "N", "Freq")]

setnames(df_april, "N", "N_april")
setnames(df_april, "Freq", "Freq_april")
bound<-cbind(df_march, df_april)

bound[,N_april_m:=ifelse(N_april<5&N_april>0,5,N_april)]
bound[,not_equal:=ifelse(N_april_m==N,0,1)]

bound_not_equal<-bound[not_equal==1,]

bound_not_equal<-bound_not_equal[,-c(4,6,8)]
bound_not_equal<-bound_not_equal[,c("YM", "N", "N_april", "N_april_m", "Freq")]
