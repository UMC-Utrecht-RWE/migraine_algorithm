#set look_back parameter

my_lookback<- -365

#test that date of migraine signal is within lookback window
A1_result<-lookback_test(expos_data=Mig_A1, preg_data=my_PREG, lookback=my_lookback)

#test that date of migraine signal is within lookback window
A2_result<-lookback_test(expos_data=Mig_A2, preg_data=my_PREG, lookback=my_lookback)

A3_result<-lookback_test(expos_data=Mig_A3, preg_data=my_PREG, lookback=my_lookback)

A4_result<-lookback_test(expos_data=Mig_A4, preg_data=my_PREG, lookback=my_lookback)

A5_result<-lookback_test(expos_data=Mig_A5, preg_data=my_PREG, lookback=my_lookback)

A6_result<-lookback_test(expos_data=Mig_A6, preg_data=my_PREG, lookback=my_lookback)

A7_result<-lookback_test(expos_data=Mig_A7, preg_data=my_PREG, lookback=my_lookback)

T1_result<-lookback_test(expos_data=Mig_T1, preg_data=my_PREG, lookback=my_lookback)

T2_result<-lookback_test(expos_data=Mig_T2, preg_data=my_PREG, lookback=my_lookback)

T3_result<-lookback_test(expos_data=Mig_T3, preg_data=my_PREG, lookback=my_lookback)

T4_result<-lookback_test(expos_data=Mig_T4, preg_data=my_PREG, lookback=my_lookback)

S1_lookback<-lookback_test(expos_data=Mig_S1, preg_data=my_PREG, lookback=my_lookback)
S1_id_match<-S1_lookback[S1_lookback$person_id%in%A1_result$person_id,]
A1_id_match<-A1_result[A1_result$person_id%in%S1_id_match$person_id,]
S1_result<-rbindlist(list(S1_id_match, A1_id_match), fill=T)[,lapply(.SD, sum, na.rm=T),by="person_id"]

S2_result<-lookback_or_during_test(expos_data = Mig_S2, preg_data = my_PREG, lookback = my_lookback )

#problem here is that the S3 and S3_pr can have different numbers of columns... 

S3_result<-lookback_or_during_test(expos_data = Mig_S3, preg_data = my_PREG, lookback = my_lookback )

S3_pr_result<-lookback_test(expos_data = Mig_S3_pr, preg_data = my_PREG, lookback = my_lookback)

merge(S3_result, S3_pr_result, by="person_id", all=T)[-1]

S4_result<-during_test(expos_data = Mig_S4, preg_data = my_PREG)


A_group<-list(A1_result, A2_result, A3_result, A4_result, A5_result, A6_result, A7_result)

T_group<-list(T1_result, T2_result, T3_result, T4_result, T5_result, T6_result)

S_group<-list(S1_result, S2_result, S3_result, S4_result)



for (j in 1: length(A_group)){
  fwrite(A_group[j], paste0(output_alg_A, "/", "LB_",(-1*my_lookback), "Mig_A",j,".csv"))
}

for (j in 1: length(T_group)){
  fwrite(T_group[j], paste0(output_alg_T, "/","LB_",(-1*my_lookback), "Mig_T",j,".csv"))
}

for (j in 1: length(S_group)){
  fwrite(S_group[j], paste0(output_alg_S, "/","LB_",(-1*my_lookback),"Mig_S", j,".csv"))
}
