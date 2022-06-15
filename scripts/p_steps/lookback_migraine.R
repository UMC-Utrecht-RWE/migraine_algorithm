

#test that date of migraine signal is within lookback window
A1_result<-lookback_test(alg_data=Mig_A1, preg_data=my_PREG, lookback=my_lookback)

#test that date of migraine signal is within lookback window
A2_result<-lookback_test(alg_data=Mig_A2, preg_data=my_PREG, lookback=my_lookback)

A3_result<-lookback_test(alg_data=Mig_A3, preg_data=my_PREG, lookback=my_lookback)

A4_result<-lookback_test(alg_data=Mig_A4, preg_data=my_PREG, lookback=my_lookback)

A5_result<-lookback_test(alg_data=Mig_A5, preg_data=my_PREG, lookback=my_lookback)

A6_result<-lookback_test(alg_data=Mig_A6, preg_data=my_PREG, lookback=my_lookback)

A7_result<-lookback_test(alg_data=Mig_A7, preg_data=my_PREG, lookback=my_lookback)

T1_result<-lookback_test(alg_data=Mig_T1, preg_data=my_PREG, lookback=my_lookback)

T2_result<-lookback_test(alg_data=Mig_T2, preg_data=my_PREG, lookback=my_lookback)

T3_result<-lookback_test(alg_data=Mig_T3, preg_data=my_PREG, lookback=my_lookback)

S1_result<-lookback_test(alg_data=Mig_S1, preg_data=my_PREG, lookback=my_lookback)

S2_result<-lookback_or_during_test(alg_data = Mig_S2, preg_data = my_PREG, lookback = my_lookback )

S3_result<-lookback_or_during_test(alg_data = Mig_S3, preg_data = my_PREG, lookback = my_lookback )

S3_pr_result<-lookback_test(alg_data = Mig_S3_pr, preg_data = my_PREG, lookback = my_lookback)

S4_result<-during_test(alg_data = Mig_S4, preg_data = my_PREG)