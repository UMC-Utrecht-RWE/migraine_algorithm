# table 1

# mig_A 1-7 with different lookbacks (DAP specific)


my_lookback<-c((5*365), 365, 93)

A1_result<-rep(NA,length(my_lookback))

for (i in 1:length(my_lookback)){
  A1_result[i]<-lookback_test( alg_data = Mig_A1, preg_data = my_PREG, lookback = my_lookback[i])
  
}

A2_result<-rep(NA,length(my_lookback))

for (i in 1:length(my_lookback)){
  A2_result[i]<-lookback_test( alg_data = Mig_A2, preg_data = my_PREG, lookback = my_lookback[i])
  
}

A3_result<-rep(NA,length(my_lookback))

for (i in 1:length(my_lookback)){
  A3_result[i]<-lookback_test( alg_data = Mig_A3, preg_data = my_PREG, lookback = my_lookback[i])
  
}

A4_result<-rep(NA,length(my_lookback))

for (i in 1:length(my_lookback)){
  A4_result[i]<-lookback_test( alg_data = Mig_A4, preg_data = my_PREG, lookback = my_lookback[i])
  
}

A5_result<-rep(NA,length(my_lookback))

for (i in 1:length(my_lookback)){
  A5_result[i]<-lookback_test( alg_data = Mig_A5, preg_data = my_PREG, lookback = my_lookback[i])
  
}

A6_result<-rep(NA,length(my_lookback))

for (i in 1:length(my_lookback)){
  A6_result[i]<-lookback_test( alg_data = Mig_A6, preg_data = my_PREG, lookback = my_lookback[i])
  
}

A7_result<-rep(NA,length(my_lookback))

for (i in 1:length(my_lookback)){
  A7_result[i]<-lookback_test( alg_data = Mig_A7, preg_data = my_PREG, lookback = my_lookback[i])
  
}

group_A_algorithms<-c("Mig_A1", " Mig_A2", "Mig_A3", "Mig_A4", "Mig_A5", "Mig_A6", "Mig_A7")

my_col_names<-c("look back 5 years < LMP", "look back 12 months <LMP", "look back 3 months <LMP")

table_1<-as.data.frame(rbind(A1_result, A2_result, A3_result, A4_result, A5_result, A6_result, A7_result))

colnames(table_1)<-my_col_names

table_1<-cbind(group_A_algorithms, table_1)

fwrite(table_1, paste0(output_dir, "/table_1.csv"))
