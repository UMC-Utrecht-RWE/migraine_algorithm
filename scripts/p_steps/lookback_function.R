#lookback test function

lookback_test<-function(alg_data, preg_data, lookback=-365){
  alg_data$num_date<-as.numeric(Mig_A2$date)
  #long to wide
  alg_data<-dcast(setDT(alg_data), person_id ~ rowid(person_id), value.var = ("num_date"))
  
  alg_data <- alg_data[order(person_id),]  
  
alg_data_preg<- preg_data[preg_data$person_id%in%alg_data$person_id,]
total_preg<-nrow(alg_data_preg)
#select necessary columns
alg_data_preg<-alg_data_preg %>% select(c(pregnancy_id,person_id, pregnancy_start_date ))
# convert dates to numeric
alg_data_preg$num_preg_start<- as.numeric(alg_data_preg$pregnancy_start_date)
#long to wide
preg_start<-dcast(setDT(alg_data_preg), person_id ~ rowid(person_id), value.var = ("num_preg_start"))
preg_start <- preg_start[order(person_id),]


# for each pregnancy w/i eachperson compare each migraine date and each pregnancy date in a vectorized way...
alg_diff_start<-list()
# alg_diff_start<-data.frame(matrix((ncol(alg_data)-1), nrow = nrow(alg_data)))

for(i in 2:ncol(preg_start)){
  my_start_dates<-preg_start[,..i]
  preg_start_mig_diff <- as.data.frame(apply(my_start_dates,2,function(x) alg_data[,2:ncol(alg_data)] - x ))
  alg_diff_start[[i-1]]<-preg_start_mig_diff
}


# test the the alg_data date is within lookback
# store results, one row per person, one column for 1:max pregnancies

alg_result<-matrix(NA, nrow(alg_data), (ncol(preg_start)-1))

for(i in 1:length(alg_diff_start)){
  #this first loop won't take long, it's just 1:max number of pregnancies
  results_start<-alg_diff_start[[i]]
  for (j in 1:nrow(results_start)){
    #this loop is row by row, but only needs to be run once
    if((any(results_start[j,]<=0 & results_start[j,]>=my_lookback, na.rm=T))==T){
      alg_result[j,i]<-1}else{alg_result[j,i]<-0}
  }
}
return(alg_result, total_preg)
}

