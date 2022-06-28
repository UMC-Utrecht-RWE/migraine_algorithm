#lookback test function

lookback_test<-function(expos_data, preg_data, lookback=-365){
  if(nrow(expos_data)>0){
  
  # convert mig_dates to numeric
  expos_data$num_date<-as.numeric(expos_data$date)
  #long to wide
  expos_data<-dcast(setDT(expos_data), person_id ~ rowid(person_id), value.var = ("num_date"))
  #order by person_id
  expos_data <- expos_data[order(person_id),]  
  
  # extract pregnancies from same person_ids
  expos_data_preg<- preg_data[preg_data$person_id%in%expos_data$person_id,]
  #select necessary columns
  expos_data_preg<-expos_data_preg %>% select(c(pregnancy_id,person_id, pregnancy_start_date ))
  # convert dates to numeric
  expos_data_preg$num_preg_start<- as.numeric(expos_data_preg$pregnancy_start_date)
  #long to wide
  preg_start<-dcast(setDT(expos_data_preg), person_id ~ rowid(person_id), value.var = ("num_preg_start"))
  preg_start <- preg_start[order(person_id),]
  
  #test that person_id for exposure data and pregnancy data are the same
  if((all(preg_start$person_id==expos_data$person_id))==T){print("person_ids match, OK")}else{print((preg_start$person_id==expos_data$person_id))}
  
  # for each pregnancy w/i eachperson compare each migraine date and each pregnancy date in a vectorized way...
  alg_diff_start<-list()
  
  # loop only over the maximum number of pregnancies (maybe 5?) shouldn't be too slow
  
  for(i in 2:ncol(preg_start)){
    my_start_dates<-preg_start[,..i]
    preg_start_mig_diff <- as.data.frame(apply(my_start_dates,2,function(x) expos_data[,2:ncol(expos_data)] - x ))
    alg_diff_start[[i-1]]<-preg_start_mig_diff
  }
  
  
  # test the the expos_data date is within lookback
  # store results, one row per person, one column for 1:max pregnancies
  
  alg_result<-matrix(NA, nrow(expos_data), (ncol(preg_start)-1))
  
  for(i in 1:length(alg_diff_start)){
    #this first loop won't take long, it's just 1:max number of pregnancies
    results_start<-alg_diff_start[[i]]
    for (j in 1:nrow(results_start)){
      #this loop is row by row, but only needs to be run once
      if((any(results_start[j,]<=-1 & results_start[j,]>=my_lookback, na.rm=T))==T){
        alg_result[j,i]<-1}else{alg_result[j,i]<-0}
    }
  }
  my_results<-cbind(expos_data$person_id, as.data.frame(alg_result))
  preg_num<-vector()
  for(i in 1:ncol(alg_result)){
    preg_num[i]<-paste0("pregnancy_",i)
  }
  colnames(my_results)<-c("person_id",preg_num )
  return(my_results)}
  else{print("no records in migraine algorithm dataframe")
    return("no patients matching this algorithm")}
}


#lookback test function

lookback_or_during_test<-function(expos_data, preg_data, lookback=-365){
  if(nrow(expos_data)>0){
    
    # convert mig_dates to numeric
    expos_data$num_date<-as.numeric(expos_data$date)
    #long to wide
    expos_data<-dcast(setDT(expos_data), person_id ~ rowid(person_id), value.var = ("num_date"))
    #order by person_id
    expos_data <- expos_data[order(person_id),]  
    
    # extract pregnancies from same person_ids
    expos_data_preg<- preg_data[preg_data$person_id%in%expos_data$person_id,]
    #select necessary columns
    expos_data_preg<-expos_data_preg %>% select(c(pregnancy_id,person_id, pregnancy_start_date, pregnancy_end_date ))
    # convert dates to numeric
    expos_data_preg$num_preg_start<- as.numeric(expos_data_preg$pregnancy_start_date)
    expos_data_preg$num_preg_end<- as.numeric(expos_data_preg$pregnancy_end_date)
    #long to wide
    preg_start<-dcast(setDT(expos_data_preg), person_id ~ rowid(person_id), value.var = ("num_preg_start"))
    preg_start <- preg_start[order(person_id),]
    
    preg_end<-dcast(setDT(expos_data_preg), person_id ~ rowid(person_id), value.var = ("num_preg_end"))
    preg_end <- preg_end[order(person_id),]
    
    #test that person_id for exposure data and pregnancy data are the same
    if((all(preg_start$person_id==expos_data$person_id))==T){print("person_ids match start, OK")}else{print((preg_start$person_id==expos_data$person_id))}
    
    #test that person_id for exposure data and pregnancy data are the same
    if((all(preg_end$person_id==expos_data$person_id))==T){print("person_ids match end, OK")}else{print((preg_start$person_id==expos_data$person_id))}
    
    # for each pregnancy w/i eachperson compare each migraine date and each pregnancy date in a vectorized way...
    alg_diff_start<-list()
    alg_diff_end<-list()
    
    for(i in 2:ncol(preg_start)){
      my_start_dates<-preg_start[,..i]
      preg_start_mig_diff <- as.data.frame(apply(my_start_dates,2,function(x) expos_data[,2:ncol(expos_data)] - x ))
      alg_diff_start[[i-1]]<-preg_start_mig_diff
      
      my_end_dates<-preg_end[,..i]
      preg_end_mig_diff <- as.data.frame(apply(my_end_dates,2,function(x) expos_data[,2:ncol(expos_data)] - x ))
      alg_diff_end[[i-1]]<-preg_end_mig_diff
    }
    
    
    # test the the expos_data date is within lookback
    # store results, one row per person, one column for 1:max pregnancies
    
    alg_result<-matrix(NA, nrow(expos_data), (ncol(preg_start)-1))
    
    for(i in 1:length(alg_diff_start)){
      #this first loop won't take long, it's just 1:max number of pregnancies
      results_start<-alg_diff_start[[i]]
      results_end<-alg_diff_end[[i]]
      for (j in 1:nrow(results_start)){
        #this loop is row by row, but only needs to be run once
        if((any(results_end[j,]<=0 & results_start[j,]>=my_lookback, na.rm=T))==T){
          alg_result[j,i]<-1}else{alg_result[j,i]<-0}
      }
    }
    my_results<-cbind(expos_data$person_id, as.data.frame(alg_result))
    preg_num<-vector()
    for(i in 1:ncol(alg_result)){
      preg_num[i]<-paste0("pregnancy_",i)
    }
    colnames(my_results)<-c("person_id",preg_num )
    return(my_results)}
  else{print("no records in migraine algorithm dataframe")
    return("no patients matching this algorithm")}
}



during_test<-function(expos_data, preg_data){
  if(nrow(expos_data)>0){
    
    
    # convert mig_dates to numeric
    expos_data$num_date<-as.numeric(expos_data$date)
    #long to wide
    expos_data<-dcast(setDT(expos_data), person_id ~ rowid(person_id), value.var = ("num_date"))
    #order by person_id
    expos_data <- expos_data[order(person_id),]  
    
    # extract pregnancies from same person_ids
    expos_data_preg<- preg_data[preg_data$person_id%in%expos_data$person_id,]
    #select necessary columns
    expos_data_preg<-expos_data_preg %>% select(c(pregnancy_id,person_id, pregnancy_start_date, pregnancy_end_date ))
    # convert dates to numeric
    expos_data_preg$num_preg_start<- as.numeric(expos_data_preg$pregnancy_start_date)
    expos_data_preg$num_preg_end<- as.numeric(expos_data_preg$pregnancy_end_date)
    #long to wide
    preg_start<-dcast(setDT(expos_data_preg), person_id ~ rowid(person_id), value.var = ("num_preg_start"))
    preg_start <- preg_start[order(person_id),]
    
    preg_end<-dcast(setDT(expos_data_preg), person_id ~ rowid(person_id), value.var = ("num_preg_end"))
    preg_end <- preg_end[order(person_id),]
    
    #test that person_id for exposure data and pregnancy data are the same
    if((all(preg_start$person_id==expos_data$person_id))==T){print("person_ids match start, OK")}else{print((preg_start$person_id==expos_data$person_id))}
    
    #test that person_id for exposure data and pregnancy data are the same
    if((all(preg_end$person_id==expos_data$person_id))==T){print("person_ids match end, OK")}else{print((preg_start$person_id==expos_data$person_id))}
    
    
    # for each pregnancy w/i eachperson compare each migraine date and each pregnancy date in a vectorized way...
    alg_diff_start<-list()
    alg_diff_end<-list()
    
    for(i in 2:ncol(preg_start)){
      my_start_dates<-preg_start[,..i]
      preg_start_mig_diff <- as.data.frame(apply(my_start_dates,2,function(x) expos_data[,2:ncol(expos_data)] - x ))
      alg_diff_start[[i-1]]<-preg_start_mig_diff
      
      my_end_dates<-preg_end[,..i]
      preg_end_mig_diff <- as.data.frame(apply(my_end_dates,2,function(x) expos_data[,2:ncol(expos_data)] - x ))
      alg_diff_end[[i-1]]<-preg_end_mig_diff
    }
    
    
    # test the the expos_data date is within lookback
    # store results, one row per person, one column for 1:max pregnancies
    
    alg_result<-matrix(NA, nrow(expos_data), (ncol(preg_start)-1))
    
    for(i in 1:length(alg_diff_start)){
      #this first loop won't take long, it's just 1:max number of pregnancies
      results_start<-alg_diff_start[[i]]
      results_end<-alg_diff_end[[i]]
      for (j in 1:nrow(results_start)){
        #this loop is row by row, but only needs to be run once
        if((any(results_end[j,]<=0 & results_start[j,]>=0, na.rm=T))==T){
          alg_result[j,i]<-1}else{alg_result[j,i]<-0}
      }
    }
    my_results<-cbind(expos_data$person_id, as.data.frame(alg_result))
    preg_num<-vector()
    for(i in 1:ncol(alg_result)){
      preg_num[i]<-paste0("pregnancy_",i)
    }
    colnames(my_results)<-c("person_id",preg_num )
    return(my_results)}
  else{print("no records in migraine algorithm dataframe")
    return("no patients matching this algorithm")}
}




