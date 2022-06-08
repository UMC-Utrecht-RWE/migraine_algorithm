#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#8/6/2022


# This script will detect migraine, migraine type and migraine severity using 
# several algorithms, with hierarchical sensitivity and specificity

#Group A migraine algorithm

my_path<-preg_folder

my_MED_tables<-list.files(path=my_path, pattern = "MEDICINES_")
my_PROC_tables<-list.files(path=my_path, pattern = "PROCEDURES_")
my_EVENT_tables<-list.files(path=my_path, pattern = "EVENTS_")

################################################

load(paste0(path_CDM,"D3_pregnancy_final.RData"))

my_PREG<-D3_pregnancy_final
#################################################
#
# first group (A): presence of migraine (yes/no)
#
#################################################
# https://stackoverflow.com/questions/61088740/concern-with-startswith-and-multiple-patterns-in-r
#Mig_A1: at least of the following diagnostic codes: c("G43", 346, "F26","N89")

Mig_A1_codes<-c("G43", "346", "F26","N89")

# my_dt_EV[Reduce(`|`, lapply(Mig_A1_codes, startsWith, x = my_dt_EV$event_code))]


Mig_A1_ID<-list()
Mig_A1_Date<-list()


for(j in 1:length(my_EVENT_tables)){
  my_dt_EV<-fread(paste0(my_path, my_EVENT_tables[j]))
  Mig_A1_ID[j]<-list(my_dt_EV$person_id[Reduce(`|`, lapply(Mig_A1_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
  Mig_A1_Date[j]<-list(my_dt_EV$start_date_record[Reduce(`|`, lapply(Mig_A1_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
}


Mig_A1_ID<-unlist(Mig_A1_ID)
Mig_A1_Date<-unlist(Mig_A1_Date)
Mig_A1<-as.data.frame(cbind(Mig_A1_ID, Mig_A1_Date))

colnames(Mig_A1)<-c("person_id", "date")
Mig_A1$date<-as.Date(Mig_A1$date, format = "%Y%m%d")


###########################################################################################################

#Mig_A2: tryptan ATC: N02CC%%
#partial string match

Mig_A2_ID<-list()
Mig_A2_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Mig_A2_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,"N02CC")]
  Mig_A2_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,"N02CC")]
}

Mig_A2<-as.data.frame(cbind(unlist(Mig_A2_ID), unlist(Mig_A2_Date)))

colnames(Mig_A2)<-c("person_id", "date")
Mig_A2$date<-as.Date(Mig_A2$date, format = "%Y%m%d")


################################################################################

#Mig_A3: migraine procedure ATC: M03AX01
if(length(my_PROC_tables>0)){
  
  Mig_A3_ID<-list()
  Mig_A3_Date<-list()
  
  for (j in 1:length(my_PROC_tables)){
    my_dt_PROC<-fread(paste0(my_path, my_PROC_tables[j]))
    Mig_A3_ID[[j]]<- my_dt_PROC$person_id[my_dt_PROC$procedure_code=="M03AX01"]
    Mig_A3_Date[[j]] <- my_dt_PROC$procedure_date[my_dt_MED$medicinal_product_atc_code=="M03AX01"]
  }
  
  Mig_A3<-as.data.frame(cbind(unlist(Mig_A3_ID), unlist(Mig_A3_Date)))
  colnames(Mig_A3)<-c("person_id", "date")} else{print("No PROCEDURES table for Mig_A3")
    Mig_A3<-data.frame(person_id=character(), date=numeric())}

Mig_A3$date<-as.Date(Mig_A3$date, format = "%Y%m%d")

#Mig_A4: MigA1 AND/OR MigA2
Mig_A4<-as.data.frame(inner_join(Mig_A1, Mig_A2))

#Mig_A5: MigA1 AND/OR Mig_A3

Mig_A5<-inner_join(Mig_A1 , Mig_A3)

#Mig_A6: MigA2 AND/OR MigA3

Mig_A6<-inner_join(Mig_A2, Mig_A3)

#Mig_A7: MigA1 AND/OR MigA2 AND/OR Mig_A3

#needs 2 steps because of multiple joins
Mig_A7<-inner_join(Mig_A1 , Mig_A2)

  #second join
  Mig_A7<-inner_join(Mig_A7, Mig_A3)


####################################################################
# save output
A_group<-list(Mig_A1, Mig_A2, Mig_A3, Mig_A4, Mig_A5, Mig_A6, Mig_A7)

for (j in 1: length(A_group)){
  fwrite(A_group[j], paste0(output_alg_A, "/", j,".csv"))
}

