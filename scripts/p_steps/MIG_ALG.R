#Author: Ema Alsina, M.Sc.
#e.m.alsina-2@umcutrecht.nl
#University Medical Center Utrecht
#28/3/2022


# This script will detect migraine, migraine type and migraine severity using 
#several algorithms, with hierarchical sensitivity and specificity

#to.run
#paths (create and store)
#packages: dplyr, stringr 


# parameters: looking for migraine in pregnancy

my_path<-path_CDM


my_MED_tables<-list.files(path=my_path, pattern = "MEDICINES_")
my_PROC_tables<-list.files(path=my_path, pattern = "PROCEDURES_")
my_EVENT_tables<-list.files(path=my_path, pattern = "EVENTS_")

################################################
# load(paste0(path_CDM,"D3_pregnancy_final.RData"))
# my_PREG<-as.data.table(D3_pregnancy_final)
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


#################################################
#
# second group (T): type of migraine (w/o aura, w/aura, migrainosus, complicated, other, unspecified)
#
#################################################

# Mig_T1 without aura ICD-10: G43.0; ICD-9: any(346.1, 346.7), RCD: F261


Mig_T1_codes<-c("G43.0", "346.1","346.7", "F261")

Mig_T1_ID<-list()
Mig_T1_Date<-list()


for(j in 1:length(my_EVENT_tables)){
  my_dt_EV<-fread(paste0(my_path, my_EVENT_tables[j]))
  Mig_T1_ID[j]<-list(my_dt_EV$person_id[Reduce(`|`, lapply(Mig_T1_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
  Mig_T1_Date[j]<-list(my_dt_EV$start_date_record[Reduce(`|`, lapply(Mig_T1_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
}


Mig_T1_ID<-unlist(Mig_T1_ID)
Mig_T1_Date<-unlist(Mig_T1_Date)
Mig_T1<-as.data.frame(cbind(Mig_T1_ID, Mig_T1_Date))
colnames(Mig_T1)<-c("person_id", "date")

Mig_T1_ID<-unlist(Mig_T1_ID)
Mig_T1_Date<-unlist(Mig_T1_Date)
Mig_T1<-as.data.frame(cbind(Mig_T1_ID, Mig_T1_Date))
colnames(Mig_T1)<-c("person_id", "date")

#Mig_T2: with aura: "G43.1", "346.0", "346.5" , "346.6", "F260"

Mig_T2_codes<-c("G43.1", "346.0", "346.5" , "346.6", "F260")

Mig_T2_ID<-list()
Mig_T2_Date<-list()


for(j in 1:length(my_EVENT_tables)){
  my_dt_EV<-fread(paste0(my_path, my_EVENT_tables[j]))
  Mig_T2_ID[j]<-list(my_dt_EV$person_id[Reduce(`|`, lapply(Mig_T2_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
  Mig_T2_Date[j]<-list(my_dt_EV$start_date_record[Reduce(`|`, lapply(Mig_T2_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
}


Mig_T2_ID<-unlist(Mig_T2_ID)
Mig_T2_Date<-unlist(Mig_T2_Date)
Mig_T2<-as.data.frame(cbind(Mig_T2_ID, Mig_T2_Date))
colnames(Mig_T2)<-c("person_id", "date")

#Mig_T3: migrainosus

Mig_T3_codes<-c("G43.2", "346.12", "X007R")

Mig_T3_ID<-list()
Mig_T3_Date<-list()

for(j in 1:length(my_EVENT_tables)){
  my_dt_EV<-fread(paste0(my_path, my_EVENT_tables[j]))
  Mig_T3_ID[j]<-list(my_dt_EV$person_id[Reduce(`|`, lapply(Mig_T3_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
  Mig_T3_Date[j]<-list(my_dt_EV$start_date_record[Reduce(`|`, lapply(Mig_T3_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
}

Mig_T3_ID<-unlist(Mig_T3_ID)
Mig_T3_Date<-unlist(Mig_T3_Date)
Mig_T3<-as.data.frame(cbind(Mig_T3_ID, Mig_T3_Date))
colnames(Mig_T3)<-c("person_id", "date")

#Mig_T4: complicated
# https://snomedbrowser.com/Codes/Details/193039006

Mig_T4_codes<-c("G43.3", "F26y3")

Mig_T4_ID<-list()
Mig_T4_Date<-list()

for(j in 1:length(my_EVENT_tables)){
  my_dt_EV<-fread(paste0(my_path, my_EVENT_tables[j]))
  Mig_T4_ID[j]<-list(my_dt_EV$person_id[Reduce(`|`, lapply(Mig_T4_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
  Mig_T4_Date[j]<-list(my_dt_EV$start_date_record[Reduce(`|`, lapply(Mig_T4_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
}

Mig_T4_ID<-unlist(Mig_T4_ID)
Mig_T4_Date<-unlist(Mig_T4_Date)
Mig_T4<-as.data.frame(cbind(Mig_T4_ID, Mig_T4_Date))
colnames(Mig_T4)<-c("person_id", "date")


#Mig_T5: other

Mig_T5_codes<-c("G43.8", "346.8", "F262","Fyu53", "F26y", "X0070")

Mig_T5_ID<-list()
Mig_T5_Date<-list()

for(j in 1:length(my_EVENT_tables)){
  my_dt_EV<-fread(paste0(my_path, my_EVENT_tables[j]))
  Mig_T5_ID[j]<-list(my_dt_EV$person_id[Reduce(`|`, lapply(Mig_T5_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
  Mig_T5_Date[j]<-list(my_dt_EV$start_date_record[Reduce(`|`, lapply(Mig_T5_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
}


Mig_T5_ID<-unlist(Mig_T5_ID)
Mig_T5_Date<-unlist(Mig_T5_Date)
Mig_T5<-as.data.frame(cbind(Mig_T5_ID, Mig_T5_Date))
colnames(Mig_T5)<-c("person_id", "date")

#Mig_T6: unspecified


Mig_T6_codes<-c("G43.9", "346.9","346.2", "N89", "F26z")

Mig_T6_ID<-list()
Mig_T6_Date<-list()

for(j in 1:length(my_EVENT_tables)){
  my_dt_EV<-fread(paste0(my_path, my_EVENT_tables[j]))
  Mig_T6_ID[j]<-list(my_dt_EV$person_id[Reduce(`|`, lapply(Mig_T6_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
  Mig_T6_Date[j]<-list(my_dt_EV$start_date_record[Reduce(`|`, lapply(Mig_T6_codes, startsWith, x = as.character(my_dt_EV$event_code)))])
}


Mig_T6_ID<-unlist(Mig_T6_ID)
Mig_T6_Date<-unlist(Mig_T6_Date)
Mig_T6<-as.data.frame(cbind(Mig_T6_ID, Mig_T6_Date))
colnames(Mig_T6)<-c("person_id", "date")

#################################################
#
# third group (S): severity of migraine (mild, moderate, severe, very severe)
# using ATC from MEDICINES
#
#################################################

#Mig_S1: mild: Mig_A1 positive AND one of the following ATC: "M01A%%%", "N02BE01", "N05AB04"


Mig_S1_codes<-c("M01A", "N02BE01", "N05AB04")

Mig_S1_ID<-list()
Mig_S1_Date<-list()

for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Mig_S1_ID[j]<-list(my_dt_MED$person_id[Reduce(`|`, lapply(Mig_S1_codes, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code)))])
  Mig_S1_Date[j]<-list(my_dt_MED$date_dispensing[Reduce(`|`, lapply(Mig_S1_codes, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code)))])
}


Mig_S1_ID<-unlist(Mig_S1_ID)
Mig_S1_Date<-unlist(Mig_S1_Date)
Mig_S1<-as.data.frame(cbind(Mig_S1_ID, Mig_S1_Date))
colnames(Mig_S1)<-c("person_id", "date")
#inner join with Mig_A1
Mig_S1<-inner_join(Mig_S1, Mig_A1, by="person_id")

#Mig_S2: moderate: Mig_A2 PRIOR to, AND/OR DURING pregnancy 
#need to match with person_id and pregnancy algorithm dates
Mig_S2<-Mig_A2
# format dates to match pregnancy data


# Mig_A2$date<-as.Date(Mig_A2$date,format = "%Y%m%d")
# A2_preg<-my_PREG[my_PREG$person_id%in%Mig_A2$person_id,]
# A2_Mig<-Mig_A2[Mig_A2$person_id%in%my_PREG$person_id,]
# Mig_S2<- Mig_A2[(Mig_A2$date<A2_preg$pregnancy_end_date)]


#Mig_S3: severe: N02CC01 injection PRIOR to, AND/OR DURING pregnancy 
#AND/OR prophylaxis PRIOR but NOT DURING pregnancy 

#HELP need clarification on "prophylaxis prior to pregnancy"
# table 5 drug utilization SAP

#split into 2 sections

Mig_S3_ID<-list()
Mig_S3_Date<-list()

for (j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Mig_S3_ID[[j]] <- my_dt_MED$person_id[startsWith(my_dt_MED$medicinal_product_atc_code,"N02CC01")]
  Mig_S3_Date[[j]] <- my_dt_MED$date_dispensing [startsWith(my_dt_MED$medicinal_product_atc_code,"N02CC01")]
}

Mig_S3<-as.data.frame(cbind(unlist(Mig_S3_ID), unlist(Mig_S3_Date)))
colnames(Mig_S3)<-c("person_id", "date")
#AND/OR prophylaxis PRIOR but NOT DURING pregnancy

Mig_S3_pr_codes<-c("C07AB02","C07AA05","N06AA09","C09CA06", "N03AX11", "N03AG01","M03AX01", "N02CX01", 
             "N02CX02","C09AA03","C08DA01", "N02CD",  "N02CD01" ,	"N02CD02", "N02CD03" )

Mig_S3_pr_ID<-list()
Mig_S3_pr_Date<-list()

for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Mig_S3_pr_ID[j]<-list(my_dt_MED$person_id[Reduce(`|`, lapply(Mig_S3_pr_codes, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code)))])
  Mig_S3_pr_Date[j]<-list(my_dt_MED$date_dispensing[Reduce(`|`, lapply(Mig_S3_pr_codes, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code)))])
}

Mig_S3_pr<-as.data.frame(cbind(unlist(Mig_S3_pr_ID), unlist(Mig_S3_pr_Date)))
colnames(Mig_S3_pr)<-c("person_id", "date")

#test date<start of pregnancy

# Mig_S4 very severe:Mig_A7 AND one or more (N02CD%%, N02CX%%, C07A%%%, C09A%%%, C09C%%%, 
#M03AX%%, N03A%%%, N06AA%%, N06AX16, N07CA03, N02CB%%, C08DA%%) DURING pregnancy

Mig_S4_codes<-c("N02CD", "N02CX", "C07A", "C09A", "C09C", "M03AX",
                "N03A", "N06AA", "N06AX16", "N07CA03", "N02CB", "C08DA")

Mig_S4_ID<-list()
Mig_S4_Date<-list()


for(j in 1:length(my_MED_tables)){
  my_dt_MED<-fread(paste0(my_path, my_MED_tables[j]))
  Mig_S4_ID[j]<-list(my_dt_MED$person_id[Reduce(`|`, lapply(Mig_S4_codes, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code)))])
  Mig_S4_Date[j]<-list(my_dt_MED$date_dispensing[Reduce(`|`, lapply(Mig_S4_codes, startsWith, x = as.character(my_dt_MED$medicinal_product_atc_code)))])
}


Mig_S4_ID<-unlist(Mig_S4_ID)
Mig_S4_Date<-unlist(Mig_S4_Date)
Mig_S4<-as.data.frame(cbind(Mig_S4_ID, Mig_S4_Date))
colnames(Mig_S4)<-c("person_id", "date")

# Mig_S4<-Mig_S4[Mig_S4$date>"lmp"&Mig_S4_Date<"birth_date"]

A_group<-list(Mig_A1, Mig_A2, Mig_A3)

T_group<-list(Mig_T1, Mig_T2, Mig_T3, Mig_T4, Mig_T5, Mig_T6)

S_group<-list(Mig_S1, Mig_S2, Mig_S3, Mig_S4)

for (j in 1: length(A_group)){
  fwrite(A_group[j], paste0(output_alg, "/Mig_A", j,".csv"))
}

for (j in 1: length(T_group)){
  fwrite(T_group[j], paste0(output_alg, "/Mig_T", j,".csv"))
}

for (j in 1: length(S_group)){
  fwrite(S_group[j], paste0(output_alg, "/Mig_S", j,".csv"))
}


