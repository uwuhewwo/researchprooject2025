library(dplyr)
library(tidyverse)
library(plyr) 

library(tidyr)
library(faersquarterlydata)
library(datawizard)
library(data.table)


faers_data <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified <- unify_tabular_ascii(ascii_list = faers_data)
#summary_faers <- summary_faersdata(faers_data_unified)

#drugs_summary=summary_faers[["prod_ai"]]

faers_data_unified$outcome_all = ifelse(faers_data_unified$outcome_OT=="1"|faers_data_unified$outcome_HO == "1"|faers_data_unified$outcome_DE == "1"|faers_data_unified$outcome_LT == "1"|faers_data_unified$outcome_DS == "1"|faers_data_unified$outcome_RI == "1", TRUE, FALSE)


zoloft <- filter( 
  faers_data_unified,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE") 
zoloft_summary=(summary_faersdata(zoloft))
zoloft_adr=zoloft_summary$reaction
not_zoloft=filter( 
  faers_data_unified,drugname_all != "SERTRALINE" | drugname_all != "ZOLOFT" |drugname_all != "SERTRALINE HYDROCHLORIDE") 
#not_zoloft_summary=summary_faersdata(not_zoloft)
#of patients taking zoloft
length(unique(zoloft[["caseid"]]))
zoloft_unique=data_unique(zoloft, select="primaryid")
#gender/indication breakdown of zoloft takers
zoloft_sex=as.data.frame(table(zoloft_unique$sex))
zoloft_indi=as.data.frame(table(zoloft_unique$indi_pt_all))
zoloft_indi_final=count_indiations(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
report_country=zoloft_unique %>% group_by(reporter_country) %>% count()
occr_country=zoloft_unique %>% group_by(occr_country) %>% count()

#filter for if any outcome is true
zoloft$outcome_all = ifelse(zoloft$outcome_OT=="1"|zoloft$outcome_HO == "1"|zoloft$outcome_DE == "1"|zoloft$outcome_LT == "1"|zoloft$outcome_DS == "1"|zoloft$outcome_RI == "1", TRUE, FALSE)


#indication work

zoloft_unique_druginfo=zoloft_unique$patient_drug

#filtering
conditions_anxiety <- c("Anxiety disorder", "Riya")
replacement_values <- c("NewSita", "NewRiya")



zoloft_unique_druginfo$dose_amt=zoloft_unique_druginfo$dose_amt%>%as.double()
hist(zoloft_unique_druginfo$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique$age)
summary(zoloft_unique$age)
#AE table not zoloft
not_zoloft_adr=as.data.frame(table(not_zoloft$pt))
length(unique(not_zoloft[["caseid"]]))

#not zoloft info
not_zoloft_unique=data_unique(not_zoloft, select="primaryid")
not_zoloft_druglist=as.data.frame(table(not_zoloft$drugname_ps))
not_zoloft_unique_druginfo=rbindlist(not_zoloft_unique$patient_drug)
not_zoloft_sex=as.data.frame(table(not_zoloft_unique$sex))
not_zoloft_indi=as.data.frame(table(not_zoloft_unique$indi_pt_all))
not_zoloft_unique_druginfo$dose_amt=not_zoloft_unique_druginfo$dose_amt%>%as.double()
not_zoloft_indi_final=count_indiations(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),not_zoloft)

hist(not_zoloft_unique_druginfo$dose_amt)
hist(not_zoloft_unique$age)


#function for counting indications, can feed list of common indications into this 
count_indiations <- function(indication, faers_dataset) {
  count=NULL
  new_DF=data.frame(indication=character(), count=double())
  for (i in indication){
    #count=append(i, sum(str_count(zoloft$indi_pt_all,i)))
    new_DF=new_DF %>% add_row(indication=i, count=sum(str_count(faers_dataset$indi_pt_all,i)))
  }
    
  return (new_DF)
}
#calculate adr rate
#cutoff rate for low frequency? for smaller datasets 10 may be too high
adr_rate=function(faers_dataset){
  table=as.data.frame(table(faers_dataset$pt))
  table=table%>%filter(Freq>10)
  table$Var1=as.character(table$Var1)

  total = length(unique(faers_dataset$primaryid))
  print(total)
  new_DF=data.frame(ADRs=table$Var1, rate=table$Freq/total)


  return (new_DF)
}
zoloft_adr_rate=adr_rate(zoloft)
not_zoloft_adr_rate=adr_rate(not_zoloft)
