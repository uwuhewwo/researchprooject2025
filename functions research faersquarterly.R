#replaces colnmn age_yr, leaves col age intact
#MAYBE CHCANGE TO append new age col?


#' replace_age_outliers
#'
#' @param faers_dataset dataframe in the unified FAERS tabular format. Output by function unify_tabular_ascii

#'
#' @returns faers_dataset dataframe
#' @export faersquarterlydata
#'
#' @examples 
#' 
replace_age_outliers=function(faers_dataset){
  
  median_age=median(faers_dataset$age_YR[is.na(faers_dataset$age_YR)==FALSE])
  #faers_dataset$age_fixed=faers_dataset$age_YR
  #faers_dataset$age_fixed[is.na(faers_dataset$age_fixed)==TRUE]=median_age
  faers_dataset$age_YR[(faers_dataset$age_YR)<0]=median_age
  faers_dataset$age_YR[(faers_dataset$age_YR)>110]=median_age
  #faers_dataset = faers_dataset%>%relocate(age_fixed, .after=age_cod)
  #print(faers_dataset$age_YR[is.na(faers_dataset$age_YR)==TRUE])

  return (faers_dataset)
}
#FIX
return_dosage_outliers=function(faers_dataset,max_dose){
  drugz= rbindlist(faers_dataset$patient_drug)
  new_dataset=faers_dataset$age_YR[is.na(faers_dataset$age_YR)==TRUE]=median(faers_dataset$age_YR[is.na(faers_dataset$age_YR)==FALSE])
  new_dataset=faers_dataset$age_YR[(faers_dataset$age_YR)<0]=median(faers_dataset$age_YR[(faers_dataset$age_YR)<0])
  new_dataset=faers_dataset$age_YR[(faers_dataset$age_YR)>110]=median(faers_dataset$age_YR[(faers_dataset$age_YR)>110])
  return (new_dataset)
}

return_ROR=function(faers_dataset, drug_of_interest){
  #filter faers by drug of interest/not drug of interest, getting drug synonyms first but im not doing that now
  #fix the hard coded system aand get the drug synonym thing working eventually
  doi=filter( 
    faers_dataset,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE") 
  #print(doi)
  not_doi=filter( 
    faers_data_unified,drugname_all != "SERTRALINE" | drugname_all != "ZOLOFT" |drugname_all != "SERTRALINE HYDROCHLORIDE") 
  
  ##doi=doi%>%filter(Freq>10)
  #not_doi=not_doi%>%filter(Freq>10)
  doitable=as.data.frame(table(doi$pt))
  #print(doitable)
  #doitable=doitable%>%filter(Freq>10)
  doitable$Var1=as.character(doitable$Var1)
  
  
  not_doitable=as.data.frame(table(not_doi$pt))
  #not_doitable=not_doitable%>%filter(Freq>10)
  not_doitable$Var1=as.character(not_doitable$Var1)
  doi_overlapping=as.data.frame(table(doi$pt[doi$pt %in% not_doi$pt]))
  not_doi_overlapping=as.data.frame(table(not_doi$pt[not_doi$pt %in% doi$pt]))
  ROR_table=data.frame(ADRs=doitable$Var1)
  ROR_table$ROR=estimate_ror(doi_overlapping$Freq, not_doi_overlapping$Freq,sum(doi_overlapping$Freq), sum(doi_overlapping$Freq) + sum(not_doi_overlapping$Freq))$estimate
  interval=(estimate_ror(doi_overlapping$Freq, not_doi_overlapping$Freq,sum(doi_overlapping$Freq), sum(doi_overlapping$Freq) + sum(not_doi_overlapping$Freq)))
  ROR_table$lower=interval$ic[1:(length(interval$ic)/2)]
  ROR_table$upper=interval$ic[(length(interval$ic)/2+1):length(interval$ic)]
  
  #print(estimate_ror(doi_overlapping$Freq, not_doi_overlapping$Freq,sum(doi_overlapping$Freq), sum(doi_overlapping$Freq) + sum(not_doi_overlapping$Freq)))
  #print(not_doi_overlapping)
  #print(sum(doi_overlapping$Freq))
  #print(not_doi_overlapping$Freq)
  return (ROR_table)
}
# make another function like this but calcs based on dataframes put in it


#' ROR_comparison
#'
#' @param ds1 
#' @param ds2 
#'
#' @returns
#' @export
#'
#' @examples
ROR_comparison=function(ds1, ds2){
  adrs1=as.data.frame(table(ds1$pt))
  adrs2=as.data.frame(table(ds2$pt))
  
  overlapping=merge(adrs1,adrs2, by.x="Var1",by.y="Var1")
  
  
  ROR_table=data.frame(ADRs=overlapping$Var1)

  ROR_table$ROR=estimate_ror(overlapping$Freq.x, overlapping$Freq.y,sum(overlapping$Freq.x), sum(overlapping$Freq.x) + sum(overlapping$Freq.y))$estimate
    
  interval=estimate_ror(overlapping$Freq.x, overlapping$Freq.y,sum(overlapping$Freq.x), sum(overlapping$Freq.x) + sum(overlapping$Freq.y))$ic

  ROR_table$lower=interval[1:(length(interval)/2)]
  
  ROR_table$upper=interval[(length(interval)/2+1):length(interval)]
  
  return (ROR_table)
}

PRR_comparison=function(ds1, ds2){
  adrs1=as.data.frame(table(ds1$pt))
  adrs2=as.data.frame(table(ds2$pt))
  
  overlapping=merge(adrs1,adrs2, by.x="Var1",by.y="Var1")
  
  
  PRR_table=data.frame(ADRs=overlapping$Var1)
  
  PRR_table$PRR=estimate_prr(overlapping$Freq.x, overlapping$Freq.y,sum(overlapping$Freq.x), sum(overlapping$Freq.x) + sum(overlapping$Freq.y))$estimate
  
  interval=estimate_prr(overlapping$Freq.x, overlapping$Freq.y,sum(overlapping$Freq.x), sum(overlapping$Freq.x) + sum(overlapping$Freq.y))$ic
  
  PRR_table$lower=interval[1:(length(interval)/2)]
  
  PRR_table$upper=interval[(length(interval)/2+1):length(interval)]
  
  return (PRR_table)
}


#takes faers datasets filtered to include and exclude drug of interest
return_PRR=function(faers_dataset_1, faers_dataset_2){
  #
  
  #doi=doi%>%filter(Freq>10)
  #not_doi=not_doi%>%filter(Freq>10)
  doitable=as.data.frame(table(doi$pt))
  #print(doitable)
  #doitable=doitable%>%filter(Freq>10)
  doitable$Var1=as.character(doitable$Var1)
  
  
  not_doitable=as.data.frame(table(not_doi$pt))
  #not_doitable=not_doitable%>%filter(Freq>10)
  not_doitable$Var1=as.character(not_doitable$Var1)
  doi_overlapping=as.data.frame(table(doi$pt[doi$pt %in% not_doi$pt]))
  not_doi_overlapping=as.data.frame(table(not_doi$pt[not_doi$pt %in% doi$pt]))
  PRR_table=data.frame(ADRs=doitable$Var1)
  PRR_table$PRR=estimate_prr(doi_overlapping$Freq, not_doi_overlapping$Freq,sum(doi_overlapping$Freq), sum(doi_overlapping$Freq) + sum(not_doi_overlapping$Freq))$estimate
  interval=(estimate_prr(doi_overlapping$Freq, not_doi_overlapping$Freq,sum(doi_overlapping$Freq), sum(doi_overlapping$Freq) + sum(not_doi_overlapping$Freq)))
  PRR_table$lower=interval$ic[1:(length(interval$ic)/2)]
  PRR_table$upper=interval$ic[(length(interval$ic)/2+1):length(interval$ic)]
  
  #print(estimate_prr(doi_overlapping$Freq, not_doi_overlapping$Freq,sum(doi_overlapping$Freq), sum(doi_overlapping$Freq) + sum(not_doi_overlapping$Freq)))
  #print(not_doi_overlapping)
  #print(sum(doi_overlapping$Freq))
  #print(not_doi_overlapping$Freq)
  return (PRR_table)
}

#function for counting indications, feed list of common indications into this 
count_indications <- function(indication, faers_dataset) {
  count=NULL
  faers_unique=distinct(faers_dataset, primaryid, .keep_all = TRUE)
  new_DF=data.frame(indication=character(), count=double(),by_case=double())
  for (i in indication){
    #count=append(i, sum(str_count(zoloft$indi_pt_all,i)))
    new_DF=new_DF %>% add_row(indication=i, count=sum(str_count(faers_unique$indi_pt_all,i)),by_case=sum(str_count(faers_dataset$indi_pt_all,i)))
    
  
  }
  
  
  new_DF$pct=100*new_DF$count/length(unique(faers_unique[["caseid"]]))
  new_DF$pct_cases=100*new_DF$by_case/length((faers_dataset[["caseid"]]))
  return (new_DF)
}
#calculate adr rate
#cutoff rate for low frequency? for smaller datasets 10 may be too high
adr_rate=function(faers_dataset,cutoff=10){
  table=as.data.frame(table(faers_dataset$pt))
  table=table%>%filter(Freq>cutoff)
  table$Var1=as.character(table$Var1)
  
  total = length(unique(faers_dataset$primaryid))
  
  new_DF=data.frame(ADRs=table$Var1, rate=table$Freq/total)
  
  
  return (new_DF)
}
#patient no/total cases reported, reporting country
#ask indu how to use library functions in own functions?
return_demographic_table=function(faers_dataset){
  
  faers_dataset$sex[faers_dataset$sex=="UNK"]=""
  faers_unique=distinct(faers_dataset, caseid, .keep_all = TRUE)
  sex_table= as.data.frame(table(faers_unique$sex))
  
  levels(sex_table$Var1)=c("Unknown","Female","Male")
  colnames(sex_table)=c("Variable", "Count")
  sex_table$Category=c("Gender","Gender","Gender")
  sex_table=sex_table%>%relocate(Category)
  sex_table$Percent=(sex_table$Count/sum(sex_table$Count))*100
  
  wt_table=as.data.frame(table(faers_unique$wt))
  wt_table$Var1=as.numeric(wt_table$Var1)
  #print(sum(wt_table$Freq[wt_table$Var1<50]))
  bin_wt_table=data.frame(Category=c("Weight", "Weight","Weight","Weight"), Variable=c("Unknown","<50",">100","50-100"))

  bin_wt_table$Count=c(sum(is.na(faers_unique$wt)),sum(wt_table$Freq[wt_table$Var1<50]),sum(wt_table$Freq[wt_table$Var1>100]),sum(wt_table$Freq[wt_table$Var1>50&wt_table$Var1<100]))
  bin_wt_table$Percent=(bin_wt_table$Count/sum(bin_wt_table$Count))*100
  
  age_table=as.data.frame(table(faers_unique$age_YR))
  age_table$Var1=as.numeric(age_table$Var1)
  #print(sum(wt_table$Freq[wt_table$Var1<50]))
  bin_age_table=data.frame(Category=c("Age", "Age","Age","Age"), Variable=c("Unknown","<18",">90","18-90"))

  bin_age_table$Count=c(sum(is.na(faers_unique$age_YR)),sum(age_table$Freq[age_table$Var1<18]),sum(age_table$Freq[age_table$Var1>90]),sum(age_table$Freq[age_table$Var1>18&age_table$Var1<90]))
  bin_age_table$Percent=(bin_age_table$Count/sum(bin_age_table$Count))*100
  
  patients=data.frame(Category="Total Patients",Variable="-", Count=nrow(faers_unique),Percent="-")
  cases=data.frame(Category="Total Cases",Variable="-", Count=nrow(faers_dataset),Percent="-")
  
  report_country_table=as.data.frame(table(faers_unique$reporter_country))
  report_country_table=top_n(report_country_table, 5)
  reporting_country= data.frame(Category=c("Reporting Country","Reporting Country","Reporting Country","Reporting Country","Reporting Country"),Variable=report_country_table$Var1, Count=report_country_table$Freq, Percent=(100*report_country_table$Freq/(nrow(faers_unique))))
  
  #forget about automation, just output indication table for zoloft/non zoloft
  
  all_table=rbind(sex_table, bin_wt_table)
  all_table=rbind(all_table, bin_age_table)
  all_table=rbind(all_table, patients)
  all_table=rbind(all_table, cases)
  all_table=rbind(all_table, reporting_country)
  return(all_table)
}

count_qualifying_ADRs=function(PRR_dataset, ROR_dataset){
  qualifying_PRR=PRR_dataset[PRR_dataset$PRR>=2,]
  qualifying_ROR=ROR_dataset[ROR_dataset$lower>=1,]
  merged=(merge(qualifying_PRR, qualifying_ROR,
              by = 'ADRs', all = TRUE))
  merged=na.omit(merged)
  return (merged)
}
