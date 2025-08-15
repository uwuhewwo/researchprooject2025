

zoloft_2022 <- filter( 
  faers_data_unified_2022,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE"|drugname_all == "LUSTRAL") 

zoloft_2022_adr=as.data.frame(table(zoloft_2022$pt))
#WHOOPS SHIT THIS STILL HAS ZOLOFT IN IT
#rerun all nonzoloft and analysis
not_zoloft_2022=filter( 
  faers_data_unified_2022,drugname_all != "SERTRALINE" & drugname_all != "ZOLOFT" &drugname_all != "SERTRALINE HYDROCHLORIDE"&drugname_all != "LUSTRAL") 
#not_zoloft_summary=summary_faersdata(not_zoloft)
#of patients taking zoloft
length(unique(zoloft_2022[["caseid"]]))
zoloft_unique_2022=distinct(zoloft_2022, primaryid, .keep_all = TRUE)
#gender/indication breakdown of zoloft takers
zoloft_sex_2022=as.data.frame(table(zoloft_unique_2022$sex))
zoloft_indi_2022=as.data.frame(table(zoloft_unique_2022$indi_pt_all))
zoloft_indi_final_2022=count_indications (c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft_2022)
zoloft_report_country_2022=as.data.frame(table(zoloft_unique_2022$reporter_country))
zoloft_occr_country_2022=as.data.frame(table(zoloft_unique_2022$occr_country))
not_zoloft_unique_2022=distinct(not_zoloft_2022, primaryid, .keep_all = TRUE)

not_zoloft_report_country_2022=as.data.frame(table(not_zoloft_unique_2022$reporter_country))
not_zoloft_occr_country_2022=as.data.frame(table(not_zoloft_unique_2022$occr_country))



#indication work

zoloft_unique_druginfo_2022=rbindlist(zoloft_unique_2022$patient_drug)



zoloft_unique_druginfo_2022$dose_amt=zoloft_unique_druginfo_2022$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2022$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2022$age)
summary(zoloft_unique_2022$age)
#AE table not zoloft
not_zoloft_adr=as.data.frame(table(not_zoloft_2022$pt))
length(unique(not_zoloft_2022[["caseid"]]))

#not zoloft info
not_zoloft_druglist=as.data.frame(table(not_zoloft_2022$drugname_ps))
not_zoloft_unique_druginfo_2022=rbindlist(not_zoloft_unique_2022$patient_drug)
not_zoloft_sex=as.data.frame(table(not_zoloft_unique_2022$sex))
not_zoloft_indi=as.data.frame(table(not_zoloft_unique_2022$indi_pt_all))
not_zoloft_unique_druginfo_2022$dose_amt=not_zoloft_unique_druginfo_2022$dose_amt%>%as.double()
not_zoloft_indi_2022=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),not_zoloft_2022)

hist(not_zoloft_unique_druginfo_2022$dose_amt)
hist(not_zoloft_unique_2022$age)


zoloft_adr_rate_2022=adr_rate(zoloft_2022,cutoff=1)
not_zoloft_adr_rate_2022=adr_rate(not_zoloft_2022)
zoloft_indi_2022=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),zoloft_2022)

zoloft_split_2022=(split(zoloft_2022, zoloft_2022$indi_pt_all))
zoloft_anxiety_2022=bind_rows(zoloft_split_2022$Anxiety,zoloft_split_2022$`Anxiety disorder`,zoloft_split_2022$`Anxiety/Depression`,zoloft_split_2022$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2022$`Anxiety/Obsessive-compulsive disorder`)
zoloft_depression_2022=bind_rows(zoloft_split_2022$Depression, zoloft_split_2022$`Depression suicidal`,zoloft_split_2022$`Depression/Obsessive-compulsive disorder`,zoloft_split_2022$`Depression/Panic attack`,zoloft_split_2022$`Anxiety/Depression`,zoloft_split_2022$`Anxiety/Depression/Post-traumatic stress disorder`)
zoloft_other=bind_rows(zoloft_split_2022$`Panic attack`,zoloft_split_2022$`Panic disorder`, zoloft_split_2022$`Depression/Panic attack`,zoloft_split_2022$`Post-traumatic stress disorder`,zoloft_split_2022$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2022$`Depression/Obsessive-compulsive disorder`,zoloft_split_2022$`Obsessive-compulsive disorder`)  
zoloft_anxiety_adr=as.data.frame(table(zoloft_anxiety$pt))
zoloft_depression_adr=as.data.frame(table(zoloft_depression$pt))

not_zoloft_split_2022=(split(not_zoloft, not_zoloft$indi_pt_all))
not_zoloft_anxiety_2022=bind_rows(not_zoloft_split_2022$Anxiety,not_zoloft_split_2022$`Anxiety disorder`,not_zoloft_split_2022$`Anxiety/Depression`,not_zoloft_split_2022$`Anxiety/Depression/Post-traumatic stress disorder`,not_zoloft_split_2022$`Anxiety/Obsessive-compulsive disorder`)
not_zoloft_depression_2022=bind_rows(not_zoloft_split_2022$Depression, not_zoloft_split_2022$`Depression suicidal`,not_zoloft_split_2022$`Depression/Obsessive-compulsive disorder`,not_zoloft_split_2022$`Depression/Panic attack`,not_zoloft_split_2022$`Anxiety/Depression`,not_zoloft_split_2022$`Anxiety/Depression/Post-traumatic stress disorder`)
not_zoloft_other_2022=bind_rows(zoloft_split_2022$`Panic attack`,zoloft_split_2022$`Panic disorder`, zoloft_split_2022$`Depression/Panic attack`,zoloft_split_2022$`Post-traumatic stress disorder`,zoloft_split_2022$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2022$`Depression/Obsessive-compulsive disorder`,zoloft_split_2022$`Obsessive-compulsive disorder`)  
not_zoloft_anxiety_adr_2022=as.data.frame(table(not_zoloft_anxiety_2022$pt))
not_zoloft_depression_adr_2022=as.data.frame(table(not_zoloft_depression_2022$pt))


#zoloft_anxiety_2022=bind_rows(zoloft_split_2022$Anxiety,zoloft_split_2022$`Anxiety disorder`,zoloft_split_2022$`Anxiety/Depression`,zoloft_split_2022$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2022$`Anxiety/Obsessive-compulsive disorder`)
#zoloft_depression=bind_rows(zoloft_split_2022$Depression, zoloft_split_2022$`Depression suicidal`,zoloft_split_2022$`Depression/Obsessive-compulsive disorder`,zoloft_split_2022$`Depression/Panic attack`,zoloft_split_2022$`Anxiety/Depression`,zoloft_split_2022$`Anxiety/Depression/Post-traumatic stress disorder`)

#compare demographics

zoloft_sex_2022=as.data.frame(table(zoloft_unique_2022$sex))
#zoloft_indi=as.data.frame(table(zoloft_unique_2022$indi_pt_all))
#zoloft_indi_final=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
#report_country=as.data.frame(table(zoloft_unique$reporter_country))
#occr_country=as.data.frame(table(zoloft_unique$occr_country))


zoloft_unique_druginfo_2022=rbindlist(zoloft_unique_2022$patient_drug)



zoloft_unique_druginfo_2022$dose_amt=zoloft_unique_druginfo_2022$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2022$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2022$age)
summary(zoloft_unique_2022$age)

#compare adr rates between indications

ic_2022=ROR_comparison(zoloft_2022,not_zoloft_2022)
ip_2022=PRR_comparison(zoloft_2022,not_zoloft_2022)

#chisq using patients, not cases no

c_table_sex=matrix(c(83,641,287  ,521   ,4435,2258),ncol=3,byrow=TRUE)
colnames(c_table_sex)=c("U","F","M")
rownames(c_table_sex)=c("zoloft","not-zoloft")
chisq.test(c_table_sex)

#chisq test for indication + country, cases

c_table_ind=matrix(c(715, 1626,35  ,83   ,127,1, 5589,16177,421,674,665,177),ncol=6,byrow=TRUE)
colnames(c_table_ind)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind)=c("zoloft","not-zoloft")
chisq.test(c_table_ind)

#demographics tables
d_table_z_2022=return_demographic_table(zoloft_2022)
d_table_c_2022=return_demographic_table(not_zoloft_2022)

#ror/prr indications
AD_ROR=ROR_comparison(zoloft_anxiety_2022,zoloft_depression_2022)
AD_PRR=PRR_comparison(zoloft_anxiety_2022,zoloft_depression_2022)

#depression ROR/prr counts of patients
c_table_ind2=matrix(c(243, 460,13  ,11   ,28,0, 1747,5140,136,838,152,29),ncol=6,byrow=TRUE)
colnames(c_table_ind2)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind2)=c("zoloft","not-zoloft")
chisq.test(c_table_ind2,simulate.p.value=TRUE)

#chisquared weight

c_table_ind2=matrix(c(113 , 151,26  ,433   ,101, 124, 2117, 4907),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared age
c_table_ind2=matrix(c(104 , 500,1  ,186   ,96, 4598, 89, 1596),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2)

#find all qualifying adrs for this year
qualifying_adrs_2022=count_qualifying_ADRs(ip_2022,ic_2022)