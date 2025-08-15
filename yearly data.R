faers_2022 <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2022/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2022 <- unify_tabular_ascii(ascii_list = faers_2022)

faers_2021 <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2021/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2021 <- unify_tabular_ascii(ascii_list = faers_2021)

faers_2020 <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2020/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2020 <- unify_tabular_ascii(ascii_list = faers_2020)

faers_2023<- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2023/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2023 <- unify_tabular_ascii(ascii_list = faers_2023)

faers_2019 <- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2019/ascii",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2019 <- unify_tabular_ascii(ascii_list = faers_2019)

faers_2024<- retrieve_faersascii(ascii_dir = "C:/Users/kjinl/Documents/2024 tri1/research/2024/ASCII",drug_indication_pattern ="Anxiety|Depression|Premenstrual dysphoric disorder|Binge eating disorder|Post-traumatic stress disorder|PTSD|Generalized anxiety disorder|Bulimia|Depression|Major depressive disorder|Panic|OCD|Obsessive-compulsive disorder")
faers_data_unified_2024 <- unify_tabular_ascii(ascii_list = faers_2024)



zoloft_2024 <- filter( 
  faers_data_unified_2024,drugname_all == "SERTRALINE" | drugname_all == "ZOLOFT" |drugname_all == "SERTRALINE HYDROCHLORIDE"|drugname_all == "LUSTRAL") 

zoloft_2024_adr=as.data.frame(table(zoloft_2024$pt))
#WHOOPS SHIT THIS STILL HAS ZOLOFT IN IT
#rerun all nonzoloft and analysis
not_zoloft_2024=filter( 
  faers_data_unified_2024,drugname_all != "SERTRALINE" & drugname_all != "ZOLOFT" &drugname_all != "SERTRALINE HYDROCHLORIDE"&drugname_all != "LUSTRAL") 
#not_zoloft_summary=summary_faersdata(not_zoloft)
#of patients taking zoloft
length(unique(zoloft_2024[["caseid"]]))
zoloft_unique_2024=distinct(zoloft_2024, primaryid, .keep_all = TRUE)
#gender/indication breakdown of zoloft takers
zoloft_sex_2024=as.data.frame(table(zoloft_unique_2024$sex))
zoloft_indi_2024=as.data.frame(table(zoloft_unique_2024$indi_pt_all))
zoloft_indi_final_2024=count_indications (c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft_2024)
zoloft_report_country_2024=as.data.frame(table(zoloft_unique_2024$reporter_country))
zoloft_occr_country_2024=as.data.frame(table(zoloft_unique_2024$occr_country))
not_zoloft_unique_2024=distinct(not_zoloft_2024, primaryid, .keep_all = TRUE)

not_zoloft_report_country_2024=as.data.frame(table(not_zoloft_unique_2024$reporter_country))
not_zoloft_occr_country_2024=as.data.frame(table(not_zoloft_unique_2024$occr_country))



#indication work

zoloft_unique_druginfo_2024=rbindlist(zoloft_unique_2024$patient_drug)



zoloft_unique_druginfo_2024$dose_amt=zoloft_unique_druginfo_2024$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2024$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2024$age)
summary(zoloft_unique_2024$age)
#AE table not zoloft
not_zoloft_adr=as.data.frame(table(not_zoloft_2024$pt))
length(unique(not_zoloft_2024[["caseid"]]))

#not zoloft info
not_zoloft_druglist=as.data.frame(table(not_zoloft_2024$drugname_ps))
not_zoloft_unique_druginfo_2024=rbindlist(not_zoloft_unique_2024$patient_drug)
not_zoloft_sex=as.data.frame(table(not_zoloft_unique_2024$sex))
not_zoloft_indi=as.data.frame(table(not_zoloft_unique_2024$indi_pt_all))
not_zoloft_unique_druginfo_2024$dose_amt=not_zoloft_unique_druginfo_2024$dose_amt%>%as.double()
not_zoloft_indi_2024=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),not_zoloft_2024)

hist(not_zoloft_unique_druginfo_2024$dose_amt)
hist(not_zoloft_unique_2024$age)


zoloft_adr_rate_2024=adr_rate(zoloft_2024)
not_zoloft_adr_rate_2024=adr_rate(not_zoloft_2024)
zoloft_indi_2024=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual","Bulimia"),zoloft_2024)

zoloft_split_2024=(split(zoloft_2024, zoloft_2024$indi_pt_all))
zoloft_anxiety_2024=bind_rows(zoloft_split_2024$Anxiety,zoloft_split_2024$`Anxiety disorder`,zoloft_split_2024$`Anxiety/Depression`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2024$`Anxiety/Obsessive-compulsive disorder`)
zoloft_depression_2024=bind_rows(zoloft_split_2024$Depression, zoloft_split_2024$`Depression suicidal`,zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,zoloft_split_2024$`Depression/Panic attack`,zoloft_split_2024$`Anxiety/Depression`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`)
zoloft_other=bind_rows(zoloft_split_2024$`Panic attack`,zoloft_split_2024$`Panic disorder`, zoloft_split_2024$`Depression/Panic attack`,zoloft_split_2024$`Post-traumatic stress disorder`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,zoloft_split_2024$`Obsessive-compulsive disorder`)  
zoloft_anxiety_adr=as.data.frame(table(zoloft_anxiety$pt))
zoloft_depression_adr=as.data.frame(table(zoloft_depression$pt))

not_zoloft_split_2024=(split(not_zoloft, not_zoloft$indi_pt_all))
not_zoloft_anxiety_2024=bind_rows(not_zoloft_split_2024$Anxiety,not_zoloft_split_2024$`Anxiety disorder`,not_zoloft_split_2024$`Anxiety/Depression`,not_zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,not_zoloft_split_2024$`Anxiety/Obsessive-compulsive disorder`)
not_zoloft_depression_2024=bind_rows(not_zoloft_split_2024$Depression, not_zoloft_split_2024$`Depression suicidal`,not_zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,not_zoloft_split_2024$`Depression/Panic attack`,not_zoloft_split_2024$`Anxiety/Depression`,not_zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`)
not_zoloft_other_2024=bind_rows(zoloft_split_2024$`Panic attack`,zoloft_split_2024$`Panic disorder`, zoloft_split_2024$`Depression/Panic attack`,zoloft_split_2024$`Post-traumatic stress disorder`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,zoloft_split_2024$`Obsessive-compulsive disorder`)  
not_zoloft_anxiety_adr_2024=as.data.frame(table(not_zoloft_anxiety_2024$pt))
not_zoloft_depression_adr_2024=as.data.frame(table(not_zoloft_depression_2024$pt))


#zoloft_anxiety_2024=bind_rows(zoloft_split_2024$Anxiety,zoloft_split_2024$`Anxiety disorder`,zoloft_split_2024$`Anxiety/Depression`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_2024$`Anxiety/Obsessive-compulsive disorder`)
#zoloft_depression=bind_rows(zoloft_split_2024$Depression, zoloft_split_2024$`Depression suicidal`,zoloft_split_2024$`Depression/Obsessive-compulsive disorder`,zoloft_split_2024$`Depression/Panic attack`,zoloft_split_2024$`Anxiety/Depression`,zoloft_split_2024$`Anxiety/Depression/Post-traumatic stress disorder`)

#compare demographics

zoloft_sex_2024=as.data.frame(table(zoloft_unique_2024$sex))
#zoloft_indi=as.data.frame(table(zoloft_unique_2024$indi_pt_all))
#zoloft_indi_final=count_indications(c("Anxiety", "Depression", "Panic", "Post-traumatic", "Obsessive-compulsive", "Premenstrual "),zoloft)
#report_country=as.data.frame(table(zoloft_unique$reporter_country))
#occr_country=as.data.frame(table(zoloft_unique$occr_country))


zoloft_unique_druginfo_2024=rbindlist(zoloft_unique_2024$patient_drug)



zoloft_unique_druginfo_2024$dose_amt=zoloft_unique_druginfo_2024$dose_amt%>%as.double()
hist(zoloft_unique_druginfo_2024$dose_amt)
#zoloft_no_overdose=zoloft %>%filter("dose_amt"<900)
#age breakdown 
hist(zoloft_unique_2024$age)
summary(zoloft_unique_2024$age)

#compare adr rates between indications

ic_2024=ROR_comparison(zoloft_2024,not_zoloft_2024)
ip_2024=PRR_comparison(zoloft_2024,not_zoloft_2024)

#chisq using patients, not cases no

c_table_sex=matrix(c(79, 419,300  ,679   ,3666,2072),ncol=3,byrow=TRUE)
colnames(c_table_sex)=c("U","F","M")
rownames(c_table_sex)=c("zoloft","not-zoloft")
chisq.test(c_table_sex)

#chisq test for indication + country, cases

c_table_ind=matrix(c(715, 1626,35  ,83   ,127,1, 5589,16177,421,674,665,177),ncol=6,byrow=TRUE)
colnames(c_table_ind)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind)=c("zoloft","not-zoloft")
chisq.test(c_table_ind)

#demographics tables
d_table_z_2024=return_demographic_table(zoloft_2024)
d_table_c_2024=return_demographic_table(not_zoloft_2024)

#ror/prr indications
AD_ROR=ROR_comparison(zoloft_anxiety_2024,zoloft_depression_2024)
AD_PRR=PRR_comparison(zoloft_anxiety_2024,zoloft_depression_2024)

#depression ROR/prr counts of patients
c_table_ind2=matrix(c(226, 521,5  ,19   ,41,1, 1267,4845,77,139,126,20),ncol=6,byrow=TRUE)
colnames(c_table_ind2)=c("Anxiety","Depression","Panic", "Post-traumatic","Obsessive-Compulsive","Other")
rownames(c_table_ind2)=c("zoloft","not-zoloft")
chisq.test(c_table_ind2,simulate.p.value=TRUE)

#chisquared weight

c_table_ind2=matrix(c(109 , 189,24  ,474   ,90, 156, 1423, 4746),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2,simulate.p.value=FALSE)

#chisquared age
c_table_ind2=matrix(c(104 , 500,1  ,186   ,96, 4598, 89, 1596),ncol=4,byrow=TRUE)
chisq.test(c_table_ind2)


#find all qualifying adrs for this year
qualifying_adrs_2024=count_qualifying_ADRs(ip_2024,ic_2024)