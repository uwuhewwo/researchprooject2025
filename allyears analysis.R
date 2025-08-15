not_zoloft_allyears=rbind(not_zoloft_2022, rbind(not_zoloft_2023, not_zoloft_2024))

PRR_allyears_not_zoloft=PRR_comparison(not_zoloft_allyears,zoloft_allyears)
ROR_allyears_not_zoloft=ROR_comparison(not_zoloft_allyears,zoloft_allyears)

zoloft_allyears=rbind(zoloft_2022, rbind(zoloft_2023, zoloft_2024))
zoloft_allyears$sex[zoloft_allyears$sex=="UNK"]=""
zoloft_allyears_unique=distinct(zoloft_allyears, primaryid, .keep_all = TRUE)
ADRs_allyears=count_qualifying_ADRs(PRR_dataset = PRR_allyears_not_zoloft, ROR_dataset = ROR_allyears_not_zoloft)


zoloft_split_allyears=(split(zoloft_allyears, zoloft_allyears$indi_pt_all))
zoloft_anxiety_allyears=bind_rows(zoloft_split_allyears$Anxiety,zoloft_split_allyears$`Anxiety disorder`,zoloft_split_allyears$`Anxiety/Depression`,zoloft_split_allyears$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_allyears$`Anxiety/Obsessive-compulsive disorder`)
zoloft_depression_allyears=bind_rows(zoloft_split_allyears$Depression, zoloft_split_allyears$`Depression suicidal`,zoloft_split_allyears$`Depression/Obsessive-compulsive disorder`,zoloft_split_allyears$`Depression/Panic attack`,zoloft_split_allyears$`Anxiety/Depression`,zoloft_split_allyears$`Anxiety/Depression/Post-traumatic stress disorder`)
zoloft_other_allyears=bind_rows(zoloft_split_allyears$`Panic attack`,zoloft_split_allyears$`Panic disorder`, zoloft_split_allyears$`Depression/Panic attack`,zoloft_split_allyears$`Post-traumatic stress disorder`,zoloft_split_allyears$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_allyears$`Depression/Obsessive-compulsive disorder`,zoloft_split_allyears$`Obsessive-compulsive disorder`)  
zoloft_anxiety_adr_allyears=as.data.frame(table(zoloft_anxiety$pt))
zoloft_depression_adr_allyears=as.data.frame(table(zoloft_depression$pt))

not_zoloft_split_allyears=(split(zoloft_allyears, zoloft_allyears$indi_pt_all))
not_zoloft_anxiety_allyears=bind_rows(not_zoloft_split_allyears$Anxiety,not_zoloft_split_allyears$`Anxiety disorder`,not_zoloft_split_allyears$`Anxiety/Depression`,not_zoloft_split_allyears$`Anxiety/Depression/Post-traumatic stress disorder`,not_zoloft_split_allyears$`Anxiety/Obsessive-compulsive disorder`)
not_zoloft_depression_allyears=bind_rows(not_zoloft_split_allyears$Depression, not_zoloft_split_allyears$`Depression suicidal`,not_zoloft_split_allyears$`Depression/Obsessive-compulsive disorder`,not_zoloft_split_allyears$`Depression/Panic attack`,not_zoloft_split_allyears$`Anxiety/Depression`,not_zoloft_split_allyears$`Anxiety/Depression/Post-traumatic stress disorder`)
not_zoloft_other_allyears=bind_rows(zoloft_split_allyears$`Panic attack`,zoloft_split_allyears$`Panic disorder`, zoloft_split_allyears$`Depression/Panic attack`,zoloft_split_allyears$`Post-traumatic stress disorder`,zoloft_split_allyears$`Anxiety/Depression/Post-traumatic stress disorder`,zoloft_split_allyears$`Depression/Obsessive-compulsive disorder`,zoloft_split_allyears$`Obsessive-compulsive disorder`)  
not_zoloft_anxiety_adr_allyears=as.data.frame(table(not_zoloft_anxiety$pt))
not_zoloft_depression_adr_allyears=as.data.frame(table(not_zoloft_depression$pt))

all_years_adr_rate=adr_rate(zoloft_allyears)

#find 8 adrs already discovered and compare rates btwn indication/year etc
PRR_allyears_anxiety=PRR_comparison(not_zoloft_anxiety_allyears,zoloft_anxiety_allyears)
ROR_allyears_anxiety=ROR_comparison(not_zoloft_anxiety_allyears,zoloft_anxiety_allyears)


PRR_allyears_depression=PRR_comparison(not_zoloft_depression_allyears,zoloft_depression_allyears)
ROR_allyears_depression=ROR_comparison(not_zoloft_depression_allyears,zoloft_depression_allyears)

#nothing: PRR too low. probably no significant differences among indication.
significant_adrs_depression_allyears=count_qualifying_ADRs(PRR_allyears_depression,ROR_allyears_depression)
significant_adrs_anxiety_allyears=count_qualifying_ADRs(PRR_allyears_anxiety,ROR_allyears_anxiety)

zoloft_allyears_age_under18=subset(zoloft_allyears, age_YR<18)
zoloft_allyears_age_adult=subset(zoloft_allyears, age_YR>=18)
zoloft_allyears_age_NA=subset(zoloft_allyears, is.na(age_YR))

zoloft_allyears_age_under18_aes=as.data.frame(table(zoloft_allyears_age_under18$pt))
zoloft_allyears_age_adult_aes=as.data.frame(table(zoloft_allyears_age_adult$pt))
zoloft_allyears_age_NA_aes=as.data.frame(table(zoloft_allyears_age_NA$pt))
zoloft_allyears_age_shared_aes=merge(merge(zoloft_allyears_age_under18_aes, zoloft_allyears_age_adult_aes,by="Var1",all=TRUE), zoloft_allyears_age_NA_aes,by="Var1",all=TRUE)
colnames(zoloft_allyears_age_shared_aes)=c("ADRs","18-","18+","NA")
zoloft_allyears_age_shared_aes=merge(zoloft_allyears_age_shared_aes, significant_adrs_merged,by="ADRs")

zoloft_cont_table=as.data.frame(zoloft_allyears$primaryid)
zoloft_cont_table=zoloft_cont_table%>%add_column(zoloft_allyears$pt)
zoloft_cont_table$sex=as.factor(zoloft_allyears$sex)
#make three way age+indication factor: over18/under18/na_age
zoloft_cont_table$bruxism=as.factor(zoloft_cont_table$`zoloft_allyears$pt`=="Bruxism")
zoloft_cont_table$colitis=as.factor(zoloft_cont_table$`zoloft_allyears$pt`=="Colitis microscopic")
zoloft_cont_table$diarrhea=as.factor(zoloft_cont_table$`zoloft_allyears$pt`=="Diarrhoea")
zoloft_cont_table$FSD=as.factor(zoloft_cont_table$`zoloft_allyears$pt`=="Female sexual dysfunction")
zoloft_cont_table$GA=as.factor(zoloft_cont_table$`zoloft_allyears$pt`=="Genital anaesthesia")
zoloft_cont_table$hypophagia=as.factor(zoloft_cont_table$`zoloft_allyears$pt`=="Hypophagia")
zoloft_cont_table$ME=as.factor(zoloft_cont_table$`zoloft_allyears$pt`=="Medication error")


#install.packages("MASS")
library(MASS) 

age_grp=zoloft_allyears  %>%mutate(new_column = case_when(
  age_YR <18 & age_YR>=0 ~ "Under 18",
  age_YR>=18  ~ "Over 18",
  is.na(age_YR) ~ "NA"
))%>% dplyr::select(new_column)
age_grp$new_column=as.factor(age_grp$new_column)
zoloft_cont_table=zoloft_cont_table%>%add_column(age_grp$new_column)

table(str_detect(zoloft_allyears$indi_pt_all, 'Anxiety'))
table(str_detect(zoloft_allyears$indi_pt_all, 'Depression'))
test=zoloft_allyears  %>%mutate(new_column = case_when(
  str_detect(zoloft_allyears$indi_pt_all, 'Anxiety') ~ "Anxiety",
  str_detect(zoloft_allyears$indi_pt_all, 'Depression')  ~ "Depression",
  TRUE ~ "OTHER"
))%>% dplyr::select(new_column)
test$new_column=as.factor(test$new_column)
zoloft_cont_table=zoloft_cont_table%>%add_column(test$new_column)

names(zoloft_cont_table)[names(zoloft_cont_table) == 'age_grp$new_column'] <- 'age_grp'

names(zoloft_cont_table)[names(zoloft_cont_table) == 'test$new_column'] <- 'indication'

cont_table_test=table(zoloft_cont_table$indication, zoloft_cont_table$ME)
chisq.test(cont_table_test)
print(cont_table_test)
