library(RocheTeradata)
library(tidyverse)
library(RWDSverse)
library(FlatironData)
library(eeptools)
library(tibble)
library(readr)
library(naniar)
library(lubridate)
library(stringr)
library(purrr)
library(caret)
library(Publish)
library(DescTools)
library(PropCIs)
################################################################
### Loading data
options(java.parameters = "-Xmx8g")
devtools::install_github('RWDScodeshare/FlatironData', host='https://github.roche.com/api/v3')
demo <- FlatironData::demographics(datamart = "SCLC")
diagnosis  <- FlatironData::diagnosis(datamart = "SCLC")
visit <- FlatironData::visit(datamart = "SCLC")
lab <- FlatironData::lab(datamart = "SCLC")
vitals <- FlatironData::vitals(datamart = "SCLC")
order <- FlatironData::medicationorder(datamart = "SCLC")
admin <- FlatironData::medicationadmin(datamart = "SCLC")
ecog  <- FlatironData::ecog(datamart = "SCLC")
lot <- FlatironData::lineoftherapy(datamart = "SCLC")
insurance <- FlatironData::insurance(datamart = "SCLC")
mortality <- FlatironData::mortality(datamart = "SCLC")
sclc <- FlatironData::sclc(datamart = "SCLC")
sclcbiomarkers <- FlatironData::sclcbiomarkers(datamart = "SCLC")


##### Exprot dataset 
#write.csv(demo,"3643_demo.csv", row.names = FALSE)
#write.csv(diagnosis,"3643_diagnosis.csv", row.names = FALSE)
#write.csv(visit,"3643_visit.csv", row.names = FALSE)
#write.csv(lab,"3643_lab.csv", row.names = FALSE)
#write.csv(vitals,"3643_vitals.csv", row.names = FALSE)
#write.csv(order,"3643_order.csv", row.names = FALSE)
#write.csv(admin,"3643_admin.csv", row.names = FALSE)
#write.csv(lot,"3643_lot.csv", row.names = FALSE)
#write.csv(ecog,"3643_ecog.csv", row.names = FALSE)
#write.csv(insurance,"3643_insurance.csv", row.names = FALSE)
#write.csv(mortality,"3643_mortality.csv", row.names = FALSE)
#write.csv(sclc,"3643_sclc.csv", row.names = FALSE)
#write.csv(sclcbiomarkers,"3643_sclcbiomarkers.csv", row.names = FALSE)




#### Data preprocessing


# Create Tibble for the cohort 
# Remove duplicated Patient ID
tib_lot <- as_tibble(lot)
tib_lot_treat_day <-tib_lot %>% 
  select(PatientID, LineNumber, StartDate) %>%
  filter(LineNumber == 1) %>% 
  group_by(PatientID) %>%
  arrange(PatientID) %>%
  filter(StartDate ==min(StartDate)) %>%
  distinct() %>%
  ungroup()  %>% 
  filter(StartDate < "2017-07-01")


# For patient flow chart-------------------------- 
# patient with 1st line treatment 
pa_first_line <- tib_lot %>% 
  select(PatientID, LineNumber, StartDate) %>%
  filter(LineNumber == 1) %>% 
  group_by(PatientID) %>%
  arrange(PatientID)%>%
  filter(StartDate ==min(StartDate)) %>%
  distinct() %>%
  ungroup()  
nrow(pa_first_line) # after selecting 1st line treatment 4501


#pa_chemo_stday_one_year #3643
pa_chemo_stday_one_year <- pa_first_line %>% 
  filter(StartDate < "2017-07-01")
nrow(pa_chemo_stday_one_year)  #3643

nrow(pa_chemo_stday_one_year)


#Patient visit within 90 after diagnosis  3511
Patient_visit_90  <- tib_visit %>% 
  right_join(pa_chemo_stday_one_year, by="PatientID") %>% 
  left_join(sclc,by="PatientID" )%>%
  select(PatientID,VisitDate,DiagnosisDate,StartDate) %>%
  mutate(days_90 = DiagnosisDate + days(90)) %>%
  filter((DiagnosisDate < VisitDate) & ( VisitDate <= days_90)) %>% 
  group_by(PatientID) %>% 
  filter(row_number()==1) %>%
  select(PatientID) 

nrow(Patient_visit_90)


#Creating tib_lot_treat_day
tib_lot_treat_day <- pa_chemo_stday_one_year %>% right_join(Patient_visit_90, by="PatientID") 
tib_treated_patients <- Patient_visit_90

# ----------------------------------------
#Tib Demo
tib_demo <- as_tibble(demo) %>%
  right_join(tib_lot_treat_day, by="PatientID")
length(tib_demo$PatientID) #3643


# Data description#
length(unique(tib_lot_treat_day$PatientID)) #unique PatientID = 3643 First-line patient
colSums(is.na(tib_lot_treat_day))





#-----------------------------------
# sclc diagnosisdate
a <-sclc %>% select(PatientID,DiagnosisDate) %>% 
  right_join(tib_treated_patients) %>%
  arrange(DiagnosisDate)




#Extract diagnosis date from SCLC
tib_sclc_dday_non_filter <- sclc %>%
  as_tibble() %>%
  select(PatientID,DiagnosisDate)




###MORTALITY-------------------------------------------------------------------------------------------------------------------------------
tib_mortality <- as_tibble(mortality) %>%
  right_join(tib_lot_treat_day, by="PatientID")%>%
  select(PatientID, DateOfDeath,StartDate)


# One year after treatment start date - Variable  
tib_mortality <- tib_mortality %>% mutate(NewDate = StartDate %m+% years(1) )


#Impute day for date of death
tib_mortality<- tib_mortality %>% mutate(DateOfDeath = paste(tib_mortality$DateOfDeath, "15", sep = "-"))%>%
  replace_with_na(replace = list(DateOfDeath = c("NA-15"))) %>%
  mutate(DateOfDeath = ymd(DateOfDeath)) 


# Create 0 / 1 for dead or alive after 1 year   (1= died 0 =alive)   
tib_mortality<- tib_mortality %>%  
  mutate(alive_1_year =  case_when((DateOfDeath <= NewDate) ~ 1, 
                                   ((DateOfDeath > NewDate)| is.na(DateOfDeath))  ~ 0 )) 
# make tib out of Survived_patients 
tib_survived_patients <- tib_mortality %>% select(PatientID, alive_1_year) %>% mutate(alive_1_year = factor(alive_1_year, levels = c(0,1)))
tib_survived_patients <- tib_survived_patients %>% right_join(tib_treated_patients, by="PatientID")


# Plot
ggplot(tib_survived_patients, aes(x= alive_1_year, y=  (..count..)/sum(..count..) ,fill=alive_1_year))+
  geom_bar() +
  labs(title ="Mortality Distribution of SCLC patients", subtitle = " one year after received first line chemo therapy")+
  scale_y_continuous(name="proportion", labels = scales::percent) + 
  scale_x_discrete(name="Mortality", labels = c( "Alive","Death","Missing")) + 
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..) ), color = "white", stat= "count", vjust = 2)+
  guides(fill=FALSE)


# Insurance ---------------------------------------------------------------


#Commercial Health Plan is any non-governmental insurance 
#Medicaid : low income 
#Self pay 
#Workers compensation : employee injured at work 
#Patient Assistance Program: can't afford their prescriptions 
#Medicare: +65 and others 
#Other government program: governmental programs 
#NA 
#Other Payer - Type Unknown 


tib_insurance <- as_tibble(insurance) %>%
  right_join(tib_lot_treat_day, by= 'PatientID') %>%
  mutate(SES = case_when(PayerCategory=='Commercial Health Plan' ~ 1 ,
                         PayerCategory=='Self Pay'  ~ 2,
                         PayerCategory=='Medicaid' ~ 3 ,
                         PayerCategory=='Medicare'  ~ 4 ,
                         PayerCategory=='Other Government Program' ~ 5,
                         PayerCategory=='Patient Assistance Program' ~ 6,
                         PayerCategory=='Workers Compensation' ~ 7,
                         PayerCategory=='Other Payer - Type Unknown' ~ 8,
                         is.na(PayerCategory) ~ 9))


tib_insurance_ses <- tib_insurance %>% 
  group_by(PatientID) %>%  
  filter((StartDate.x <=  StartDate.y  )| is.na(StartDate.x))%>%
  summarise(final_SES = min(SES)) %>%
  right_join( tib_treated_patients, by="PatientID"  )%>%
  ungroup()








# SCLC --------------------------------------------------------------------


tib_sclc <- as_tibble(sclc) %>%
  right_join(tib_lot_treat_day, by="PatientID")
length(tib_sclc$PatientID) #4436


#Extract diagnosis date from SCLC
tib_sclc_dday <- as_tibble(sclc) %>%
  right_join(tib_lot_treat_day, by="PatientID") %>%
  select(PatientID,DiagnosisDate, LineNumber, StartDate) %>%
  filter(LineNumber==1) %>% 
  group_by(PatientID) %>%  
  arrange(PatientID) %>% 
  filter(StartDate == min(StartDate)) %>% 
  distinct() %>% 
  ungroup()




#### Mix race and ethnicity columns
unique(tib_demo$Race)
unique(tib_demo$Ethnicity)


#Some race missing values can be found in ethnicity
race_ethnicity <- tib_demo %>%
  select(PatientID,Race, Ethnicity) 


#Fill missing values that can be extracted from ethnicity


typeof(race_ethnicity$Ethnicity)
race_ethnicity$Race[is.na(race_ethnicity$Race)] <- as.character(race_ethnicity$Ethnicity[is.na(race_ethnicity$Race)])


#Only use race from now on. Categorise NA into other race


race_ethnicity$Race[is.na(race_ethnicity$Race)] <- 'NA'
sum(is.na(race_ethnicity$Race))
summary(race_ethnicity$Race)


#Move some categories into others?


race_ethnicity %>% 
  group_by(Race) %>%
  summarise(no_rows = length(Race))


race_ethnicity$Race[race_ethnicity$Race == "Asian"] <- "Other Race"
race_ethnicity$Race[race_ethnicity$Race == "Hispanic or Latino"] <- "Other Race"


# Demo --------------------------------------------------------------------
#Age


tib_demo_age <- tib_demo 
tib_demo_age$day <- c("15")
tib_demo_age$month <- c("01")
tib_demo_age$birth_date <- as.Date(with(tib_demo_age, paste(BirthYear, month, day,sep="-")), "%Y-%m-%d")
tib_demo_age$age <- age_calc(tib_demo_age$birth_date, units="years")
tib_demo_age$age <- round(tib_demo_age$age, 0)


#### Get age of diagnosis 


#Take only the years
tib_sclc_dday$DiagnosisDate_year <- as.numeric(year(tib_sclc_dday$DiagnosisDate))
length(tib_sclc_dday$DiagnosisDate) #4436


sum(is.na(tib_sclc_dday$DiagnosisDate))


#Filter diagnosis dates for those patients on treatment


diagnosis_date_chemo <- merge(tib_sclc_dday, tib_lot_treat_day)


diagnosis_date_chemo <- diagnosis_date_chemo %>%
  select(PatientID,LineNumber,StartDate, DiagnosisDate , DiagnosisDate_year) %>%
  as_tibble() %>%
  filter(LineNumber==1)  %>% 
  group_by(PatientID) %>%  
  arrange(PatientID) %>% 
  filter(StartDate == min(StartDate)) %>% 
  distinct() %>% 
  ungroup()






#Merge birthyear and diagnosisdate and calculate age of diagnosis


age_of_diagnosis <- left_join(tib_demo,diagnosis_date_chemo,by="PatientID")
age_of_diagnosis <- as_tibble(age_of_diagnosis)


age_of_diagnosis$age_of_diagnosis_column <- (age_of_diagnosis$DiagnosisDate_year - age_of_diagnosis$BirthYear)  
age_of_diagnosis <- age_of_diagnosis %>%
  select(PatientID, age_of_diagnosis_column)


summary(age_of_diagnosis$age_of_diagnosis_column,na.rm=FALSE)
ci.mean(age_of_diagnosis$age_of_diagnosis_column)


#### Get variable of time from diagnosis to treatment


#Convert string columns to dates


tib_sclc_dday$DiagnosisDate <- as.Date(tib_sclc_dday$DiagnosisDate,
                                       format = "%Y/%m/%d")
tib_lot_treat_day$StartDate_time <- as.Date(tib_lot_treat_day$StartDate,
                                            format = "%Y/%m/%d")


diagnosis_treatment <- tib_sclc_dday %>%
  select(PatientID, DiagnosisDate) %>%
  left_join(select(tib_lot_treat_day, PatientID, StartDate_time), by="PatientID")


diagnosis_treatment$time_diagnosis_to_chemo <- (diagnosis_treatment$StartDate_time - diagnosis_treatment$DiagnosisDate) 


diagnosis_treatment


diagnosis_treatment$time_diagnosis_to_chemo[diagnosis_treatment$time_diagnosis_to_chemo < 0] <- 'NA'


sclc_stage <- tib_sclc_dday




##########Preprocessing of tib_SCLC variable


#Diagnosis date to Resection and radiation date 


sel_sclc_tib <- tib_sclc %>% select(PatientID, Resection, ResectionDate, RadiationTherapy, RadiationTherapyDate )


diagnosis_resection_radiation <- as_tibble(tib_sclc_dday) %>%
  select(PatientID, DiagnosisDate) %>%
  left_join( sel_sclc_tib, by= "PatientID")


diagnosis_resection_radiation$Resection[diagnosis_resection_radiation$Resection=='No/unknown'] <- 'NA'
diagnosis_resection_radiation$RadiationTherapy[diagnosis_resection_radiation$RadiationTherapy =='No/unknown'] <- 'NA'




time_diagnosis_resection<- as_tibble(diagnosis_resection_radiation) %>%
  right_join(tib_lot_treat_day, by="PatientID")


time_diagnosis_resection$Resection[time_diagnosis_resection$ResectionDate <= time_diagnosis_resection$StartDate_time] <- 'NA'


time_diagnosis_resection<- diagnosis_resection_radiation %>%
  right_join(tib_lot_treat_day, by="PatientID") %>%
  group_by(PatientID) %>%  
  arrange(desc(ResectionDate)) %>%
  filter((ResectionDate <= StartDate_time)| is.na(ResectionDate)) %>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  distinct() %>%
  right_join(tib_lot_treat_day, by="PatientID") %>%
  select(PatientID, DiagnosisDate, Resection, ResectionDate, StartDate_time.x)




time_diagnosis_radiation <- as_tibble(diagnosis_resection_radiation) %>%
  right_join(tib_lot_treat_day, by="PatientID")


time_diagnosis_radiation$RadiationTherapy[time_diagnosis_radiation$RadiationTherapyDate <= time_diagnosis_radiation$StartDate_time] <- 'NA'


time_diagnosis_radiation<- diagnosis_resection_radiation %>%
  right_join(tib_lot_treat_day, by="PatientID") %>%
  group_by(PatientID) %>%  
  arrange(desc(RadiationTherapyDate)) %>%
  filter((RadiationTherapyDate <= StartDate_time)| is.na(RadiationTherapyDate)) %>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  distinct() %>%
  right_join(tib_lot_treat_day, by="PatientID") %>%
  select(PatientID, DiagnosisDate, RadiationTherapy, RadiationTherapyDate, StartDate_time.x)




time_diagnosis_resection$diagnosis_to_resection <- (time_diagnosis_resection$ResectionDate - time_diagnosis_resection$DiagnosisDate) 
time_diagnosis_radiation$diagnosis_to_radiation <- (time_diagnosis_radiation$RadiationTherapyDate - time_diagnosis_radiation$DiagnosisDate) 


#####Final data frame last changes
tib_sclc_stage <- tib_sclc %>%
  select(PatientID, SCLCStage, SmokingStatus)


tib_demo_age_final <- tib_demo_age %>%
  select(PatientID, PracticeType, Gender, age)




race_ethnicity_final <- race_ethnicity %>%
  select(PatientID, Race)


diagnosis_treatment_final <- diagnosis_treatment %>%
  select(PatientID, time_diagnosis_to_chemo)


time_diagnosis_resection_final <- time_diagnosis_resection %>%
  select(PatientID, Resection, diagnosis_to_resection)


time_diagnosis_radiation_final <- time_diagnosis_radiation %>%
  select(PatientID, RadiationTherapy, diagnosis_to_radiation)
# FINAL data frame from Marta  --------------------------------------------
tib_marta_feature <- as_tibble(tib_demo_age_final)  %>%
  right_join(tib_insurance_ses, by="PatientID") %>%
  right_join(tib_sclc_stage, by="PatientID") %>%
  right_join(race_ethnicity_final, by="PatientID") %>%
  right_join(age_of_diagnosis, by="PatientID") %>%
  right_join(diagnosis_treatment_final, by="PatientID") %>%
  right_join(time_diagnosis_resection_final, by="PatientID") %>%
  right_join(time_diagnosis_radiation_final, by="PatientID")




###VITALS ----------------------------------------------------------------------------------------------------------------------------------
tib_vitals <- as_tibble(vitals) %>%
  select(PatientID, TestDate, LabComponent, TestUnits, TestUnitsCleaned, TestResult, TestResultCleaned)


as.data.frame(table(vitals$LabComponent))


##### Systolic Blood Pressure #####


#Create a tibble for systolic blood pressure
sys_bp <- tib_vitals%>%
  group_by(PatientID) %>%
  filter(LabComponent=='Systolic blood pressure') %>%
  right_join(tib_lot_treat_day, by = "PatientID")


#Filter dates to get results closer to treatment dates
sys_tidy <- sys_bp %>%
  group_by(PatientID) %>%
  arrange(desc(TestDate)) %>%
  filter((TestDate <= StartDate)| is.na(TestDate))%>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  distinct() %>%
  select(PatientID, TestDate,TestResult) %>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID, TestResult)


#Calculate the percentage of missing values 
#Check Ditribution of the Data - Highest and lowest mean etc 
sys_tidy$TestResult <- as.numeric(as.character(sys_tidy$TestResult))
summary(sys_tidy$TestResult) # 523 11.78% missing 


# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0   113.0   126.0   127.6   140.0  1234.0     523


#All the outliers 
boxplot.stats(sys_tidy$TestResult)$out


#Convert these values to NA (values higher than 1000 or equal to 0)
sys_tidy$TestResult[sys_tidy$TestResult > 1000.00] <- NA
sys_tidy$TestResult[sys_tidy$TestResult == 0] <- NA


#Check if max and NA have changed 
summary(sys_tidy$TestResult)


#Categories the values / Create Missing category for NA's
sys_tidy$cat[65.00 <=sys_tidy$TestResult & sys_tidy$TestResult <= 99.99 ] <- 'Low'
sys_tidy$cat[100.00 <= sys_tidy$TestResult & sys_tidy$TestResult <= 119.99 ] <- 'Normal'
sys_tidy$cat[120.00<=sys_tidy$TestResult & sys_tidy$TestResult <= 139.99 ] <- 'Pre-High'
sys_tidy$cat[140.00<=sys_tidy$TestResult] <- 'High'  
sys_tidy$cat[is.na(sys_tidy$TestResult)] <- 'Missing'




#Distribution of the categories 
as.data.frame(table(sys_tidy$cat))


sys_tidy <- sys_tidy %>%
  select(PatientID, sys_Result = TestResult, sys_cat = cat)%>%
  right_join(tib_survived_patients, by = "PatientID")


sys <- table(sys_tidy$alive_1_year,sys_tidy$sys_cat)
sys




##### Diastolic Blood Pressure #####
dia_bp <- tib_vitals%>%
  group_by(PatientID) %>%
  filter(LabComponent=='Diastolic blood pressure') %>%
  right_join(tib_lot_treat_day, by = "PatientID")


dia_tidy <- dia_bp %>%
  group_by(PatientID) %>%
  arrange(desc(TestDate)) %>%
  filter((TestDate <= StartDate)| is.na(TestDate))%>%
  filter(row_number()==1) %>%
  #  slice(1) %>%
  distinct() %>%
  select(PatientID, TestDate, TestResult) %>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID,TestResult)


dia_tidy$TestResult <- as.numeric(as.character(dia_tidy$TestResult))
summary(dia_tidy$TestResult) # 523 11.78% missing 


#Detect/Remove outliers/weird values -> NA
#Boxplot
outlier_values_dia <- boxplot.stats(dia_tidy$TestResult)$out  # outlier values.
boxplot(dia_tidy$TestResult, main="Diastolic Blood Pressure", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_dia, collapse=", ")), cex=0.6) #600 very high 


#All the outliers 
boxplot.stats(dia_tidy$TestResult)$out


#Convert these values to NA
dia_tidy$TestResult[dia_tidy$TestResult > 600.00] <- NA
dia_tidy$TestResult[dia_tidy$TestResult == 0] <- NA


#Check if max and NA have changed 
summary(dia_tidy$TestResult)




#Categories the values / Create Missing category for NA's
dia_tidy$dia_cat[38.00<=dia_tidy$TestResult & dia_tidy$TestResult <= 60.00 ] <- 'Low'
dia_tidy$dia_cat[60.99<=dia_tidy$TestResult & dia_tidy$TestResult <= 80.00 ] <- 'Normal'
dia_tidy$dia_cat[80.99<=dia_tidy$TestResult & dia_tidy$TestResult <= 90.00 ] <- 'Pre-High'
dia_tidy$dia_cat[90.00<=dia_tidy$TestResult] <- 'High'
dia_tidy$dia_cat[is.na(dia_tidy$TestResult)] <- 'Missing'


dia_tidy <- dia_tidy %>%
  select(PatientID, dia_Result = TestResult, dia_cat )%>%
  right_join(tib_survived_patients, by = "PatientID")


dia <- table(dia_tidy$alive_1_year,dia_tidy$dia_cat)
dia


# Probability table: category vs mortality
dia /nrow(tib_treated_patients) *100


##### Pain Severity #####
## Pain severity - 0-10 verbal numeric rating ##
## test results before treatment start date and last result 


tib_pain <- as.tibble(vitals)%>%
  filter(LabComponent=='Pain severity - 0-10 verbal numeric rating')%>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID, TestDate, Test, TestResult, StartDate)


#Pain result before treatment 
pain_tidy <- tib_pain %>%
  group_by(PatientID) %>%
  arrange(desc(TestDate))%>%
  filter((TestDate <= StartDate)| is.na(TestDate))%>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  distinct() %>%
  select(PatientID, TestDate, TestResult) %>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID,TestResult) 


#create a category for NA
pain_tidy$TestResult <- as.numeric(as.character(pain_tidy$TestResult))
summary(pain_tidy$TestResult) #4011 missing 90.42% Missing 


pain_tidy <- pain_tidy %>%
  right_join(tib_survived_patients, by = "PatientID")%>%
  select(PatientID, pain = TestResult, alive_1_year )


pain <- table(pain_tidy$alive_1_year,pain_tidy$pain)
pain




### HEART RATE ###
heart_rate <- tib_vitals%>%
  group_by(PatientID) %>%
  filter(LabComponent=='Heart rate') %>%
  right_join(tib_lot_treat_day, by = "PatientID")


heart_rate_tidy <- heart_rate %>%
  group_by(PatientID) %>%
  arrange(desc(TestDate)) %>%
  filter((TestDate <= StartDate)| is.na(TestDate))%>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  distinct() %>%
  select(PatientID, TestDate, TestResult) %>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID,TestResult)


heart_rate_tidy$TestResult <- as.numeric(as.character(heart_rate_tidy$TestResult))
summary(heart_rate_tidy$TestResult) #375 % missing  - 8.45% missing 


#All the outliers 
boxplot.stats(heart_rate_tidy$TestResult)$out


#Convert these values to NA
heart_rate_tidy$TestResult[heart_rate_tidy$TestResult > 150.00] <- NA
heart_rate_tidy$TestResult[heart_rate_tidy$TestResult <= 20] <- NA


#Check if max and NA have changed 
summary(heart_rate_tidy$TestResult)


#Categorise the data 
heart_rate_tidy$cat[42.00<=heart_rate_tidy$TestResult & heart_rate_tidy$TestResult <= 59.99 ] <- 'Low'
heart_rate_tidy$cat[60.00<=heart_rate_tidy$TestResult & heart_rate_tidy$TestResult <= 100.00 ] <- 'Normal'
heart_rate_tidy$cat[100.99 <= heart_rate_tidy$TestResult] <- 'High'
heart_rate_tidy$cat[is.na(heart_rate_tidy$TestResult)] <- 'Missing'


heart_rate_tidy <- heart_rate_tidy %>%
  right_join(tib_survived_patients, by = "PatientID")%>%
  select(PatientID, heart_rate = TestResult, heart_rate_cat = cat, alive_1_year )


heart <- table(heart_rate_tidy$alive_1_year,heart_rate_tidy$heart_rate_cat)
heart




##8.56% Missing (Impute Median = 84.00 = Normal)##




### RESPIRATORY RATE ###
tib_res <- tib_vitals%>%
  group_by(PatientID) %>%
  filter(LabComponent=='Respiratory rate') %>%
  right_join(tib_lot_treat_day, by = "PatientID")


#RES rate before treatment start date. 
res_rate <- tib_res %>%
  filter((TestDate < StartDate)|(is.na(TestDate))) %>%
  group_by(PatientID) %>%
  filter(TestDate == max(TestDate)|(is.na(TestDate))) %>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  ungroup()%>%
  distinct()%>%
  select(PatientID, TestDate, TestResult) %>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID,TestResult) 


res_rate$TestResult <- as.numeric(as.character(res_rate$TestResult))
summary(res_rate$TestResult) #4197 missing 94.61% missing 


#All the outliers 
boxplot.stats(res_rate$TestResult)$out


#Categories the values / Create Missing category for NA's
res_rate$cat[40 <= res_rate$TestResult ] <- 'NA' #remove outlier greater than 40


res_rate$cat[res_rate$TestResult<12.00] <- 'Abnormal'
res_rate$cat[25 <= res_rate$TestResult ] <- 'Abnormal'
res_rate$cat[12.99<=res_rate$TestResult & res_rate$TestResult <= 24.99 ] <- 'Normal'
res_rate$cat[is.na(res_rate$cat)] <- 'Missing'




res_rate <- res_rate %>%
  right_join(tib_survived_patients, by = "PatientID")


res_rate <- res_rate %>%
  select(PatientID, res_rate_result = TestResult, res_rate_cat = cat, alive_1_year)


res_rate_tidy <- res_rate %>%
  select(PatientID, res_rate_result, res_rate_cat , alive_1_year)




res <- table(res_rate$alive_1_year,res_rate$res_rate_cat)
res




### ORIGINAL BMI ####
tib_old_bmi <- tib_vitals %>%
  group_by(PatientID)%>%
  filter( LabComponent == "Body mass index") %>%
  right_join(tib_lot_treat_day, by = "PatientID")


tib_old_bmi <- tib_old_bmi %>%
  filter((TestDate < StartDate)|(is.na(TestDate))) %>%
  group_by(PatientID) %>%
  filter(TestDate == max(TestDate)|(is.na(TestDate))) %>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  ungroup()%>%
  distinct()%>%
  select(PatientID, old_bmi = TestResult, TestResultCleaned)%>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID, old_bmi, TestResultCleaned)


tib_old_bmi$old_bmi <- as.numeric(as.character(tib_old_bmi$old_bmi))
summary(tib_old_bmi$old_bmi) #4355 98.17% missing


### HEIGHT ###
#Extract the height and weight and calculate BMI - 


tib_height <- tib_vitals%>%
  group_by(PatientID) %>%
  filter(LabComponent=='Body Height') %>%
  right_join(tib_lot_treat_day, by = "PatientID")


#Height before treatment start date 
before_height <- tib_height%>%
  group_by(PatientID)%>%
  arrange(desc(TestDate)) %>%
  filter((TestDate <= StartDate)| is.na(TestDate))%>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  distinct() %>%
  select(PatientID, TestDate, LabComponent, before_height_result = TestResultCleaned) %>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID,before_height_result)


sum(is.na(before_height$before_height_result))/ nrow(before_height) #297 missing 6.66%




#Impute Missing Height with Median 
before_height= transform(before_height, before_height_result = ifelse(is.na(before_height_result), median(before_height_result, na.rm=TRUE), before_height_result)) 
summary(before_height$before_height_result) 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 67.31  161.29  167.64  168.17  175.26  203.20


# Add alive or dead
before_height <- before_height %>% 
  full_join(tib_survived_patients,by= "PatientID")




### WEIGHT ###
tib_weight <-tib_vitals %>%
  group_by(PatientID)%>%
  filter(LabComponent == 'Body Weight') %>%
  right_join(tib_lot_treat_day, by = "PatientID")


#Weight Before Treatment Date 
before_weight <- tib_weight %>%
  group_by(PatientID) %>%
  arrange(desc(TestDate)) %>%
  filter((TestDate <= StartDate)| is.na(TestDate))%>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  distinct() %>%
  select(PatientID, TestDate, before_weight_result = TestResultCleaned) %>%
  select(PatientID,before_weight_result)%>%
  right_join(before_height, by="PatientID")


sum(is.na(before_weight$before_weight_result))/ nrow(before_weight) #471 missing  10.5%


## How much is missing 
weight_miss<-before_weight$PatientID[is.na(before_weight$before_weight_result) ]
height_miss<-before_weight$PatientID[is.na(before_weight$before_height_result) ]
bmi_missing_list <- unique(c(weight_miss,height_miss))
length(bmi_missing_list)


# Missing bmi table
tib_bmi_missing <- before_weight %>% mutate( bmi = before_weight_result/((before_height_result/100)^2))%>% 
  select(PatientID,bmi)




boxplot.stats(before_weight$before_weight_result)$out
# 128.4000 153.2234 137.8920 127.4594 138.9800 134.9890 131.0881 191.9148 139.8878 168.6455 134.8983 137.8920 169.5527 169.1898 132.3581 136.9848 151.4997 162.8395 130.4531 141.1034 129.7300 131.0881 131.0881
# 128.0944 154.0398 132.9025 132.4489 132.0860 145.9659 143.7887 128.3665 143.6100 127.9129 141.5207 128.9108 150.5925 141.7929 148.3246 138.3456 139.2527 144.9680 129.7273 145.1494 151.9533 149.3225 130.6799
# 137.4384 136.0776 137.9373 174.1793 127.8222 128.1851 180.9378 129.3644 155.1285 131.1788 174.1793 151.0461 127.9129 137.2569 154.6749 140.1599 128.3665 190.9622 142.2465 182.6161 159.8458 143.7887 136.9848
# 131.9953 130.6345


#Median Imputation 
before_weight= transform(before_weight, before_weight_result  = ifelse(is.na(before_weight_result ), median(before_weight_result , na.rm=TRUE), before_weight_result )) 
summary(before_weight$before_weight_result)


# Add alive or dead
before_weight <- before_weight %>% 
  select(PatientID, before_height_result, before_weight_result, alive_1_year = alive_1_year)


weight <- table(before_weight$alive_1_year,before_weight$before_weight_result)


before_weight <- before_weight %>% mutate(before_weight_result = as.numeric(before_weight_result) )
str(before_weight)
### NEW BMI ###
#Calculate BMI from height and Weight Before and After treatment 
#convert cm to meters
before_weight["before_height_result"] <- (before_weight["before_height_result"]*0.01) 


before_weight <- transform(before_weight, before_bmi1 = (before_weight_result / (before_height_result)^2))
before_weight$before_weight_result <- format(round(before_weight$before_weight_result, 2), nsmall = 2)
before_weight$before_bmi1 <- format(round(before_weight$before_bmi1, 2), nsmall = 2)


before_weight$before_bmi1 <- as.numeric(as.character(before_weight$before_bmi1))
summary(before_weight$before_bmi1)


before_weight$cat[before_weight$before_bmi1<18.49] <- 'Underweight'
before_weight$cat[18.50<=before_weight$before_bmi1 & before_weight$before_bmi1 <= 24.99 ] <- 'Normal'
before_weight$cat[25.00<=before_weight$before_bmi1 & before_weight$before_bmi1 <= 29.99 ] <- 'Overweight'
before_weight$cat[30.00<=before_weight$before_bmi1 & before_weight$before_bmi1 <= 40.99 ] <- 'Obese'
before_weight$cat[40.00 <= before_weight$before_bmi1 ] <- 'Very Obese'


as.data.frame(table(before_weight$alive_1_year))
as.data.frame(table(before_weight$cat))


bmi <- table(before_weight$alive_1_year,before_weight$cat)
bmi 


#   Normal Obese Overweight Underweight Very Obese
#0    715   559        797         106         88
#1    754   464        766         116         71


bmi_tidy <- before_weight %>%
  select(PatientID, weight = before_weight_result, height_m = before_height_result, bmi = before_bmi1, bmi_cat = cat, alive_1_year)


### BSA ###


tib_bsa <- tib_vitals%>%
  group_by(PatientID) %>%
  filter(LabComponent=='Body Surface Area (BSA)') %>%
  right_join(tib_lot_treat_day, by = "PatientID")


#Before BSA
bsa_tidy <- tib_bsa %>%
  group_by(PatientID) %>%
  arrange(desc(TestDate)) %>%
  filter((TestDate <= StartDate)| is.na(TestDate))%>%
  filter(row_number()==1) %>%
  #slice(1) %>%
  distinct() %>%
  select(PatientID, TestDate, before_bsa_result = TestResult) %>%
  right_join(tib_lot_treat_day, by = "PatientID")%>%
  select(PatientID,before_bsa_result)


bsa_tidy$before_bsa_result <- as.numeric(as.character(bsa_tidy$before_bsa_result))
summary(bsa_tidy$before_bsa_result) #4287 96.64%




#Create a missing category for bsa
bsa_tidy$category <- bsa_tidy$before_bsa_result
bsa_tidy$category <- as.numeric(as.character(bsa_tidy$category))






bsa_tidy <- bsa_tidy %>%
  right_join(tib_survived_patients, by = "PatientID")%>%
  select(PatientID, bsa = before_bsa_result, bsa_cat = category, alive_1_year )


bsa <- table(bsa_tidy$alive_1_year,bsa_tidy$bsa_cat)
bsa




####Drug Type###


drug_com_table <- read.csv("drug_combinations_table.csv")


tib_drug <-tib_lot %>% 
  select(PatientID, LineName, LineNumber, StartDate) %>%
  filter(LineNumber == 1) %>% 
  group_by(PatientID) %>%
  arrange(PatientID) %>%
  filter(StartDate ==min(StartDate)) %>%
  distinct() %>%
  ungroup()  #IndexDate = Start Date for therapy 


#A full drug table 
tib_drug <- tib_drug %>% 
  full_join(tib_treated_patients,by= "PatientID")


#table for counting drug frequency 
drug_frequency <-tib_drug %>% group_by(LineName)%>% 
  summarise(count = n()) %>%
  arrange(desc(count))


#Change category
colnames(tib_drug)[2]<-"Treatment"
tib_drug_cat <- tib_drug %>% 
  right_join(drug_com_table,  by = "Treatment")




#
tib_drug_cat<- tib_drug_cat %>% right_join(tib_survived_patients,by="PatientID" )


#Check distribution
table(tib_drug_cat$Class)
table(tib_drug_cat$Class)/ nrow(tib_treated_patients) *100






# categorize by dead and alive
table(tib_drug_cat$Class,tib_drug_cat$alive_1_year )
table(tib_drug_cat$Class,tib_drug_cat$alive_1_year )/nrow(tib_treated_patients) *100


#prep data
tib_drug_cat_prep <-tib_drug_cat %>% select(PatientID,treatment_class=Class)
tib_drug_cat_prep$treatment_class <- str_trim(tib_drug_cat_prep$treatment_class)
tib_drug_cat_prep$treatment_class <- str_replace_all(tib_drug_cat_prep$treatment_class ," ", "_")
tib_drug_cat_prep <-tib_drug_cat_prep %>% mutate(treatment_class=factor(treatment_class))
str(tib_drug_cat_prep) 




##### Weekend Visit/Admin #####
tib_day <- tib_lot_treat_day
tib_day$day <-weekdays(as.Date(tib_day$StartDate))


table(tib_day$day)


tib_day<-tib_day %>%
  mutate(day = ifelse(( (day != "Saturday") & 
                          (day != "Sunday")), "0", day))




tib_day<-tib_day %>%
  mutate(day = ifelse(( (day != "0")), "1", day))%>%
  select(c(PatientID, day))


table(tib_day$day)


#Complete Table
tib_hilary_complete <- bsa_tidy %>%
  right_join(bmi_tidy, by ="PatientID") %>%
  right_join(res_rate_tidy, by ="PatientID") %>%
  right_join(heart_rate_tidy, by ="PatientID") %>%
  right_join(pain_tidy, by = "PatientID")%>%
  right_join(dia_tidy, by = "PatientID")%>%
  right_join(sys_tidy, by = "PatientID")%>%
  right_join(tib_old_bmi, by = "PatientID") %>%
  left_join(tib_drug_cat_prep, by = "PatientID")%>%
  left_join(tib_day, by = "PatientID")


str(tib_hilary_complete)






###### HILARY FINAL TABLE ##### 
tib_hilary_complete <- tib_hilary_complete%>%
  select(PatientID, alive_dead =  alive_1_year.x, bmi, res_rate = res_rate_result, heart_rate, diastolic = dia_Result, systolic = sys_Result, pain, chemotherapy_type=treatment_class, weekend =day)






###############################




# LABs --------------------------------------------------------------------
# Making tibble for treatment date from lot first line therapy "lot"
# Patients who recieved 1st treatments. and remove duplicate patientId
#After remove duplication total 4436 patients 




tib_SCLC_treated_patient <- tib_lot_treat_day %>%
  select(PatientID)


#lab tibble
library(tidyverse)
tib_lab <- as_tibble(lab)


#Extract diagnosis date from SCLC
tib_sclc_dday_non_filter <- sclc %>%
  as_tibble() %>%
  select(PatientID,DiagnosisDate)


#Extracting lab table 
tib_lab_sel <- tib_lab %>%
  select(PatientID,TestDate,LOINC,Test,TestResultCleaned,TestUnitsCleaned, MinNormCleaned, MaxNormCleaned)


#write.csv(tib_lab_sel,"tib_lab_sel.csv",row.names = FALSE)


# LAB function ---------------------------------------------------


LOINC_list = list(Alt = c("1742-6","1743-4","1744-2"), AST = c("1920-8","30239-8"), Alb=c("1751-7"),  Alp=c("6768-6"), 
                  basophil_count=c("26444-0","704-7","705-4"), Bil_direct=c("15152-2","1968-7"), CRP = c("1988-5","76485-2","48421-2"),
                  CA125 = c("10334-1","2006-5"), CA153=c("6875-9","2007-3"), CA19_9=c("24108-3"), CA27_29 =c("17842-6"),
                  CEA =c("2039-6"), EGFR = c("14049-1", "21666-3"), ER = c("16112-5"), Ferritin=c("24373-3","2276-4","20567-4"),
                  Globulin=c("2336-6"), Glucose=c("2345-7"), HER2_serum=c("32996-1"),HER2_tissue=c("48676-1"),IgG=c("2465-3","17007-6"),
                  LDH=c("2532-0","14804-9"), Lymphocyte=c("26474-7","731-0","732-8","30364-4"), Monocyte=c("26484-6","742-7","743-5"),
                  Neutrophil=c("26499-4","751-8","753-4"), Progesterone=c("2839-9"),Protein_serum =c("2885-2"),RBC=c("26453-1","789-8"),
                  Sodium_serum = c("2951-2"), Testosterone =c("2986-8"), Troponin_I= c("10839-9","42757-5"), Wbc=c("26464-8","6690-2"),
                  creatinine_serum =c("2160-0"), Urea_nitrogen=c("3094-0"), hematocrit=c("20570-8","4544-3"), calcium_serum =c("17861-6","2000-8"), 
                  bilirubin_total= c("14629-0","1975-2","42719-5"), platelet_count =c("26515-7","777-3","778-1","49497-1"), hemoglobin=c("718-7", "20509-6")
                  
)


tib_lot_treat_day<- tib_lot_treat_day %>%
  select(-StartDate_time )


# Extracting different lab tib
Extract_labtib <- function( x_LOINC ){ 
  test<-tib_lab_sel %>%
    filter(LOINC %in% x_LOINC ) %>%
    right_join(tib_lot_treat_day, by="PatientID")  %>%
    filter((TestDate <= StartDate)|( is.na(TestDate))) %>%     #Some patients have testdate later then treatment....
    group_by(PatientID) %>%
    filter((TestDate == max(TestDate))|(is.na(TestDate))) %>%
    filter(!is.na(TestResultCleaned))%>%
    summarize(LOINC=first(LOINC),Test=first(Test),TestDate=first(TestDate),TestResultCleaned= mean(TestResultCleaned,na.rm = TRUE),
              MinNormCleaned=mean(MinNormCleaned,na.rm=TRUE),MaxNormCleaned=mean(MaxNormCleaned,na.rm = TRUE),  
              TestUnitsCleaned=first(TestUnitsCleaned), LineNumber=first(LineNumber),StartDate=first(StartDate))%>%
    right_join(tib_SCLC_treated_patient, by="PatientID")%>%
    ungroup() %>% 
    arrange(desc(TestResultCleaned)) 
}




lab_name_vector <- vector("character", length= (length(LOINC_list)))
missing_num <- vector("double", length(lab_name_vector))
missing_rate <- vector("double", length(lab_name_vector))
total_patients<-  vector("double", length(lab_name_vector))


for (i in seq_along(LOINC_list)) {
  j = 2*i-1 
  k = 2*i 
  
  # create lab tib  
  name <- paste("tib_lab_",names(LOINC_list[i]),sep = "") 
  assign(name, Extract_labtib(LOINC_list[[i]])) 
  lab_name_vector[i] <- names(LOINC_list[i])
  
  # summary of missing lab 
  missing_vector <- get(name) %>%  summarise_all(funs(sum(is.na(.)))) 
  missing_num[i] <-  missing_vector[[5]]
  missing_rate[i] <- missing_num[[i]]/nrow(tib_treated_patients)
  
  # Total patients in lab tests
  total_patients[i] <- nrow(get(name))
} 




tib_missing_lab <- tibble(lab_name = lab_name_vector, missing_num = missing_num, missing_rate= missing_rate, total_patients=total_patients)
tib_missing_lab
tib_missing_lab <- tib_missing_lab %>% arrange(missing_rate)


#Export tib_missing_lab to excel 
#library(xlsx)
#write.xlsx(tib_missing_lab, file = "tib_missing_lab.xlsx")


# missing rate in bar chart 
# ggplot(data=tib_missing_lab) + geom_col(aes(x= reorder(lab_name, missing_rate) , y=missing_rate)) + coord_flip()




#### Cleaning lab range  -----------------------------------------------------


Fillna_0_min_max <- function(x_tibble) { 
  x_tibble %>%
    replace_na(list(MinNormCleaned=median(x_tibble$MinNormCleaned, na.rm=TRUE),
                    MaxNormCleaned=median(x_tibble$MaxNormCleaned, na.rm=TRUE))) %>%
    mutate( MinNormCleaned = ifelse((MinNormCleaned==0 & MaxNormCleaned==0),
                                    median(x_tibble$MinNormCleaned, na.rm=TRUE),MinNormCleaned ), 
            MaxNormCleaned = ifelse((MinNormCleaned== median(x_tibble$MinNormCleaned, na.rm=TRUE) & MaxNormCleaned==0), 
                                    median(x_tibble$MaxNormCleaned, na.rm=TRUE),MaxNormCleaned))
}




#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3 


categorize_lab <- function(x_tibble){
  x_tibble %>% 
    mutate(category = case_when((TestResultCleaned < MinNormCleaned ) ~ 1, 
                                (TestResultCleaned <= MaxNormCleaned | TestResultCleaned == MinNormCleaned ) ~ 2,  
                                (TestResultCleaned >  MaxNormCleaned ) ~ 3,
                                is.na(TestResultCleaned) ~ 0 )) %>%
    mutate(category= factor(category, levels=c(0,1,2,3)))
}


# Hemoglobin -----------------------------------------------------------------
#Min max NA= median(), min=0, max=0 --> median 
tib_lab_hemoglobin <-  Fillna_0_min_max(tib_lab_hemoglobin)


#Categorize by Test result ---  "tib_lab_alt_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_hemoglobin_cat <- categorize_lab(tib_lab_hemoglobin) 


# Add alive or death
tib_lab_hemoglobin_cat <- tib_lab_hemoglobin_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Remove the outliers
tib_lab_hemoglobin_cat %>% arrange(desc(TestResultCleaned))
tib_lab_hemoglobin_cat %>% arrange(TestResultCleaned)


# Probability table: category
table(tib_lab_hemoglobin_cat$category, exclude = NULL)
table(tib_lab_hemoglobin_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_hemoglobin <- table(tib_lab_hemoglobin_cat$category, tib_lab_hemoglobin_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_hemoglobin /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_hemoglobin_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Hemoglobin ",x="Test Result (g/dL)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_hemoglobin_cat <- tib_lab_hemoglobin_cat %>% 
  mutate(category = factor(category)) 
hemoglobin_distribution <- ggplot(data=tib_lab_hemoglobin_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Hemoglobin Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_hemoglobin <- tib_lab_hemoglobin_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( hemoglobin_cat = category ) %>%
  mutate( Test = "Hemoglobin") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_hemoglobin_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   5.80   11.40   12.80   12.66   13.90   19.10     543 


# ALT -----------------------------------------------------------------
#Min max NA= median(), min=0, max=0 --> median 
tib_lab_Alt <-  Fillna_0_min_max(tib_lab_Alt)


#Categorize by Test result ---  "tib_lab_alt_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_alt_cat <- categorize_lab(tib_lab_Alt) 


# Add alive or death
tib_lab_alt_cat <- tib_lab_alt_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Remove the outliers
tib_lab_alt_cat %>% arrange(desc(TestResultCleaned))
tib_lab_alt_cat %>% arrange(TestResultCleaned)


# Probability table: category
table(tib_lab_alt_cat$category, exclude = NULL)
table(tib_lab_alt_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_alt <- table(tib_lab_alt_cat$category, tib_lab_alt_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_alt /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_alt_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Alanine aminotransferase ",x="Test Result (U/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_alt_cat <- tib_lab_alt_cat %>% 
  mutate(category = factor(category)) 
alt_distribution <- ggplot(data=tib_lab_alt_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Alanine aminotransferase Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_alt <- tib_lab_alt_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( alt_cat = category ) %>%
  mutate( Test = "ALT") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_alt_cat$TestResultCleaned)
#   Min.  1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.00   14.00   21.00   33.95   35.00  779.00     766 


# platelet_count -----------------------------------------------------------------
#Min max NA= median(), min=0, max=0 --> median 
tib_lab_platelet <-  Fillna_0_min_max(tib_lab_platelet_count)




# Change the unit if MIn, Max, result < 1 , < 0.00001 ,  if (minNorm=0 or NA) ==> use median,  . 
tib_lab_platelet  <-tib_lab_platelet %>% 
  mutate ( TestResultCleaned = case_when( ((TestResultCleaned < 0.001) & (MinNormCleaned <0.001)) ~TestResultCleaned*1000000,
                                          ((TestResultCleaned >= 0.001 ) ~ TestResultCleaned)),
           MinNormCleaned = case_when( ((MinNormCleaned <0.001)) ~ MinNormCleaned * 1000000,
                                       ((MinNormCleaned >= 0.001 ) ~ MinNormCleaned )),
           MaxNormCleaned = case_when( ((MaxNormCleaned <0.001)) ~ MaxNormCleaned*1000000,
                                       ((MaxNormCleaned >= 0.001 ) ~ MaxNormCleaned))) 


#Categorize by Test result ---  "tib_lab_alt_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_platelet_cat <- categorize_lab(tib_lab_platelet) 


# Add alive or death
tib_lab_platelet_cat <- tib_lab_platelet_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Remove the outliers
tib_lab_platelet_cat %>% arrange(desc(TestResultCleaned))
tib_lab_platelet_cat %>% arrange(TestResultCleaned)




# Probability table: category
table(tib_lab_platelet_cat$category, exclude = NULL)
table(tib_lab_platelet_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_platelet <- table(tib_lab_platelet_cat$category, tib_lab_platelet_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_platelet /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_platelet_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Platelet ",x="Test Result (U/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_platelet_cat <- tib_lab_platelet_cat %>% 
  mutate(category = factor(category)) 
platelet_distribution <- ggplot(data=tib_lab_platelet_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Alanine aminotransferase Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_platelet <- tib_lab_platelet_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( platelet_cat = category ) %>%
  mutate( Test = "platelet") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_platelet_cat$TestResultCleaned)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  11.0   206.8   265.0   285.0   339.0  1457.0    1113 




# Creatinine --------------------------------------------------------------
# Noraml range (  U/L) 




#Min max NA= median(), min=0, max=0 --> median 
tib_lab_creatinine_serum <-  Fillna_0_min_max(tib_lab_creatinine_serum)


#Categorize by Test result ---  "tib_lab_Alp_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_creatinine_serum_cat <- categorize_lab(tib_lab_creatinine_serum) 


# Add alive or death
tib_lab_creatinine_serum_cat <- tib_lab_creatinine_serum_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Remove the outliers
tib_lab_creatinine_serum_cat %>% arrange(desc(TestResultCleaned))


# Probability table: category
table(tib_lab_creatinine_serum_cat$category, exclude = NULL)
table(tib_lab_creatinine_serum_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_creatinine_serum <- table(tib_lab_creatinine_serum_cat$category, tib_lab_creatinine_serum_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_creatinine_serum /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_creatinine_serum_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Serum Creatinine Histogram",x="Test Result (mg/dL)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_creatinine_serum_cat <- tib_lab_creatinine_serum_cat %>% 
  mutate(category = factor(category)) 
creatinine_serum_distribution <- ggplot(data=tib_lab_creatinine_serum_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Serum Creatinine Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_creatinine_serum <- tib_lab_creatinine_serum_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( creatinine_cat = category ) %>%
  mutate( Test = "creatinine") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_creatinine_serum_cat$TestResultCleaned)
#   Min.  1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.2200  0.7000  0.8200  0.9114  1.0100 11.6400     519 


# Urea --------------------------------------------------------------------
#Min max NA= median(), min=0, max=0 --> median 
tib_lab_urea <-  Fillna_0_min_max(tib_lab_Urea_nitrogen)


#Categorize by Test result ---  "tib_lab_urea_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_urea_cat <- categorize_lab(tib_lab_urea) 


# Add alive or death
tib_lab_urea_cat <- tib_lab_urea_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Remove the outliers
tib_lab_urea_cat %>% arrange(desc(TestResultCleaned))
tib_lab_urea_cat %>% arrange(TestResultCleaned)


# Probability table: category
table(tib_lab_urea_cat$category, exclude = NULL)
table(tib_lab_urea_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_urea <- table(tib_lab_urea_cat$category, tib_lab_urea_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_urea /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_urea_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Serum urea",x="Test Result (mg/dL)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_urea_cat <- tib_lab_urea_cat %>% 
  mutate(category = factor(category)) 
urea_distribution <- ggplot(data=tib_lab_urea_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Serum Urea Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_urea <- tib_lab_urea_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( urea_cat = category ) %>%
  mutate( Test = "urea") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_urea_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   2.00   11.00   15.00   16.66   20.00   85.00     645 




# hematocrit --------------------------------------------------------------
#Min max NA= median(), min=0, max=0 --> median 
tib_lab_hematocrit <-  Fillna_0_min_max(tib_lab_hematocrit)


#Categorize by Test result ---  "tib_lab_hematocrit_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_hematocrit_cat <- categorize_lab(tib_lab_hematocrit) 


# Add alive or death
tib_lab_hematocrit_cat <- tib_lab_hematocrit_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Remove the outliers
tib_lab_hematocrit_cat %>% arrange(desc(TestResultCleaned))
tib_lab_hematocrit_cat %>% arrange(TestResultCleaned)


# Probability table: category
table(tib_lab_hematocrit_cat$category, exclude = NULL)
table(tib_lab_hematocrit_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_hematocrit <- table(tib_lab_hematocrit_cat$category, tib_lab_hematocrit_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_hematocrit /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_hematocrit_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Hematocrit",x="Test Result (mg/dL)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_hematocrit_cat <- tib_lab_hematocrit_cat %>% 
  mutate(category = factor(category)) 
hematocrit_distribution <- ggplot(data=tib_lab_hematocrit_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Hematocrit Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_hematocrit <- tib_lab_hematocrit_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( hematocrit_cat = category ) %>%
  mutate( Test = "hematocrit") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_hematocrit_cat$TestResultCleaned)
#  Min.   1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  16.60   34.90   38.50   38.27   41.90   60.10     663 




# calcium -----------------------------------------------------------------
#Min max NA= median(), min=0, max=0 --> median 
tib_lab_calcium <-  Fillna_0_min_max(tib_lab_calcium_serum)


#Categorize by Test result ---  "tib_lab_hematocrit_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_calcium_cat <- categorize_lab(tib_lab_calcium) 


# Add alive or death
tib_lab_calcium_cat <- tib_lab_calcium_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Remove the outliers
tib_lab_calcium_cat %>% arrange(desc(TestResultCleaned))
tib_lab_calcium_cat %>% arrange(TestResultCleaned)


# Probability table: category
table(tib_lab_calcium_cat$category, exclude = NULL)
table(tib_lab_calcium_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_calcium <- table(tib_lab_calcium_cat$category, tib_lab_calcium_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_calcium /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_calcium_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Calcium",x="Test Result (mg/dL)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_calcium_cat <- tib_lab_calcium_cat %>% 
  mutate(category = factor(category)) 
calcium_distribution <- ggplot(data=tib_lab_calcium_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Calcium Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_calcium <- tib_lab_calcium_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( calcium_cat = category ) %>%
  mutate( Test = "Calcium") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_calcium_cat$TestResultCleaned)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.190   8.900   9.300   9.299   9.600  15.000     748




# Bilirubin total -----------------------------------------------------------------
#Min max NA= median(), min=0, max=0 --> median 
tib_lab_bilirubin <-  Fillna_0_min_max(tib_lab_bilirubin_total)


#Categorize by Test result ---  "tib_lab_hematocrit_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_bilirubin_cat <- categorize_lab(tib_lab_bilirubin) 


# Add alive or death
tib_lab_bilirubin_cat <- tib_lab_bilirubin_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# check the outliers
tib_lab_bilirubin_cat %>% arrange(desc(TestResultCleaned))
tib_lab_bilirubin_cat %>% arrange(TestResultCleaned)


# Probability table: category
table(tib_lab_bilirubin_cat$category, exclude = NULL)
table(tib_lab_bilirubin_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_bilirubin <- table(tib_lab_bilirubin_cat$category, tib_lab_bilirubin_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_bilirubin /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_bilirubin_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Bilirubin",x="Test Result (mg/dL)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_bilirubin_cat <- tib_lab_bilirubin_cat %>% 
  mutate(category = factor(category)) 
bilirubin_distribution <- ggplot(data=tib_lab_bilirubin_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Bilirubin Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_bilirubin <- tib_lab_bilirubin_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( bilirubin_cat = category ) %>%
  mutate( Test = "Bilirubin") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_bilirubin_cat$TestResultCleaned)
#   Min.       1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
# 0.1000000  0.3000000  0.4200000  0.6096726  0.6000000 22.1000000        810 




# ALP ---------------------------------------------------------------------
# Noraml range (44 - 147 U/L) 


#Remove outlier ptientID = F5D716CF55F9C to NA
tib_lab_Alp <- tib_lab_Alp %>% 
  replace_with_na_at(.var =c("TestResultCleaned","MinNormCleaned","MaxNormCleaned"),
                     condition = ~.x >10000 )




#Min max NA= median(), min=0, max=0 --> median 
tib_lab_Alp <-  Fillna_0_min_max(tib_lab_Alp)


#Categorize by Test result ---  "tib_lab_Alp_cat "


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_Alp_cat <- categorize_lab(tib_lab_Alp) 


# Add alive or death
tib_lab_Alp_cat <- tib_lab_Alp_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Probability table: category
table(tib_lab_Alp_cat$category, exclude = NULL)
table(tib_lab_Alp_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100


# Frequency_table 2: category vs mortality
frequency_table_alp <- table(tib_lab_Alp_cat$category, tib_lab_Alp_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_alp /nrow(tib_treated_patients) *100


# Histogram 
ggplot(data=tib_lab_Alp_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Alkaline Phosphatase Histogram",x="Test Result (U/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


# Bar Plot Category dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_Alp_cat <- tib_lab_Alp_cat %>% 
  mutate(category = factor(category)) 
Alp_distribution <- ggplot(data=tib_lab_Alp_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Alkaline Phosphatase Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


#prepare tib to merge
tib_prep_ALP <- tib_lab_Alp_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( ALP_cat = category ) %>%
  mutate( Test = "ALP") %>%
  spread( Test, TestResultCleaned) 


#Summary 
summary(tib_lab_Alp_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  21.0    69.0    89.0   124.8   119.0  2127.0     728




# AST ---------------------------------------------------------------------
# Normal range (10-40 U/L)


#  Min_NA, max_NA= median()   -------   if min, max == 0 --> min max = median()   
tib_lab_AST <- tib_lab_AST %>% 
  replace_na(list(MinNormCleaned=median(tib_lab_AST$MinNormCleaned, na.rm=TRUE),
                  MaxNormCleaned=median(tib_lab_AST$MaxNormCleaned, na.rm=TRUE))) %>% 
  mutate( MinNormCleaned = ifelse((MinNormCleaned==0 & MaxNormCleaned==0),
                                  median(tib_lab_AST$MinNormCleaned, na.rm=TRUE),MinNormCleaned ), 
          MaxNormCleaned = ifelse((MinNormCleaned== median(tib_lab_AST$MinNormCleaned, na.rm=TRUE) & MaxNormCleaned==0), 
                                  median(tib_lab_AST$MaxNormCleaned, na.rm=TRUE),MaxNormCleaned))


#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_AST_cat <- categorize_lab(tib_lab_AST)   


# Add alive or death
tib_lab_AST_cat <- tib_lab_AST_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Probability table: category
table(tib_lab_AST_cat$category, exclude = NULL)
table(tib_lab_AST_cat$category, exclude = NULL)/nrow(tib_treated_patients) *100
# Frequency_table 2: category vs mortality
frequency_table_AST <- table(tib_lab_AST_cat$category, tib_lab_AST_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_AST /nrow(tib_treated_patients) *100




# Color Plot dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_AST_cat <- tib_lab_AST_cat %>% 
  mutate(category = factor(category), alive_1_year = factor(alive_1_year)) 
AST_distribution <- ggplot(data=tib_lab_AST_cat,aes(x = category, y=(..count..)/sum(..count..), fill = alive_1_year))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Aspartate Aminotransferase Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=15))


# Histogram 
ggplot(data=tib_lab_AST_cat, aes(x=TestResultCleaned, fill=alive_1_year))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Patients' Aspartate Aminotransferase Histogram",x="Test Result (U/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


#prepare tib to merge
tib_prep_AST <- tib_lab_AST_cat %>% 
  select( PatientID, Test, TestResultCleaned, category) %>%
  rename( AST_cat = category ) %>%
  mutate( Test = "AST") %>%
  spread( Test, TestResultCleaned) 


summary(tib_lab_AST_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  5.00   17.00   23.00   35.98   34.00  610.00     733 






# Serum Protein -----------------------------------------------------------
# Normal range (60-80 g/L)




#  Min_NA, max_NA= median()   -------   if min, max == 0 --> min max = median()   
tib_lab_Protein_serum <-  Fillna_0_min_max(tib_lab_Protein_serum)


#Categorize by Test result
#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_Protein_serum_cat <- categorize_lab(tib_lab_Protein_serum)   


# Add alive or death
tib_lab_Protein_serum_cat <- tib_lab_Protein_serum_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Probability table: category
table(tib_lab_Protein_serum_cat$category, exclude = NULL)
table(tib_lab_Protein_serum_cat$category, exclude = NULL)/nrow(tib_treated_patients) *100
# Frequency_table 2: category vs mortality
frequency_table_Protein_serum <- table(tib_lab_Protein_serum_cat$category, tib_lab_Protein_serum_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_Protein_serum /nrow(tib_treated_patients)*100


# Color Plot dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_Protein_serum_cat <- tib_lab_Protein_serum_cat %>% 
  mutate(category = factor(category)) 
serum_protein_distribution <- ggplot(data=tib_lab_Protein_serum_cat,aes(x = category, y=(..count..)/sum(..count..), fill = factor(alive_1_year)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Serum protein Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Histogram 
ggplot(data=tib_lab_Protein_serum_cat, aes(x=TestResultCleaned, fill=factor(alive_1_year)))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Serum Protein Histogram",x="Test Result (g/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


#prepare tib to merge
tib_prep_Serum_protein <- tib_lab_Protein_serum_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( serum_protein_cat = category ) %>%
  mutate( Test = "Serum_protein") %>%
  spread( Test, TestResultCleaned) 


summary(tib_lab_Protein_serum_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  41.00   64.00   68.00   67.95   72.00  131.00     815 


# ALB ---------------------------------------------------------------------
# Normal range (35-50 g/L)




ggplot(data=tib_lab_Protein_serum)+geom_boxplot(aes(y=tib_lab_Protein_serum$TestResultCleaned))
ggplot(data=tib_lab_Protein_serum)+geom_histogram(aes(x=tib_lab_Protein_serum$TestResultCleaned))


#  Min_NA, max_NA= median()   -------   if min, max == 0 --> min max = median()   
tib_lab_Alb <-  Fillna_0_min_max(tib_lab_Alb)


#Categorize by Test result
#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_Alb_cat <- categorize_lab(tib_lab_Alb) 


# Add alive or death
tib_lab_Alb_cat <- tib_lab_Alb_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Probability table: category
table(tib_lab_Alb_cat$category, exclude = NULL)
table(tib_lab_Alb_cat$category, exclude = NULL)/nrow(tib_treated_patients)*100
# Frequency_table 2: category vs mortality
frequency_table_Alb <- table(tib_lab_Alb_cat$category, tib_lab_Alb_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_Alb /nrow(tib_treated_patients) *100


# Color Plot dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_Alb_cat <- tib_lab_Alb_cat %>% 
  mutate(category = factor(category)) 
albumin_distribution <- ggplot(tib_lab_Alb_cat,aes(x = category, y=(..count..)/sum(..count..), fill = factor(alive_1_year)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Albumin Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


# Histogram 
ggplot(data=tib_lab_Alb_cat, aes(x=TestResultCleaned, fill=factor(alive_1_year)))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Albumin Histogram",x="Test Result (g/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


#prepare tib to merge
tib_prep_Alb <- tib_lab_Alb_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( Alb_cat = category ) %>%
  mutate( Test = "Albumin") %>%
  spread( Test, TestResultCleaned) 


summary(tib_lab_Alb_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  17.00   34.00   38.00   37.43   41.00   51.00     896  


# Glucose -----------------------------------------------------------------
# Normal range (70-110 g/L)




ggplot(data=tib_lab_Glucose)+geom_boxplot(aes(y=tib_lab_Glucose$TestResultCleaned))
ggplot(data=tib_lab_Glucose)+geom_histogram(aes(x=tib_lab_Glucose$TestResultCleaned))


#  Min_NA, max_NA= median()   -------   if min, max == 0 --> min max = median()   
tib_lab_Glucose <-  Fillna_0_min_max(tib_lab_Glucose)


#Categorize by Test result
#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_Glucose_cat <- categorize_lab(tib_lab_Glucose) 


# Add alive or death
tib_lab_Glucose_cat <- tib_lab_Glucose_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Probability table: category
table(tib_lab_Glucose_cat$category, exclude = NULL)
table(tib_lab_Glucose_cat$category, exclude = NULL)/nrow(tib_treated_patients) * 100
# Frequency_table 2: category vs mortality
frequency_table_glucose <- table(tib_lab_Glucose_cat$category, tib_lab_Glucose_cat$alive_1_year)
# Probability table: category vs mortality
frequency_table_glucose /nrow(tib_treated_patients) *100 


# Color Plot dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_Glucose_cat <- tib_lab_Glucose_cat %>% 
  mutate(category = factor(category)) 
glucose_distribution <- ggplot(tib_lab_Glucose_cat,aes(x = category, y=(..count..)/sum(..count..), fill = factor(alive_1_year)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Blood Glucose Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


# Histogram 
ggplot(data=tib_lab_Glucose_cat, aes(x=TestResultCleaned, fill=factor(alive_1_year)))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Blood Glucose Histogram",x="Test Result (mg/dL)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


#prepare tib to merge
tib_prep_glucose <- tib_lab_Glucose_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( glucose_cat = category ) %>%
  mutate( Test = "Glucose") %>%
  spread( Test, TestResultCleaned) 


summary(tib_lab_Glucose_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   35.0    96.0   111.0   127.7   139.0   641.0     927  


# RBC ---------------------------------------------------------------------
# Normal range (male: 4.6-6.2 x 10^12/L , female: 4.2-5.4 *10^12)
unique(tib_lab_RBC$LOINC)
unique(tib_lab_RBC$TestUnitsCleaned)


# pickout the abnormal low value 
low_RBC <- tib_lab_RBC %>% filter(MinNormCleaned < 1) 
write.csv(low_RBC, file = "low_RBC.csv", row.names = FALSE)


# pickout the abnormal high value 
high_RBC <- tib_lab_RBC %>% filter(MinNormCleaned > 1000) 
write.csv(high_RBC, file = "high_RBC.csv", row.names = FALSE)


# Change the unit if MIn, Max, result >1000 . 
tib_lab_RBC <- tib_lab_RBC %>% 
  mutate( TestResultCleaned = ifelse((TestResultCleaned > 1000), TestResultCleaned/1000, TestResultCleaned ),
          MinNormCleaned = ifelse((MinNormCleaned > 1000), MinNormCleaned/1000, MinNormCleaned ),
          MaxNormCleaned = ifelse((MaxNormCleaned > 1000), MaxNormCleaned/1000, MaxNormCleaned )) 




# Change the unit if MIn, Max, result < 1 , < 0.00001 ,  if (minNorm=0 or NA) ==> use median,  . 
tib_lab_RBC <-tib_lab_RBC %>% 
  mutate ( TestResultCleaned = case_when( ((TestResultCleaned < 0.00001) & (MinNormCleaned <0.00001)) ~TestResultCleaned*1000000,
                                          ((TestResultCleaned < 0.01) & (MinNormCleaned < 0.01)) ~ TestResultCleaned*1000,
                                          ((TestResultCleaned >= 0.01 ) ~ TestResultCleaned)),
           MinNormCleaned = case_when( ((MinNormCleaned <0.00001)) ~ MinNormCleaned * 1000000,
                                       ((MinNormCleaned < 0.01)) ~ MinNormCleaned * 1000,
                                       ((MinNormCleaned >= 0.01 ) ~ MinNormCleaned )),
           MaxNormCleaned = case_when( ((MaxNormCleaned <0.00001)) ~ MaxNormCleaned*1000000,
                                       ((MaxNormCleaned < 0.01)) ~ MaxNormCleaned * 1000,
                                       ((MaxNormCleaned >= 0.01 ) ~ MaxNormCleaned))) %>%
  mutate(MinNormCleaned = ifelse((MinNormCleaned==0 | is.na(MinNormCleaned)) , median(tib_lab_RBC$MinNormCleaned, na.rm=TRUE), MinNormCleaned),
         MaxNormCleaned = ifelse((MaxNormCleaned==0 | is.na(MaxNormCleaned)), median(tib_lab_RBC$MaxNormCleaned, na.rm=TRUE), MaxNormCleaned))






#  Min_NA, max_NA= median()   -------   if min, max == 0 --> min max = median()   
tib_lab_RBC <-  Fillna_0_min_max(tib_lab_RBC)


tib_lab_RBC <- tib_lab_RBC %>% arrange(TestResultCleaned)








#Categorize by Test result
#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_RBC_cat <- categorize_lab(tib_lab_RBC) 


# Add alive or death
tib_lab_RBC_cat <- tib_lab_RBC_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Probability table: category
table(tib_lab_RBC_cat$category, exclude = NULL)
table(tib_lab_RBC_cat$category, exclude = NULL)/nrow(tib_treated_patients) * 100
# Frequency_table 2: category vs mortality
frequency_table_RBC <- table(tib_lab_RBC_cat$category, tib_lab_RBC_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_RBC /nrow(tib_treated_patients) *100 


# Color Plot dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_RBC_cat <- tib_lab_RBC_cat %>% 
  mutate(category = factor(category)) 
RBC_distribution <- ggplot(tib_lab_RBC_cat,aes(x = category, y=(..count..)/sum(..count..), fill = factor(alive_1_year)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Red Blood Cell Count Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


# Histogram 
ggplot(data=tib_lab_RBC_cat, aes(x=TestResultCleaned, fill=factor(alive_1_year)))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="Red Blood Cell Histogram",x="Test Result (10^12/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


#prepare tib to merge
tib_prep_RBC <- tib_lab_RBC_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( RBC_cat = category ) %>%
  mutate( Test = "RBC") %>%
  spread( Test, TestResultCleaned) 


summary(tib_lab_RBC_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   3.750   4.200   4.015   4.610   6.700    1071   


# Lymphocyte ---------------------------------------------------------------
# Normal range (1.5-3 x 10^9/L)
unique(tib_lab_Lymphocyte$LOINC)
unique(tib_lab_Lymphocyte$TestUnitsCleaned)




ggplot(data=tib_lab_Lymphocyte)+geom_boxplot(aes(y=tib_lab_Lymphocyte$TestResultCleaned))
ggplot(data=tib_lab_Lymphocyte)+geom_histogram(aes(x=tib_lab_Lymphocyte$TestResultCleaned))


#  Min_NA, max_NA= median()   -------   if min, max == 0 --> min max = median()   
tib_lab_Lymphocyte <-  Fillna_0_min_max(tib_lab_Lymphocyte)


#Categorize by Test result
#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_Lymphocyte_cat <- categorize_lab(tib_lab_Lymphocyte)


# Add alive or death
tib_lab_Lymphocyte_cat <- tib_lab_Lymphocyte_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Probability table: category
table(tib_lab_Lymphocyte_cat$category, exclude = NULL)
table(tib_lab_Lymphocyte_cat$category, exclude = NULL)/nrow(tib_treated_patients) * 100
# Frequency_table 2: category vs mortality
frequency_table_lymphocyte <- table(tib_lab_Lymphocyte_cat$category, tib_lab_Lymphocyte_cat$alive_1_year, exclude = NULL)
# Probability table: category vs mortality
frequency_table_lymphocyte /nrow(tib_treated_patients) *100 


# Color Plot dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_Lymphocyte_cat <- tib_lab_Lymphocyte_cat %>% 
  mutate(category = factor(category)) 
lymphocyte_distribution <- ggplot(tib_lab_Lymphocyte_cat,aes(x = category, y=(..count..)/sum(..count..), fill = factor(alive_1_year)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Lymphocyte Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


# Histogram 
ggplot(data=tib_lab_Lymphocyte_cat, aes(x=TestResultCleaned, fill=factor(alive_1_year)))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title=" Lymphocyte Histogram",x="Test Result (10^9/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 


#prepare tib to merge
tib_prep_lymph <- tib_lab_Lymphocyte_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( Lymph_cat = category ) %>%
  mutate( Test = "Lymphocyte") %>%
  spread( Test, TestResultCleaned) 


summary(tib_lab_Lymphocyte_cat$TestResultCleaned)


#   Min.   1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   1.050   1.500   1.692   2.000 110.000    1131   


# WBC ---------------------------------------------------------------------
# Normal range (3.5-12.0 x 10^9/L)
unique(tib_lab_Wbc$LOINC)
unique(tib_lab_Wbc$TestUnitsCleaned)
summary(tib_lab_Wbc$TestResultCleaned)


#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.00    4.20    5.60   10.16    7.80 6208.00    2620     


#  Min_NA, max_NA= median()   -------   if min, max == 0 --> min max = median()   
tib_lab_Wbc <-  Fillna_0_min_max(tib_lab_Wbc)


#Categorize by Test result
#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_Wbc_cat <- categorize_lab(tib_lab_Wbc)


# check the outliers
tib_lab_Wbc_cat %>% arrange(desc(TestResultCleaned))
tib_lab_Wbc_cat %>% arrange(TestResultCleaned)


# Add alive or death
tib_lab_Wbc_cat <- tib_lab_Wbc_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Chenge outliers   # PatientID = F99C77F84D48A 
tib_lab_Wbc_cat <- tib_lab_Wbc_cat %>% mutate(TestResultCleaned = ifelse((TestResultCleaned>1000) , TestResultCleaned/1000 ,TestResultCleaned))%>%
  mutate(MinNormCleaned = ifelse((MinNormCleaned>1000), MinNormCleaned/1000 , MinNormCleaned))%>%
  mutate(MaxNormCleaned = ifelse((MaxNormCleaned>1000), MaxNormCleaned/1000 , MaxNormCleaned))
# filter those with low WBC 




tib_lab_Wbc_cat <- tib_lab_Wbc_cat %>% mutate(TestResultCleaned = ifelse((TestResultCleaned<0.001) , TestResultCleaned*1000000 ,TestResultCleaned))%>%
  mutate(MinNormCleaned = ifelse((MinNormCleaned<0.001), MinNormCleaned*1000000 , MinNormCleaned))%>%
  mutate(MaxNormCleaned = ifelse((MaxNormCleaned<0.001), MaxNormCleaned*1000000 , MaxNormCleaned))


tib_lab_Wbc_cat<-tib_lab_Wbc_cat %>% mutate(TestResultCleaned = ifelse((PatientID=="F75BBD363489B") , TestResultCleaned*1000,TestResultCleaned))%>%
  mutate(MinNormCleaned = ifelse((PatientID=="F75BBD363489B"), MinNormCleaned*1000 , MinNormCleaned))%>%
  mutate(MaxNormCleaned = ifelse((PatientID=="F75BBD363489B"), MaxNormCleaned*1000 , MaxNormCleaned))




# Probability table: category
table(tib_lab_Wbc_cat$category, exclude = NULL)
table(tib_lab_Wbc_cat$category, exclude = NULL)/nrow(tib_treated_patients) * 100
# Frequency_table 2: category vs mortality
frequency_table_wbc <- table(tib_lab_Wbc_cat$category, tib_lab_Wbc_cat$alive_1_year)
# Probability table: category vs mortality
frequency_table_wbc /nrow(tib_treated_patients) *100 


# Color Plot dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_Wbc_cat <- tib_lab_Wbc_cat %>% 
  mutate(category = factor(category)) 
wbc_distribution <- ggplot(tib_lab_Wbc_cat,aes(x = category, y=(..count..)/sum(..count..), fill = factor(alive_1_year)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' White Blood Cell Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


# Histogram 
ggplot(data=tib_lab_Wbc_cat, aes(x=TestResultCleaned, fill=factor(alive_1_year)))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title=" White Blood Cell Histogram",x="Test Result (10^9/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 




#prepare tib to merge
tib_prep_WBC <- tib_lab_Wbc_cat %>% 
  select(PatientID, Test, TestResultCleaned, category) %>%
  rename( WBC_cat = category ) %>%
  mutate( Test = "WBC") %>%
  spread( Test, TestResultCleaned) 


summary(tib_lab_Wbc_cat$TestResultCleaned)


#    Min.     1st Qu.     Median       Mean    3rd Qu.       Max.       NA's 
#  2.090000   6.700000   8.405000   9.581432  10.800000 489.000000        969 


# LDH ---------------------------------------------------------------------
# Normal range (100–190 /L)
unique(tib_lab_LDH$LOINC)
unique(tib_lab_LDH$TestUnitsCleaned)




ggplot(data=tib_lab_LDH)+geom_boxplot(aes(y=tib_lab_LDH$TestResultCleaned))
ggplot(data=tib_lab_LDH)+geom_histogram(aes(x=tib_lab_LDH$TestResultCleaned))


#  Min_NA, max_NA= median()   -------   if min, max == 0 --> min max = median()   
tib_lab_LDH <-  Fillna_0_min_max(tib_lab_LDH)


#Categorize by Test result
#                minNorm          MaxNorm    
# -----------------|-----------------|-------------------
#        1                  2                  3


tib_lab_LDH_cat <- categorize_lab(tib_lab_LDH)


# Add alive or death
tib_lab_LDH_cat <- tib_lab_LDH_cat %>% 
  full_join(tib_survived_patients,by= "PatientID")


# Probability table: category
table(tib_lab_LDH_cat$category, useNA = "always")
table(tib_lab_LDH_cat$category, useNA = "always")/nrow(tib_treated_patients) * 100
# Frequency_table 2: category vs mortality
frequency_table_LDH <- table(tib_lab_LDH_cat$category, tib_lab_LDH_cat$alive_1_year)
# Probability table: category vs mortality
frequency_table_LDH /nrow(tib_treated_patients) *100 


# Color Plot dsitribution 
x_label= c("missing", " < min normal", "Normal ", "> max normal")
fill_label =c ("Alive", "Dead" )
tib_lab_LDH_cat <- tib_lab_LDH_cat %>% 
  mutate(category = factor(category)) 
LDH_distribution <- ggplot(tib_lab_LDH_cat,aes(x = category, y=(..count..)/sum(..count..), fill = factor(alive_1_year)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Range",  labels = x_label) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Lactate dehydrogenase Distribution") +
  geom_text(aes( label = (..count..), y= (..count..)/sum(..count..)), stat= "count", position = position_dodge(0.9), vjust = -.5)+
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 15))


# Histogram 
ggplot(data=tib_lab_LDH_cat, aes(x=TestResultCleaned, fill=factor(alive_1_year)))+
  geom_histogram( position="identity", alpha=0.5)+
  labs(title="  Lactate dehydrogenase Histogram",x="Test Result (U/L)", y = "Patient number")+
  scale_fill_discrete(name= "1 year mortality", labels = c("Alive","Death")) 




#prepare tib to merge
tib_prep_LDH <- tib_lab_LDH_cat %>% 
  select(PatientID,Test, TestResultCleaned, category, alive_1_year) %>%
  rename( LDH_cat = category ) %>%
  mutate( Test = "LDH") %>%
  spread(Test, TestResultCleaned) 


summary(tib_lab_LDH_cat$TestResultCleaned)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   98.0   187.0   261.0   463.4   463.0 25685.0    2995


### Ecog --------------------------------------------------------------------
# ecog tibble for patients from sclc diagnosis join 1-line treatment day (n=4436)
tib_ecog <- as_tibble(ecog) %>%
  select(PatientID, EcogDate, EcogValue) %>%
  right_join(tib_sclc_dday_non_filter, by="PatientID") %>%
  right_join(tib_lot_treat_day, by="PatientID") 


# get patients who ecog done before treatment day and NA's, also their latest ecog test.  
tib_ecog <- tib_ecog %>% 
  filter((EcogDate < StartDate)|(is.na(EcogDate))) %>% 
  group_by(PatientID) %>%
  filter(EcogDate == max(EcogDate)|(is.na(EcogDate))) %>%
  ungroup() %>%
  right_join(tib_SCLC_treated_patient, by="PatientID")


# Distribution
#  0    1    2    3     4 <NA> 
# 688  921  378   75    6 2368
table(tib_ecog$EcogValue, useNA = "always")
round((table(tib_ecog$EcogValue, useNA = "always")/nrow(tib_treated_patients) *100),2)


tib_ecog <- tib_ecog %>% right_join(tib_survived_patients,by="PatientID")


# Frequency_table 2: category vs mortality
frequency_table_ecog <- table(tib_ecog$EcogValue, tib_ecog$alive_1_year, useNA = "always")
# Probability table: category vs mortality
round((frequency_table_ecog /nrow(tib_treated_patients) *100) ,2)


# Color Plot dsitribution 
fill_label =c ("Alive", "Dead" )
plot_ecog <- ggplot(tib_ecog,aes(x = factor(EcogValue), y=(..count..)/sum(..count..), fill = factor(alive_1_year)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Ecog Value", labels=c("0","1","2","3","4", "Missing")) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Patients' Ecog Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))
# ggplot chart for ecog
plot_ecog <- ggplot(data= tib_ecog) + geom_bar(mapping=aes(x=EcogValue))






# Times of visiting  -------------------------------------------------------
tib_visit <- as.tibble(visit)


a <- tib_visit %>% select(-c(LOAD_ID,YEAR_MONTH))%>%
  right_join(tib_sclc_dday_non_filter,by="PatientID" )%>%
  right_join(tib_SCLC_treated_patient,by="PatientID") %>%
  right_join(tib_lot_treat_day, by="PatientID") %>%
  select(-LineNumber)%>%
  filter( (VisitDate < StartDate) & (VisitDate >= DiagnosisDate )) %>%
  group_by(PatientID) %>%
  summarise( visiting_time = n())


tib_visiting_time <- a %>% right_join(tib_SCLC_treated_patient,by="PatientID")
summary(tib_visiting_time$visiting_time)


# Making total table & Visualize  -----------------------------------------------------


tib_lab_prep <- tib_prep_LDH %>%
  right_join(tib_prep_Alb, by="PatientID") %>%
  right_join(tib_prep_ALP, by="PatientID") %>%
  right_join(tib_prep_AST, by="PatientID") %>%
  right_join(tib_prep_glucose, by="PatientID") %>%
  right_join(tib_prep_lymph, by="PatientID") %>%
  right_join(tib_prep_RBC, by="PatientID") %>%
  right_join(tib_prep_Serum_protein, by="PatientID") %>%
  right_join(tib_prep_WBC, by="PatientID") %>%  
  right_join(tib_prep_calcium, by="PatientID") %>%
  right_join(tib_prep_bilirubin, by="PatientID") %>%
  right_join(tib_prep_urea, by="PatientID") %>%
  right_join(tib_prep_hematocrit, by="PatientID") %>%
  right_join(tib_prep_creatinine_serum, by="PatientID")%>%
  right_join(tib_prep_hemoglobin, by="PatientID")%>%
  right_join(tib_prep_alt, by="PatientID")%>%
  right_join(tib_prep_platelet, by="PatientID")


str(tib_lab_prep)


tib_lab_prep_miss <- tib_lab_prep %>% 
  select (PatientID, LDH, ALP, AST, Albumin, Glucose, Lymphocyte, RBC, Serum_protein, WBC, Calcium, Bilirubin, urea, hematocrit, creatinine, Hemoglobin, ALT, platelet)


tib_lab_prep_cat <- tib_lab_prep %>% 
  select (PatientID, alive_1_year, LDH_cat, ALP_cat, AST_cat, Alb_cat, glucose_cat, Lymph_cat, RBC_cat, serum_protein_cat, WBC_cat,calcium_cat,bilirubin_cat,urea_cat,hematocrit_cat,creatinine_cat,hemoglobin_cat,alt_cat,platelet_cat )


str(tib_lab_prep_cat)


#### Merge Marta Richard Hilary's variable
tib_ecog_select <- tib_ecog %>% 
  select(PatientID,EcogValue)


tib_complete1_select<- tib_hilary_complete %>%
  select(-alive_dead) 


# CHanged missing to NA , select column needed for algorithm, merge table 


tib_prep_data <-  tib_lab_prep_cat %>% 
  right_join(tib_ecog_select, by="PatientID") %>%
  right_join(tib_complete1_select, by="PatientID") %>%
  right_join(tib_marta_feature, by="PatientID") %>%
  right_join(tib_visiting_time,  by="PatientID") %>%
  replace_with_na_at(.vars = c("pain","Resection","RadiationTherapy","time_diagnosis_to_chemo"), condition = ~.x == "NA") %>%
  replace_with_na_at(.vars = "Race", condition = ~.x == "NA") %>%
  replace_with_na_at(.vars = c("pain"), condition = ~.x == "Missing") %>%
  replace_with_na_at(.vars = c("pain"), condition = ~.x == "NA") %>%
  replace_with_na_at(.vars = c("SmokingStatus", "SCLCStage"), condition = ~.x == "Unknown/not documented") %>%
  mutate(Resection = case_when(Resection == "Yes" ~ 1,
                               is.na(Resection) ~ 0  )) %>%
  mutate(RadiationTherapy = case_when(RadiationTherapy == "Yes" ~ 1,
                                      is.na(RadiationTherapy) ~ 0 )) %>%
  mutate(diagnosis_to_resection= as.numeric(diagnosis_to_resection) ) %>%
  mutate(diagnosis_to_radiation= as.numeric(diagnosis_to_radiation)) %>%
  mutate( pain = as.numeric(pain)) %>%
  mutate(time_diagnosis_to_chemo = as.numeric(time_diagnosis_to_chemo)) %>%
  select(-bmi)%>% 
  right_join(tib_bmi_missing, by="PatientID")%>%
  mutate(final_SES= factor(final_SES)) %>%
  select(-age)


##Continues for missing table


tib_prep_data_miss <-  tib_lab_prep_miss %>% 
  right_join(tib_ecog_select, by="PatientID") %>%
  right_join(tib_complete1_select, by="PatientID") %>%
  right_join(tib_marta_feature, by="PatientID") %>%
  right_join(tib_visiting_time,  by="PatientID") %>%
  replace_with_na_at(.vars = c("pain","Resection","RadiationTherapy","time_diagnosis_to_chemo"), condition = ~.x == "NA") %>%
  replace_with_na_at(.vars = "Race", condition = ~.x == "NA") %>%
  replace_with_na_at(.vars = c("pain"), condition = ~.x == "Missing") %>%
  replace_with_na_at(.vars = c("pain"), condition = ~.x == "NA") %>%
  replace_with_na_at(.vars = c("SmokingStatus", "SCLCStage"), condition = ~.x == "Unknown/not documented") %>%
  mutate(Resection = case_when(Resection == "Yes" ~ 1,
                               is.na(Resection) ~ 0  )) %>%
  mutate(RadiationTherapy = case_when(RadiationTherapy == "Yes" ~ 1,
                                      is.na(RadiationTherapy) ~ 0 )) %>%
  mutate(diagnosis_to_resection= as.numeric(diagnosis_to_resection) ) %>%
  mutate(diagnosis_to_radiation= as.numeric(diagnosis_to_radiation)) %>%
  mutate( pain = as.numeric(pain)) %>%
  mutate(time_diagnosis_to_chemo = as.numeric(time_diagnosis_to_chemo)) %>%
  select(-bmi)%>% 
  right_join(tib_bmi_missing, by="PatientID")%>%
  mutate(final_SES= factor(final_SES)) %>% 
  select(-age)


#Change the type for variables 
str(tib_prep_data)
str(tib_prep_data_miss)




# Tibble for all missing rate 
tib_missing_feature <-tib_prep_data_miss %>%
  select(-PatientID) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(feature, missing_number ) %>%
  mutate(missing_rate = missing_number/nrow(tib_treated_patients) ) %>%
  arrange(desc(missing_rate)) 


#Make an excel file 
#write.csv(tib_missing_feature, file = "missing_features.csv", row.names = FALSE)




# plot for missing 
ggplot(tib_missing_feature, aes(x=reorder(feature,-missing_rate), y=missing_rate)) +
  geom_col(fill="#23b28e")+
  scale_x_discrete(name="Feature") + 
  scale_y_continuous(name=" Percentage", labels = scales::percent) +
  ggtitle("Feature missing rate") +
  geom_text(aes( label = scales::percent(missing_rate)), position = position_dodge(0.9), hjust = -.5)+
  theme(axis.text.x = element_text( hjust = 1))+
  coord_flip()


# change 
tib_prep_data <- tib_prep_data %>% mutate(final_SES = ifelse(final_SES == 8, 7, final_SES),
                                          final_SES = ifelse(final_SES == 9, 8, final_SES)      
)


#export tib_prep_data
write.csv(tib_prep_data, file = "prep_data.csv", row.names = FALSE)


#change ecog NA to 6 
tib_prep_data <- tib_prep_data%>%
  mutate(EcogValue = ifelse(is.na(EcogValue),6,EcogValue ))






# Multiple imputation -----------------------------------------------------
#required library 
library(mice)
library(VIM)
library(lattice)


str(tib_prep_data )


tib_prep_data <- tib_prep_data %>% mutate( LDH_cat= as.numeric(levels(LDH_cat))[LDH_cat],  
                                           ALP_cat= as.numeric(levels(ALP_cat))[ALP_cat],
                                           AST_cat= as.numeric(levels(AST_cat))[AST_cat],
                                           Alb_cat= as.numeric(levels(Alb_cat))[Alb_cat],
                                           glucose_cat= as.numeric(levels(glucose_cat))[glucose_cat],
                                           Lymph_cat= as.numeric(levels(Lymph_cat))[Lymph_cat],
                                           RBC_cat= as.numeric(levels(RBC_cat))[RBC_cat],
                                           serum_protein_cat= as.numeric(levels(serum_protein_cat))[serum_protein_cat],
                                           WBC_cat= as.numeric(levels(WBC_cat))[WBC_cat],
                                           calcium_cat= as.numeric(levels(calcium_cat))[calcium_cat],
                                           bilirubin_cat= as.numeric(levels(bilirubin_cat))[bilirubin_cat],
                                           urea_cat= as.numeric(levels(urea_cat))[urea_cat],
                                           hematocrit_cat= as.numeric(levels(hematocrit_cat))[hematocrit_cat],
                                           creatinine_cat= as.numeric(levels(creatinine_cat))[creatinine_cat],
                                           hemoglobin_cat= as.numeric(levels(hemoglobin_cat))[hemoglobin_cat],
                                           alt_cat= as.numeric(levels(alt_cat))[alt_cat],
                                           platelet_cat= as.numeric(levels(platelet_cat))[platelet_cat])


# remove lab category 0 to NA 
tib_pre_imputation <- tib_prep_data %>% mutate(LDH_cat = ifelse(LDH_cat==0, NA, LDH_cat),
                                               ALP_cat = ifelse(ALP_cat==0, NA, ALP_cat),
                                               AST_cat = ifelse(AST_cat==0, NA, AST_cat),
                                               Alb_cat = ifelse(Alb_cat==0, NA, Alb_cat),
                                               glucose_cat = ifelse(glucose_cat==0, NA, glucose_cat),
                                               Lymph_cat = ifelse(Lymph_cat==0, NA, Lymph_cat),
                                               RBC_cat = ifelse(RBC_cat==0, NA, RBC_cat),
                                               serum_protein_cat = ifelse(serum_protein_cat==0, NA, serum_protein_cat),
                                               WBC_cat = ifelse(WBC_cat==0, NA, WBC_cat),
                                               calcium_cat = ifelse(calcium_cat==0, NA, calcium_cat),
                                               bilirubin_cat = ifelse(bilirubin_cat==0, NA, bilirubin_cat),
                                               urea_cat = ifelse(urea_cat==0, NA, urea_cat),
                                               hematocrit_cat = ifelse(hematocrit_cat==0, NA, hematocrit_cat),
                                               creatinine_cat = ifelse(creatinine_cat==0, NA, creatinine_cat),
                                               hemoglobin_cat = ifelse(hemoglobin_cat==0, NA, hemoglobin_cat),
                                               alt_cat = ifelse(alt_cat==0, NA, alt_cat),
                                               platelet_cat = ifelse(platelet_cat==0, NA, platelet_cat)) %>%
  select(-diagnosis_to_radiation,-diagnosis_to_resection)


tib_pre_imputation <- tib_pre_imputation %>% select(-LDH_cat,-pain,-res_rate)
#-------------------------------------------------------------------------
# Changing type 


tib_pre_imputation<-tib_pre_imputation %>% mutate( weekend=factor(weekend),
                                                   PracticeType=factor(PracticeType),
                                                   Gender=factor(Gender),
                                                   SCLCStage =factor(SCLCStage),
                                                   SmokingStatus =factor(SmokingStatus),
                                                   Race=factor(Race)
) 


#Imputing the missing data 
tempData <- mice(tib_pre_imputation,m=1,maxit=50,meth='pmm',seed=500)
summary(tempData)
tib_one_hot_prep <- complete(tempData,1)


tib_one_hot_prep <- tib_one_hot_prep %>% mutate( final_SES = factor(final_SES), 
                                                 EcogValue = factor(EcogValue))




# One hot encode 
one_prep <- tib_one_hot_prep %>% select(-PatientID)
dmy <- dummyVars(" ~ . ", data = one_prep)
tib_prep_data_onehot <- data.frame(predict(dmy, newdata = one_prep))
tib_ultimate_table <- cbind( select(tib_prep_data, PatientID), tib_prep_data_onehot)


completedData <- tib_ultimate_table  %>% select(-alive_1_year.0,-PracticeType.ACADEMIC,-Gender.F, 
                                                -SCLCStage.Limited.disease..LD.,-SmokingStatus.No.history.of.smoking,-weekend.0)


unique(completedData$final_SES)
str(completedData)
write.csv(completedData, file = "completed_data.csv", row.names = FALSE)
# ---------------------------------------------------------------------------------------


##distribution
# Gender
table(completedData$Gender)/ nrow(completedData)*100
binom.test(1772,3643, conf.level = 0.95)
# Race 
table(completedData$Race)
observed = c(223,361,3059)
MultinomCI(observed, conf.level=0.95, method="sisonglaz")


# Practice type 
table(completedData$PracticeType)
table(completedData$PracticeType)/ nrow(completedData)*100


# Stage
table(completedData$SCLCStage)
table(completedData$SCLCStage)/ nrow(completedData)*100
binom.test(2409,3643, conf.level = 0.95)


# smoking history 
table(completedData$SmokingStatus)
table(completedData$SmokingStatus)/ nrow(completedData)*100
binom.test(3563,3643, conf.level = 0.95)


#resection 
table(completedData$Resection)
table(completedData$Resection)/ nrow(completedData)*100
binom.test(3558,3643, conf.level = 0.95)




# for continuous variable Correlation -----------------------------------------------------------------------
library(corrplot)
library(RColorBrewer)
continuous_data <- completedData %>% select(res_rate,heart_rate,diastolic,systolic,age_of_diagnosis_column,time_diagnosis_to_chemo,bmi,visiting_time)
{plot.new(); dev.off()}
# calculate correlations
correlations <- cor(continuous_data)
#p-value 
res1 <- cor.mtest(correlations, conf.level = .95)
# create correlation plot
#corrplot(correlations, method="circle")
corrplot.mixed(correlations, number.cex = .7 )
corrplot(correlations, type = "upper", order = "hclust", p.mat = res1$p, 
         col = brewer.pal(n = 8, name = "RdBu"), insig = "pch", addrect = 3)


# for continuoes varibale and categorical variable 




# for categorical variable and categorical variable 
cat_variable <- completedData %>% select(ALP_cat, AST_cat, Alb_cat, glucose_cat, Lymph_cat,RBC_cat, serum_protein_cat, 
                                         WBC_cat, calcium_cat, bilirubin_cat,urea_cat, hematocrit_cat, creatinine_cat,hemoglobin_cat,
                                         EcogValue, chemotherapy_type, weekend, PracticeType,Gender,final_SES,SCLCStage,SmokingStatus,
                                         Race, Resection, RadiationTherapy)


# correlation with radiotherapy (p_value)
cor_with_radiotherapy <- mapply(function(x, y) chisq.test(x, y)$p.value, cat_variable[, -25], MoreArgs=list(cat_variable[,25]))
a <- cor_with_radiotherapy[cor_with_radiotherapy< 0.05]
names(a)




#checking inclusion criteria ----------------------------------------------
a <- diagnosis %>% 
  filter(str_detect(DiagnosisCode,c("^C39.9$","^162.","^C34")))%>%
  right_join(tib_treated_patients ,by="PatientID") %>% 
  filter(DiagnosisDate < "2013-01-01" )


tib_lot_treat_day %>% right_join(a,by="PatientID")


b<-diagnosis %>% 
  filter(PatientID == "F08F9A00E46FD"|PatientID == "F993128A03AB9"|PatientID == "FE08BE8CC113A" | PatientID == "FF0AE2CCD3517" )


c <- tib_lot_treat_day  %>% right_join(b, by="PatientID")%>%
  select(PatientID, chemo_start_date= StartDate, DiagnosisDate, DiagnosisCode, DiagnosisDescription ) 
d<-c[c(1,7,13,15,18,21,30,31,38,40),]
d %>% arrange(PatientID, DiagnosisDate)


