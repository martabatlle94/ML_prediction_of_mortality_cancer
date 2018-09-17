library(tidyverse)
library(aod)


#Load data 
prep_data = read.csv("prep_data.csv",sep=',',row.names = 1 )
str(prep_data)
prep_data <- prep_data %>% mutate( EcogValue = ifelse(is.na(EcogValue),6 ,EcogValue ) )


imp_prep <- prep_data %>% 
  select(-diagnosis_to_radiation,-diagnosis_to_resection,-res_rate,-pain) %>%
  mutate (LDH_cat = ifelse(LDH_cat==0,NA ,LDH_cat), 
          ALP_cat = ifelse(ALP_cat==0,NA ,ALP_cat),
          AST_cat = ifelse(AST_cat==0,NA ,AST_cat),
          Alb_cat = ifelse(Alb_cat==0,NA, Alb_cat),
          glucose_cat = ifelse(glucose_cat==0,NA,glucose_cat),
          Lymph_cat = ifelse(Lymph_cat==0,NA,Lymph_cat),
          RBC_cat = ifelse(RBC_cat==0,NA,RBC_cat),
          serum_protein_cat= ifelse(serum_protein_cat==0,NA,serum_protein_cat),
          WBC_cat = ifelse(WBC_cat==0,NA,WBC_cat),
          calcium_cat = ifelse(calcium_cat==0,NA ,calcium_cat),
          bilirubin_cat = ifelse(bilirubin_cat==0,NA ,bilirubin_cat),
          urea_cat = ifelse(urea_cat==0,NA , urea_cat),
          hematocrit_cat = ifelse(hematocrit_cat==0,NA ,hematocrit_cat),
          creatinine_cat = ifelse(creatinine_cat==0,NA ,creatinine_cat),
          hemoglobin_cat = ifelse(hemoglobin_cat==0,NA ,hemoglobin_cat),
          alt_cat = ifelse(alt_cat==0,NA ,alt_cat),
          platelet_cat = ifelse(platelet_cat==0,NA ,platelet_cat),
          EcogValue = factor(EcogValue))  


#  Multiple Imputation
#tempData <- mice(imp_prep,m=1,maxit=50,meth='pmm',seed=500)
#completedData <- complete(tempData,1)


# Categorize continuos variable 
#cat_data <- completedData %>%
#  mutate ( bmi = case_when( (bmi < 18.5 ) ~ "underweight",
#                            (bmi == 18.5 | bmi < 25  ) ~ "normal",
#                            (bmi == 25 | bmi < 30 ) ~ "overweight", 
#                            (bmi ==30 | bmi >30)  ~ "obese"), 
#           heart_rate = case_when((heart_rate <100) ~ " <100 ", 
#                                  (heart_rate ==100| heart_rate < 160) ~ "100-160", 
#                                  (heart_rate == 160 | heart_rate > 160) ~ ">160"),
#           diastolic = case_when((diastolic < 80 ) ~ "<80", 
#                                 (diastolic ==80|diastolic<90) ~ "80-90",
#                                 (diastolic ==90|diastolic<120)~ "90-120",
#                                  (diastolic==120|diastolic>120) ~ ">120"),
#           systolic = case_when((systolic <120) ~ "<120", 
#                                (systolic==120|systolic<130) ~ "120-130", 
#                                (systolic==130|systolic<140) ~ "130-140", 
#                                (systolic==140|systolic<180) ~ "140-180",
#                                (systolic==180|systolic>180) ~ ">180"),
#           age_of_diagnosis_column = case_when( (age_of_diagnosis_column < 50) ~ "<50",
#                                          (age_of_diagnosis_column ==50| age_of_diagnosis_column<70) ~ "50-70",
#                                          (age_of_diagnosis_column==70| age_of_diagnosis_column>70) ~ ">70"),
#           time_diagnosis_to_chemo= case_when((time_diagnosis_to_chemo < 7) ~ "< 1week", 
#                                        (time_diagnosis_to_chemo==7|time_diagnosis_to_chemo<21) ~ "<3weeks",
#                                        (time_diagnosis_to_chemo==21|time_diagnosis_to_chemo<35)  ~"<5weeks",
#                                        (time_diagnosis_to_chemo==35|time_diagnosis_to_chemo<56) ~ "<2 months",
#                                        (time_diagnosis_to_chemo==56| time_diagnosis_to_chemo>56) ~ ">2months"))




# set to factor 
str(cat_data)
order_data <-imp_prep %>%  mutate(alive_1_year=as.factor(alive_1_year), 
                                  LDH_cat=factor(LDH_cat, levels = c(2,1,3)),
                                  ALP_cat=factor(ALP_cat, levels = c(2,1,3)),
                                  AST_cat=factor(AST_cat, levels = c(2,1,3)),
                                  Alb_cat=factor(Alb_cat, levels = c(2,1,3)),
                                  glucose_cat=factor(glucose_cat, levels = c(2,1,3)),
                                  Lymph_cat=factor(Lymph_cat, levels = c(2,1,3)),
                                  RBC_cat=factor(RBC_cat, levels = c(2,1,3)),
                                  serum_protein_cat=factor(serum_protein_cat, levels = c(2,1,3)),
                                  WBC_cat=factor(WBC_cat, levels = c(2,1,3)),
                                  calcium_cat=factor(calcium_cat, levels = c(2,1,3)),
                                  bilirubin_cat=factor(bilirubin_cat, levels = c(2,1,3)),
                                  urea_cat=factor(urea_cat, levels = c(2,1,3)),
                                  hematocrit_cat=factor(hematocrit_cat, levels = c(2,1,3)),
                                  creatinine_cat=factor(creatinine_cat, levels = c(2,1,3)),
                                  hemoglobin_cat=factor(hemoglobin_cat, levels = c(2,1,3)),
                                  alt_cat=factor(alt_cat, levels = c(2,1,3)),
                                  platelet_cat=factor(platelet_cat, levels = c(2,1,3)),
                                  EcogValue=factor(EcogValue, ordered=FALSE),
                                  #                     bmi = factor(bmi, levels=c("underweight","normal","overweight","obese"),ordered = TRUE),
                                  #                     heart_rate =factor(heart_rate,levels = c(" <100 ","100-160",">160"),ordered = TRUE),
                                  #                     diastolic = factor(diastolic, levels =c("<80","80-90","90-120",">120"),ordered = TRUE),
                                  #                     systolic =factor(systolic, levels =c("<120","120-130","130-140","140-180",">180"),ordered = TRUE),
                                  #                     age_of_diagnosis_column=factor(age_of_diagnosis_column,levels = c("<50","50-70",">70"),ordered = TRUE),
                                  #                     time_diagnosis_to_chemo=factor(time_diagnosis_to_chemo,levels=c("< 1week", "<3weeks","<5weeks","<2 months",">2months"),ordered = TRUE),
                                  final_SES=factor(final_SES),
                                  weekend=factor(weekend),
                                  Resection=factor(Resection),
                                  RadiationTherapy=factor(RadiationTherapy)
)


str(order_data)
unique(order_data$AST_cat)
levels(order_data$EcogValue)
unique(order_data$EcogValue)


# Check distributioin 
table(order_data$ALP_cat)
table(order_data$AST_cat)
table(order_data$alt_cat)
table(order_data$bilirubin_cat)
table(order_data$Alb_cat)
table(order_data$serum_protein_cat)
table(order_data$glucose_cat)
table(order_data$Lymph_cat)
table(order_data$RBC_cat)
table(order_data$hematocrit_cat)
table(order_data$hemoglobin_cat)
table(order_data$WBC_cat)
table(order_data$platelet_cat)
table(order_data$calcium_cat)
table(order_data$urea_cat)
table(order_data$creatinine_cat)
table(order_data$EcogValue)
table(order_data$chemotherapy_type)
table(order_data$weekend)
table(order_data$PracticeType)
table(order_data$final_SES)
table(order_data$SmokingStatus)
table(order_data$Resection)
table(order_data$RadiationTherapy)
table(order_data$Gender)
table(order_data$Race)
table(order_data$SCLCStage)
nrow(order_data)
# Data partition
#set.seed(123)
#a <- createDataPartition(y=order_data$alive_1_year  , p=0.8 , list = FALSE)
#train <-  order_data[a,]
#test  <-  order_data[-a,]
#str(train)
#str(test)


name_dataframe <- order_data %>% select(-1) 


ll <-list()
# Model fitting -- logistic regression/ Univariate analysis 
var_name <-  colnames(name_dataframe)


d <- data.frame(feature_name=var_name, estimates=rep(0,35),Std_Error=rep(0,35),z_value=rep(0,35),Pr=rep(0,35))
for (i in 1:35){
  exposure <- var_name[i]
  model <- glm(alive_1_year ~ get(exposure) , family = binomial(link='logit'),data=order_data)
  ll[[i]]<- coef(summary(model))[-1,]
}
names(ll)<-colnames(name_dataframe) 






df_all <- do.call(rbind, ll)
str(df_all)


rname  <- c("LDH_low","LDH_high","ALP_low","ALP_high","AST_low","AST_high",
            "Albumin_low","Albumin_high","glucose_low","glucose_high","lymphocyte_low",
            "lymphocyte_high","RBC_low","RBC_high","serum_protein_low","serum_protein_high",
            "WBC_low","WBC_high","calcium_low","calcium_high","bilirubin_low","bilirubin_high","urea_low","urea_high","hematocrit_low",
            "hematocrit_high","creatinine_low","creatinine_high","hemoglobin_low","hemoglobin_high","ALT_low","ALT_high","platelet_low","platelet_high",
            "Ecog_1","Ecog_2","Ecog_3","Ecog_4","Ecog_NA","Heart_Rate","diastolic","systolic",
            "Cisplatin_and_Etoposide","CIT","clinical_study_drug","Other_platinum","Other_treatment","Platinum_and_Irinotecan",
            "Platinum_and_Paclitaxel","treatment_on_weekend","community_practice","male","Self_Pay","Medicaid","Medicare","Other_government_program",
            "Patient_assistance_program","Other_payer_type_unknown","Missing",
            "Limited_disease","No history of smoking status","race_other","race_white","age_of_diagnosis", 
            "time_to_chemo","resection","Radiation_therapy","visiting_events","bmi")
row.names(df_all)<-rname


# calculate other
colnames(df_all)<- c("Estimates","std_error","z_value","Pr")


df_all<- as.tibble(df_all)
row.names(df_all)


df_all <- df_all %>% as_tibble() %>% mutate(feature = rname)
df_all <- df_all[, c(5, 1, 2, 3, 4)]


uni_table <- df_all %>% 
  mutate( Odds_ratio = exp(Estimates)) %>%
  mutate (`95%CI` = paste(round(exp(Estimates - 1.96*std_error),2)," - ",round(exp(Estimates + 1.96*std_error),2)))%>%
  mutate( Estimates=round(Estimates,2), std_error=round(std_error,6),
          Odds_ratio=round(Odds_ratio,2), z_value = round(z_value,2),
          Pr= round(Pr,3))%>%
  mutate(Pr = ifelse (Pr < 0.001, "<0.001",Pr)) 




final_table <- uni_table %>% select(feature,Odds_ratio,`95%CI`,Pr)


write.csv(final_table, file = "uni_table.csv", row.names = FALSE)


#sig <- final_table %>% filter(Pr<0.05)%>%
#  arrange(desc(Odds_ratio))


#write.csv(sig, file = "sig_arrange.csv", row.names = FALSE)

