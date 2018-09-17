# Normalisation -------------------------------------------------------

tib_completedData <- as.tibble(completedData)



#Normalization: min-max method with scale [0,1]

x <- tib_completedData[, c(3:57)]
normalized <- function(x) (x- min(x))/(max(x) - min(x))
tib_completedData[, c(3:57)] <- lapply(tib_completedData[, c(3:57)], normalized)

#Set index Patient ID

df <- as.tibble(tib_completedData)
rownames(df) <- df$PatientID
df <- df %>% select (-PatientID)

#Split into training and test datasets
index <- sample(1:nrow(df),round(0.80*nrow(df)))
train <- df[index,]
test <- df[-index,]
x_train <- train %>% select(-alive_1_year.1)
y_train <- as.factor(train$alive_1_year.1)
x_test <- test %>% select(-alive_1_year.1)
y_test <- as.factor(test$alive_1_year.1)

#Check for multicollinearity

library(corrplot)
library(RColorBrewer)

correlation_plot <- df %>% select(heart_rate,diastolic,systolic,age_of_diagnosis_column,time_diagnosis_to_chemo,bmi,DiagnosisDate)
{plot.new(); dev.off()}
# calculate correlations
correlations <- cor(correlation_plot) #diastolic and systolic (0.61 correlation)
#p-value 
res1 <- cor.mtest(correlations, conf.level = .95)
# create correlation plot
colnames(correlations) <- c("Heart rate","Diastolic","Systolic","Age of diagnosis","Time diagnosis to chemo","BMI","Visit times")
rownames(correlations) <- c("Heart rate","Diastolic","Systolic","Age of diagnosis","Time diagnosis to chemo","BMI","Visit times")

corrplot.mixed(correlations, number.cex = .7 )
corrplot(correlations, type = "upper", order = "hclust", p.mat = res1$p, 
         col = brewer.pal(n = 8, name = "RdBu"), insig = "pch", addrect = 3)

#chi squared test for labs
chisq.test(df$ALP_cat, df$glucose_cat, correct=FALSE)
chi1 <- chisq.test(df$ALP_cat, df$Alb_cat, correct=FALSE)
chi2 <- chisq.test(df$AST_cat, df$Alb_cat, correct=FALSE)
chi3 <- chisq.test(df$AST_cat, df$bilirubin_cat, correct=FALSE)
chi4 <- chisq.test(df$ALP_cat, df$bilirubin_cat, correct=FALSE)
chi5 <- chisq.test(df$AST_cat, df$serum_protein_cat, correct=FALSE)
chi6 <- chisq.test(df$ALP_cat, df$serum_protein_cat, correct=FALSE)
chi7 <- chisq.test(df$AST_cat, df$alt_cat, correct=FALSE)
chi8 <- chisq.test(df$ALP_cat, df$alt_cat, correct=FALSE)

#chisquared for lymph and WBC

chi9 <- chisq.test(df$Lymph_cat, df$WBC_cat, correct=FALSE)

#chisquared for RBC , hematocrit and hemoglobin

chi10 <- chisq.test(df$RBC_cat, df$hematocrit_cat, correct=FALSE)
chi11 <- chisq.test(df$RBC_cat, df$hemoglobin_cat, correct=FALSE)
chi12 <- chisq.test(df$hemoglobin_cat, df$hematocrit_cat, correct=FALSE)


list <- list(chi, chi1, chi2, chi3, chi4, chi5, chi6, chi7, chi8, chi9, chi10, chi11, chi12)

#Plots

plot_df <- tib_mortality %>% 
  left_join(tib_completedData, by="PatientID") %>% 
  select(PatientID, alive_1_year.1, Resection, RadiationTherapy)

plot_df_table <- plot_df %>%
  left_join(tib_demo_age_final,by="PatientID") %>%  
  left_join(race_ethnicity,by="PatientID") %>% 
  left_join(tib_insurance_ses,by="PatientID") %>% 
  left_join(age_of_diagnosis,by="PatientID") %>%
  left_join(diagnosis_treatment,by="PatientID") %>%
  left_join(tib_sclc_stage,by="PatientID") %>%
  left_join(tib_data_forplot, by="PatientID")         

##Practice type plot
fill_label =c ("Alive", "Dead" )

plot_practicetype <- ggplot(plot_df_table,aes(x = factor(PracticeType), y=(..count..)/sum(..count..), fill = factor(alive_1_year.1)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Practice Type", labels=c("Academic","Community")) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Practice Type Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))

##Gender plot

plot_gender <- ggplot(plot_df_table,aes(x = factor(Gender), y=(..count..)/sum(..count..), fill = factor(alive_1_year.1)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Gender", labels=c("Female","Male")) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Gender Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))

## Race plot


plot_race <- ggplot(plot_df_table,aes(x = factor(Race), y=(..count..)/sum(..count..), fill = factor(alive_1_year.1)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Race", labels=c("Black or African American","NA", "Other Race", "White")) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Race Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))


##Insurance

plot_insurance <- ggplot(tib_completedData, aes(x = factor(final_SES), y=(..count..)/sum(..count..), fill = factor(alive_1_year.1)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Insurance Category", labels=c("Commercial Health Plan",
                                                       "Self Pay",
                                                       "Medicaid",
                                                       "Medicare",
                                                       "Other Government Program",
                                                       "Patient Assistance Program",
                                                       "Workers Compensation",
                                                       "Other Payer/ Unknown",
                                                       "NA")) + 
  
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Insurance Category Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))
plot_insurance + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#SCLC plot


plot_sclc <- ggplot(plot_df_table,aes(x = factor(SCLCStage), y=(..count..)/sum(..count..), fill = factor(alive_1_year.1)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="SCLC Stage", labels=c("Extensive Disease","Limited Disease", "Unknown")) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("SCLC Stage Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))

##Smoking


plot_smoking <- ggplot(plot_df_table,aes(x = factor(SmokingStatus), y=(..count..)/sum(..count..), fill = factor(alive_1_year.1)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Smoking Status", labels=c("Smoking history","No smoking history", "Unknown")) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Smoking Status Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))

##Resection


plot_resection <- ggplot(plot_df_table,aes(x = factor(Resection), y=(..count..)/sum(..count..), fill = factor(alive_1_year.1)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Resection Status", labels=c("No","Yes")) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Resection Status Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))

##Radiation


plot_radiation <- ggplot(plot_df_table,aes(x = factor(RadiationTherapy), y=(..count..)/sum(..count..), fill = factor(alive_1_year.1)))+ 
  geom_bar( position= "dodge") +
  scale_x_discrete(name="Radiation Status", labels=c("No","Yes")) + 
  scale_y_continuous(name=" Percentage" , labels = scales::percent) +
  scale_fill_discrete(name= "1 year mortality", labels = fill_label)+ 
  ggtitle("Radiation Status Distribution") +
  
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)), y= (..count..)/sum(..count..)), color = "Black",position = position_dodge(0.9), stat= "count", vjust = 1.5)+
  theme(axis.text.x = element_text(hjust = 1))



##Resection time 

plot_time_resection <- tib_marta_feature %>%
  filter(Resection != 'NA' )

plot_time_resection_ <- ggplot( plot_time_resection, aes(x = PatientID, y = diagnosis_to_resection))+
  scale_x_discrete(name="Patients", labels=NULL) + 
  scale_y_continuous(name=" Time in days") +
  ggtitle("Time Distribution")
# Scatter plot with regression line
plot_time_resection_ + geom_point()+
  geom_smooth(method = "lm") 



##Radiation time 

plot_time_radiation <- tib_marta_feature %>%
  filter(RadiationTherapy != 'NA' )

plot_time_radiation_ <- ggplot( plot_time_radiation, aes(x = PatientID, y = diagnosis_to_radiation))+
  scale_x_discrete(name="Patients", labels=NULL) + 
  scale_y_continuous(name=" Time in days") +
  ggtitle("Time Distribution")

# Scatter plot with regression line
plot_time_radiation_ + geom_point()+
  geom_smooth(method = "lm")


##Age 

plotplot_age <- plot(table(plot_df_table$age), main="Age Distribution", xlab= "Age", ylab="Frequency", col="orange")

##Age of diagnosis

plotplot_agediagnosis <- plot(table(plot_df_table$age_of_diagnosis_column), main="Age of Diagnosis Distribution", xlab= "Age of diagnosis", ylab="Frequency", col="darkblue")


plotplot_timeres <- plot(table(plot_time_resection$diagnosis_to_resection), main="Age of Diagnosis Distribution", xlab= "Time to resection", ylab="Frequency", col="darkblue")

plotplot_timerad <- plot(table(plot_time_resection$diagnosis_to_resection), main="Age of Diagnosis Distribution", xlab= "Time to resection", ylab="Frequency", col="darkblue")


# Correlation matrix ------------------------------------------------------
### Table for correlation  
# load library
#library(corrplot)
# calculate correlations
#correlations <- cor(df)
# create correlation plot
#corrplot(correlations, method="circle", mar = c(1, 1, 1, 1), cex.var=0.4)

# Correlation matrix RICHARD ------------------------------------------------------
### Table for correlation 
# select out category labs 
#tib_correlation<- completedData %>% select(-c(3:30)) 

# select contineous lab
#tib_lab_contineous <- tib_prep_data_miss %>% select(PatientID,ALP,AST,Albumin,Glucose,Lymphocyte,RBC,Serum_protein)
#tib_correlation<- tib_correlation %>% right_join(tib_lab_contineous, by="PatientID")


#Imputing the missing data for continuos variable 
#tempData <- mice(tib_correlation,m=1,maxit=50,meth='pmm',seed=500)
#tib_correlation_complete <- complete(tempData,1)
#str(tib_correlation_complete)
#tib_cor_complete_without <- tib_correlation_complete %>% select(-1)

# Correlation plot with continuous variable -------------------------------
#library(corrplot)
#{plot.new(); dev.off()}
# calculate correlations
#correlations <- cor(tib_cor_complete_without)
# create correlation plot
#corrplot(correlations, method="circle")
