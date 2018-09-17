library(tidyverse)
library(tibble)
library(stringr)
# All lab Missing table 
tib_lab_select_duplicate  <- read.csv("tib_lab_sel.csv")


# import 
lab_LOINC_table <- read.csv("123.csv")


lab_LOINC_table <- lab_LOINC_table %>% select(Lab.Component.Name , FH.Mapped.LOINC)
lab_LOINC_table <- lab_LOINC_table %>% mutate(FH.Mapped.LOINC = as.character(FH.Mapped.LOINC)) %>% 
  mutate(FH.Mapped.LOINC = str_replace_all(FH.Mapped.LOINC,"\n",""))%>%
  mutate(Lab.Component.Name = str_replace_all(Lab.Component.Name," ","_"))%>%
  separate(FH.Mapped.LOINC, c("LOINC_1","LOINC_2","LOINC_3","LOINC_4"),sep= " ")%>% 
  replace_with_na(replace=list(LOINC_1=c(""), LOINC_2=c(""),LOINC_3=c(""), LOINC_4=c("")))


lab_LOINC_list <- list()
for ( i in 1 : nrow(lab_LOINC_table)){
  lab_LOINC_list[[i]] <- c(lab_LOINC_table[i,2],lab_LOINC_table[i,3],lab_LOINC_table[i,4],lab_LOINC_table[i,5])
}
names(lab_LOINC_list) <-  as.character(lab_LOINC_table[,1])
lab_LOINC_list <- lapply(lab_LOINC_list, function(x) x[!is.na(x)])


# GET LIST 


#------------------------------------------------------------------------------------------------------------------------
# tib_lot_treat_day<- tib_lot_treat_day %>%
#   select(-StartDate_time )


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




lab_name_vector <- vector("character", length= (length(lab_LOINC_list)))
missing_num <- vector("double", length(lab_name_vector))
missing_rate <- vector("double", length(lab_name_vector))
total_patients<-  vector("double", length(lab_name_vector))


for (i in seq_along(lab_LOINC_list)) {
  
  # create lab tib  
  name <- paste("tib_lab_",names(lab_LOINC_list[i]),sep = "") 
  assign(name, Extract_labtib(lab_LOINC_list[[i]])) 
  lab_name_vector[i] <- names(lab_LOINC_list[i])
  
  # summary of missing lab 
  missing_vector <- get(name) %>%  summarise_all(funs(sum(is.na(.)))) 
  missing_num[i] <-  missing_vector[[5]]
  missing_rate[i] <- missing_num[[i]]/4436
  
  # Total patients in lab tests
  total_patients[i] <- nrow(get(name))
} 




tib_missing_lab <- tibble(lab_name = lab_name_vector, missing_num = missing_num, missing_rate= missing_rate, total_patients=total_patients)
tib_missing_lab
tib_missing_lab <- tib_missing_lab %>% arrange(missing_rate)


#Export tib_missing_lab to excel 
library(xlsx)
write.xlsx(tib_missing_lab, file = "tib_missing_all_lab.xlsx")


# missing rate in bar chart 
# ggplot(data=tib_missing_lab) + geom_col(aes(x= reorder(lab_name, missing_rate) , y=missing_rate)) + coord_flip()

