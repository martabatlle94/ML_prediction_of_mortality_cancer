library(glmnet)
library(caret)
library(pROC)
library(MLmetrics)
library(coefplot)
library(magrittr)




write.csv(completedData, file = "completed_data.csv", row.names = FALSE)
completed_data = read.csv("completed_data.csv",sep=',', row.names =1 ) 
#completed_data <- completed_data[,-c(1,2)] #remove the first and second columns - diagnosis_to_radiation/diagnosis_to_resection missingness too much 


#Normalization the variables
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


completed_data[, c(-1)]  <- as.data.frame(lapply(completed_data[, c(-1)], normalize))
completed_data$alive_1_year.1 <- as.factor(completed_data$alive_1_year.1)


#Remove correlated variables that are less recorded
completed_data_1 <- completed_data %>% select(-c("hematocrit_cat", "RBC_cat", "Lymph_cat", "AST_cat", "Alb_cat", "bilirubin_cat", "serum_protein_cat"))




#Initial model#
set.seed(123)
p <- createDataPartition(y=completed_data_1$alive_1_year.1 , p=0.8 , list = FALSE)
training_1 <-  completed_data_1[p,]
testing_1 <-  completed_data_1[-p,]


#levels(testing) <- c("first_class", "second_class")


# Splitting Dataset -------------------------------------------------------
X_train_1 <-as.matrix(training_1[2:44])
y_train_1 <- as.factor(training_1[[1]]) 
X_test_1 <-as.matrix(testing_1[2:44])
y_test_1 <- as.factor(testing_1[[1]]) 




### initial model ####
cv.lasso1 <- cv.glmnet(X_train_1, y_train_1, alpha = 1, family = "binomial", nlambda = 100, type.measure = "mse")
plot(cv.lasso1)


bestlam1 = cv.lasso1$lambda.min
lasso_coef1 <- coef(cv.lasso1,s=bestlam)


coefplot(cv.lasso1, parm = -2, color = "black", sort = "magnitude") #Importance plot 


# Final model with lambda.min/best lambda
model1 <- glmnet(X_train_1, y_train_1, alpha = 1, family = "binomial",
                 lambda = bestlam1)


lasso.coef = predict(model1, type = "coefficients", s = bestlam)


# Make prediction on test data
probabilities <- model1 %>% predict(newx = X_test_1)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)


# Model accuracy rate
observed.classes <- y_test_1
mean(predicted.classes == observed.classes)
plot(lasso.coef)


###Nested Cross validation ### 


folds  <- createFolds(completed_data_1$alive_1_year.1,k=5, returnTrain = TRUE )


#split the data into testing and training folds 
train_folds_1 <- lapply(folds, function(ind, dat) dat[ind,], dat = completed_data_1)
unlist(lapply(train_folds,nrow))
test_folds_1 <- lapply(folds, function(ind, dat) dat[-ind,], dat = completed_data_1)
unlist(lapply(test_fold,nrow))
str(test_folds_1)




fitControl_1 <- trainControl(method = "cv",
                             number = 10,
                             classProbs = FALSE,
                             allowParallel = TRUE,
                             verboseIter = TRUE,
                             returnData = FALSE)




tuneGridb_1 <- expand.grid(.alpha = 1,
                           .lambda = seq(0.0001, 1, by = 0.0001))




log_loss_result<- vector(mode="numeric", length=5)
auc_result <- vector(mode="numeric", length=5)
tune_parameter<- list()
accuracy_result <- vector(mode="numeric", length=5) 


#create fold for trainign the data 
set.seed(123)
for (i in 1:5) 
  X_train_outer_1 <- train_folds_1[[i]] %>% select(-alive_1_year.1)
y_train_outer_1 <- as.factor(train_folds_1[[i]]$alive_1_year.1)
X_test_outer_1 <-  test_folds_1[[i]] %>% select(-alive_1_year.1)
y_test_outer_1 <-  as.factor(test_folds_1[[i]]$alive_1_year.1)


lasso_1 = train(X_train_outer_1, y_train_outer_1, 
                method="glmnet", 
                family = "binomial",
                trControl = fitControl_1,
                tuneGrid = tuneGridb_1)




tune_parameter<- lasso_1$bestTune$lambda
tune_parameter<- lasso_1$bestTune
lasso_1


# Model evaluation -- AUC 
predicted_1 <- predict(lasso_1, X_test_outer_1)
roc1 <- roc(as.numeric(levels(y_test_outer_1)[y_test_outer_1]), as.numeric(levels(predicted_1)[predicted_1]))   
auc_result1<- auc(roc1)


# Model evaluation -- log loss 
log_loss_result <- LogLoss(as.numeric(levels(predicted_1)[predicted_1]), as.numeric(levels(y_test_outer_1)[y_test_outer_1]))


#Accuracy
accuracy_result<- Accuracy(predicted_1, y_test_outer_1)


#Correlation Metrics
table(predicted_1, y_test_outer_1)


#Misclassification Error 
mean(predicted_1 != y_test_outer_1)




plot(roc1, print.auc=TRUE)
plot(lasso_1, metric= "Accuracy")


print(auc_result1)
print(mean(auc_result1))


print(log_loss_result)
print(mean(log_loss_result))


print(accuracy_result)
print(mean(accuracy_result))


lasso_1$bestTune
######## 

