library(tidyverse)
library(xgboost)
library(gbm)
library(caret)
library(pROC)
library(MLmetrics)


completed_data = read.csv("completed_data.csv",sep=',', row.names =1 )
str(completed_data)


completed_data <- completed_data %>% select(-diagnosis_to_radiation,-diagnosis_to_resection )


complete_y  <- completed_data$alive_1_year.1


train_folds  <- createFolds( complete_y ,k=5, returnTrain = TRUE )
str(train_folds) #gave 5 training numbers
train_folds_data <- lapply(train_folds, function(ind,data) data[ind,], data=completed_data )
str(train_folds_data) #gave 5 training dataset for outer
test_folds_data <- lapply(train_folds, function(ind,data) data[-ind,], data=completed_data )
str(test_folds_data) #gave 5 testing dataset for outer


# Specify Cross-Validation and number of folds, enable parallel computation 
xgb_trcontorl = trainControl( method= "cv", number = 5, allowParallel = FALSE, verboseIter = TRUE, returnData = FALSE )




# grid space to search for best hyperparameters  
xgbGrid <- expand.grid(nrounds=c(200), 
                       max_depth=c(3),
                       colsample_bytree = c(0.9) , 
                       eta= c(0.01),
                       gamma= c(5),
                       min_child_weight = c(1),
                       subsample = c(0.8))


log_loss_result<- vector(mode="numeric", length=5)
auc_result <- vector(mode="numeric", length=5)
tune_parameter<- list()
accuracy_result <- vector(mode="numeric", length=5) 




# train model 
set.seed(123)
for (i in 1:5) {
  
  X_train_outer <- train_folds_data[[i]] %>% select(-alive_1_year.1)
  y_train_outer <- as.factor(train_folds_data[[i]]$alive_1_year.1)
  X_test_outer <-  test_folds_data[[i]] %>% select(-alive_1_year.1)
  y_test_outer <-  as.factor(test_folds_data[[i]]$alive_1_year.1)
  
  xgb_model = train(X_train_outer, y_train_outer, 
                    trControl = xgb_trcontorl,
                    tuneGrid = xgbGrid,
                    method="xgbTree")
  # best value for hyperparaeters  
  tune_parameter[[i]]<- xgb_model$bestTune
  
  # Model evaluation -- AUC 
  predicted <- predict(xgb_model, X_test_outer)
  roc_obj <- roc(as.numeric(levels(y_test_outer)[y_test_outer]), as.numeric(levels(predicted)[predicted]))   
  auc_result[i] <- auc(roc_obj)
  
  # Model evaluation -- log loss 
  log_loss_result[i] <- LogLoss(as.numeric(levels(predicted)[predicted]), as.numeric(levels(y_test_outer)[y_test_outer]))
  
  #Accuracy
  accuracy_result[i] <- Accuracy(predicted, y_test_outer)
  
  
}


print(tune_parameter)
print(auc_result)
print(mean(auc_result))


print(log_loss_result)
print(mean(log_loss_result))


print(accuracy_result)
print(mean(accuracy_result))


# Best performance hyperparameter 
#       nrounds  max_depth  eta   gamma  colsample_bytree    min_child_weight  subsample
#   27     100      15     0.01     5           0.5                  1            0.8


# View tree
#xgb.plot.tree(model=xgb_model$finalModel)


#Importance
roc_imp <- varImp(xgb_model, scale = FALSE )
summary.gbm(xgb_model)
plot(roc_imp)


featurePlot(x=completed_data[ ,41], y=completed_data[,1],plot="box", 
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales =  list(x=list(relation="free"), 
                           y=list(relation="free")))


### Compare to simple regression model  ## Accuracy 0.66 -----------------------------------------------------------------------------------------------------------
set.seed(123)
completed_data <- completed_data %>% mutate( alive_1_year.1 = factor(alive_1_year.1) )


# Define cv control 
train_control <- trainControl(method="cv", number=10)
#train model 
model <- train(alive_1_year.1 ~ age_of_diagnosis_column + AST_cat + Alb_cat + final_SES.8 + 
                 SCLCStage.Extensive.disease..ED., data=completed_data, trControl=train_control,
               method="glm", family=binomial )


# print cv scores
summary(model)
model


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------




### Compare to simple regression model   ## ACccuracy 0.55 with one variable 
set.seed(123)
completed_data <- completed_data %>% mutate( alive_1_year.1 = factor(alive_1_year.1) )


# Define cv control 
train_control <- trainControl(method="cv", number=10 )
#train model 
model <- train(alive_1_year.1 ~ age_of_diagnosis_column , data=completed_data, trControl=train_control,
               method="glm", family=binomial)


# print cv scores
summary(model)
model






# Collinearity ------------------------------------------------------------


#set.seed(123)
# Splitting Dataset -------------------------------------------------------
#a <- createDataPartition(y=completed_data$alive_1_year.1  , p=0.8 , list = FALSE)
#training <-  completed_data[a,]
#testing  <-  completed_data[-a,]


#X_train <-  training %>% select(-alive_1_year.1)
#y_train <-  training$alive_1_year.1  
#X_test  <-  testing %>% select(-alive_1_year.1) 
#y_test  <-  testing$alive_1_year.1


#X_train <-  xgb.DMatrix(as.matrix( training %>% select(-alive_1_year.1)))     
#y_train <-  training$alive_1_year.1    
#X_test  <-  xgb.DMatrix(as.matrix( testing %>% select(-alive_1_year.1))) 
#y_test  <-  testing$alive_1_year.1


#class(X_train)
#class(y_train)


# XGboost model -----------------------------------------------------------
# set data into DMatrix
#dtrain  <- xgb.DMatrix(data = as.matrix(X_train), label=y_train )
#dtest   <- xgb.DMatrix(data = as.matrix(X_test), label=y_test )


#Set parameters 
#params <- list(booster="gbtree", objective="binary:logistic", eta=0.3, gamma=0, max_depth=6, 
#               min_child_weight=1,subsample=1, colsample_bytree=1) 


#Cross validation for best nround
#xgbcv <- xgb.cv(params = params, data=dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, 
#                print_every_n = 10, early_stopping_rounds = 20, maximize = F)




#First default model training 
#xgb1<- xgb.train(params = params, data=dtrain, nrounds=11, watchlist = list(val=dtest,train=dtrain), 
#          print_every_n = 10, early_stopping_rounds = 20, maximize = F, evel_metric ="error")


#Model prediction 
#xgbpred <- predict(xgb1, dtest)
#xgbpred <- ifelse (xgbpred>0.5, 1, 0)


#confusion matrix 
#confusionMatrix(as.factor(xgbpred), as.factor(y_test))  #Accuracy 0.65


# Importance plot
#mat <- xgb.importance(feature_names = colnames(X_train), model=xgb1)
#xgb.plot.importance(importance_matrix = mat[1:20])




# Training with DMatrix
#bstDMatrix <-  xgboost(data=dtrain, max.depth=2, eta=1, nround=2, objective="binary:logistic")


# Predict
#pred <- predict(bstDMatrix, as.matrix(X_test))


# transform to binary 
#prediction <- as.numeric(pred > 0.5)


# Measure model performance 
#err <- mean(prediction != y_test)
#print(paste("test_error",err))


# View tree
#xgb.dump(bstDMatrix, with_stats = TRUE)
#xgb.plot.tree(model=bstDMatrix )






############## Another "Caret"  method #############--------------------------




# Splitting Dataset -------------------------------------------------------
# a <- createDataPartition(y=completed_data$alive_1_year.1  , p=0.8 , list = FALSE)
# training <-  completed_data[a,]
# testing  <-  completed_data[-a,]

# X_train <-  training %>% select(-alive_1_year.1)
# y_train <-  as.factor(training$alive_1_year.1) 
# X_test  <-  testing %>% select(-alive_1_year.1) 
# y_test  <-  as.factor(testing$alive_1_year.1)


# ----------------------------------------------------------------START
# Creating outer CV 

