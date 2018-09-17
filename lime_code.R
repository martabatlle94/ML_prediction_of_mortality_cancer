all_x <- df$alive_1_year.1
train_ext  <- createFolds(all_x ,k=5, returnTrain = TRUE )
str(train_ext) #5 training numbers
train_ext_data <- lapply(train_ext, function(ind,data) data[ind,], data=df )
str(train_ext_data) #5 training dataset for outer
test_ext_data <- lapply(train_ext, function(ind,data) data[-ind,], data=df )
str(test_ext_data) #5 testing dataset for outer


log_train_mlpweight <- vector(mode="numeric", length=5)
log_test_mlpweight <- vector(mode="numeric", length=5)

auc_train_mlpweight <- vector(mode="numeric", length=5)
auc_test_mlpweight <- vector(mode="numeric", length=5)

accuracy_train_mlpweight <- vector(mode="numeric", length=5) 
accuracy_test_mlpweight <- vector(mode="numeric", length=5) 

#grid = expand.grid(decay = c(1), layer1 = c(1), layer2 = c(1), layer3 = c(1))
grid = expand.grid(decay = c(0,0.001,0.1), layer1 = c(3,6,9), layer2 = c(0,3,6,9), layer3 = c(0,3,6,9))
traincontrol = trainControl(method = "cv",verboseIter=TRUE, number=10)

set.seed(123)
for (i in 1:5) {
  
  x_train_outer <- (train_ext_data[[i]] %>% select(-alive_1_year.1))
  y_train_outer <- as.factor(train_ext_data[[i]]$alive_1_year.1)
  x_test_outer <-  (test_ext_data[[i]] %>% select(-alive_1_year.1))
  y_test_outer <-  as.factor(test_ext_data[[i]]$alive_1_year.1)
  
  mlpweight = caret::train(x = x_train_outer, 
                           y = y_train_outer, 
                           method = "mlpWeightDecayML", 
                           trControl = traincontrol,
                           linOut = FALSE, 
                           tuneGrid = grid, 
                           batch_size="max",
                           metric="accuracy",
                           verbose=TRUE)
  
  #Performance metrics for train set
  predict_train_mlpweight <- predict(mlpweight, newdata=x_train_outer) 
  predict_train_mlpweight <- as.numeric(predict_train_mlpweight)
  
  y_train_outer <- as.numeric(y_train_outer)
  roc_train_mlpweight <- roc(y_train_outer, predict_train_mlpweight, plotit=TRUE)
  auc_train_mlpweight[i] <- auc(y_train_outer, predict_train_mlpweight)
  log_train_mlpweight[i] <- LogLoss(predict_train_mlpweight, y_train_outer)
  accuracy_train_mlpweight[i] <- Accuracy(predict_train_mlpweight, y_train_outer)
  
  #Performance metrics with test data
  
  predict_test_mlpweight <- predict(mlpweight, newdata=x_test_outer)
  predict_test_mlpweight <- as.numeric(predict_test_mlpweight)
  
  y_test_outer <- as.numeric(y_test_outer)
  roc_test_mlpweight <- roc(y_test_outer, predict_test_mlpweight, plotit=TRUE)
  auc_test_mlpweight[i] <- auc(y_test_outer, predict_test_mlpweight)
  log_test_mlpweight[i] <- LogLoss(predict_test_mlpweight, y_test_outer)
  accuracy_test_mlpweight[i] <- Accuracy(predict_test_mlpweight, y_test_outer)
  
}

#Lime : feature selection


# Create an explainer object
explainer <- lime::lime (x_train_outer, mlpweight)
# Explain new observation
explanation <- lime::explain(x_test_outer[1:351,], explainer, n_labels = 1, n_features = 20)

explanation_one <- lime::explain(x_test_outer[1,], explainer, n_labels = 1, n_features = 4)

#Plot
summary(explanation)
head(explanation)
plot_features(explanation_one)                     
#


explanation_tib <- as_tibble(explanation) 

explanation_label <- as_tibble(explanation_tib)

explanation_label <- explanation_label[!duplicated(explanation_label[,2]),]

explanation_label_one <- explanation_label['label']

some_actualvalues <- as_tibble(y_test_outer[1:351])

some_actualvalues <- some_actualvalues$value - 1 #change 1 to 2 

bind_actualvalues <- cbind(explanation_label_one, some_actualvalues) #Predictions made by predict and by LIME are the same

colnames(bind_actualvalues) <- c("predictedvalue", "realvalue")

bind_actualvalues$diff <- bind_actualvalues$predictedvalue == bind_actualvalues$realvalue

bind_actualvalues$case <- rownames(bind_actualvalues)

bind_actualvalues <- as_tibble(bind_actualvalues)

final_lime <- as_tibble(bind_actualvalues) %>%
  left_join(explanation_tib, by="case")

final_lime_true <- as_tibble(final_lime) %>%
  filter(diff=="TRUE")


final_lime_false <- as_tibble(final_lime) %>%
  filter(diff=="FALSE")

#### True predictions
plot_lime <- aggregate(final_lime_true$feature_value, by=list(feature=final_lime_true$feature), FUN=sum)

colnames(plot_lime) <- c("feature", "value")

plot_lime <- as_tibble(plot_lime) %>%
  arrange(desc(value))

plot_final <- ggplot(plot_lime, aes(x=reorder (feature, -value), y=value)) + geom_col(fill = "mediumorchid") +
  scale_x_discrete(name="Feature") + 
  scale_y_continuous(name="Value") +
  ggtitle("Feature importance") 

plot_final + theme(axis.text.x = element_text(angle = 90, hjust = 1))

####  


plot(roc_train_mlpweight)
plot((roc_test_mlpweight), main="Receiver Operating Curve - Test dataset")

print(auc_train_mlpweight)
print(mean(auc_train_mlpweight))
print(auc_test_mlpweight)
print(mean(auc_test_mlpweight))

print(log_train_mlpweight)
print(mean(log_train_mlpweight))
print(log_test_mlpweight)
print(mean(log_test_mlpweight))

print(accuracy_train_mlpweight)
print(mean(accuracy_train_mlpweight))
print(accuracy_test_mlpweight)
print(mean(accuracy_test_mlpweight))


