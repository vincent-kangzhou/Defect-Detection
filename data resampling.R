library(ggplot2)
library(readr)
install.packages("readr")
system("ls../input")

x<-c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
     "MASS", "rpart", "gbm", "ROSE")
lapply(x, require, character.only = TRUE)

roc.curve(actual value, predicted value)

##### random forest

## Smoted Data
set.seed(1234)
rf_model_tomek_smote = randomForest(Class~.,train_tomek_smote,ntree=500,mtry=2,sampsize=c(50,250),nodesize=1, rules = TRUE)
rf_test_predictions = predict(rf_model_tomek_smote, test, type="class")
conf_matrix_rf = table(test$Class,rf_test_predictions)
confmatrix =confusionMatrix(conf_matrix_rf)
randomforest_smote_roc = roc.curve(test$Class,rf_test_predictions)
randomforest_smote_recall = confmatrix$byClass[1]

## Matthews correlation coefficient
tn=as.double(conf_matrix_rf[1,1]);
fp=as.double(conf_matrix_rf[1,2]);
fn=as.double(conf_matrix_rf[2,1]);
tp=as.double(conf_matrix_rf[2,2]);
MCC_rf = (tp*tn - fp*fn) / sqrt( (tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))

## Variable Importance List
rf_var_imp = data.frame(rf_model_tomek_smote$importance)
rf_var_imp$Variables = row.names(rf_var_imp)
rf_var_imp = rf_var_imp[order(-rf_var_imp$MeanDecreaseGini),]
rf_var_imp


###  C50
## Smoted Data
set.seed(1234)
C50_model <- C5.0(Class ~.,train_tomek_smote, trials = 100)
C50_test_predictions = predict(C50_model, test, type="class")
conf_matrix_c50 = table(test$Class,C50_test_predictions)
confmatrix = confusionMatrix(conf_matrix_c50)
c50_smote_roc = roc.curve(test$Class,C50_test_predictions)
c50_smote_recall = confmatrix$byClass[1]

## Matthews correlation coefficient
tn=as.double(conf_matrix_c50[1,1]);
fp=as.double(conf_matrix_c50[1,2]);
fn=as.double(conf_matrix_c50[2,1]);
tp=as.double(conf_matrix_c50[2,2]);
MCC_c50 = (tp*tn - fp*fn) / sqrt( (tp + fp)*(tp + fn)*(tn + fp)*(tn + fn))

###k-fold cross-validation
folds <- createFolds(factor(train_tomek_smote$Response), k = 5, list = FALSE)
k=5
train_tomek_smote$id <- folds
list=1:5
prediction=data.frame()
testsetCopy=data.frame()
Acc_val=vector()
Recall_val=vector()
Precision_val=vector()
for(i in 1:k){
  trainingset <- subset(train_tomek_smote, id %in% list[-i])
  testset <- subset(train_tomek_smote, id %in% c(i))
  set.seed(1234)
  rf_model_tomek_smote = randomForest(Class~.,trainingset,ntree=500,mtry=2,sampsize=c(50,250),nodesize=1)
  rf_test_predictions = predict(rf_model_tomek_smote, testset, type="class")
  conf_matrix = table(testset$Class,rf_test_predictions)
  conf_matrix_results = confusionMatrix(conf_matrix)
  
  # Accuracy
  accu_metric = conf_matrix_results$overall['Accuracy']
  Recall_metric = conf_matrix_results$byClass[1]
  Precision_metric = conf_matrix_results$byClass['Pos Pred Value']
  Acc_val = c(Acc_val, accu_metric)
  Recall_val = c(Recall_val,Recall_metric)
  Precision_val = c(Precision_val,Precision_metric)
  temp <- as.data.frame(predict(rf_model_tomek_smote, testset[,-31]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,31]))
}
print(mean(Acc_val))
print(mean(Recall_val))
print(mean(Precision_val))
print(Recall_val)
