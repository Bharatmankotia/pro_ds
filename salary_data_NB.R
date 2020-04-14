salary_data<-read.csv(file.choose())
View(salary_data)

summary(salary_data)

str(salary_data)

# check for duplicate rows

dim(unique(salary_data)) # 26903

salary_data<-unique(salary_data)

# Feature scaling of train data

norm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

salary_data[,c(1,4,10:12)]<-as.data.frame(lapply(salary_data[,c(1,4,10:12)],norm))

View(salary_data)

prop.table(table(salary_data$Salary)) # checking proportion of output class

# importing  test data

salary_data_test<-read.csv(file.choose())

View(salary_data_test)

summary(salary_data_test)

str(salary_data_test)

# check for duplicate rows

dim(unique(salary_data_test)) # 14130

salary_data_test<-unique(salary_data_test)

# Feature scaling of test data

salary_data_test[,c(1,4,10:12)]<-as.data.frame(lapply(salary_data_test[,c(1,4,10:12)],norm))

# Applying Naive Bayes classifier

library(e1071)

model_naive<-naiveBayes(salary_data$Salary~.,data = salary_data[,-14])
pred_naive<-predict(model_naive,newdata=salary_data_test[,-14])
mean(pred_naive==salary_data_test$Salary) # 0.81

# applying K fold cross validation 
# k=27

library(caret)

folds<-createFolds(salary_data$Salary,k=27)
cv<-lapply(folds,function(x){
  train_fold<-salary_data[-x,]
  test_fold<-salary_data[x,]
  classifier<-naiveBayes(train_fold$Salary~.,data = train_fold)
  y_pred<-predict(classifier,newdata=test_fold[,-14])
  accuracy<-mean(y_pred==test_fold$Salary)
  return(accuracy)
})

cv

mean(as.numeric(cv)) # 0.81

# k =10

folds_10<-createFolds(salary_data$Salary,k=10)
cv_10<-lapply(folds_10,function(x){
  train_fold_10<-salary_data[-x,]
  test_fold_10<-salary_data[x,]
  classifier_10<-naiveBayes(train_fold_10$Salary~.,data = train_fold_10)
  y_pred_10<-predict(classifier_10,newdata=test_fold_10[,-14])
  accuracy_10<-mean(y_pred_10==test_fold_10$Salary)
  return(accuracy_10)
})

cv_10

mean(as.numeric(cv_10)) # 0.82

# k =5

folds_5<-createFolds(salary_data$Salary,k=5)
cv_5<-lapply(folds_5,function(x){
  train_fold_5<-salary_data[-x,]
  test_fold_5<-salary_data[x,]
  classifier_5<-naiveBayes(train_fold_5$Salary~.,data = train_fold_5)
  y_pred_5<-predict(classifier_5,newdata=test_fold_5[,-14])
  accuracy_5<-mean(y_pred_5==test_fold_5$Salary)
  return(accuracy_5)
})

cv_5

mean(as.numeric(cv_5)) # 0.82
