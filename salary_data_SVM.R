salary_data<-read.csv(file.choose())
View(salary_data)

summary(salary_data)

str(salary_data)

# converting factors to numeric to apply to svm kernel

library(dplyr)

levels(salary_data$workclass)

salary_data$workclass<-unclass(salary_data$workclass) %>% as.numeric

levels(salary_data$education)

salary_data$education<-unclass(salary_data$education) %>% as.numeric

levels(salary_data$maritalstatus)

salary_data$maritalstatus<-unclass(salary_data$maritalstatus) %>% as.numeric

levels(salary_data$occupation)

salary_data$occupation<-unclass(salary_data$occupation) %>% as.numeric

levels(salary_data$relationship)

salary_data$relationship<-unclass(salary_data$relationship) %>% as.numeric

levels(salary_data$race)

salary_data$race<-unclass(salary_data$race) %>% as.numeric

levels(salary_data$sex)

salary_data$sex<-unclass(salary_data$sex) %>% as.numeric

levels(salary_data$native)

salary_data$native<-unclass(salary_data$native) %>% as.numeric

summary(salary_data)

str(salary_data)

# check for duplicate rows

dim(unique(salary_data)) # 26903

salary_data<-unique(salary_data)

# Feature scaling of train data

norm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

salary_data[,1:13]<-as.data.frame(lapply(salary_data[,1:13],norm))

prop.table(table(salary_data$Salary)) # checking proportion of output class

# importing  test data

salary_data_test<-read.csv(file.choose())

View(salary_data_test)

summary(salary_data_test)

str(salary_data_test)

levels(salary_data_test$workclass)

salary_data_test$workclass<-unclass(salary_data_test$workclass) %>% as.numeric

levels(salary_data_test$education)

salary_data_test$education<-unclass(salary_data_test$education) %>% as.numeric

levels(salary_data_test$maritalstatus)

salary_data_test$maritalstatus<-unclass(salary_data_test$maritalstatus) %>% as.numeric

levels(salary_data_test$occupation)

salary_data_test$occupation<-unclass(salary_data_test$occupation) %>% as.numeric

levels(salary_data_test$relationship)

salary_data_test$relationship<-unclass(salary_data_test$relationship) %>% as.numeric

levels(salary_data_test$race)

salary_data_test$race<-unclass(salary_data_test$race) %>% as.numeric

levels(salary_data_test$sex)

salary_data_test$sex<-unclass(salary_data_test$sex) %>% as.numeric

levels(salary_data_test$native)

salary_data_test$native<-unclass(salary_data_test$native) %>% as.numeric

# check for duplicate rows

dim(unique(salary_data_test)) # 14130

salary_data_test<-unique(salary_data_test)

# Feature scaling of test data

salary_data_test[,1:13]<-as.data.frame(lapply(salary_data_test[,1:13],norm))

prop.table(table(salary_data_test$Salary)) # checking proportion of output class

# Applying SVM classifier

library(kernlab)
library(caret)

model_ksvm<-ksvm(salary_data$Salary~.,data= salary_data,kernel='vanilladot')

pred_ksvm<-predict(model_ksvm,newdata=salary_data_test)

mean(pred_ksvm==salary_data_test$Salary) # 0.80

# Using rbfdot kernel

model_rbfdot<-ksvm(salary_data$Salary~.,data= salary_data,kernel='rbfdot')

pred_ksvm_rbfdot<-predict(model_rbfdot,newdata=salary_data_test)

mean(pred_ksvm_rbfdot==salary_data_test$Salary) # 0.84

# using polydot kernel

model_polydot<-ksvm(salary_data$Salary~.,data= salary_data,kernel='polydot')

pred_ksvm_polydot<-predict(model_polydot,newdata=salary_data_test)

mean(pred_ksvm_polydot==salary_data_test$Salary) # 0.80

# using besseldot kernel

model_besseldot<-ksvm(salary_data$Salary~.,data= salary_data,kernel='besseldot')

pred_ksvm_besseldot<-predict(model_besseldot,newdata=salary_data_test)

mean(pred_ksvm_besseldot==salary_data_test$Salary)

