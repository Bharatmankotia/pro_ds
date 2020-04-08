fraud<-read.csv(file.choose())

# check for duplicate rows

dim(unique(fraud))

attach(fraud)

# check for outliers

boxplot(Taxable.Income)

boxplot(City.Population)

boxplot(Work.Experience)

str(fraud)

summary(fraud)

# converting taxable income to good and risky

fraud$Taxable.Income<-cut(fraud$Taxable.Income,c(9000,30000,100000),labels = c('risky','good'))

View(fraud)

# Moving the output variable column to last column

fraud<-fraud[,c(1:2,4:6,3)]
View(fraud)

table(fraud$Taxable.Income)
prop.table(table(fraud$Taxable.Income))

# splitting data into train and test data 

library(caTools)
set.seed(143)
split<-sample.split(fraud$Taxable.Income,SplitRatio = 0.8)
train_set<-subset(fraud,split==TRUE)
test_set<-subset(fraud,split==FALSE)

prop.table(table(train_set$Taxable.Income))
prop.table(table(test_set$Taxable.Income))

# Applying Random Forest

library(randomForest)

fraud_rf<-randomForest(x=train_set[,-6],
                       y=train_set$Taxable.Income)
y_pred<-predict(fraud_rf,newdata = test_set[,-6])

# Confusion Matrix

confuse<-table(y_pred,test_set$Taxable.Income)
confuse

# Accuracy 

acc<-sum(diag(confuse))/sum(confuse)
acc # 0.78

# Bulding model with number of trees as 40

fraud_rf_1<-randomForest(x=train_set[,-6],
                       y=train_set$Taxable.Income,
                       ntree = 40)
y_pred_1<-predict(fraud_rf_1,newdata = test_set[,-6])

# Confusion Matrix

confuse_1<-table(y_pred_1,test_set$Taxable.Income)
confuse_1

# Accuracy 

acc_1<-sum(diag(confuse_1))/sum(confuse_1)
acc_1 # 0.783

# Applying K fold cross validation

library(caret)

folds<-createFolds(train_set$Taxable.Income,k=10)
cv<-lapply(folds,function(x){
  train_fold<-train_set[-x,]
  test_fold<-train_set[x,]
  classifier<-randomForest(x=train_fold[,-6],y=train_fold$Taxable.Income)
  y_pred<-predict(classifier,newdata=test_fold[,-6])
  confusion_k<-table(test_fold[,6],y_pred)
  accuracy_k<-sum(diag(confusion_k))/sum(confusion_k)
  return(accuracy_k)
})

cv

mean(as.numeric(cv)) # accuracy 0.787

