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

# Building decision tree using c5.0

library(C50)

fraud_c<-C5.0(train_set$Taxable.Income~.,data = train_set[,-6])
plot(fraud_c)

# Predicting Training accuracy

pred_train_c<-predict(fraud_c,train_set)
mean(pred_train_c==train_set$Taxable.Income) # 0.79

# Predicting accuracy on test set

pred_test_c<-predict(fraud_c,newdata = test_set)
mean(pred_test_c==test_set$Taxable.Income) # 0.79

# Using tree function 

library(tree)

fraud_tree<-tree(train_set$Taxable.Income~.,data = train_set[,-6])

pred_tree<-as.data.frame(predict(fraud_tree,newdata = test_set))
pred_tree['Final']<-NULL
pred_test_df<-predict(fraud_tree,newdata=test_set)

pred_tree$final<-colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
mean(pred_tree$final==test_set$Taxable.Income) # 0.79

# Using Bagging

model_bag<-C5.0(train_set$Taxable.Income~.,data = train_set[,-6])
pred_bag<-predict.C5.0(model_bag,test_set[,-6])
confusionMatrix(pred_bag,test_set$Taxable.Income)

fittree_bag<-vector(mode = 'list',length = 59)
test_1<-vector(mode = 'list',length = 59)

set.seed(232)
for(i in 1:59){
  split_bag<-sample.split(fraud$Taxable.Income,SplitRatio = 0.8)
  train_bag<-subset(fraud,split_bag==TRUE)
  test_1[[i]]<-subset(fraud,split_bag==FALSE)
  fittree_bag[[i]]<-C5.0(train_bag$Taxable.Income~.,data = train_bag[,-6])
}

fittree_bag
test_1

# predictions, confusion matrixand accuracy for the 59 models

pred_bag<-vector(mode='list',length = '59')
confusion_bag<-vector(mode = 'list',length = '59')
accuracy_bag<-vector(mode = 'list',length = '59')

for(j in 1:length(fittree_bag)){
  
  pred_bag[[j]]<-predict.C5.0(fittree_bag[[j]],test_1[[j]][,-6],type = 'class')
  confusion_bag[[j]]<-table(pred_bag[[j]],test_1[[j]]$Taxable.Income)
  accuracy_bag[[j]]<-sum(diag(confusion_bag[[j]]))/sum(confusion_bag[[j]])
}

pred_bag
confusion_bag
accuracy_bag
class(accuracy_bag)
mean(unlist(accuracy_bag)) # 0.79 Accuracy

# Applying K fold cross validation

library(caret)

folds<-createFolds(train_set$Taxable.Income,k=10)
cv<-lapply(folds,function(x){
  train_fold<-train_set[-x,]
  test_fold<-train_set[x,]
  classifier<-C5.0(train_fold$Taxable.Income~.,data = train_fold)
  y_pred<-predict(classifier,newdata=test_fold[,-6])
  confusion_k<-table(test_fold[,6],y_pred)
  accuracy_k<-sum(diag(confusion_k))/sum(confusion_k)
  return(accuracy_k)
})

cv

mean(as.numeric(cv)) # 0.79
