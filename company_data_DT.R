company<-read.csv(file.choose())

View(company)

summary(company)

# converting sales to low, medium and high

company$Sales<-cut(company$Sales,c(-1,6,12,18),labels = c('low','medium','high'))
View(company)

# checking proportion of output class

prop.table(table(company$Sales))

# checking for duplicate rows

dim(unique(company))

attach(company)

# checking for outliers 

boxplot(CompPrice)
boxplot(CompPrice,plot = F)$out

boxplot(Income)
boxplot(Income,plot = F)$out

boxplot(Advertising)
boxplot(Advertising,plot = F)$out

boxplot(Population)
boxplot(Population,plot = F)$out

boxplot(Price)
boxplot(Price,plot = F)$out

boxplot(Population)
boxplot(Population,plot = F)$out

boxplot(Age)
boxplot(Age,plot = F)$out

boxplot(Education)
boxplot(Education,plot = F)$out

# splitting data into train and test

library(caTools)

set.seed(123)

split<-sample.split(company$Sales,SplitRatio = 0.8)
train_set<-subset(company,split==TRUE)
test_set<-subset(company,split==FALSE)

prop.table(table(train_set$Sales))

prop.table(table(test_set$Sales))

# Applying C5.0


library(C50)

company_tree<-C5.0(train_set[,-1],train_set$Sales)
plot(company_tree)

# Training accuracy

pred_train<-predict(company_tree,train_set)
mean(pred_train==train_set$Sales) # 0.91 accuracy

# Predicting on test data

library(caret)

pred_test<-predict(company_tree,newdata = test_set)
mean(pred_test==test_set$Sales) # 0.66 accuracy. Problem of overfitting

# Adaboost

acc<-c()

# building 101 models

for(i in 1:101){
  fittree<-C5.0(train_set$Sales~.,data = train_set[,-1],trials=15)
  pred_ada<-predict.C5.0(fittree,test_set[,-1])
  a<-table(pred_ada,test_set$Sales)
  acc<-c(acc,sum(diag(a))/sum(a))
}
mean(acc) # 0.72 is the accuracy. We will try now bagging

# Applying bagging

model_bag<-C5.0(train_set$Sales~.,data = train_set[,-1])
pred_bag<-predict.C5.0(model_bag,test_set[,-1])
confusionMatrix(pred_bag,test_set$Sales)

fittree_bag<-vector(mode = 'list',length = 40)
test_1<-vector(mode = 'list',length = 40)

set.seed(2)
for(i in 1:40){
split_bag<-sample.split(company$Sales,SplitRatio = 0.8)
train_bag<-subset(company,split_bag==TRUE)
test_1[[i]]<-subset(company,split_bag==FALSE)
fittree_bag[[i]]<-C5.0(train_bag$Sales~.,data = train_bag[,-1])
}

fittree_bag
test_1

# predictions, confusion matrixand accuracy for the 40 models

pred_bag<-vector(mode='list',length = '40')
confusion_bag<-vector(mode = 'list',length = '40')
accuracy_bag<-vector(mode = 'list',length = '40')

for(j in 1:length(fittree_bag)){
  
  pred_bag[[j]]<-predict.C5.0(fittree_bag[[j]],test_1[[j]][,-1],type = 'class')
  confusion_bag[[j]]<-table(pred_bag[[j]],test_1[[j]]$Sales)
  accuracy_bag[[j]]<-sum(diag(confusion_bag[[j]]))/sum(confusion_bag[[j]])
}

pred_bag
confusion_bag
accuracy_bag
class(accuracy_bag)
mean(unlist(accuracy_bag)) # Accuracy of 0.72


# using tree function

library(tree)

company_tree_new<-tree(train_set$Sales~.,data = train_set)
summary(company_tree_new)

# predicting test data using the model

pred_tree_new <- as.data.frame(predict(company_tree_new,newdata=test_set))
pred_tree_new["final"] <- NULL
pred_test_df <- predict(company_tree_new,newdata=test_set)

pred_tree_new$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
mean(pred_tree_new$final==test_set$Sales) # accuracy 0.61

# Best model formed was using bagging technique.
