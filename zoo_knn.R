zoo<-read.csv(file.choose())
View(zoo)
zoo_new<-zoo[-1]
View(zoo_new)

table(zoo_new$type)
prop.table(table(zoo_new$type))

str(zoo_new)

zoo_new<-lapply(zoo_new,as.factor)

str(zoo_new)

zoo_new<-data.frame(zoo_new)

summary(zoo_new)

# chcking for different levels of features

for(i in 1:17){
  message(i,
          ".",
          "classes of",
          " ",
          colnames(zoo_new[i]),
          "(",
          sum(table(levels(zoo_new[,i]))),
          ")",
          "=",
          paste(levels(zoo_new[[i]]),
          collapse = " "))
}

# checking proportions of each class

for (i in 1:17){
  message(i,
          ".",
          colnames(zoo_new[i]),
          "/n",
          paste(levels(zoo_new[[i]]),
          prop.table(table(zoo_new[[i]]))*100,
          collapse = "\n"),
          collapse= "\n")
}

# splitting data into train and test

library(caTools)

set.seed(2123)

split_1<-sample.split(zoo_new$type,SplitRatio = 0.8)
train_set<-subset(zoo_new,split_1==TRUE)
test_set<-subset(zoo_new,split_1==FALSE)


# Fitting KNN to the training set

library(class) # classification library

y_pred<-knn(train = train_set[,-17],
            test = test_set[,-17],
            cl= train_set[,17],
            k=5)

confusionMatrix(y_pred,test_set[,17])# 0.81


# Building model with k= 8

y_pred_1<-knn(train = train_set[,-17],
            test = test_set[,-17],
            cl= train_set[,17],
            k=8)

cm_1<- table(test_set[,17],y_pred_1)
cm_1
accuracy_1<-sum(diag(cm_1))/sum(cm_1)
accuracy_1 # 0.81

# Building model with k= 15

y_pred_2<-knn(train = train_set[,-17],
              test = test_set[,-17],
              cl= train_set[,17],
              k=15)

cm_2<- table(test_set[,17],y_pred_2)
cm_2
accuracy_2<-sum(diag(cm_2))/sum(cm_2)
accuracy_2 # 0.76


# Building model with k= 10

y_pred_3<-knn(train = train_set[,-17],
              test = test_set[,-17],
              cl= train_set[,17],
              k=10)

cm_3<- table(test_set[,17],y_pred_3)
cm_3
accuracy_3<-sum(diag(cm_3))/sum(cm_3)
accuracy_3 # 0.76


# Applying K Fold Cross validation technique

library(caret)

folds<-createFolds(train_set$type,k=10)

cv<-lapply(folds,function(x){
  train_fold<-train_set[-x,]
  test_fold<-train_set[x,]
  classifier<-knn(train = train_fold[,-17],
                  test = test_fold[,-17],
                  cl= train_fold[,17],
                  k=8)
  cm_4<- table( test_fold[,17],classifier)
  cm_4
  accuracy_4<-sum(diag(cm_4))/sum(cm_4)
  return(accuracy_4) 
})

cv
mean(as.numeric(cv)) # 0.79






