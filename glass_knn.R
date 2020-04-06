glass<-read.csv(file.choose())
View(glass)
summary(glass)
str(glass)

glass$Type<-as.factor(glass$Type)

str(glass)

levels(glass$Type) # total of 6 levels


dim(unique(glass)) # 1 row duplicated

# Removing duplicate row

glass<-unique(glass)

nrow(glass)

attach(glass)

# Checking for outliers

boxplot(RI)
boxplot(RI,plot = F)$out

boxplot(Na)
boxplot(Na,plot = F)$out

boxplot(Mg)
boxplot(Mg,plot = F)$out

boxplot(Al)
boxplot(Al,plot = F)$out

boxplot(Si)
boxplot(Si,plot = F)$out

boxplot(K)
boxplot(K,plot = F)$out

boxplot(Ca)
boxplot(Ca,plot = F)$out

boxplot(Ba)
boxplot(Ba,plot = F)$out

boxplot(Fe)
boxplot(Fe,plot = F)$out

# Feature scaling

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

glass[,(1:9)]<-lapply(glass[,(1:9)],normalize)

glass<-data.frame(glass)

View(glass)

table(glass$Type)
prop.table(table(glass$Type))

# splitting data into trAINING AND TEST set

library(caTools)

set.seed(123)
split<-sample.split(glass$Type,SplitRatio = 0.8)
train_set<-subset(glass,split==TRUE)
test_set<-subset(glass,split==FALSE)

prop.table(table(train_set$Type))
prop.table(table(test_set$Type))
# applying knn 
library(class)


y_pred<-knn(train = train_set[,-10],test = test_set[,-10], cl=train_set[,10],k=13)

cm<-table(y_pred,test_set[,10])
cm

accuracy<-sum(diag(cm))/sum(cm)
accuracy # 0.60

y_pred_1<-knn(train = train_set[,-10],test = test_set[,-10], cl=train_set[,10],k=7)

cm_1<-table(y_pred_1,test_set[,10])
cm_1

accuracy_1<-sum(diag(cm_1))/sum(cm_1)
accuracy_1 # 0.55

y_pred_2<-knn(train = train_set[,-10],test = test_set[,-10], cl=train_set[,10],k=21)

cm_2<-table(y_pred_2,test_set[,10])
cm_2

accuracy_2<-sum(diag(cm_2))/sum(cm_2)
accuracy_2 # 0.56

y_pred_3<-knn(train = train_set[,-10],test = test_set[,-10], cl=train_set[,10],k=3)

cm_3<-table(y_pred_3,test_set[,10])
cm_3


accuracy_3<-sum(diag(cm_3))/sum(cm_3)
accuracy_3 # 0.72

y_pred_4<-knn(train = train_set[,-10],test = test_set[,-10], cl=train_set[,10],k=5)

cm_4<-table(y_pred_4,test_set[,10])
cm_4

accuracy_4<-sum(diag(cm_4))/sum(cm_4)
accuracy_4 # 0.69

# applying k fold cross validation

library(caret)

folds<-createFolds(train_set$Type,k=10)

cv<-lapply(folds,function(x){
  train_fold<-train_set[-x,]
  test_fold<-train_set[x,]
  classifier<-knn(train = train_fold[,-10],test = test_fold[,-10],cl=train_fold[,10],k=3)
  cm_k<-table(test_fold[,10],classifier)
  accuracy_k<-sum(diag(cm_k))/sum(cm_k)
  return(accuracy_k)
})
cv
mean(as.numeric(cv)) # 0.69
