company<-read.csv(file.choose())

View(company)

summary(company)

str(company)
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

# Feature scaling. Feature scaling is not required in DT and RF as there is no distance measure
# Feature scaling just helps in visualization

normalized <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

company[,c(2:6,8,9)]<-lapply(company[,c(2:6,8,9)],normalized)

# Applying Random Forest

library(randomForest)

model_rf<-randomForest(x= train_set[,-1],y=train_set$Sales)

y_pred<-predict(model_rf,newdata = test_set[,-1])

# Confusion matrix

cm<-table(test_set[,1],y_pred)
cm

accuracy<-sum(diag(cm))/sum(cm)
accuracy # 0.7 accuracy

# Building Random Forest with number of trees as 10

model_rf_1<-randomForest(x= train_set[,-1],y=train_set$Sales,ntree = 9)


y_pred_1<-predict(model_rf_1,newdata = test_set[,-1])

# Confusion matrix

cm_1<-table(test_set[,1],y_pred_1)
cm_1

accuracy_1<-sum(diag(cm_1))/sum(cm_1)
accuracy_1 # 0.80 accuracy

