forest<-read.csv(file.choose())
View(forest)

str(forest)

# check for duplicate rows

dim(unique(forest))

# Removing dupicate values

forest<-unique(forest)

nrow(forest)

attach(forest)

# check for the outliers

boxplot(FFMC)
boxplot(FFMC,plot = F)$out

boxplot(DMC)
boxplot(DMC,plot = F)$out

boxplot(DC)
boxplot(DC,plot = F)$out

boxplot(ISI)
boxplot(ISI,plot = F)$out

boxplot(temp)
boxplot(temp,plot = F)$out

boxplot(RH)
boxplot(RH,plot = F)$out

boxplot(wind)
boxplot(wind,plot = F)$out

boxplot(rain)
boxplot(rain,plot = F)$out

boxplot(area)
boxplot(area,plot = F)$out


# Removing month and day column 

forest<-forest[,-c(1,2)]

View(forest)

# Feature scaling

norm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

forest[,1:28]<-as.data.frame(lapply(forest[,1:28],norm))

# # splitting data into train and test data

library(caTools)

set.seed(123)

split<-sample.split(forest$size_category,SplitRatio = 0.8)

train_set<-subset(forest,split==TRUE)
test_set<-subset(forest,split==FALSE)

attach(forest)
# Applying SVM

library(kernlab)
library(caret)

# Building model using vanilladot kernel

model_vanilla<-ksvm(size_category~.,data= train_set,kernel='vanilladot')

pred_vanilla<-predict(model_vanilla,newdata=test_set)

mean(pred_vanilla==test_set$size_category) # 0.83

# Building model using rbfdot kernel

model_rbfdot<-ksvm(size_category~.,data= train_set,kernel='rbfdot')

pred_rbfdot<-predict(model_rbfdot,newdata=test_set)

mean(pred_rbfdot==test_set$size_category) # 0.75

# Building model using polydot kernel

model_polydot<-ksvm(size_category~.,data= train_set,kernel='polydot')

pred_polydot<-predict(model_polydot,newdata=test_set)

mean(pred_polydot==test_set$size_category) # 0.83

# Building model using besseldot kernel

model_besseldot<-ksvm(size_category~.,data= train_set,kernel='besseldot')

pred_besseldot<-predict(model_besseldot,newdata=test_set)

mean(pred_besseldot==test_set$size_category) # 0.69

# Building model using vtanhdot kernel

model_tanhdot<-ksvm(size_category~.,data= train_set,kernel='tanhdot')

pred_tanhdot<-predict(model_tanhdot,newdata=test_set)

mean(pred_tanhdot==test_set$size_category) # 0.57

# Building model using laplacedot kernel

model_laplace<-ksvm(size_category~.,data= train_set,kernel='laplace')

pred_laplace<-predict(model_laplace,newdata=test_set)

mean(pred_laplace==test_set$size_category) # 0.73

# Building model using anovadot kernel

model_anovadot<-ksvm(size_category~.,data= train_set,kernel='anovadot')

pred_anovadot<-predict(model_anovadot,newdata=test_set)

mean(pred_anovadot==test_set$size_category)# 0.88

# Building model using splinedot kernel

model_splinedot<-ksvm(size_category~.,data= train_set,kernel='splinedot')

pred_splinedot<-predict(model_splinedot,newdata=test_set)

mean(pred_splinedot==test_set$size_category) # 0.65



