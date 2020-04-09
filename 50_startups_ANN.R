startups<-read.csv(file.choose())
View(startups)

summary(startups)

# converting states to numeric as required by neural network.

startups$State<-as.numeric(factor(startups$State,levels = c('California','Florida','New York'),labels = c(1,2,3)))

str(startups)                           

dim(unique(startups)) # check for duplicate rows

attach(startups)

# check for outliers

boxplot(R.D.Spend)

boxplot(Administration)

boxplot(Marketing.Spend)

boxplot(Profit)

# Feature scaling

norm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
startups<-as.data.frame(lapply(startups,norm))

# splitting dataset into train and test

library(caTools)

set.seed(234)

split<-sample.split(startups$Profit,SplitRatio = 0.8)

train_set<-subset(startups,split==TRUE)
test_set<-subset(startups,split==FALSE)

# Using ANN 

library(neuralnet)

startups_model<-neuralnet(Profit~.,data = train_set)

plot(startups_model)

results_model<- compute(startups_model,test_set[1:4])

str(results_model)

predicted_strength<-results_model$net.result

cor(predicted_strength,test_set$Profit) # 0.98 accuracy

# Increasing no. of hidden layers

startups_model_2<-neuralnet(Profit~.,data = train_set,hidden = 5)

plot(startups_model_2)

results_model_2<- compute(startups_model_2,test_set[1:4])

str(results_model_2)

predicted_strength_2<-results_model_2$net.result

cor(predicted_strength_2,test_set$Profit) # 0.98

# Using h20 library 

install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)

classifier<-h2o.deeplearning(y='Profit',
                             training_frame = as.h2o(train_set),
                             activation = 'Rectifier',
                             hidden = c(2,2),
                             epochs = 100,
                             train_samples_per_iteration = -2)

y_pred_h<-h2o.predict(classifier,newdata=as.h2o(test_set[,-5]))

y_pred_h<-as.vector(y_pred_h)

cor(y_pred_h,test_set$Profit) # 0.86 accuracy

