forest<-read.csv(file.choose())

str(forest)

levels(forest$size_category)

# converting size_category to numeric

forest$size_category<-as.numeric(factor(forest$size_category,levels = c('small','large'),labels = c(0,1)))

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
forest<-as.data.frame(lapply(forest,norm))

# moving output column to the last column

forest<-forest[,c(1:8,10:29,9)]

# splitting data into train and test data

library(caTools)

set.seed(213)

split<-sample.split(forest$area,SplitRatio = 0.8)

train_set<-subset(forest,split==TRUE)
test_set<-subset(forest,split==FALSE)

# Applying ANN

library(neuralnet)

attach(train_set)

model_forest<-neuralnet(formula = area~.,data = train_set)

results_model<- compute(model_forest,test_set[,1:28])

plot(model_forest)

str(results_model)

predicted_strength<-results_model$net.result

cor(predicted_strength,test_set$area) # 0.999




