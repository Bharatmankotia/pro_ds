summary(Salary_Data)
library(lattice)
plot(Salary_Data$YearsExperience,Salary_Data$Salary,pch=20)
dotplot(Salary_Data$YearsExperience)
dotchart(Salary_Data$YearsExperience)

dotplot(Salary_Data$Salary)
dotchart(Salary_Data$Salary)

boxplot(Salary_Data$YearsExperience)
boxplot(Salary_Data$Salary)

hist(Salary_Data$YearsExperience)
hist(Salary_Data$Salary)

attach(Salary_Data)
cor(Salary_Data)

library(caTools)
set.seed(231)
split<-sample.split(Salary_Data$Salary,SplitRatio = 2/3)
training_set<-subset(Salary_Data,split==T)
test_set<-subset(Salary_Data,split==F)

reg<-lm(Salary~YearsExperience,data = training_set)
summary(reg)

confint(reg,level = 0.95)

pred<-predict(reg,interval = "predict",newdata = test_set)
pred

pred<-data.frame(pred)

cor(pred$fit,test_set$Salary)

library(ggplot2)
ggplot() + 
  geom_point(aes(x= training_set$YearsExperience,y=training_set$Salary),colour='red') + 
  geom_line(aes(training_set$YearsExperience, y= predict(reg,newdata = training_set)),colour='blue') +
  ggtitle('salary vs years of experience(training set)') + 
  xlab('years of experience') + 
  ylab('Salary')


ggplot() + 
  geom_point(aes(x= test_set$YearsExperience,y=test_set$Salary),colour='red') + 
  geom_line(aes(training_set$YearsExperience, y= predict(reg,newdata = training_set)),colour='blue') +
  ggtitle('salary vs years of experience(test set)') + 
  xlab('years of experience') + 
  ylab('Salary')

y_pred<-predict(reg,data.frame(YearsExperience=1.1))
y_pred
y_pred<-predict(reg,data.frame(YearsExperience=2.2))
y_pred
