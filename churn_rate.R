churn<-read.csv(file.choose())
View(churn)
summary(churn)


library(lattice)

qqnorm(churn$Salary_hike)
qqline(churn$Salary_hike)

hist(churn$Salary_hike)
hist(churn$Churn_out_rate)

barplot(churn$Salary_hike)
barplot(churn$Churn_out_rate)

boxplot(churn$Salary_hike)
boxplot(churn$Churn_out_rate)

cor(churn)
plot(churn$Salary_hike,churn$Churn_out_rate,colour='red',xlab = 'salary hike',ylab = 'churn out rate',
     pch=20)
attach(churn)
linear_model<-lm(Churn_out_rate~.,data = churn)
summary(linear_model)
confint(linear_model,level=0.95)
pred<-predict(linear_model,interval='predict')
pred<-data.frame(pred)

library(ggplot2)
ggplot() + 
  geom_point(aes(x=churn$Salary_hike, y=churn$Churn_out_rate),colour='red') +
  geom_line(aes(x=churn$Salary_hike, y=predict(linear_model,newdata = churn)),colour='blue') + 
  ggtitle('salary hike vs churn out rate') + 
  xlab('salary hike') + 
  ylab('churn out rate')

churn$salary_hike2<-churn$Salary_hike^2

quad_model<-lm(Churn_out_rate~.,data = churn)
summary(quad_model)
pred_quad<-predict(quad_model,interval = 'predict')

ggplot() + 
  geom_point(aes(x=churn$Salary_hike, y=churn$Churn_out_rate),colour='red') +
  geom_line(aes(x=churn$Salary_hike, y=predict(quad_model,newdata = churn)),colour='blue') + 
  ggtitle('salary hike vs churn out rate') + 
  xlab('salary hike') + 
  ylab('churn out rate')

churn$salary_hike3<-churn$Salary_hike^3

poly_model<-lm(Churn_out_rate~.,data = churn)
summary(poly_model)
confint(poly_model,level = 0.95)
pred_poly<-predict(poly_model,interval = 'predict')
pred_poly<-data.frame(pred_poly)
ggplot() + 
  geom_point(aes(x=churn$Salary_hike, y=churn$Churn_out_rate),colour='red') +
  geom_line(aes(x=churn$Salary_hike, y=predict(poly_model,newdata = churn)),colour='blue') + 
  ggtitle('salary hike vs churn out rate') + 
  xlab('salary hike') + 
  ylab('churn out rate')

cor(churn)
cor(pred_poly$fit,churn$Churn_out_rate) 

# predicting churn out rate for salary hike 1660

y_pred<-predict(poly_model,data.frame(Salary_hike= 1660,
                                      salary_hike2= 1660^2,
                                      salary_hike3= 1660^3))
y_pred #72

