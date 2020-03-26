calories<-read.csv(file.choose())
View(calories)
library(lattice)

qqnorm(calories$Weight.gained..grams.)
qqline(calories$Weight.gained..grams.)

qqnorm(calories$Calories.Consumed)
qqline(calories$Calories.Consumed)

boxplot(calories$Weight.gained..grams.)
boxplot(calories$Calories.Consumed)

hist(calories$Weight.gained..grams.)
hist(calories$Calories.Consumed)

plot(calories$Calories.Consumed,calories$Weight.gained..grams.,colour='blue',xlab = 'calories consumed',ylab = 'weight gained',pch=20)

cor(calories)

attach(calories)

reg<-lm(Weight.gained..grams.~.,data = calories)
summary(reg)
pred<-predict(reg,interval = 'predict')
pred<-data.frame(pred)


library(ggplot2)

ggplot()+
  geom_point(aes(x=calories$Calories.Consumed, y=calories$Weight.gained..grams.),colour='red') +
  geom_line(aes(x=calories$Calories.Consumed, y=predict(reg, newdata = calories)),colour='blue') + 
  ggtitle('calories consumed vs weight gained') + 
  xlab('calories consumed') + 
  ylab('weight gained')

calories$calories2<- calories$Calories.Consumed^2
poly_reg<-lm(calories$Weight.gained..grams.~.,data=calories)
summary(poly_reg)
pred_poly<-predict(poly_reg,interval = 'predict')

ggplot()+
  geom_point(aes(x=calories$Calories.Consumed, y=calories$Weight.gained..grams.),colour='red') +
  geom_line(aes(x=calories$Calories.Consumed, y=predict(poly_reg, newdata = calories)),colour='blue') + 
  ggtitle('calories consumed vs weight gained') + 
  xlab('calories consumed') + 
  ylab('weight gained')

calories$calories3<- calories$Calories.Consumed^3

poly_reg_3<-lm(calories$Weight.gained..grams.~.,data = calories)
summary(poly_reg_3)

pred_poly_3<-predict(poly_reg_3,interval = 'predict')

ggplot()+
  geom_point(aes(x=calories$Calories.Consumed, y=calories$Weight.gained..grams.),colour='red') +
  geom_line(aes(x=calories$Calories.Consumed, y=predict(poly_reg_3, newdata = calories)),colour='blue') + 
  ggtitle('calories consumed vs weight gained') + 
  xlab('calories consumed') + 
  ylab('weight gained')

calories$calories4<- calories$Calories.Consumed^4

poly_reg_4<-lm(calories$Weight.gained..grams.~.,data = calories)
summary(poly_reg_4)

pred_poly_4<-predict(poly_reg_4,interval = 'predict')
pred_poly_4<-data.frame(pred_poly_4)
pred_poly_4
ggplot()+
  geom_point(aes(x=calories$Calories.Consumed, y=calories$Weight.gained..grams.),colour='red') +
  geom_line(aes(x=calories$Calories.Consumed, y=predict(poly_reg_4, newdata = calories)),colour='blue') + 
  ggtitle('calories consumed vs weight gained') + 
  xlab('calories consumed') + 
  ylab('weight gained')

# Predicting weight gain for 1500 calories consumed.
y_pred<-predict(poly_reg_4,data.frame(Calories.Consumed=1500,
                                       calories2=1500^2,
                                       calories3=1500^3,
                                       calories4=1500^4))
y_pred

# Predicting weight gain for 2000 calories consumed.
y_pred<-predict(poly_reg_4,data.frame(Calories.Consumed=2000,
                                      calories2=2000^2,
                                      calories3=2000^3,
                                      calories4=2000^4))
y_pred
