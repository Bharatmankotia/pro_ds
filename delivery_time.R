delivery_time<-read.csv(file.choose())
View(delivery_time)
summary(delivery_time)

library(lattice)
qqnorm(delivery_time$Delivery.Time)
qqline(delivery_time$Delivery.Time)

qqnorm(delivery_time$Sorting.Time)
qqline(delivery_time$Sorting.Time)

boxplot(delivery_time$Sorting.Time)
boxplot(delivery_time$Delivery.Time)

hist(delivery_time$Sorting.Time)
hist(delivery_time$Delivery.Time)

plot(delivery_time$Sorting.Time,delivery_time$Delivery.Time,colour='blue',xlab = 'sorting time',ylab = 'delivery time',pch=20)

cor(delivery_time)
attach(delivery_time)

reg<-lm(Delivery.Time~.,data = delivery_time)
summary(reg)

pred<-predict(reg,interval = 'predict')

library(ggplot2)
ggplot() + 
  geom_point(aes(x= delivery_time$Sorting.Time, y= delivery_time$Delivery.Time),colour='red') + 
  geom_line(aes(x= delivery_time$Sorting.Time, y= predict(reg,newdata = delivery_time)),colour='blue') +
  ggtitle('sorting time vs delivery time') + 
  xlab('sorting time') + 
  ylab('delivery time')

reg_log<-lm(delivery_time$Delivery.Time~I(log(delivery_time$Sorting.Time)),data=delivery_time)
summary(reg_log)

ggplot() + 
  geom_point(aes(x= delivery_time$Sorting.Time, y= delivery_time$Delivery.Time),colour='red') + 
  geom_line(aes(x= delivery_time$Sorting.Time, y= predict(reg_log,newdata = delivery_time)),colour='blue') +
  ggtitle('sorting time vs delivery time') + 
  xlab('sorting time') + 
  ylab('delivery time')

reg_poly<-lm(Delivery.Time~Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3))
summary(reg_poly)

ggplot() + 
  geom_point(aes(x= delivery_time$Sorting.Time, y= delivery_time$Delivery.Time),colour='red') + 
  geom_line(aes(x= delivery_time$Sorting.Time, y= predict(reg_poly,newdata = delivery_time)),colour='blue') +
  ggtitle('sorting time vs delivery time') + 
  xlab('sorting time') + 
  ylab('delivery time')

reg_poly_1<-lm(Delivery.Time~Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3) + I(Sorting.Time^4) )
summary(reg_poly_1)

ggplot() + 
  geom_point(aes(x= delivery_time$Sorting.Time, y= delivery_time$Delivery.Time),colour='red') + 
  geom_line(aes(x= delivery_time$Sorting.Time, y= predict(reg_poly_1,newdata = delivery_time)),colour='blue') +
  ggtitle('sorting time vs delivery time') + 
  xlab('sorting time') + 
  ylab('delivery time')

reg_poly_2<-lm(Delivery.Time~Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3) + I(Sorting.Time^4)+ I(Sorting.Time^5)  )
summary(reg_poly_2)

ggplot() + 
  geom_point(aes(x= delivery_time$Sorting.Time, y= delivery_time$Delivery.Time),colour='red') + 
  geom_line(aes(x= delivery_time$Sorting.Time, y= predict(reg_poly_2,newdata = delivery_time)),colour='blue') +
  ggtitle('sorting time vs delivery time') + 
  xlab('sorting time') + 
  ylab('delivery time')

reg_poly_3<-lm(Delivery.Time~Sorting.Time + I(Sorting.Time^2) + I(Sorting.Time^3) + I(Sorting.Time^4)+ I(Sorting.Time^5)+ I(Sorting.Time^6)  )
summary(reg_poly_3)

ggplot() + 
  geom_point(aes(x= delivery_time$Sorting.Time, y= delivery_time$Delivery.Time),colour='red') + 
  geom_line(aes(x= delivery_time$Sorting.Time, y= predict(reg_poly_3,newdata = delivery_time)),colour='blue') +
  ggtitle('sorting time vs delivery time') + 
  xlab('sorting time') + 
  ylab('delivery time')




reg_reciprocal_quad<-lm(1/Delivery.Time~I(Sorting.Time)+ I(Sorting.Time^2))
summary(reg_reciprocal_quad)

ggplot() + 
  geom_point(aes(x= delivery_time$Sorting.Time, y= delivery_time$Delivery.Time),colour='red') + 
  geom_line(aes(x= delivery_time$Sorting.Time, y= predict(reg_reciprocal_quad,newdata = delivery_time)),colour='blue') +
  ggtitle('sorting time vs delivery time') + 
  xlab('sorting time') + 
  ylab('delivery time')

pred<-predict(reg_reciprocal_quad,interval = 'predict')
pred<-data.frame(pred)
cor(pred$fit,delivery_time$Delivery.Time)
