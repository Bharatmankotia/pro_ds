computer<-read.csv(file.choose())
View(computer)

# checking for duplicate rows

dim(computer)
dim(unique(computer))

# Deleting First column

computer<-computer[-1]
View(computer)
str(computer)
summary(computer)

# Measures of central tendency

mean(computer$price)
median(computer$price)

# Measures of dispersion

range(computer$price)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(computer$price)
var(computer$price)
sd(computer$price)

# Third and Fourth moment business decision

library(moments)

skewness(computer$price)
kurtosis(computer$price)
# plotting price

hist(computer$price)
boxplot(computer$price)
price_log<-log(computer$price)
hist(price_log)

# Transforming price to log_price

computer$price<-price_log

# changing the column name of price to price_log

names(computer)[1]<-"price_log"


# Measures of central tendency

mean(computer$speed)
median(computer$speed)

# Measures of dispersion

range(computer$speed)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(computer$speed)
var(computer$speed)
sd(computer$speed)

# Third and Fourth moment business decision

skewness(computer$speed)
kurtosis(computer$speed)
# plotting speed

hist(computer$speed)
boxplot(computer$speed)

# Measures of central tendency

mean(computer$hd)
median(computer$hd)

# Measures of dispersion

range(computer$hd)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(computer$hd)
var(computer$hd)
sd(computer$hd)

# Third and Fourth moment business decision

skewness(computer$hd)
kurtosis(computer$hd)

# Plot of hd

hist(computer$hd)
boxplot(computer$hd)
qqnorm(computer$hd)
qqline(computer$hd)
hist(computer$hd^(1/5))
boxplot(computer$hd^(1/5))
qqnorm(computer$hd^(1/5))
qqline(computer$hd^(1/5))

computer$hd<-hd^(1/5)
names(computer)[3]<-"hd(^1/5)"
# Measures of central tendency

mean(computer$ram)
median(computer$ram)

# Measures of dispersion

range(computer$ram)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(computer$ram)
var(computer$ram)
sd(computer$ram)

# Third and Fourth moment business decision

skewness(computer$ram)
kurtosis(computer$ram)


# Plot of ram

hist(computer$ram)
boxplot(computer$ram)

boxplot(computer$ram,plot = F)$out
length(unique(boxplot(computer$ram,plot = F)$ou))
box<-boxplot(computer$ram)
box$out
which(computer$ram%in%box$out) # position of outliers


# Measures of central tendency

mean(computer$screen)
median(computer$screen)

# Measures of dispersion

range(computer$screen)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(computer$screen)
var(computer$screen)
sd(computer$screen)

# Third and Fourth moment business decision

skewness(computer$screen)
kurtosis(computer$screen)

# plot for screen
hist(computer$screen)
boxplot(computer$screen)

# Measures of central tendency

mean(computer$ads)
median(computer$ads)

# Measures of dispersion

range(computer$ads)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(computer$ads)
var(computer$ads)
sd(computer$ads)

# Third and Fourth moment business decision

skewness(computer$ads)
kurtosis(computer$ads)

# plot for screen
hist(computer$ads)
boxplot(computer$ads)
ads_transform<-exp(computer$ads^(1/3))
hist(ads_transform)
boxplot(ads_transform)
computer$ads<-ads_transform
names(computer)[9]<-"exp(ads^(1/3))"
# Measures of central tendency

mean(computer$trend)
median(computer$trend)

# Measures of dispersion

range(computer$trend)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(computer$trend)
var(computer$trend)
sd(computer$trend)

# Third and Fourth moment business decision

skewness(computer$trend)
kurtosis(computer$trend)

# plot for trend
hist(computer$trend)
boxplot(computer$trend)


# Measures of central tendency 

getmode<- function(x){
  uniq<-unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}
getmode(computer$cd)

cd_prop<-sort(round(prop.table(table(computer$cd))*100,2),decreasing = T)
View(cd_prop)

# barplot for cd

barplot(cd_prop)

# Measures of central tendency
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(computer$multi)
multi_prop<-sort(round(prop.table(table(computer$multi))*100,2),decreasing = T)
View(multi_prop)

# barplot for multi
barplot(multi_prop)

# Measures of central tendency
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(computer$premium)
premium_prop<-sort(round(prop.table(table(computer$premium))*100,2),decreasing = T)
View(premium_prop)

# barplot for multi
barplot(premium_prop)


# Bivariate analysis

plot(computer)
cor(computer[,c(1:5,9:10)])


attach(computer)
model<-lm(price~.,data = computer)
summary(model)

plot(model_out)

library(car)
influence.measures(model)
influenceIndexPlot(model)
influencePlot(model)

avPlots(model)
library(MASS)
stepAIC(model)

# Building model on transformed variables
model_new<-lm(computer$price_log~.,data = computer)
summary(model_new)

plot(model_new)
library(car)
influenceIndexPlot(model_new)
influencePlot(model_new)
avPlots(model_new)

y_pred<-predict(model_new,interval = 'predict')
y_pred<-data.frame(y_pred)

cor(y_pred$fit,computer$price_log)

