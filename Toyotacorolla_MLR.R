corolla<-read.csv(file.choose())
View(corolla)
toyota_corolla<-corolla[,c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(toyota_corolla)

# checking for duplicate rows
dim(unique(toyota_corolla)) # 1 duplicated row
dim(toyota_corolla[duplicated(toyota_corolla),])

# Removing duplicate row

toyota_corolla<-unique(toyota_corolla)
nrow(toyota_corolla)

str(toyota_corolla)

# univariate analysis

attach(toyota_corolla)

# Measures of central tendency
mean(Price)
median(Price)

# Measures of dispersion
range(Price)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(Price)
var(Price)
sd(Price)

# Third and fourth moment business decision
library(moments)
skewness(Price)
kurtosis(Price)

# Plot 
hist(Price)
boxplot(Price)

hist(Price^(1/4))
boxplot(Price^(1/4))
boxplot(Price,plot=F)$out

# checking for outliers
boxplot(Price,plot=F)$out
length(boxplot(Price,plot = F)$out)
length(boxplot(Price^(1/4),plot = F)$out)
length(unique(boxplot(Price,plot = F)$out))
length(unique(boxplot(Price^(1/4),plot = F)$out))

# replacing values of Price with Price^1/4

toyota_corolla$Price<-toyota_corolla$Price^(1/4)
names(toyota_corolla)[1]<-"Price^(1/4)"
# Measures of central tendency
mean(Age_08_04)
median(Age_08_04)

# Measures of dispersion
range(Age_08_04)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(Age_08_04)
var(Age_08_04)
sd(Age_08_04)

# Third and fourth moment business decision

skewness(Age_08_04)
kurtosis(Age_08_04)

# Plot 
hist(Age_08_04)
boxplot(Age_08_04)

# Measures of central tendency
mean(KM)
median(KM)

# Measures of dispersion
range(KM)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(KM)
var(KM)
sd(KM)

# Third and fourth moment business decision

skewness(KM)
kurtosis(KM)

# Plot
hist(KM)
boxplot(KM)
hist(KM^(1/2))
boxplot(KM^(1/2))

toyota_corolla$KM<-KM^(1/2)
names(toyota_corolla)[3]<-"KM^(1/2)"

# Measures of central tendency
mean(HP)
median(HP)

# Measures of dispersion
range(HP)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(HP)
var(HP)
sd(HP)

# Third and fourth moment business decision

skewness(HP)
kurtosis(HP)

# Plot
hist(HP)
boxplot(HP)
boxplot(HP,plot = F)$out
length(boxplot(HP,plot = F)$out)
length(unique(boxplot(HP,plot = F)$out))
box<-boxplot(HP)
which(HP%in%box$out)

# # Measures of central tendency
mean(cc)
median(cc)

# Measures of dispersion
range(cc)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(cc)
var(cc)
sd(cc)

# Third and fourth moment business decision

skewness(cc)
kurtosis(cc)

# Plot
hist(cc)
box_cc<-boxplot(cc)
boxplot(cc,plot = F)$out

unique(boxplot(cc,plot = F)$out)
box_cc$out
which(cc%in% box_cc$out) # an outlier with 16000 value found

# replacing the outlier value by 95th percentile value

quantile(toyota_corolla$cc,.95)
toyota_corolla$cc<-replace(toyota_corolla$cc,toyota_corolla$cc>=16000,2000)
summary(toyota_corolla)
boxplot(toyota_corolla$cc)

# Measures of central tendency
mean(Doors)
median(Doors)

# Measures of Dispersion
range(Doors)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(Doors)
var(Doors)
sd(Doors)

# Third and fourth moment business decision

skewness(Doors)
kurtosis(Doors)

#Plot
hist(Doors)
boxplot(Doors)

# Measures of central tendency
mean(Gears)
median(Gears)

# Measures of Dispersion
range(Gears)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(Gears)
var(Gears)
sd(Gears)

# Third and fourth moment business decision

skewness(Gears)
kurtosis(Gears)

# Plot
hist(Gears)
boxplot(Gears)
boxplot(Gears,plot = F)$out

# Measures of central tendency
mean(Quarterly_Tax)
median(Quarterly_Tax)

# Measures of Dispersion
range(Quarterly_Tax)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(Quarterly_Tax)
var(Quarterly_Tax)
sd(Quarterly_Tax)

# Plot
hist(Quarterly_Tax)
boxplot(Quarterly_Tax)
boxplot(Quarterly_Tax,plot = F)$out
box<-boxplot(Quarterly_Tax)
box$out
which(Quarterly_Tax%in%box$out)
unique(boxplot(Quarterly_Tax,plot = F)$out)


# Measures of central tendency
mean(Weight)
median(Weight)

# Measures of Dispersion
range(Weight)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(Weight)
var(Weight)
sd(Weight)

# Third and fourth moment business decision

skewness(Weight)
kurtosis(Weight)

# Plot
hist(Weight)
boxplot(Weight)
boxplot(Weight,plot=F)$out
unique(boxplot(Weight,plot=F)$out)


exp_weight<-1/exp(Weight^(1/2))
hist(exp_weight)
boxplot(exp_weight) # somewhat normal distribution

# replacing values of weight with exp(weight^(1/2))
toyota_corolla$Weight<-exp_weight
names(toyota_corolla)[9]<-"exp(weight^(1/2))"

plot(toyota_corolla)

cor(toyota_corolla)

model<-lm(toyota_corolla$`Price^(1/4)`~.,data = toyota_corolla)
summary(model)

model_cc<-lm(toyota_corolla$`Price^(1/4)`~toyota_corolla$cc,data = toyota_corolla)
summary(model_cc)
library(corpcor)
cor2pcor(cor(toyota_corolla))

library(car)
plot(model)

avPlots(model) # Doors is opposed the most
influenceIndexPlot(model) # 192 , 602 seems to be influential
influencePlot(model)# 192 and 602 are most influential

vif(model)
vif_age<-lm(toyota_corolla$Age_08_04~toyota_corolla$`KM^(1/2)`+toyota_corolla$HP + toyota_corolla$cc + toyota_corolla$Doors +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_km<-lm(toyota_corolla$`KM^(1/2)`~toyota_corolla$Age_08_04+toyota_corolla$HP + toyota_corolla$cc + toyota_corolla$Doors +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_hp<-lm(toyota_corolla$HP~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$cc + toyota_corolla$Doors +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_cc<-lm(toyota_corolla$cc~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP + toyota_corolla$Doors +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_door<-lm(toyota_corolla$Doors~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP + toyota_corolla$cc +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_gear<-lm(toyota_corolla$Gears~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP + toyota_corolla$cc +toyota_corolla$Doors+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_tax<-lm(toyota_corolla$Quarterly_Tax~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP + toyota_corolla$cc +toyota_corolla$Doors+toyota_corolla$Gears+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_weight<-lm(toyota_corolla$`exp(weight^(1/2))`~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP + toyota_corolla$Doors +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$cc,data = toyota_corolla)
summary(vif_age)
summary(vif_km)
summary(vif_hp)
summary(vif_cc)
summary(vif_door)
summary(vif_gear)
summary(vif_tax)
summary(vif_weight)

# R^2 value for vif of weight and cc is high almost 0.73
library(MASS)
stepAIC(model)

# Removing doors from the model

model_doorless<-lm(toyota_corolla$`Price^(1/4)`~.,data = toyota_corolla[,-6])
summary(model_doorless)
plot(model_doorless)

avPlots(model_doorless)

vif_age_new<-lm(toyota_corolla$Age_08_04~toyota_corolla$`KM^(1/2)`+toyota_corolla$HP + toyota_corolla$cc  +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_km_new<-lm(toyota_corolla$`KM^(1/2)`~toyota_corolla$Age_08_04+toyota_corolla$HP + toyota_corolla$cc + toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_hp_new<-lm(toyota_corolla$HP~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$cc + toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_cc_new<-lm(toyota_corolla$cc~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP + toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_gear_new<-lm(toyota_corolla$Gears~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP + toyota_corolla$cc+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_tax_new<-lm(toyota_corolla$Quarterly_Tax~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP + toyota_corolla$cc+toyota_corolla$Gears+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_weight_new<-lm(toyota_corolla$`exp(weight^(1/2))`~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$cc,data = toyota_corolla)
summary(vif_age_new)
summary(vif_km_new)
summary(vif_hp_new)
summary(vif_cc_new)
summary(vif_gear_new)
summary(vif_tax_new)
summary(vif_weight_new)


# Removing cc from the equation as it has low correlation and high collinearity

model_ccless<-lm(toyota_corolla$`Price^(1/4)`~.,data=toyota_corolla[,-c(5,6)])
summary(model_ccless)
avPlots(model_ccless)
plot(model_ccless)

vif_age_new1<-lm(toyota_corolla$Age_08_04~toyota_corolla$`KM^(1/2)`+toyota_corolla$HP +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_km_new1<-lm(toyota_corolla$`KM^(1/2)`~toyota_corolla$Age_08_04+toyota_corolla$HP  + toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_hp_new1<-lm(toyota_corolla$HP~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$Gears+toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_gear_new1<-lm(toyota_corolla$Gears~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP +toyota_corolla$Quarterly_Tax+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_tax_new1<-lm(toyota_corolla$Quarterly_Tax~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP +toyota_corolla$Gears+toyota_corolla$`exp(weight^(1/2))`,data = toyota_corolla)
vif_weight_new1<-lm(toyota_corolla$`exp(weight^(1/2))`~toyota_corolla$Age_08_04+toyota_corolla$`KM^(1/2)` + toyota_corolla$HP +toyota_corolla$Gears+toyota_corolla$Quarterly_Tax,data = toyota_corolla)
summary(vif_age_new1)
summary(vif_km_new1)
summary(vif_hp_new1)
summary(vif_gear_new1)
summary(vif_tax_new1)
summary(vif_weight_new1)

y_pred<-predict(model_ccless,interval = 'predict')
y_pred<-data.frame(y_pred)
cor(y_pred$fit,toyota_corolla$`Price^(1/4)`) # accuracy 

