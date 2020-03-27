startups<-read.csv(file.choose())
View(startups)
boxplot(startups$Profit)
boxplot(startups$R.D.Spend)
boxplot(startups$Administration)
boxplot(startups$Marketing.Spend)

summary(startups)

startups$State<-factor(startups$State,levels = c('New York','California','Florida'),labels = c(1,2,3))

plot(startups)

names(startups)
attach(startups)
cor(R.D.Spend,Administration)
cor(R.D.Spend,Marketing.Spend)
cor(R.D.Spend,Profit)

cor(Administration,Marketing.Spend)
cor(Administration,Profit)

cor(Marketing.Spend,Profit)


# Performing Lasso Regression
install.packages("lasso2")
library(lasso2)

lm_lasso<-l1ce(Profit~.,data = startups)
summary(lm_lasso)$coefficients #lasso regression shows just R.D. spend as significant feature


# Further analysis of the features

#Models with single features
names(startups)
model_1<-lm(Profit~Marketing.Spend,data = startups)
summary(model_1)

library(ggplot2)
ggplot() +
  geom_point(aes(x= startups$Marketing.Spend, y=startups$Profit),colour='red') +
  geom_line(aes(x=startups$Marketing.Spend, y=predict(model_1,newdata = startups)),colour='black')

model_2<-lm(Profit~Administration,data = startups)
summary(model_2)
ggplot() +
  geom_point(aes(x= startups$Administration, y=startups$Profit),colour='red') +
  geom_line(aes(x=startups$Administration, y=predict(model_2,newdata = startups)),colour='black')

model_3<-lm(Profit~State,data = startups)
summary(model_3)
ggplot() +
  geom_point(aes(x= startups$State, y=startups$Profit),colour='red') +
  geom_line(aes(x=startups$State, y=predict(model_3,newdata = startups)),colour='black')

model_4<-lm(Profit~R.D.Spend,data = startups)
summary(model_4)
ggplot() +
  geom_point(aes(x= startups$R.D.Spend, y=startups$Profit),colour='red') +
  geom_line(aes(x=startups$R.D.Spend, y=predict(model_4,newdata = startups)),colour='black')

# Creating Models with insignificant features
model_insig_1<-lm(Profit~Marketing.Spend + Administration + State,data = startups)
summary(model_insig_1) # State is still insignificant

plot(model) # Record 50 seems to be an outlier Let's inspect further

influence.measures(model) # Record 7, 47, 50 are influential Let's inspect further

library(car)

influenceIndexPlot(model,id=3) # Record 47, 50 seem to be influential
influencePlot(model)   # Record 47,49,50 seem to be influential

#Let's remove record # 50 

model_outlier<-lm(Profit~.,data = startups[-50,])
summary(model_outlier) # still features are insignificant

# so Let's remove States as it is highly insignificant and build the model

model_stateless<-lm(Profit~ R.D.Spend + Marketing.Spend + Administration,data = startups[-50,])
summary(model_stateless) # still features are insignificant

# Determining which feature to be removed
vif(model_stateless)
vif_RD<-lm(R.D.Spend~Marketing.Spend + Administration,data = startups)
summary(vif_RD)
vif_marketing<-lm(Marketing.Spend~ R.D.Spend + Administration,data = startups)
summary(vif_marketing)
vif_administration<-lm(Administration~Marketing.Spend + R.D.Spend,data = startups)
summary(vif_administration)

avPlots(model_stateless,id.n=2,id.cex=0.8,col='red')

library(MASS)
stepAIC(model_stateless) # removing Administration

cor(startups[-50,-c(2,4)])

model_aic<-lm(Profit~ Marketing.Spend + R.D.Spend,data=startups[-50,])
summary(model_aic)

y_pred<-predict(model_aic,interval = 'predict')
y_pred<-data.frame(y_pred)
cor(y_pred$fit,startups[-50,]$Profit)

# Prediction of values in the model
y_new<-predict(model_aic,data.frame(R.D.Spend=165349,
                                    Marketing.Spend=471784))
y_new
