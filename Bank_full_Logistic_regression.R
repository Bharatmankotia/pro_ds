library(data.table)
bank.full<-fread(file.choose(),data.table=F)
str(bank.full)
bank.full[,c(2:5,7:11,16,17)]<-lapply(bank.full[,c(2:5,7:11,16,17)],as.factor)
str(bank.full)
View(bank.full)
bank<-bank.full
summary(bank) # check for NA's 
str(bank)
bank[,c(2:5,7:11,16,17)]<-lapply(bank[,c(2:5,7:11,16,17)],as.factor)

str(bank)

# checking for duplicate rows

dim(bank)
dim(unique(bank))

# Measures of central tendency
attach(bank)
mean(age)
median(age)

# Measures of dispersion
range(age)
rangevalue<-function(x){max(x-min(x))}
rangevalue(age)
var(age)
sd(age)

# Third and fourth moment business decision
library(moments)
skewness(age)
kurtosis(age)

# Plot
hist(age)
boxplot(age)
boxplot(age,plot=F)$out
unique(boxplot(age,plot = F)$out)
box<-boxplot(age)
box$out
which(age%in%box$out)

log_age<-log(age^3)
hist(log_age)
boxplot(log_age)
bank$age<-log_age 
names(bank)[1]<-"log(age^3)"


# Measures of central tendency

mean(balance)
median(balance)

# Measures of dispersion
range(balance)
rangevalue<-function(x){max(x-min(x))}
rangevalue(balance)
var(balance)
sd(balance)

# Third and fourth moment business decision

skewness(balance)
kurtosis(balance)

# Plot 
hist(balance)
length(unique(boxplot(balance,plot = F)$out))
box_bal<-boxplot(balance)
box_bal$out
unique(box_bal$out)

# Measures of central tendency

mean(duration)
median(duration)

# Measures of dispersion
range(duration)
rangevalue<-function(x){max(x-min(x))}
rangevalue(duration)
var(duration)
sd(duration)

# Third and fourth moment business decision

skewness(duration)
kurtosis(duration)

#Plot
hist(duration)
boxplot(duration)
unique(boxplot(duration,plot = F)$out)

hist(duration^(1/4))
boxplot(duration^(1/4))

bank$duration<-bank$duration^(1/4)
names(bank)[12]<-"duration^(1/4)"

# Measures of central tendency

mean(campaign)
median(campaign)

# Measures of dispersion
range(campaign)
rangevalue<-function(x){max(x-min(x))}
rangevalue(campaign)
var(campaign)
sd(campaign)

# Third and fourth moment business decision

skewness(campaign)
kurtosis(campaign)

# Plot
hist(campaign)
boxplot(campaign)
boxplot(campaign,plot=F)$out
unique(boxplot(campaign,plot=F)$out)
length(unique(boxplot(campaign,plot = F)$out))



# Measures of central tendency

mean(pdays)
median(pdays)

# Measures of dispersion
range(pdays)
rangevalue<-function(x){max(x-min(x))}
rangevalue(pdays)
var(pdays)
sd(pdays)

# Third and fourth moment business decision

skewness(pdays)
kurtosis(pdays)

# Plot
hist(pdays)
boxplot(pdays)
unique(boxplot(pdays,plot=F)$out)


# Measures of central tendency

mean(previous)
median(previous)

# Measures of dispersion
range(previous)
rangevalue<-function(x){max(x-min(x))}
rangevalue(previous)
var(previous)
sd(previous)

# Third and fourth moment business decision

skewness(previous)
kurtosis(previous)

# Plot
hist(previous)
boxplot(previous)
unique(boxplot(previous,plot=F)$out)

# Measures of central tendency
getmode<-function(x){
  uniq<-unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}
getmode(job)
job_prop<-sort(round(prop.table(table(job))*100,2),decreasing =T)
job_prop
barplot(job_prop)

# Measures of central tendency

getmode(marital)
marital_prop<-sort(round(prop.table(table(marital))*100,2),decreasing =T)
marital_prop
barplot(marital_prop)

# Measures of central tendency

getmode(education)
education_prop<-sort(round(prop.table(table(education))*100,2),decreasing =T)
education_prop
barplot(education_prop)

# Measures of central tendency

getmode(default)
default_prop<-sort(round(prop.table(table(default))*100,2),decreasing =T)
default_prop
barplot(default_prop)

# Measures of central tendency

getmode(housing)
housing_prop<-sort(round(prop.table(table(housing))*100,2),decreasing =T)
housing_prop
barplot(housing_prop)

# Measures of central tendency

getmode(loan)
loan_prop<-sort(round(prop.table(table(loan))*100,2),decreasing =T)
loan_prop
barplot(loan_prop)

# Measures of central tendency

getmode(contact)
contact_prop<-sort(round(prop.table(table(contact))*100,2),decreasing =T)
contact_prop
barplot(contact_prop)

# Measures of central tendency

getmode(day)
day_prop<-sort(round(prop.table(table(day))*100,2),decreasing =T)
day_prop
barplot(day_prop)


# Measures of central tendency

getmode(month)
month_prop<-sort(round(prop.table(table(month))*100,2),decreasing =T)
month_prop
barplot(month_prop)

# Measures of central tendency

getmode(poutcome)
poutcome_prop<-sort(round(prop.table(table(poutcome))*100,2),decreasing =T)
poutcome_prop
barplot(poutcome_prop)

# Measures of central tendency

getmode(y)
y_prop<-sort(round(prop.table(table(y))*100,2),decreasing =T)
y_prop
barplot(y_prop)

# feature scaling on both bank.full dataset and bank dataset
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

bank.full[,c(1,6,12,13,14,15)]<-lapply(bank.full[,c(1,6,12,13,14,15)],normalize)

bank_normalized<-as.data.frame(lapply(bank[,c(1,6,12,13,14,15)],normalize))
bank$`log(age^3)`<-bank_normalized$log.age.3.
bank$balance<-bank_normalized$balance
bank$`duration^(1/4)`<-bank_normalized$duration..1.4.
bank$campaign<-bank_normalized$campaign
bank$pdays<-bank_normalized$pdays
bank$previous<-bank_normalized$previous

# Preparing model on bank.full dataset

model_1<-glm(bank.full$y~.,data = bank.full,family = "binomial")
summary(model_1)

# Preparing model on bank dataset

model_2<-glm(bank$y~.,data = bank,family = "binomial")
summary(model_2) # Residual deviance and AIC value reduced

# Preparing model on bank dataset by removing default variable as it is insignificant

model_3<-glm(bank$y~.,data = bank[,-5],family = "binomial")
summary(model_3) # AIC value reduced slightly

# Preparing model on bank dataset by removing pdays variable as it is insignificant

model_4<-glm(bank$y~.,data = bank[,-14],family = "binomial")
summary(model_4) # AIC value reduced slightly

# Preparing model on bank dataset by removing pdays and default variable as both are insignificant

model_5<-glm(bank$y~.,data = bank[,-c(5,14)],family = "binomial")
summary(model_5) # AIC value reduced slightly

# Preparing model on bank dataset by removing pdays,previous and default variable as they are insignificant

model_6<-glm(bank$y~.,data = bank[,-c(5,14,15)],family = "binomial")
summary(model_6)

# Preparing model on bank dataset by removing age,pdays,previous and default variable as they are insignificant

model_7<-glm(bank$y~.,data = bank[,-c(1,5,14,15)],family = "binomial")
summary(model_7) # All the values are significant now

# Predicting probabilities

prob<-predict(model_7,type=c("response"),bank)
prob

# confusion matrix

confusion<-table(prob>0.5,bank$y)
confusion

pred_values<-NULL
yes_no<-NULL
for(i in 1:45211){
  pred_values[i]<-ifelse(prob[i]>=0.5,1,0)
  yes_no[i]<-ifelse(prob[i]>=0.5,"yes","no")
}
bank[,"probab"]<-prob
bank[,"pred_values"]<-pred_values
bank[,"yes_no_opt"]<-yes_no

# accuracy of the model

accuracy<-sum(diag(confusion))/sum(confusion)
accuracy  #0.90

# Plotting ROC 

library(ROCR)
rocr<-prediction(prob,bank$y)
rocrper<-performance(rocr,'tpr','fpr')
plot(rocrper,colorize=T,text.adj=c(-0.2,1.7))


