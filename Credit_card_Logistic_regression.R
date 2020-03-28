credit_card<-read.csv(file.choose())
View(credit_card)
str(credit_card)
credit_card<-credit_card[-1]
summary(credit_card)

dim(unique(credit_card)) # checking for duplicate rows

attach(credit_card)

# Measures of central tendency
mean(reports)
median(reports)

# Measures of dispersion

var(reports)
sd(reports)
range(reports)
rangevalue<-function(x){max(x)-min(x)}
rangevalue(reports)

# Third and fourth M.B.D
library(moments)

skewness(reports)
kurtosis(reports)

# EDA
hist(reports)
boxplot(reports)
boxplot(reports,plot = F)$out
unique(boxplot(reports,plot = F)$out)

# Measures of central tendency
mean(age)
median(age)

# Measures of dispersion

var(age)
sd(age)
range(age)
rangevalue(age)

# Third and fourth M.B.D
skewness(age)
kurtosis(age)

# EDA
hist(age)
boxplot(age)
boxplot(age,plot = F)$out
unique(boxplot(age,plot = F)$out)
length(unique(boxplot(age,plot = F)$out))
hist(age^(1/2))
boxplot(age^(1/2))

credit_card$age<-credit_card$age^1/2
summary(credit_card)
names(credit_card)[3]<-"age^1/2"

# Measures of central tendency
mean(income)
median(income)

# Measures of dispersion

var(income)
sd(income)
range(income)
rangevalue(income)

# Third and fourth M.B.D
skewness(income)
kurtosis(income)

# EDA

hist(income)
boxplot(income)
boxplot(income,plot = F)$out
unique(boxplot(income,plot = F)$out)
length(unique(boxplot(income,plot = F)$out))

hist(income^(1/2))
boxplot(income^(1/2))

credit_card$income<-credit_card$income^(1/2)
summary(credit_card)

names(credit_card)[4]<-"income^1/2"

# Measures of central tendency
mean(share)
median(share)

# Measures of dispersion

var(share)
sd(share)
range(share)
rangevalue(share)

# Third and fourth M.B.D
skewness(share)
kurtosis(share)

# EDA
hist(share)
boxplot(share)
boxplot(share,plot = F)$out
unique(boxplot(share,plot = F)$out)
length(unique(boxplot(share,plot = F)$out))

# Measures of central tendency
mean(expenditure)
median(expenditure)

# Measures of dispersion

var(expenditure)
sd(expenditure)
range(expenditure)
rangevalue(expenditure)

# Third and fourth M.B.D
skewness(expenditure)
kurtosis(expenditure)

# EDA
hist(expenditure)
boxplot(expenditure)
boxplot(expenditure,plot = F)$out
unique(boxplot(expenditure,plot = F)$out)
length(unique(boxplot(expenditure,plot = F)$out))

hist(expenditure^(3))
recip_expenditure<-1/(expenditure^(1/5))
hist(recip_expenditure)

log_expenditure<-log(expenditure^3)
hist(log_expenditure)
boxplot(log_expenditure)

recip_log<-1/log(expenditure^(1/5))
hist(recip_log)

exp_expenditure<-1/exp(expenditure^(1/3))
hist(exp_expenditure)

# Measures of central tendency
mean(dependents)
median(dependents)

# Measures of dispersion

var(dependents)
sd(dependents)
range(dependents)
rangevalue(dependents)

# Third and fourth M.B.D
skewness(dependents)
kurtosis(dependents)

# EDA
hist(dependents)
boxplot(dependents)
boxplot(dependents,plot = F)$out

# Measures of central tendency
mean(months)
median(months)

# Measures of dispersion

var(months)
sd(months)
range(months)
rangevalue(months)

# Third and fourth M.B.D
skewness(months)
kurtosis(months)

# EDA

hist(months)
boxplot(months)
boxplot(months,plot = F)$out
unique(boxplot(months,plot = F)$out)

hist(months^(1/4))
boxplot(months^(1/4))

credit_card$months<-credit_card$months^(1/4)
summary(credit_card)

names(credit_card)[10]<-"months^1/4"

# Measures of central tendency
mean(majorcards)
median(majorcards)

# Measures of dispersion

var(majorcards)
sd(majorcards)
range(majorcards)
rangevalue(majorcards)

# Third and fourth M.B.D
skewness(majorcards)
kurtosis(majorcards)

# EDA 
hist(majorcards)
boxplot(majorcards)
boxplot(majorcards,plot = F)$out
unique(boxplot(majorcards,plot = F)$out)

# Measures of central tendency
mean(active)
median(active)

# Measures of dispersion

var(active)
sd(active)
range(active)
rangevalue(active)

# Third and fourth M.B.D
skewness(active)
kurtosis(active)

# EDA
hist(active)
boxplot(active)
boxplot(active,plot = F)$out

# Measures of central tendency of categorical variables

getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(card)
card_prop<-sort(round(prop.table(table(card))*100,2),decreasing = T)
View(card_prop)
barplot_card<-barplot(card_prop)

getmode(owner)
owner_prop<-sort(round(prop.table(table(owner))*100,2),decreasing = T)
View(owner_prop)
barplot_owner<-barplot(owner_prop)

getmode(selfemp)
selfemp_prop<-sort(round(prop.table(table(selfemp))*100,2),decreasing = T)
View(selfemp_prop)
barplot_selfemp<-barplot(selfemp_prop)

model_1<-glm(card~.,data = credit_card,family = "binomial")
summary(model_1) # Residual deviance is too high than Null deviance

# Removing share from the model

model_2<-glm(card~.,data = credit_card[,-5],family = "binomial")
summary(model_2) 

# Removing share and expenditure from the model

model_3<-glm(card~.,data = credit_card[,-c(5,6)],family = "binomial")
summary(model_3)

# Removing share, month and expenditure from the model

model_4<-glm(card~.,data = credit_card[,-c(5,6,10)],family = "binomial")
summary(model_4)

# Removing share, month, age and expenditure from the model

model_5<-glm(card~.,data = credit_card[,-c(3,5,6,10)],family = "binomial")
summary(model_5) # All the variables are significant. Also residual deviance is less than Null deviance

prob<-predict(model_5,type=c("response"),credit_card)
prob

confusion<-table(prob>0.5,credit_card$card)
confusion

accuracy<-sum(diag(confusion))/sum(confusion)
accuracy # 0.85

library(ROCR)
rocr<-prediction(prob,credit_card$card)
rocrper<-performance(rocr,'tpr','fpr')
plot(rocrper,colorize=T,text.adj=c(-0.2,1.7))
