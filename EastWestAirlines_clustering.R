library(readxl)

airlines<-read_excel(file.choose(),2)
View(airlines)

dim(unique(airlines))

# Converting cc1_miles, cc2_miles and cc3_miles to numeric

airlines$cc1_miles<-ifelse(airlines$cc1_miles==1,2500,
                           ifelse(airlines$cc1_miles==2,7500,
                                  ifelse(airlines$cc1_miles==3,17500,
                                         ifelse(airlines$cc1_miles==4,37500,
                                                ifelse(airlines$cc1_miles==5,50000,0)))))

airlines$cc2_miles<-ifelse(airlines$cc2_miles==1,2500,
                           ifelse(airlines$cc2_miles==2,7500,
                                  ifelse(airlines$cc2_miles==3,17500,
                                         ifelse(airlines$cc2_miles==4,37500,
                                                ifelse(airlines$cc2_miles==5,50000,0)))))

airlines$cc3_miles<-ifelse(airlines$cc3_miles==1,2500,
                           ifelse(airlines$cc3_miles==2,7500,
                                  ifelse(airlines$cc3_miles==3,17500,
                                         ifelse(airlines$cc3_miles==4,37500,
                                                ifelse(airlines$cc3_miles==5,50000,0)))))

airlines<-airlines[,-1]
attach(airlines)
summary(airlines)
str(airlines)
airlines$`Award?`<-as.factor(airlines$`Award?`)

# Measures of central tendency

mean(Balance)
median(Balance)

# Second M.B.D.

var(Balance)
sd(Balance)
range(Balance)
getrange<-function(x){max(x)-min(x)}
getrange(Balance)

# Third and Fourth M.B.D
library(moments)

skewness(Balance)
kurtosis(Balance)

# EDA 
hist(Balance)
boxplot(Balance)
boxplot(Balance,plot = F)$out
unique(boxplot(Balance,plot = F)$out)

hist(Balance^(1/4))
boxplot(Balance^(1/4))
airlines$Balance<-airlines$Balance^(1/4)
names(airlines)[1]<-"balance^1/4"

# Measures of central tendency

mean(Qual_miles)
median(Qual_miles)

# Second M.B.D.

var(Qual_miles)
sd(Qual_miles)
range(Qual_miles)
getrange<-function(x){max(x)-min(x)}
getrange(Qual_miles)

# Third and Fourth M.B.D

skewness(Qual_miles)
kurtosis(Qual_miles)

# EDA

hist(Qual_miles)
boxplot(Qual_miles)
unique(boxplot(Qual_miles,plot = F)$out)

# Measures of central tendency

mean(cc1_miles)
median(cc1_miles)

# Second M.B.D.

var(cc1_miles)
sd(cc1_miles)
range(cc1_miles)
getrange<-function(x){max(x)-min(x)}
getrange(cc1_miles)

# Third and Fourth M.B.D

skewness(cc1_miles)
kurtosis(cc1_miles)

# EDA
hist(cc3_miles^(1/4))
boxplot(cc1_miles)
unique(boxplot(cc1_miles,plot = F)$out)


# Measures of central tendency

mean(cc2_miles)
median(cc2_miles)

# Second M.B.D.

var(cc2_miles)
sd(cc2_miles)
range(cc2_miles)
getrange<-function(x){max(x)-min(x)}
getrange(cc2_miles)

# Third and Fourth M.B.D

skewness(cc2_miles)
kurtosis(cc2_miles)

# EDA
hist(cc2_miles)
boxplot(cc2_miles)
unique(boxplot(cc2_miles,plot = F)$out)

# Measures of central tendency

mean(cc3_miles)
median(cc3_miles)

# Second M.B.D.

var(cc3_miles)
sd(cc3_miles)
range(cc3_miles)
getrange<-function(x){max(x)-min(x)}
getrange(cc3_miles)

# Third and Fourth M.B.D

skewness(cc3_miles)
kurtosis(cc3_miles)

# EDA
hist(cc3_miles)
boxplot(cc3_miles)
unique(boxplot(cc3_miles,plot = F)$out)

# Measures of central tendency

mean(Bonus_miles)
median(Bonus_miles)

# Second M.B.D.

var(Bonus_miles)
sd(Bonus_miles)
range(Bonus_miles)
getrange<-function(x){max(x)-min(x)}
getrange(Bonus_miles)

# Third and Fourth M.B.D

skewness(Bonus_miles)
kurtosis(Bonus_miles)

# EDA
hist(Bonus_miles)
boxplot(Bonus_miles)
unique(boxplot(Bonus_miles,plot = F)$out)

# Measures of central tendency

mean(Bonus_trans)
median(Bonus_trans)

# Second M.B.D.

var(Bonus_trans)
sd(Bonus_trans)
range(Bonus_trans)
getrange<-function(x){max(x)-min(x)}
getrange(Bonus_trans)

# Third and Fourth M.B.D

skewness(Bonus_trans)
kurtosis(Bonus_trans)

# EDA
hist(Bonus_trans)
boxplot(Bonus_trans)
unique(boxplot(Bonus_trans,plot = F)$out)

# Measures of central tendency

mean(Flight_miles_12mo)
median(Flight_miles_12mo)

# Second M.B.D.

var(Flight_miles_12mo)
sd(Flight_miles_12mo)
range(Flight_miles_12mo)
getrange<-function(x){max(x)-min(x)}
getrange(Flight_miles_12mo)

# Third and Fourth M.B.D

skewness(Flight_miles_12mo)
kurtosis(Flight_miles_12mo)

# EDA
hist(Flight_miles_12mo)
boxplot(Flight_miles_12mo)
unique(boxplot(Flight_miles_12mo,plot = F)$out)

# Measures of central tendency

mean(Flight_trans_12)
median(Flight_trans_12)

# Second M.B.D.

var(Flight_trans_12)
sd(Flight_trans_12)
range(Flight_trans_12)
getrange<-function(x){max(x)-min(x)}
getrange(Flight_trans_12)

# Third and Fourth M.B.D

skewness(Flight_trans_12)
kurtosis(Flight_trans_12)

# EDA
hist(Flight_trans_12)
boxplot(Flight_trans_12)
unique(boxplot(Flight_trans_12,plot = F)$out)

# Measures of central tendency

mean(Days_since_enroll)
median(Days_since_enroll)

# Second M.B.D.

var(Days_since_enroll)
sd(Days_since_enroll)
range(Days_since_enroll)
getrange<-function(x){max(x)-min(x)}
getrange(Days_since_enroll)

# Third and Fourth M.B.D

skewness(Days_since_enroll)
kurtosis(Days_since_enroll)

# EDA
hist(Days_since_enroll)
boxplot(Days_since_enroll)
unique(boxplot(Days_since_enroll,plot = F)$out)

# Measures of central tendency of categorical variables

getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(`Award?`)
award_prop<-sort(round(prop.table(table(`Award?`))*100,2),decreasing = T)
View(award_prop)
barplot(award_prop)

# Normalizing data to bring variables to uniform scale

normalized_airlines<-scale(airlines[,1:10])
View(normalized_airlines)

# Performing Hierarchical Clustering

d<-dist(normalized_airlines,method = "euclidean")
fit<-hclust(d,method = "complete")
plot(fit)
plot(fit,hang=-1)

groups<-cutree(fit,k=3)
groups
rect.hclust(fit,k=3,border = "red")

# Performing K-means clustering 

wcss<-vector()
for(i in 1:10) wcss[i]<-sum(kmeans(normalized_airlines,i)$withinss)

# Scree Plot

plot(1:10,wcss,type = 'b',main = paste('scree plot'),xlab = "No. of clusters",ylab = "WCSS")

# No. of recommended clusters from scree plot is 6

km<-kmeans(normalized_airlines,6)
str(km)

#Aggregating the results of the clusters

aggregate(airlines[,-11],by=list(km$cluster),FUN = mean)

# Customers in cluster 6 are the frequent flyers whereas the customers in cluster 3 are non frequent flyers. Thus more offers to be provided to customers in cluster 6.
