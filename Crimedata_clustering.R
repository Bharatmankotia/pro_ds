crime<-read.csv(file.choose())
View(crime)
summary(crime)

crime_new<-crime[-1]
View(crime_new)

# checking for duplicate rows
dim(unique(crime_new))
attach(crime_new)

# Measures of central Tendency
mean(Murder)
median(Murder)

# Second M.B.D.
var(Murder)
sd(Murder)

# Third and Fourth M.B.D

library(moments)

skewness(Murder)
kurtosis(Murder)

# Visualization
hist(Murder)
boxplot(Murder)

# Measures of central Tendency
mean(Assault)
median(Assault)

# Second M.B.D.
var(Assault)
sd(Assault)

# Third and Fourth M.B.D
skewness(Assault)
kurtosis(Assault)

# Visualization

hist(Assault)
boxplot(Assault)

# Measures of central Tendency
mean(UrbanPop)
median(UrbanPop)

# Second M.B.D.
var(UrbanPop)
sd(UrbanPop)

# Third and Fourth M.B.D
skewness(UrbanPop)
kurtosis(UrbanPop)

# Visualization

hist(UrbanPop)
boxplot(UrbanPop)

# Measures of central Tendency
mean(Rape)
median(Rape)

# Second M.B.D.
var(Rape)
sd(Rape)

# Third and Fourth M.B.D
skewness(Rape)
kurtosis(Rape)

# Visualization
hist(Rape)
boxplot(Rape)
boxplot(Rape,plot=F)$out
hist(Rape^(1/2))
boxplot(Rape^(1/2))
crime_new$Rape<-crime_new$Rape^(1/2)
names(crime_new)[4]<-"Rape^1/2"

# feature scaling

crime_new<-scale(crime_new)

# Prforming Hierarchical Clustering

d<-dist(crime_new,method = "euclidean")
fit<-hclust(d,method = "complete")

# Dendrogram
plot(fit,hang = -1)

groups<-cutree(fit,k=3)
groups
rect.hclust(fit,k=3,border = "red")

crime_cluster<-as.matrix(groups)
final<-data.frame(crime,crime_cluster)
View(final)

write.csv(final,file = "crime.csv")

# Performing K means clustering

wcss<-vector()
for(i in 1:10) wcss[i]<-sum(kmeans(crime_new,i)$withinss)
plot(1:10,wcss,type = 'b',main = paste('Scree plot'),xlab = 'No. of clusters',ylab = 'wcss')

# No. of clusters suggested by scree plot is 4
km<-kmeans(crime_new,4)
str(km)
aggregate(crime_new,by=list(km$cluster),FUN=mean)

# Conclusion - Urban population cities are more prone to crimes.
