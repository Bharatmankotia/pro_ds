wine<-read.csv(file.choose())
wine_1<-wine[-1]
View(wine_1)

dim(unique(wine_1)) # Check for duplicate rows

summary(wine_1)

# Feature scaling

wine_norm<-scale(wine_1)
View(wine_norm)

# Performing K means clustering on the wine_norm datset

wcss<-vector()
for (i in 1:10)wcss[i]<-sum(kmeans(wine_norm,i)$withinss)

# Scree plot

plot(1:10,wcss,type='b',main=paste('cluster plot with all variables'),xlab='No. of clusters',ylab='wcss')

# No. of recommended clusters is 3

km<-kmeans(wine_norm,3)
str(km)
km$cluster

# Performing Hierarchical clustering on the wine_norm dataset

d<-dist(wine_norm,method = 'euclidean')
fit<-hclust(d,method = 'complete')

# Dendrogram

plot(fit,hang=-1)
groups<-cutree(fit,k=3)
groups
rect.hclust(fit,k=3,border = 'red') # optimum number of clusters is 3

# Data pre processing 
# PCA

pcaobj<-princomp(wine_norm,cor = TRUE,scores = TRUE,covmat = NULL)
summary(pcaobj)
str(pcaobj)

loadings(pcaobj)

# Visualizing the components

plot(pcaobj)
biplot(pcaobj)

pcaobj$scores[,1:3]

wine_norm_pca<-cbind(wine_norm,pcaobj$scores[,1:3])


# Performing K means clustering on the wine_norm dataset

wcss_pca<-vector()
for (i in 1:10)wcss_pca[i]<-sum(kmeans(wine_norm_pca[,14:16],i)$withinss)

# Scree plot

plot(1:10,wcss_pca,type='b',main=paste('cluster plot with pca'),xlab='No. of clusters',ylab='wcss_pca')

# No. of recommended clusters is 3

km_pca<-kmeans(wine_norm_pca[,14:16],3)
str(km_pca)
km_pca$cluster

# Performing Hierarchical clustering on the wine_norm dataset

d_pca<-dist(wine_norm_pca[,14:16],method = 'euclidean')
fit_pca<-hclust(d_pca,method = 'complete')

# Dendrogram

plot(fit_pca,hang=-1)
groups_pca<-cutree(fit_pca,k=3)
groups_pca

rect.hclust(fit_pca,k=3,border = 'red')

# Using PCA also optimum number of clusters is 3