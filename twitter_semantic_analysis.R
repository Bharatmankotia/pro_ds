library(twitteR)
install.packages('ROAuth')
library(ROAuth)


install.packages("base64enc")
library(base64enc)

install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret


Tweets <- userTimeline('TheRock', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
setwd("D://Data science")
write.csv(TweetsDF, "Tweets.csv",row.names = F)

# Performing semantic analysis on tweets extracted

library(quanteda)
library(Matrix)
rm(list = ls())
library(rJava)
library(tm)	
library(SnowballC)
library(wordcloud)
library(RWeka)	
library(qdap)		
library(textir)
library(maptpx)
library(data.table)
library(stringr)
library(slam)
library(ggplot2)

makewordc<-function (x){
  freq<-sort(rowSums(as.matrix(x)),decreasing = T)
  freq.df<-data.frame(word=names(freq),freq=freq)
  windows()
  wordcloud(freq.df$word[1:120],freq.df$freq[1:120],scale = c(4,.5),random.order = F,colors = 1:10)
}

makeposwordc<-function (x){
  freq<-sort(rowSums(as.matrix(x)),decreasing = T)
  pos.matches<-match(names(freq),c(pos.words,"approvals"))
  pos.matches<-!is.na(pos.matches)
  freq_pos<-freq[pos.matches]
  names<-names(frq_pos)
  windows()
  wordcloud(names[1:120],freq_pos[1:120],scale = c(4,.5),colors = brewer.pal(8,"Dark2"))
}
makenegwordc<-function (x){
  freq<-sort(rowSums(as.matrix(x)),decreasing = T)
  neg.matches<-match(names(freq),neg.words)
  neg.matches<-!is.na(neg.matches)
  freq_neg<-freq[neg.matches]
  names<-names(freq_neg)
  windows()
  wordcloud(names[1:120],freq_neg[1:120],scale = c(4,.5),colors = brewer.pal(8,"Dark2"))
}
words_bar_plot<-function (x){
  freq<-sort(rowSums(as.matrix(x)),decreasing = T)
  freq.df<-data.frame(word=names(freq),freq=freq)
  head(freq.df,20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50),aes(reorder(word,frq),freq)) + geom_bar(stat = "identity") + coord_flip() + 
    xlab("words") + ylab("frequency") + ggtile("most frequent words")
}
pos_words_bar_plot<-function (x){
  pos.matches<-match(colnames(x),pos.words)
  pos.matches<-!is.na(pos.matches)
  pos_words_freq<-as.data.frame(apply(x,2,sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"]<-rownames(pos_words_freq)
  
  pos_words_freq<-pos_words_freq[order(pos_words_freq$freq,decreasing = T),]
  windows()
  ggplot(head(pos_words_freq,30),aes(reorder(word,freq),freq)) + geom_bar(stat = "identity") + coord_flip() + 
    xlab("positive words") + ylab("frequency") + ggtitle("most frequent positive words")
  
}
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("frequency") +
    ggtitle("most frequent negative words")
}
clusdend = function(a){	 	
  mydata.df = as.data.frame(inspect(a));	
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  min1 = min(ncol(mydata.df), 40) 	
  test = matrix(0,min1,min1)
  test1 = test
  for(i1 in 1:(min1-1)){ 
    for(i2 in i1:min1){
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
  }
  
  test2 = test1
  rownames(test2) = colnames(mydata1.df)[1:min1]
  
  d <- dist(test2, method = "euclidean") 
  fit <- hclust(d, method="ward")
  windows()
  plot(fit) 
}
pos.words<-scan(file.choose(),what = "character",comment.char = ";")
neg.words<-scan(file.choose(),what = "character", comment.char = ";")
stopwrds<-readLines(file.choose())

x<-readLines(file.choose())
x1<-Corpus(VectorSource(x))
x1<-tm_map(x1,stripWhitespace)
x1<-tm_map(x1,tolower)
x1<-tm_map(x1,removePunctuation)
x1<-tm_map(x1,removeNumbers)
x1<-tm_map(x1,removeWords,c(stopwords("english"),stopwrds))

tdm0<-TermDocumentMatrix(x1)
tdm1<-TermDocumentMatrix(x1,control=list(weighting=function(p) weightTfIdf(p,normalize = T)))
inspect(tdm1)


a0<-NULL
a1<-NULL
for (i1 in 1:ncol(tdm0))
{ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)} }

tdm0<-tdm0[,-a0]
tdm1<-tdm1[,-a1]


dtm0<-t(tdm0)
dtm1<-t(tdm1)

makewordc(tdm0)
title(sub="unigram- wordcloud using TF")
words_bar_plot(tdm0)

makewordc(tdm1)
words_bar_plot(tdm1)

makeposwordc(tdm0)
makenegwordc(tdm0)

dtm0_2 <- dfm(unlist(x1),ngrams=3,verbose = F)
tdm0_2 <- t(dtm0_2)
a0 = NULL
for (i1 in 1:ncol(tdm0_2)){ if (sum(tdm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)	
if (length(a0) >0) { tdm0_2 = tdm0_2[, -a0]} else {tdm0_2 = tdm0_2};	dim(tdm0_2)	
a0 <- NULL;i1 <- NULL

dtm0_2 <- t(tdm0_2)
makewordc(tdm0_2) 

dtm1_2 <- tfidf(dtm0_2)
tdm1_2 <- t(dtm1_2)
a0 = NULL
for (i1 in 1:ncol(tdm1_2)){ if (sum(tdm1_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		
if (length(a0) >0) { tdm1_2 = tdm1_2[, -a0]} else {tdm1_2 = tdm1_2};	dim(tdm1_2)
a0 <- NULL;i1 <- NULL
dtm1_2 <- t(tdm1_2)

makewordc(tdm1_2) 

clusdend(dtm0)
title(sub = "Dendrogram using TF")

clusdend(dtm1)
title(sub = "Dendrogram using TFIDF")

wss <- (nrow(dtm0)-1)*sum(apply(dtm0, 2, var))	 
for (i in 2:8) wss[i] = sum(kmeans(dtm0, centers=i)$withinss)
windows()
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  
title(sub = "K-Means Clustering Scree-Plot")

# scree plot suggests 4  clusters

k1 <- 4	

a3 <-kmeans(dtm0, k1);	a3$size

round(a3$size/sum(a3$size), 2)

a4<-NULL
for (i1 in 1:max(a3$cluster)) { 
  a4[[i1]] = t(dtm0[(a3$cluster == i1),])
}

par(ask = TRUE)
for (i2 in 1:max(a3$cluster)){	
  makewordc(a4[[i2]])	
  sub=paste("wordcloud-Clustering-",as.character(i2),"-",as.character(format(round(ncol(a4[[i2]])*100/nrow(dtm0),2),nsmall=3)),"%",collapse = " ")
  title(sub = sub)
  
}