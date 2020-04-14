sms_raw<-read.csv(file.choose())

View(sms_raw)

str(sms_raw)

table(sms_raw$type)

library(tm)

# converting to vector source

sms_corpus<-Corpus(VectorSource(sms_raw$text))

# converting to universal text format

sms_corpus<-tm_map(sms_corpus,function(x) iconv(enc2utf8(x),sub = 'byte'))

# cleaning corpus 

corpus_clean<-tm_map(sms_corpus,tolower) 
corpus_clean<-tm_map(corpus_clean,removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)

# Creating DTM

sms_dtm<-DocumentTermMatrix(corpus_clean)

sms_dtm
inspect(sms_dtm)

# splitting into train and test

library(caTools)

set.seed(124)

split<-sample.split(sms_raw$type,SplitRatio = 0.8)

sms_raw_train<-subset(sms_raw,split==TRUE)

sms_raw_test<-subset(sms_raw,split==FALSE)

sms_dtm_train<-sms_dtm[1:4448,]

sms_dtm_test<-sms_dtm[4449:5559,]

sms_corpus_train<-corpus_clean[1:4448]

sms_corpus_test<-corpus_clean[4449:5559]

prop.table(table(sms_raw_train$type))

prop.table(table(sms_raw_test$type))

prop.table(table(sms_raw$type))

# Dictionary of words used more than 5 times

sms_dict<-findFreqTerms(sms_dtm_train,5)
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))

# converting counts to factors. in the following function If a word is used more than 0 times then mention 1 else 0

convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c('No','Yes'))
}

sms_train<-apply(sms_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_test,MARGIN = 2,convert_counts)

library(e1071)

classifier<-naiveBayes(sms_train,sms_raw_train$type)
classifier

y_pred<-predict(classifier,sms_test)

prop.table(table(y_pred)) # 0.95 accuracy


