library(arules)
library(arulesViz)

movies<-read.csv(file.choose())

movies_new<-movies[,6:15]
View(movies_new)

# applying apriori algorithm


association<-apriori(as.matrix(movies_new),parameter = list(support=0.2,confidence=0.8)) # 8 rules

# inspecting the 8 rules

inspect(sort(association,by='lift')[1:8])

association_1<-apriori(as.matrix(movies_new),parameter = list(support=0.2,confidence=0.7)) # 10 rules

# inspecting top 10 rules

inspect(sort(association_1,by='lift')[1:10])

association_2<-apriori(as.matrix(movies_new),parameter = list(support=0.2,confidence=0.6)) # 15 rules

# inspecting top 10 rules

inspect(sort(association_2,by='lift')[1:10])

association_3<-apriori(as.matrix(movies_new),parameter = list(support=0.3,confidence=0.8)) # 5 rules

# inspecting the 5 rules

inspect(sort(association_3,by='lift')[1:5])

association_4<-apriori(as.matrix(movies_new),parameter = list(support=0.3,confidence=0.7)) # 7 rules

# inspecting the 7 rules

inspect(sort(association_4,by='lift')[1:7])

association_5<-apriori(as.matrix(movies_new),parameter = list(support=0.3,confidence=0.6)) # 12 rules

# inspecting top 10 rules

inspect(sort(association_5,by='lift')[1:10])

association_6<-apriori(as.matrix(movies_new),parameter = list(support=0.4,confidence=0.8)) # 5 rules

# inspecting the 5 rules

inspect(sort(association_6,by='lift')[1:5])

association_7<-apriori(as.matrix(movies_new),parameter = list(support=0.4,confidence=0.7)) # 7 rules

# inspecting the 7 rules

inspect(sort(association_7,by='lift')[1:7])

association_8<-apriori(as.matrix(movies_new),parameter = list(support=0.4,confidence=0.6)) # 12 rules

# inspecting top 10 rules

inspect(sort(association_8,by='lift')[1:10])

association_9<-apriori(as.matrix(movies_new),parameter = list(support=0.5,confidence=0.8)) # 3 rules

# inspecting the 3 rules

inspect(sort(association_9,by='lift')[1:3])

association_10<-apriori(as.matrix(movies_new),parameter = list(support=0.5,confidence=0.7)) # 5 rules

# inspecting the 5 rules

inspect(sort(association_10,by='lift')[1:5])

association_11<-apriori(as.matrix(movies_new),parameter = list(support=0.5,confidence=0.6)) # 7 rules

# inspecting the 7 rules

inspect(sort(association_11,by='lift')[1:7])

association_12<-apriori(as.matrix(movies_new),parameter = list(support=0.6,confidence=0.8)) # 2 rules

# inspecting the 2 rules

inspect(sort(association_12,by='lift')[1:2])

association_13<-apriori(as.matrix(movies_new),parameter = list(support=0.6,confidence=0.7)) # 3 rules

# inspecting the 3 rules

inspect(sort(association_13,by='lift')[1:3])

association_14<-apriori(as.matrix(movies_new),parameter = list(support=0.6,confidence=0.6)) # 5 rules

# inspecting the 5 rules

inspect(sort(association_14,by='lift')[1:5])

association_15<-apriori(as.matrix(movies_new),parameter = list(support=0.7,confidence=0.8)) # No rules


association_16<-apriori(as.matrix(movies_new),parameter = list(support=0.6,confidence=0.6)) # 5 rules

# inspecting the 5 rules

inspect(sort(association_14,by='lift')[1:5])

association_17<-apriori(as.matrix(movies_new),parameter = list(support=0.1,confidence=0.8)) # 77 rules

# inspecting top 10 rules

inspect(sort(association_17,by='lift')[1:10]) # lift 10

association_18<-apriori(as.matrix(movies_new),parameter = list(support=0.1,confidence=0.7)) # 79 rules

# inspecting top 10 rules

inspect(sort(association_18,by='lift')[1:10]) # lift 10

association_19<-apriori(as.matrix(movies_new),parameter = list(support=0.1,confidence=0.6)) # 84 rules

# inspecting top 10 rules

inspect(sort(association_19,by='lift')[1:10]) # lift 10

# association_18 has the highest lift with more number of rules

plot(association_18,jitter=0)
plot(association_18,method = 'grouped')
plot(association_18,method = 'graph')
