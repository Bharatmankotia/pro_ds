
# Data Preprocessing

library(arules)
library(arulesViz)
books<-read.csv(file.choose())
class(books)

# Applying apriori algorithm on the dataset

association<-apriori(as.matrix(books),parameter = list(support=0.01,confidence=0.8)) # 678 rules obtained

# Sorting 10 rules in descending order

inspect(sort(association,by='lift')[1:10])

# Visualizing rules
plot(association)
plot(association,method = "grouped")
plot(association,method = "graph")
association_1<-apriori(as.matrix(books),parameter = list(support=0.01,confidence=0.7)) # 905 rules obtained

# Sorting 10 rules in descending order

inspect(sort(association_1,by='lift')[1:10])

association_2<-apriori(as.matrix(books),parameter = list(support=0.01,confidence=0.6)) # 1214 rules

# Sorting 10 rules in descending order

inspect(sort(association_2,by='lift')[1:10])

# Visualizing rules
plot(association_2)
plot(association_2,method = "grouped")
plot(association_2,method = "graph")

association_3<-apriori(as.matrix(books),parameter = list(support=0.02,confidence=0.8)) # 223 rules

# Sorting 10 rules in descending order

inspect(sort(association_3,by='lift')[1:10])

# Visualizing rules
plot(association_3)
plot(association_3,method = "grouped")
plot(association_3,method = "graph")

association_4<-apriori(as.matrix(books),parameter = list(support=0.02,confidence=0.6)) # 442 rules

# Sorting 10 rules in descending order

inspect(sort(association_4,by='lift')[1:10])

# Visualizing rules
plot(association_4)
plot(association_4,method = "grouped")
plot(association_4,method = "graph")

association_5<-apriori(as.matrix(books),parameter = list(support=0.03,confidence=0.8)) # 128 rules

# Sorting 10 rules in descending order

inspect(sort(association_5,by='lift')[1:10])

# Visualizing rules
plot(association_5)
plot(association_5,method = "grouped")
plot(association_5,method = "graph")

association_6<-apriori(as.matrix(books),parameter = list(support=0.03,confidence=0.6)) # 248 rules

# Sorting 10 rules in descending order

inspect(sort(association_6,by='lift')[1:10])

# Visualizing rules
plot(association_6)
plot(association_6,method = "grouped")
plot(association_6,method = "graph")

association_7<-apriori(as.matrix(books),parameter = list(support=0.04,confidence=0.8)) # 94 rules

# Sorting 10 rules in descending order

inspect(sort(association_7,by='lift')[1:10])

# Visualizing rules
plot(association_7)
plot(association_7,method = "grouped")
plot(association_7,method = "graph")

association_8<-apriori(as.matrix(books),parameter = list(support=0.04,confidence=0.6)) # 169 rules

# Sorting 10 rules in descending order

inspect(sort(association_8,by='lift')[1:10])

# Visualizing rules
plot(association_8)
plot(association_8,method = "grouped")
plot(association_8,method = "graph")

association_9<-apriori(as.matrix(books),parameter = list(support=0.05,confidence=0.8)) # 62 rules

# Sorting 10 rules in descending order

inspect(sort(association_9,by='lift')[1:10])

# Visualizing rules
plot(association_9)
plot(association_9,method = "grouped")
plot(association_9,method = "graph")

association_10<-apriori(as.matrix(books),parameter = list(support=0.05,confidence=0.6)) # 120 rules

# Sorting 10 rules in descending order

inspect(sort(association_10,by='lift')[1:10])

# Visualizing rules
plot(association_10)
plot(association_10,method = "grouped")
plot(association_10,method = "graph")

association_11<-apriori(as.matrix(books),parameter = list(support=0.06,confidence=0.8)) # 43 rules

# Sorting 10 rules in descending order

inspect(sort(association_11,by='lift')[1:10])

# Visualizing rules
plot(association_11)
plot(association_11,method = "grouped")
plot(association_11,method = "graph")

association_12<-apriori(as.matrix(books),parameter = list(support=0.06,confidence=0.6)) # 85 rules

# Sorting 10 rules in descending order

inspect(sort(association_12,by='lift')[1:10])

# Visualizing rules
plot(association_12)
plot(association_12,method = "grouped")
plot(association_12,method = "graph")

association_13<-apriori(as.matrix(books),parameter = list(support=0.07,confidence=0.8)) # 34 rules

# Sorting 10 rules in descending order

inspect(sort(association_13,by='lift')[1:10])

# Visualizing rules
plot(association_13)
plot(association_13,method = "grouped")
plot(association_13,method = "graph")

association_14<-apriori(as.matrix(books),parameter = list(support=0.07,confidence=0.6)) # 71 rules

# Sorting 10 rules in descending order

inspect(sort(association_14,by='lift')[1:10])

# Visualizing rules
plot(association_14)
plot(association_14,method = "grouped")
plot(association_14,method = "graph")

association_15<-apriori(as.matrix(books),parameter = list(support=0.08,confidence=0.8)) # 28 rules

# Sorting 10 rules in descending order

inspect(sort(association_15,by='lift')[1:10])

# Visualizing rules
plot(association_15)
plot(association_15,method = "grouped")
plot(association_15,method = "graph")

association_16<-apriori(as.matrix(books),parameter = list(support=0.08,confidence=0.6)) # 62 rules

# Sorting 10 rules in descending order

inspect(sort(association_16,by='lift')[1:10])

# Visualizing rules
plot(association_16)
plot(association_16,method = "grouped")
plot(association_16,method = "graph")

association_17<-apriori(as.matrix(books),parameter = list(support=0.09,confidence=0.8)) # 13 rules

# Sorting 10 rules in descending order

inspect(sort(association_17,by='lift')[1:10])

# Visualizing rules
plot(association_17)
plot(association_17,method = "grouped")
plot(association_17,method = "graph")

association_18<-apriori(as.matrix(books),parameter = list(support=0.09,confidence=0.6)) # 39 rules

# Sorting 10 rules in descending order

inspect(sort(association_18,by='lift')[1:10])

# Visualizing rules
plot(association_18)
plot(association_18,method = "grouped")
plot(association_18,method = "graph")

association_19<-apriori(as.matrix(books),parameter = list(support=0.1,confidence=0.8)) # 7 rules

# Sorting 7 rules in descending order

inspect(sort(association_19,by='lift'))

# Visualizing rules
plot(association_19)
plot(association_19,method = "grouped")
plot(association_19,method = "graph")

association_20<-apriori(as.matrix(books),parameter = list(support=0.1,confidence=0.6)) # 30 rules

# Sorting 10 rules in descending order

inspect(sort(association_20,by='lift')[1:10])

# Visualizing rules
plot(association_20)
plot(association_20,method = "grouped")
plot(association_20,method = "graph")

association_21<-apriori(as.matrix(books),parameter = list(support=0.15,confidence=0.8)) # No rules

association_21<-apriori(as.matrix(books),parameter = list(support=0.15,confidence=0.6)) # 11 rules

# Sorting 10 rules in descending order

inspect(sort(association_21,by='lift')[1:10])

# Visualizing rules
plot(association_21)
plot(association_21,method = "grouped")
plot(association_21,method = "graph")