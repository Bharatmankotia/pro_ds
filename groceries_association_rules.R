library(arules)
library(arulesViz)

# Data preprocessing

groceries<-read.transactions(file.choose(),sep = ",",rm.duplicates = T)
summary(groceries) # total items in the transaction are 169
# Proportion of non zero values is 0.026
# Whole milk is the most bought item

# Visualizing Top 50 items with highest frequency

itemFrequencyPlot(groceries,topN=50) # All the items have min. support of 0.03

# apriori algorithm

association<-apriori(groceries,parameter = list(support=0.03,confidence=0.8)) # No rules

association_1<-apriori(groceries,parameter = list(support=0.03,confidence=0.7)) # No rules

association_2<-apriori(groceries,parameter = list(support=0.03,confidence=0.6)) # No rules

association_3<-apriori(groceries,parameter = list(support=0.03,confidence=0.5)) # No rules

association_4<-apriori(groceries,parameter = list(support=0.03,confidence=0.4)) # 5 rules

# inspecting the rules obtained

inspect(sort(association_4,by= 'lift'))


association_5<-apriori(groceries,parameter = list(support=0.03,confidence=0.3)) # 14 rules

# inspecting top 10 rules obtained

inspect(sort(association_5,by= 'lift')[1:10])

association_6<-apriori(groceries,parameter = list(support=0.03,confidence=0.2)) # 26 rules

# inspecting top 10 rules obtained

inspect(sort(association_6,by= 'lift')[1:10])

association_7<-apriori(groceries,parameter = list(support=0.02,confidence=0.8)) # No rules

association_8<-apriori(groceries,parameter = list(support=0.02,confidence=0.7)) # No rules

association_9<-apriori(groceries,parameter = list(support=0.02,confidence=0.6)) # No rules

association_10<-apriori(groceries,parameter = list(support=0.02,confidence=0.5)) # 1 rule

# inspecting 1 rule obtained

inspect(sort(association_10,by= 'lift'))

association_11<-apriori(groceries,parameter = list(support=0.02,confidence=0.4)) # 15 rules

# inspecting top 10 rules obtained

inspect(sort(association_11,by= 'lift')[1:10])

association_12<-apriori(groceries,parameter = list(support=0.02,confidence=0.3)) # 37 rules

# inspecting top 10 rules obtained

inspect(sort(association_12,by= 'lift')[1:10])

association_13<-apriori(groceries,parameter = list(support=0.01,confidence=0.8)) # No rules

association_14<-apriori(groceries,parameter = list(support=0.01,confidence=0.6)) # No rules

association_15<-apriori(groceries,parameter = list(support=0.01,confidence=0.5)) # 15 rules

# inspecting top 10 rules obtained

inspect(sort(association_15,by= 'lift')[1:10])

association_16<-apriori(groceries,parameter = list(support=0.01,confidence=0.4)) # 62 rules

# inspecting top 10 rules obtained

inspect(sort(association_16,by= 'lift')[1:10])

association_17<-apriori(groceries,parameter = list(support=0.01,confidence=0.3)) # 125 rules

# inspecting top 10 rules obtained

inspect(sort(association_17,by= 'lift')[1:10])

association_18<-apriori(groceries,parameter = list(support=0.009,confidence=0.8)) # No rules

association_19<-apriori(groceries,parameter = list(support=0.009,confidence=0.6)) # 1 rule

# inspecting the only rule obtained

inspect(sort(association_19,by= 'lift'))

association_20<-apriori(groceries,parameter = list(support=0.008,confidence=0.8)) # No rule

association_21<-apriori(groceries,parameter = list(support=0.008,confidence=0.6)) # 2 rules

association_22<-apriori(groceries,parameter = list(support=0.007,confidence=0.8)) # No rule

association_22<-apriori(groceries,parameter = list(support=0.007,confidence=0.6)) # 4 rules

association_23<-apriori(groceries,parameter = list(support=0.006,confidence=0.8)) # No rule

association_24<-apriori(groceries,parameter = list(support=0.006,confidence=0.6)) # 8 rules

association_25<-apriori(groceries,parameter = list(support=0.005,confidence=0.8)) # No rule

association_25<-apriori(groceries,parameter = list(support=0.005,confidence=0.6)) # 22 rules

# inspecting top 10 rules obtained

inspect(sort(association_25,by= 'lift')[1:10])

association_26<-apriori(groceries,parameter = list(support=0.004,confidence=0.8)) # No rule

association_27<-apriori(groceries,parameter = list(support=0.004,confidence=0.6)) # 40 rules

association_28<-apriori(groceries,parameter = list(support=0.003,confidence=0.8)) # 1 rule

# inspecting the only rule obtained

inspect(sort(association_28,by= 'lift'))

association_29<-apriori(groceries,parameter = list(support=0.003,confidence=0.7)) # 19 rules

# inspecting top 10 rules obtained

inspect(sort(association_29,by= 'lift')[1:10])

association_30<-apriori(groceries,parameter = list(support=0.003,confidence=0.6)) # 120 rules

# inspecting top 10 rules obtained

inspect(sort(association_30,by= 'lift')[1:10])

# Visualizing the rules with support 0.003 and confidence 0.6

plot(association_30)
plot(association_30[1:20],method = "graph")
plot(association_30[1:20],method = "grouped")
