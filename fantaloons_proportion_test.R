fantaloons<-read.csv(file.choose())

View(fantaloons)

attach(fantaloons)

table1<-table(Weekdays,Weekend)
table1

?prop.test

prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = 'two.sided')

# p-value = 0.96. Accept Null Hypothesis.

# Conclusion<- At 95% significance level the proportion of male and female coming to the store are is same. 