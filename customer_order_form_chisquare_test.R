customer_order<-read.csv(file.choose())

View(customer_order)

summary(customer_order)

customer_order$Phillippines<-as.vector(customer_order$Phillippines)

customer_order$Indonesia<-as.vector(customer_order$Indonesia)

customer_order$Malta<-as.vector(customer_order$Malta)

customer_order$India<-as.vector(customer_order$India)

stack_data<-stack(customer_order)

table(stack_data$values,stack_data$ind)
chisq.test(table(stack_data$values,stack_data$ind)) # p- value is 0.27. Accept Null hypothesis.

# Conclusion- Proportion of defectives is equal for all the centres.
