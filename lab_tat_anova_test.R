lab<-read.csv(file.choose())

View(lab)

attach(lab)

stacked_data<-stack(lab) # stacking the data

# Normality test

library(nortest)

ad.test(stacked_data$values) # p value = 0.051 . So Accept null hypothesis. Data is assumed to be normal

# Variance test

library(car)

leveneTest(stacked_data$values~stacked_data$ind, data = stacked_data) # p - value 0.051. So accept Null hypothesis i.e. variances are assumed to be equal

# Applying one way annova test

anova_test <- aov(values~ind,data = stacked_data)
summary(anova_test) # p- value = 0 . So accept alternate hypothesis.

# Conclusion- Average TAT of atleast 1 lab is not the same.

