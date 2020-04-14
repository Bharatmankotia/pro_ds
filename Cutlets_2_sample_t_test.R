cutlets<-read.csv(file.choose())

View(cutlets)

# In the cutlets problem there are 2 samples and external conditions are not same as the samples are taken from two different machines

# Using 2- sample t-test

attach(cutlets)

# Normality test

shapiro.test(Unit.A)

# p value = 0.32. . Accept Null hypothesis. Follows normal distribution

shapiro.test(Unit.B)

# p value = 0.52 . Accept Null hypothesis. Follows normal distribution

# As external conditions are not same now we will check for equal variance

# Variance test

var.test(Unit.A,Unit.B)

# p value = 0.31. So p high null fly. Variances are equal

# Performing 2 sample t test

t.test(Unit.A,Unit.B,alternative = 'two.sided',conf.level = 0.95,correct=TRUE)

# p - value = 0.47 . Accept Null hypothesis. Means are equal. 

# Conclusion - There is no significant difference in diameters of cutlets of unit A and unit B at 5% significance level.

t.test(Unit.A,Unit.B,alternative = 'greater',variance= TRUE) # p-value = 0.23. Accept null hypothesis. No significant difference in diameters of cutlets.
