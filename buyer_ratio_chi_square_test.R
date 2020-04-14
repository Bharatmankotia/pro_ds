buyer_ratio<-read.csv(file.choose())

View(buyer_ratio)

buyer_ratio<-t(buyer_ratio)
buyer_ratio<-as.data.frame(buyer_ratio)
names(buyer_ratio) <- as.matrix(buyer_ratio[1, ])
buyer_ratio <- buyer_ratio[-1, ]
buyer_ratio[] <- lapply(buyer_ratio, function(x) type.convert(as.character(x)))

attach(buyer_ratio)

# Applying Chi square test

chisq.test(buyer_ratio)$p.value # p-value = 0.66 i.e. we will accept Null hypothesis

# Conclusion - Proportion of male and female buyer ratio is assumed to be same across all the regions. 

