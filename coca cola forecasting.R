coke_data<-ts(coke$Sales,frequency = 4)
train<-coke_data[1:38]
test<-coke_data[39:42]
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)
plot(coke_data)

# moving average model

ma_model1<-Arima(train,order = c(0,0,4))
forecasted<-forecast(ma_model1,h=4)
ma<-accuracy(forecasted,x=as.numeric(test))
plot(forecasted)


# simple exponential smoothing using holt's winter

hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hwa<-forecast(hw_a,h=4)
A<-accuracy(hwa,x=as.numeric(test))
plot(forecast(hwa))


#holt's smoothing

hw_ab<-HoltWinters(train,alpha = 0.2,beta=0.1,gamma = F)
hwb<-forecast(hw_ab,h=4)
b<-accuracy(hwb,x=as.numeric(test))
plot(forecast(hwb))

#Winter's method

hw_abg<-HoltWinters(train,alpha=0.2,beta=0.1,gamma=0.1)
hwabg<-forecast(hw_abg,h=4)
c<-accuracy(hwabg,x=as.numeric(test))
plot(forecast(hwabg))

# Letting system choose the alpha value

hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na  #alpha 0.502
hwna<-forecast(hw_na,h=4)
d<-accuracy(hwna,x=as.numeric(test))
plot(forecast(hwna))

# Letting system chose the value of alpha and beta

hw_nab<-HoltWinters(train,gamma = F)
hw_nab #alpha: 0.5747386 , beta : 0.3105725
hwnab<-forecast(hw_nab,h=4)
e<-accuracy(hwnab,x=as.numeric(test))
plot(forecast(hwnab))

# Letting system choose alpha beta and gamma values

hw_nabg<-HoltWinters(train)
hw_nabg #alpha: 0.3784328 , beta : 0.2526015 , gamma: 0.8897278
hwnabg<-forecast(hw_nabg,h=4)
f<-accuracy(hwnabg,x=as.numeric(test))
plot(forecast(hwnabg))


# simple exponential smoothing with alpha = 0.2

ses_a<-ses(train,alpha=0.2)
sesa<-forecast(ses_a,h=4)
g<-accuracy(sesa,x=as.numeric(test))
plot(forecast(sesa))

# holt smoothing with alpha = 0.2 , beta =0.1
holt_ab<-holt(train,alpha=0.2,beta = 0.1)
holtab<-forecast(holt_ab,h=4)
h<-accuracy(holtab,x=as.numeric(test))
plot(forecast(holtab))

#holt and winter smoothing alpha=0.2, beta= 0.1 , gamma=0.1

hw_abg_new<-hw(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hwabgnew<-forecast(hw_abg_new,h=4)
i<-accuracy(hwabgnew,x=as.numeric(test))
plot(forecast(hwabgnew))

# SES with alpha = NULL

ses_na<-ses(train,alpha=NULL)
sesna<-forecast(ses_na,h=4)
j<-accuracy(sesna,x=as.numeric(test))
plot(forecast(sesna))


#Holt with alpha = NULL , beta = NULL
holt_nab<-holt(train,alpha=NULL,beta = NULL)
holtnab<-forecast(holt_nab,h=4)
k<-accuracy(holtnab,x=as.numeric(test))
plot(forecast(holtnab))

# Holt's winter with alpha = NULL , beta= NULL ,gamma= NULL

hw_nabg_new<-hw(train,alpha=NULL,beta = NULL,gamma = NULL)
hwnabgnew<-forecast(hw_nabg_new,h=4)
l<-accuracy(hwnabgnew,x=as.numeric(test))
plot(forecast(hwnabgnew))

# Checking error values of different model's formed

ma
A
b
c
d
e
f
g
h
i
j
k
l
ma<-as.numeric(ma[2,1:6])
a_errors<-as.numeric(A[2,1:6])
b_errors<-as.numeric(b[2,1:6])
c_errors<-as.numeric(c[2,1:6])
d_errors<-as.numeric(d[2,1:6])
e_errors<-as.numeric(e[2,1:6])
f_errors<-as.numeric(f[2,1:6])
g_errors<-as.numeric(g[2,1:6])
h_errors<-as.numeric(h[2,1:6])
i_errors<-as.numeric(i[2,1:6])
j_errors<-as.numeric(j[2,1:6])
k_errors<-as.numeric(k[2,1:6])
l_errors<-as.numeric(l[2,1:6])
values<-c(a_errors,b_errors,c_errors,d_errors,e_errors,f_errors,g_errors,h_errors,i_errors,j_errors,
          k_errors, l_errors)
          
table_test<-data.frame(matrix(values,nrow=13,ncol=6,byrow = T))
colnames(table_test)<-c("me","rmse","mae","mpe","mape","mase")
rownames(table_test)<-c("ma","hw_a","hw_ab","hw_abg","hw_na","hw_nab",
                        "hw_nabg","ses_na","holt_ab","hw_abg_new","ses_na","holt_nab","hw_nabg_new")

View(table_test)

# hw_nabg has least MAPE

hw_nabg_final<-HoltWinters(coke_data)
hwnabg_final<-forecast(hw_nabg_final,h=4)
New_acc<-accuracy(hwnabg_final)
plot(forecast(hwnabg_final))

forecast_new<-data.frame(predict(hw_nabg_final,h=4))
View(forecast_new) #5215.15
