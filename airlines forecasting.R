airlines["t"]<-c(1:96)
airlines["t_square"]<-airlines$t*airlines$t
airlines["log_passengers"]<-log(airlines$t)
dummy<-data.frame(outer(rep(month.abb,length=96),month.abb , "==")+0)
View(dummy)
colnames(dummy)<-month.abb
?month.abb
airlines<-data.frame(airlines,dummy)
library(fpp)
library(forecast)
library(tseries)
windows()
plot(airlines$Passengers,type="o")  # From the plot it is obvious that trend is curvilinear .So building model's associated with curve trends.
# omitting exponential and quadratic model's

attach(airlines)

train<-airlines[1:84,] 
test<-airlines[85:96,]

#####1 Linear model

linear_model<-lm(Passengers~t,data=train)
linear_pre<-data.frame(predict(linear_model,interval="predict",newdata = test))
rmse_linear<-sqrt(mean((test$Passengers - linear_pre$fit)^2,na.rm=T))
rmse_linear  # 53.19924

#####2 Additive seasonality model

add_sea_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
add_sea_pre<-data.frame(predict(add_sea_model,interval = "predict",newdata = test))
rmse_add_sea<-sqrt(mean((test$Passengers-add_sea_pre$fit)^2,na.rm=T))
rmse_add_sea  # 132.8198

####3 additive seasonality with quadratic trend

add_sea_quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
add_sea_quad_pre<-data.frame(predict(add_sea_quad_model,interval="predict",newdata = test))
rmse_add_sea_quad<-sqrt(mean((test$Passengers - add_sea_quad_pre$fit)^2,na.rm=T))
rmse_add_sea_quad  #26.36082

####4 multiplicative seasonality model

mult_sea_model<-lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
mult_sea_pre<-data.frame(predict(mult_sea_model,interval="predict",newdata = test))
rmse_mult_sea<-sqrt(mean((test$Passengers - exp(mult_sea_pre$fit))^2,na.rm=T))
rmse_mult_sea #299.0523

# So amongst the models build additive seasonality with quadratic trend has the least error

table_rmse<-data.frame(c("rmse_linear","rmse_add_sea","rmse_add_sea_quad","rmse_mult_sea"),c(rmse_linear,rmse_add_sea,rmse_add_sea_quad,rmse_mult_sea))
colnames(table_rmse)<-c("model type","rmse values")


#### so we will build the model using additive seasonality with quadratic trend as it has least rmse

new_model<-lm(Passengers~t + t_square + Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data =airlines)
resid<-residuals(new_model)
windows()
acf(resid,lag.max = 10) # lag 1 and 2 are significant for the errors

# Building autoregreesive model using lag 1

k<-arima(resid,order=c(1,0,0))
str(k)
View(data.frame(res=resid,newresid=k$residuals))

windows()
acf(k$residuals,lag.max=15) # lag 4 and lag 12 are again significant

#Building autoregressive model using lag 12

k1<-arima(resid,order=c(12,0,0))
str(k1)

windows()
acf(k1$residuals,lag.max = 15)  # None of the lags are significant now for the errors

pred_res<-predict(arima(k1$residuals,order=c(12,0,0)),n.ahead = 12)
str(pred_res)
pred_res$pred
write.csv(airlines,file="airlines.csv",row.names = F)
getwd()
setwd("D:\\Data science")

test_data<-read_excel(file.choose(),2) # creating input variables for next 12 months using predicted values.
View(test_data)
pred_new<-data.frame(predict(new_model,interval = "predict",newdata = test_data))
pred_new_values<-pred_new$fit + pred_res$pred
View(pred_new_values)
