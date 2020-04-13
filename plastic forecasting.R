getwd()
# performing eda
str(plastic)
windows()
plot(plastic$Sales,type="o") # upward linear trend
plastic["t"]<-c(1:60) 
plastic["t_square"]<-plastic$t*plastic$t
plastic["log_sales"]<-log(plastic$Sales)
dummy<-data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(dummy)<-month.abb
plastic<-cbind(plastic,dummy)

train<-plastic[1:48,]  # as it is time series data so we will take the data in series not randomly
test<-plastic[49:60,]
attach(plastic)
###1 Linear model

linear_model<-lm(Sales~t,data = train)
linear_model_pre<-data.frame(predict(linear_model,interval="predict",newdata = test))
rmse_linear_model<-sqrt(mean((test$Sales - linear_model_pre$fit)^2,na.rm = T))
rmse_linear_model #260.9378

###2 quadratic model

quad_model<-lm(Sales~t + t_square,data = train)
quad_pre<-data.frame(predict(quad_model,interval="predict",newdata = test))
rmse_quad_model<-sqrt(mean((test$Sales - quad_pre$fit)^2,na.rm=T))
rmse_quad_model #297.4067

###3 exponential model
expo_model<-lm(log_sales~t,data = train)
expo_pre<-data.frame(predict(expo_model,interval = "predict",newdata = test))
rmse_expo_model<-sqrt(mean((test$Sales - exp(expo_pre$fit))^2,na.rm=T))
rmse_expo_model #268.6938

###4 additive seasonality 

add_sea_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
add_sea_pre<-data.frame(predict(add_sea_model,interval = "predict",newdata = test))
rmse_add_sea_model<-sqrt(mean((test$Sales - add_sea_pre$fit)^2,na.rm = T))
rmse_add_sea_model #235.6027


###5 additive seasonality with linear trend

add_sea_linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
add_sea_linear_pre<-data.frame(predict(add_sea_linear_model,interval = "predict",newdata = test))
rmse_add_sea_linear_model<-sqrt(mean((test$Sales - add_sea_linear_pre$fit)^2,na.rm = T))
rmse_add_sea_model #235.6027

###6 additive seasonality with quadratic trend

add_sea_quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
add_sea_quad_pre<-data.frame(predict(add_sea_quad_model,interval = "predict",newdata = test))
rmse_add_sea_quad_model<-sqrt(mean((test$Sales - add_sea_quad_pre$fit)^2,na.rm = T))
rmse_add_sea_quad_model #218.1939

###7 multiplicative seasonality

mult_model<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train )
mult_pre<-data.frame(predict(mult_model,interval = "predict",newdata = test))
rmse_pre_model<-sqrt(mean((test$Sales - exp(mult_pre$fit))^2,na.rm = T))
rmse_pre_model #239.6543


###8 multiplicative seasonality with linear tend

mult_linear_model<-lm(log_sales~ t +Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train )
mult_linear_pre<-data.frame(predict(mult_linear_model,interval = "predict",newdata = test))
rmse_mult_linear_model<-sqrt(mean((test$Sales - exp(mult_linear_pre$fit))^2,na.rm=T))
rmse_mult_linear_model #160.6833


###9 multiplicative seasonality with quadratic trend

mult_quad_model<-lm(log_sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
mult_quad_pre<-data.frame(predict(mult_quad_model,interval = "predict",newdata = test))
rmse_mult_quad_model<-sqrt(mean((test$Sales - exp(mult_quad_pre$fit))^2,na.rm=T))
rmse_mult_quad_model #239.606


table<-data.frame(c("rmse_linear_model","rmse_quad_model","rmse_expo_model","rmse_add_sea_model","rmse_add_sea_model","rmse_add_sea_quad_model","rmse_pre_model","rmse_mult_linear_model","rmse_mult_quad_model"),
        c(rmse_linear_model,rmse_quad_model,rmse_expo_model,rmse_add_sea_model,rmse_add_sea_model,rmse_add_sea_quad_model,rmse_pre_model,rmse_mult_linear_model,rmse_mult_quad_model))
colnames(table)<-c("model type","rmse_values")
View(table)

# rmse of multiplicative linear model is less so we build a model 

new_model<-lm(log_sales~t +Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = plastic)
resid<-residuals(new_model)
windows()
acf(resid,lag.max = 10) # there are many significant lags so we take lag 1 

#Building autoregressive model on lag 1 

k<-arima(resid,order = c(1,0,0))
str(k)
windows()
acf(k$residuals,lag.max = 15) # none of the lags are significant now


pred_resid<-predict(arima(k$residuals,order = c(1,0,0)),n.ahead = 12)
pred_resid$pred
setwd("D:\\Data science")
write.csv(plastic,"plastic.csv",col.names = F,row.names = F)


# importing the new file with 12 observations for the future

test_data<-read_excel(file.choose(),2)
View(test_data)

pred_new<-data.frame(predict(new_model,interval = "predict",newdata = test_data))
pred_new_values<-pred_new$fit+pred_resid$pred
pred_new_values
View(exp(pred_new_values))
View(plastic)
