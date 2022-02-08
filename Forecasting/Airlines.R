library(readxl)
airlines <- read_xlsx(file.choose()) # read Airlines data
View(airlines) # Seasonlity 12 months

# pre processing 

# so creating 12 dummy variables

x<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )# Creating dummies for 12 months
View(x)
colnames(x)<-month.abb # Assigning month names 
View(x)
airlinesPassengers <- cbind(airlines,x)
colnames(airlinesPassengers)

# input t
airlinesPassengers["t"] <- c(1:96)
View(airlinesPassengers)

airlinesPassengers["log_passengers"] <- log(airlinesPassengers["Passengers"])
airlinesPassengers["t_square"] <- airlinesPassengers["t"]*airlinesPassengers["t"]
View(airlinesPassengers)
# preprocessing completed

attach(airlinesPassengers)
# partitioning 

train <- airlinesPassengers[1:84,]
test <- airlinesPassengers[85:96,]


############ Linear Model ####################

linear_Model <- lm(Passengers ~ t, data = train)
summary(linear_Model)
liner_pred <- data.frame(predict(linear_Model, interval = 'predict', newdata = test))
rmse_linear <- sqrt(mean((test$Passengers - liner_pred$fit)^2, na.rm = T))
rmse_linear # 53.19924

########### Exponential  ##############

expo_model <- lm(log_passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval = 'predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers - expo_pred$fit)^2, na.rm = T))
rmse_expo # 325.7151

########### Quadratic  ##########################

Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval = 'predict', newdata = test))
rmse_Quad <- sqrt(mean((test$Passengers - Quad_pred$fit)^2, na.rm = T))
rmse_Quad # 48.05189

 ######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers ~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add  # 132.8198

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear #  35.34896

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26.36082

######################### Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140.0632

### preparing table on model and it's RMSE values

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad", "rmse_mulri_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad, rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

write.csv(airlinesPassengers, file="airlinesPassengers.csv", row.names = F)
getwd()

### combing Training and Test data to build Additive Seasonality using Quadratic Tred #####
Add_sea_Quad_model_final <- lm(Passengers ~ t + t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = airlinesPassengers)
summary(Add_sea_Quad_model_final)

###### Predicting the new data #####

new_data <- read.csv(file.choose())
View(new_data)
pred_new <- data.frame(predict(Add_sea_Quad_model_final, newdata = new_data, interval = 'predict'))
View(pred_new)

# Plot (Add_sea_Quad_model_final)
acf(Add_sea_Quad_model_final$residuals, lag.max = 10) # take all residual value of the model

A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,0,0))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

# predicting next 12 months errors using arima(order = c(1,0,0))

library(forecast)
errors_12 <- forecast(A, h = 12)

View(errors_12)
future_errors <- data.frame(errors_12)
View(future_errors)
class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicated values for new data + future error values

predicted_new_values <- pred_new + future_errors

write.csv(predicted_new_values, file = "predicted_new_values.csv")
getwd()







