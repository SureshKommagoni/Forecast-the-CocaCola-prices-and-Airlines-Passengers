plastic <- read.csv(file.choose())
View(plastic)

# preprocessing 
# creating 12 dummy variables 

x <- data.frame(outer(rep(month.abb, length = 60), month.abb,"==")+0)
View(x)

colnames(x) <- month.abb
View(x)

plasticSales <- cbind(plastic, x)
colnames(plasticSales)
# input 
plasticSales["t"] <- c(1:60)


plasticSales["log_Sales"] <- log(plasticSales$Sales)
plasticSales["t_square"] <- plasticSales["t"] * plasticSales["t"]
View(plasticSales)
# preprocessin completed

# spilt the data into train and test

attach(plasticSales)

train <- plasticSales[1:48,]
test <- plasticSales[49:60,]

###### Linear model ############
linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)
linear_model_pred <- data.frame(predict(linear_model, interval = "predict", newdata = test))
rmse_linear_model <- sqrt(mean((test$Sales - linear_model_pred$fit)^2, na.rm = T))
rmse_linear_model


###### Additive seasonality #########

add_sea_model <- lm(Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(add_sea_model)
add_sea_model_pred <-data.frame(predict(add_sea_model,interval = "predict", newdata = test))
rmse_add_sea <- sqrt(mean((test$Sales - add_sea_model_pred$fit)^2,na.rm = T))
rmse_add_sea

##### additive seasonality with Quadratice trend ######
add_sea_Quad_model <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(add_sea_Quad_model)
add_sea_Quad_pred <- data.frame(predict(add_sea_Quad_model, interval = "predict", newdata = test))
rmse_add_sea_Quad <- sqrt(mean((test$Sales - add_sea_Quad_pred$fit)^2, na.rm = T))
rmse_add_sea_Quad


##### additive seasonality with linear trend ########
add_sea_Linear_model <- lm(Sales ~ t + Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(add_sea_Linear_model)
add_sea_Linear_pred <- data.frame(predict(add_sea_Linear_model, interval = "predict", newdata = test))
rmse_add_sea_linear <- sqrt(mean((test$Sales - add_sea_Linear_pred$fit)^2, na.rm = T))
rmse_add_sea_linear

####### preparing table on model and it's RMSE values #####

table__rmse <- data.frame(c("rmse_linear_model","rmse_add_sea","rmse_add_sea_Quad","rmse_add_sea_linear"),c(rmse_linear_model,rmse_add_sea,rmse_add_sea_Quad,rmse_add_sea_linear))
colnames(table__rmse) <- c("model","RMSE")
View(table__rmse)

#### Additive seasonality with Linear Trend gives the least RMSE value

write.csv(plasticSales, file = "plasticSales.csv", row.names = F)
getwd()

#### combining train and test data to build additive seasonality with linear trend model ####

add_sea_Linear_model_final <- lm(Sales ~ t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = plasticSales)
summary(add_sea_Linear_model_final)

##### predicting the newdata ####

new_data <- read.csv(file.choose()) ### loading to test data 
View(new_data)
pred_new <- data.frame(predict(add_sea_Linear_model_final, interval = "predict", newdata = new_data))
View(pred_new)                      

# plot add_sea_linear_model
acf(add_sea_Linear_model_final$residuals, lag.max = 10)  #take all the residuals of model 

A <- arima(add_sea_Linear_model_final$residuals, order = c(1,0,0))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

### predicting next 12 months error using arima(oreder = c(1,0,0))

library(forecast)
errors_12 <- forecast(A, h = 12)

View(errors_12)

future_errors <- data.frame(errors_12)
View(future_errors)
class(future_errors)

future_errors <- future_errors$Point.Forecast

# predicated values for new data + future error values

predicted_new_Sales_values <- pred_new + future_errors

write.csv(predicted_new_Sales_values, file = "predicted_new_Sales_values.csv")
getwd()









