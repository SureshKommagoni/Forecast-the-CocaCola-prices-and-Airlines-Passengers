library(forecast)
library(fpp)
library(smooth)

# loading cocacola data set

library(readxl)

cocacola <- read_xlsx(file.choose())
View(cocacola)

library(tseries)

# converting data into time series

cocacola_ts <- ts(cocacola$Sales, frequency = 4)
View(cocacola_ts)

# spliting data into train and test 

train <- cocacola_ts[1:38]
test <- cocacola_ts[39:42]

# convert train and test into time series objects

train_ts <- ts(train, frequency = 4)
test_ts <- ts(test, frequency = 4)

# plotting time series data for visualization

plot(cocacola_ts)

# visualization shows that it has level, trend and seasonality

ma_model1 <- Arima(train, order = c(0,0,4))
forecasted <- forecast(ma_model1, h = 4)
MA <- accuracy(forecasted, x = as.numeric(test))
MA
plot(forecasted)

# using HoltsWinters method
# optimum values
# with alpha = 0.2 which is the default value
# assuming that time series data has only level parameter 

hw_a <- HoltWinters(train, alpha = 0.2,beta = F,gamma = F)
hw_a
hwa <- forecast(hw_a, h = 4)
hwa

A <- accuracy(hwa, x = as.numeric(test))
A
plot(forecast(hwa))

# with alpha = 0.2, Beta = 0.1
# assuming that time series data has level and trend

hw_ab <- HoltWinters(train, alpha = 0.2, beta = 0.1, gamma = F)
hw_ab
hwab <- forecast(hw_ab, h = 4)
B <- accuracy(hwab, x = as.numeric(test))
B
plot(forecast(hwab))

# with alpha = 0.2, beta = 0.1, gamma = 0.1 => Winter's method
# Assuming time series data has level,trend and seasonality 

hw_abg<-HoltWinters(train_ts,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg 
hwabg <- forecast(hw_abg, h = 4)
C <- accuracy(hwabg, x = as.numeric(test))
C
plot(forecast(hwabg))

# Optimized values will be calculated by the corresponding algorithm/technique
# beta = F, gamma = F
hw_na<-HoltWinters(train,beta = F,gamma = F)
str(hw_na)
hw_na$alpha
hwna <- forecast(hw_na, h = 4)
D <- accuracy(hwna, x = as.numeric(test))
D
plot(forecast(hwna))

# gamma = F

hw_nab<-HoltWinters(train,gamma=F)
str(hw_nab)
hw_nab$alpha
hw_nab$beta
hwnab <- forecast(hw_nab, h = 4)
E <- accuracy(hwnab, x = as.numeric(test))
E
plot(forecast(hwnab))

hw_nabg<-HoltWinters(train_ts)
hw_nabg$alpha
hw_nabg$beta
hw_nabg$gamma
hwnabg <- forecast(hw_nabg, h = 4)
F <- accuracy(hwnabg, as.numeric(test))
F
plot(forecast(hwnabg))

##### using ses, holt, hw, functions ######
# logical values 
# with alpha = 0.2

ses_a <- ses(train, alpha =0.2)
sesa <- forecast(ses_a, h = 4)
G <- accuracy(sesa, x = as.numeric(test))
G
plot(forecast(sesa))

# with alpha = 0.2, beta = 0.1

holt_ab <- holt(train, alpha = 0.2, beta = 0.1)
holtab <- forecast(holt_ab, h = 4)
H <- accuracy(holt_ab, x = as.numeric(test))
H
plot(forecast(holtab))

# with alpha = 0.2, beta = 0.1, gamma = 0.1

hw_abg_new <- hw(train_ts, aalpha = 0.2, beta = 0.1, gamma = 0.1)
hwabgnew <- forecast(hw_abg_new, h = 4)
I <- accuracy(hwabgnew, x = as.numeric(test))
I
plot(forecast(hwabgnew))

# optimized values

ses_na <- ses(train, alpha = NULL)
sesna <- forecast(ses_na, h = 4)
J <- accuracy(sesna, x = as.numeric(test))
J
plot(forecast(sesna))

holt_nab <- holt(train, alpha = NULL, beta = NULL)
holtnab <- forecast(holt_nab, h = 4)
K <- accuracy(holtnab, x = as.numeric(test))
K
plot(forecast(holtnab))

hw_nabg_new <- hw(train_ts, alpha = NULL, beta = NULL, gamma = NULL)
hwnabgnew <- forecast(hw_nabg_new, h = 4)
L <- accuracy(hwnabgnew, x = as.numeric(test))
L
plot(forecast(hwnabgnew))

###### To Know which methodis better

MA <- as.numeric(MA[2, 1:6])
A <- as.numeric(A[2,1:6])
B <- as.numeric(B[2,1:6])
C <- as.numeric(C[2,1:6])
D <- as.numeric(D[2,1:6])
E <- as.numeric(E[2,1:6])
F <- as.numeric(F[2,1:6])
G <- as.numeric(G[2,1:6])
H <- as.numeric(H[2,1:6])
I <- as.numeric(I[2,1:6])
J <- as.numeric(J[2,1:6])
K <- as.numeric(K[2,1:6])
L <- as.numeric(L[2,1:6])

values <- c(MA, A, B, C, D, E, F, G, H, I, J, K, L)

table_test <- data.frame(matrix(values, nrow = 13, ncol = 6, byrow = T))
View(table_test)
colnames(table_test) <- c("ME", "RMSE", "MAE", "MPE","MAPE", "MASE")

rownames(table_test) <- c("MA", "hw_a", "hw_ab", "hw_abg", "hw_na", "hw_nab", "hw_nabg", "ses_a", "holt_ab", "hw_abg_new", "ses_na", "holt_nab", "hw_nabg_new")

View(table_test)

### forecasting on overall Data based on best model

hw_nabg_final <- HoltWinters(cocacola_ts, alpha = NULL, beta = NULL, gamma = NULL)
hwnabg_final <- forecast(hw_nabg_final, h = 4)
new <- accuracy(hwnabg_final, x = as.numeric(test))
plot(forecast(hwnabg))



