library(ggplot2)
library(devtools)
install_github('sinhrks/ggfortify', force = TRUE)
library(ggfortify)
No2<- ts(No2[,2], start = c(2001 , 1) , end = c(2015,12) ,frequency = 12)
CO<- ts(CO[,2], start = c(2001 , 1) , end = c(2015,12) ,frequency = 12)
O3<- ts(O3[,2], start = c(2001 , 1) , end = c(2015,12) ,frequency = 12)
SO2<- ts(SO2[,2], start = c(2001 , 1) , end = c(2015,12) ,frequency = 12)
Combine <- ts(Combine[,2:5], start = c(2000 , 1) , end = c(2015,12) , frequency = 12)

autoplot(Combine, facets = TRUE)

autoplot(No2, facets = TRUE)+ 
  ggtitle("NO2")
autoplot(CO, facets = TRUE)+ 
  ggtitle("CO")
autoplot(O3, facets = TRUE)+ 
  ggtitle("O3")
autoplot(SO2, facets = TRUE)+ 
  ggtitle("SO2")
#plot acf for all the pollutnts
library(ggplot2)
library(ggfortify)
library(forecast)
ggAcf(No2)+ 
  ggtitle("ACF No2 mean")
Box.test(No2 , lag = 24 , type = "Ljung")
ggseasonplot(No2)

ggAcf(CO)+ 
  ggtitle("ACF CO mean")
Box.test(CO , lag = 24 , type = "Ljung")
ggseasonplot(CO)

ggAcf(O3)+ 
  ggtitle("ACF O3 mean")
Box.test(O3 , lag = 24 , type = "Ljung")
ggseasonplot(O3)

ggAcf(SO2)+ 
  ggtitle("ACF SO2 mean")
Box.test(SO2 , lag = 24 , type = "Ljung")
ggseasonplot(SO2)

#create training and test data sets
library(ggplot2)
library(ggfortify)
library(dplyr)
library(forecast)
library(timeSeries)
library(devtools)
library(fpp)
devtools::install_github("robjhyndman/forecast")
train_No2 <- subset(No2, end = length(No2) - 60)
train_CO <- subset(CO, end = length(CO) - 60)
train_O3 <- subset(O3, end = length(O3) - 60)
train_SO2 <- subset(SO2, end = length(SO2) - 60)

#forecasting models
#Holt's Winter Additive Method
#For No2
fchw_No2 <-  hw(train_No2 , seasonal = "additive" , h = 60)
accuracy(fchw_No2 ,No2)
checkresiduals(fchw_No2)
autoplot(fchw_No2) + 
  ggtitle("Forecast of No2 by Holt's Winter Additive")
#For CO
fchw_CO <-  hw(train_CO , seasonal = "additive" , h = 60)
accuracy(fchw_CO ,CO)
checkresiduals(fchw_CO)
autoplot(fchw_CO) + 
  ggtitle("Forecast of CO by Holt's Winter Additive")
#For O3
fchw_O3 <-  hw(train_O3 , seasonal = "additive" , h = 60)
accuracy(fchw_O3 ,O3)
checkresiduals(fchw_O3)
autoplot(fchw_O3) + 
  ggtitle("Forecast of O3 by Holt's Winter Additive")
#For SO2
fchw_SO2 <-  hw(train_SO2 , seasonal = "additive" , h = 60)
accuracy(fchw_SO2 ,SO2)
checkresiduals(fchw_SO2)
autoplot(fchw_SO2) + 
  ggtitle("Forecast of SO2 by Holt's Winter Additive")

#Holt's Winter Multiplicative Method
#For No2
fchw_No2_M <-  hw(train_No2 , seasonal = "multiplicative" , h = 60)
accuracy(fchw_No2_M , No2)
checkresiduals(fchw_No2_M)
autoplot(fchw_No2_M) + 
  ggtitle("Forecast of No2 by Holt's Winter Multiplicative")
#For CO
fchw_CO_M <-  hw(train_CO , seasonal = "multiplicative" , h = 60)
accuracy(fchw_CO_M , CO)
checkresiduals(fchw_CO_M)
autoplot(fchw_CO_M) + 
  ggtitle("Forecast of CO by Holt's Winter Multiplicative")
#For O3
fchw_O3_M <-  hw(train_O3 , seasonal = "multiplicative" , h = 60)
accuracy(fchw_O3_M , O3)
checkresiduals(fchw_O3_M)
autoplot(fchw_O3_M) + 
  ggtitle("Forecast of O3 by Holt's Winter Multiplicative")
#For SO2
fchw_SO2_M <-  hw(train_SO2 , seasonal = "multiplicative" , h = 60)
accuracy(fchw_SO2_M , SO2)
checkresiduals(fchw_SO2_M)
autoplot(fchw_SO2_M) + 
  ggtitle("Forecast of SO2 by Holt's Winter Multiplicative")

#ets 
#for No2
#forecasting model
ets_No2 <- ets(train_No2) 
ets_No2
#model output
forecast(ets_No2 ,h = 60)
checkresiduals(ets_No2)
accuracy(forecast(ets_No2 ,h = 60) ,No2)
autoplot(forecast(ets_No2 , h = 60))+
  ggtitle("Forecast of No2 by auto selection")
#ets 
#for Co
#forecasting model
ets_CO <- ets(train_CO) 
ets_CO
#model output
forecast(ets_CO ,h = 60)
checkresiduals(ets_CO)
accuracy(forecast(ets_CO ,h = 60) ,CO)
autoplot(forecast(ets_CO , h = 60))+
  ggtitle("Forecast of CO by auto selection")

#ets 
#for O3
#forecasting model
ets_O3 <- ets(train_O3) 
ets_O3
#model output
forecast(ets_O3 ,h = 60)
checkresiduals(ets_O3)
accuracy(forecast(ets_O3 ,h = 60) ,O3)
autoplot(forecast(ets_O3 , h = 60))+
  ggtitle("Forecast of O3 by auto selection")
#ets 
#for SO2
#forecasting model
ets_SO2 <- ets(train_SO2) 
ets_SO2
#model output
forecast(ets_SO2 ,h = 60)
checkresiduals(ets_SO2)
accuracy(forecast(ets_SO2 ,h = 60) ,SO2)
autoplot(forecast(ets_SO2 , h = 60))+
  ggtitle("Forecast of SO2 by auto selection")
write.csv(forecast(ets_SO2 ,h = 60), "ETS SO2")
write.csv(forecast(ets_CO ,h = 60), "ETS CO")
write.csv(forecast(ets_No2 ,h = 60), "ETS NO2")
write.csv(forecast(ets_O3 ,h = 60), "ETS O3")

#auto arima
#for No2
#forecasting model
farima_No2 <-auto.arima(train_No2 )
farima_No2
#model output
forecast(farima_No2 , h = 60) 
autoplot(forecast(farima_No2 , h = 60))
accuracy(forecast(farima_No2 , h = 60) , No2)
checkresiduals(farima_No2)

#for CO
#forecasting model
farima_CO <-auto.arima(train_CO )
farima_CO
#model output
forecast(farima_CO , h = 60) 
autoplot(forecast(farima_CO , h = 60))
accuracy(forecast(farima_CO, h = 60) , CO)
checkresiduals(farima_CO)

#for O3
#forecasting model
farima_O3 <-auto.arima(train_O3 )
farima_O3
#model output
forecast(farima_O3 , h = 60) 
autoplot(forecast(farima_O3 , h = 60))
accuracy(forecast(farima_O3, h = 60) , O3)
checkresiduals(farima_O3)

#for SO2
#forecasting model
farima_SO2 <-auto.arima(train_SO2 )
farima_SO2
#model output
forecast(farima_SO2 , h = 60) 
autoplot(forecast(farima_SO2 , h = 60))
accuracy(forecast(farima_SO2, h = 60) , SO2)
checkresiduals(farima_SO2)


write.csv(forecast(farima_SO2 , h = 60), "ARIMASO2")
write.csv(forecast(farima_No2 , h = 60), "ARIMANO2")
write.csv(forecast(farima_CO , h = 60) ,"ARIMACO")
write.csv(forecast(farima_O3 , h = 60), "ARIMAO3")